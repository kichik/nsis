/*
  Copyright (C) 2002 Amir Szekely <kichik@netvision.net.il>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
  claim that you wrote the original software. If you use this software
  in a product, an acknowledgment in the product documentation would be
  appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must not be
  misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.
*/

#include "DialogTemplate.h"
#include <cassert> // for assert(3)
#ifndef _WIN32
#  include <stdio.h>
#  include <stdlib.h>
#  include <iconv.h>
#  include <errno.h>
#endif

using namespace std;

//////////////////////////////////////////////////////////////////////
// Utilities
//////////////////////////////////////////////////////////////////////

#define ALIGN(dwToAlign, dwAlignOn) dwToAlign = (dwToAlign%dwAlignOn == 0) ? dwToAlign : dwToAlign - (dwToAlign%dwAlignOn) + dwAlignOn

// Reads a variany length array from seeker into readInto and advances seeker
void ReadVarLenArr(LPBYTE &seeker, char* &readInto, unsigned int uCodePage) {
  WORD* arr = (WORD*)seeker;
  switch (arr[0]) {
  case 0x0000:
    readInto = 0;
    seeker += sizeof(WORD);
    break;
  case 0xFFFF:
    readInto = MAKEINTRESOURCE(arr[1]);
    seeker += 2*sizeof(WORD);
    break;
  default:
    {
      int iStrLen = WideCharToMultiByte(uCodePage, 0, (WCHAR*)arr, -1, 0, 0, 0, 0);
      if (iStrLen)
      {
        readInto = new char[iStrLen];
        if (!WideCharToMultiByte(uCodePage, 0, (WCHAR*)arr, -1, readInto, iStrLen, 0, 0))
        {
          delete [] readInto;
          throw runtime_error("ReadVarLenArr - Unicode conversion failed.");
        }
      }
      else
      {
        throw runtime_error("ReadVarLenArr - Unicode conversion failed.");
      }

      PWCHAR wseeker = PWCHAR(seeker);
      while (*wseeker++);
      seeker = LPBYTE(wseeker);
    }
    break;
  }
}

// A macro that writes a given string (that can be a number too) into the buffer
#define WriteStringOrId(x) \
  if (x) \
    if (IS_INTRESOURCE(x)) { \
      *(WORD*)seeker = 0xFFFF; \
      seeker += sizeof(WORD); \
      *(WORD*)seeker = WORD(DWORD(x)); \
      seeker += sizeof(WORD); \
    } \
    else { \
      int us = MultiByteToWideChar(m_uCodePage, 0, x, -1, (WCHAR*)seeker, dwSize - DWORD(seeker - pbDlg)); \
      if (!us) { \
        throw runtime_error("WriteStringOrId - Unicode conversion failed."); \
      } \
      seeker += us*sizeof(WCHAR); \
    } \
  else \
    seeker += sizeof(WORD);

// A macro that adds the size of x (which can be a string a number, or nothing) to dwSize
#define AddStringOrIdSize(x) dwSize += x ? (IS_INTRESOURCE(x) ? sizeof(DWORD) : MultiByteToWideChar(m_uCodePage, 0, x, -1, 0, 0) * sizeof(WCHAR)) : sizeof(WORD)

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CDialogTemplate::CDialogTemplate(BYTE* pbData, unsigned int uCodePage) {
  m_uCodePage = uCodePage;

  m_dwHelpId = 0;
  m_szClass = 0;
  m_szFont = 0;
  m_sFontSize = 0;
  m_sFontWeight = 0;
  m_bItalic = 0;
  m_bCharset = 0;
  m_szMenu = 0;
  m_szTitle = 0;

  WORD wItems = 0;

  if (*(DWORD*)pbData == EXTENDED_DIALOG) { // Extended dialog template signature
    m_bExtended = true;

    DLGTEMPLATEEX* dTemplateEx = (DLGTEMPLATEEX*)pbData;

    m_dwHelpId = dTemplateEx->helpID;
    m_dwStyle = dTemplateEx->style;
    m_dwExtStyle = dTemplateEx->exStyle;
    m_sX = dTemplateEx->x;
    m_sY = dTemplateEx->y;
    m_sWidth = dTemplateEx->cx;
    m_sHeight = dTemplateEx->cy;

    wItems = dTemplateEx->cDlgItems;
  }
  else {
    m_bExtended = false;

    DLGTEMPLATE* dTemplate = (DLGTEMPLATE*)pbData;

    m_dwStyle = dTemplate->style;
    m_dwExtStyle = dTemplate->dwExtendedStyle;
    m_sX = dTemplate->x;
    m_sY = dTemplate->y;
    m_sWidth = dTemplate->cx;
    m_sHeight = dTemplate->cy;

    wItems = dTemplate->cdit;
  }

  BYTE* seeker = pbData + (m_bExtended ? sizeof(DLGTEMPLATEEX) : sizeof(DLGTEMPLATE));

  // Read menu variant length array
  ReadVarLenArr(seeker, m_szMenu, m_uCodePage);
  // Read class variant length array
  ReadVarLenArr(seeker, m_szClass, m_uCodePage);
  // Read title variant length array
  ReadVarLenArr(seeker, m_szTitle, m_uCodePage);
  // Read font size and variant length array (only if style DS_SETFONT is used!)
  if (m_dwStyle & DS_SETFONT) {
    m_sFontSize = *(short*)seeker;
    seeker += sizeof(short);
    if (m_bExtended) {
      m_sFontWeight = *(short*)seeker;
      seeker += sizeof(short);
      m_bItalic = *(BYTE*)seeker;
      seeker += sizeof(BYTE);
      m_bCharset = *(BYTE*)seeker;
      seeker += sizeof(BYTE);
    }
    ReadVarLenArr(seeker, m_szFont, m_uCodePage);
  }

  // Read items
  for (int i = 0; i < wItems; i++) {
    // DLGITEMTEMPLATE[EX]s must be aligned on DWORD boundry
    if (DWORD(seeker - pbData) % sizeof(DWORD))
      seeker += sizeof(WORD);

    DialogItemTemplate* item = new DialogItemTemplate;
    ZeroMemory(item, sizeof(DialogItemTemplate));

    if (m_bExtended) {
      DLGITEMTEMPLATEEX* rawItem = (DLGITEMTEMPLATEEX*)seeker;

      item->dwHelpId = rawItem->helpID;
      item->dwStyle = rawItem->style;
      item->dwExtStyle = rawItem->exStyle;
      item->sX = rawItem->x;
      item->sY = rawItem->y;
      item->sWidth = rawItem->cx;
      item->sHeight = rawItem->cy;
      item->wId = rawItem->id;

      seeker += sizeof(DLGITEMTEMPLATEEX);
    }
    else {
      DLGITEMTEMPLATE* rawItem = (DLGITEMTEMPLATE*)seeker;

      item->dwHelpId = 0;
      item->dwStyle = rawItem->style;
      item->dwExtStyle = rawItem->dwExtendedStyle;
      item->sX = rawItem->x;
      item->sY = rawItem->y;
      item->sWidth = rawItem->cx;
      item->sHeight = rawItem->cy;
      item->wId = rawItem->id;

      seeker += sizeof(DLGITEMTEMPLATE);
    }

    // Read class variant length array
    ReadVarLenArr(seeker, item->szClass, m_uCodePage);
    // Read title variant length array
    ReadVarLenArr(seeker, item->szTitle, m_uCodePage);

    // Read creation data variant length array
    // First read the size of the array (no null termination)
    item->wCreateDataSize = *(WORD*)seeker;
    seeker += sizeof(WORD);
    // Then read the array it self (if size is not 0)
    if (item->wCreateDataSize) {
      item->wCreateDataSize -= sizeof(WORD); // Size includes size field itself...
      item->szCreationData = new char[item->wCreateDataSize];
      CopyMemory(item->szCreationData, seeker, item->wCreateDataSize);
      seeker += item->wCreateDataSize;
    }

    // Add the item to the vector
    m_vItems.push_back(item);
  }
}

CDialogTemplate::~CDialogTemplate() {
  if (m_szMenu && !IS_INTRESOURCE(m_szMenu))
    delete [] m_szMenu;
  if (m_szClass && !IS_INTRESOURCE(m_szClass))
    delete [] m_szClass;
  if (m_szTitle)
    delete [] m_szTitle;
  if (m_szFont)
    delete [] m_szFont;

  for (unsigned int i = 0; i < m_vItems.size(); i++) {
    if (m_vItems[i]->szClass && !IS_INTRESOURCE(m_vItems[i]->szClass))
      delete [] m_vItems[i]->szClass;
    if (m_vItems[i]->szTitle && !IS_INTRESOURCE(m_vItems[i]->szTitle))
      delete [] m_vItems[i]->szTitle;
    if (m_vItems[i]->szCreationData)
      delete [] m_vItems[i]->szCreationData;
  }
}

//////////////////////////////////////////////////////////////////////
// Methods
//////////////////////////////////////////////////////////////////////

// Returns the width of the dialog
short CDialogTemplate::GetWidth() {
  return m_sWidth;
}

// Returns the height of the dialog
short CDialogTemplate::GetHeight() {
  return m_sHeight;
}

// Returns info about the item with the id wId
DialogItemTemplate* CDialogTemplate::GetItem(WORD wId) {
  for (unsigned int i = 0; i < m_vItems.size(); i++)
    if (m_vItems[i]->wId == wId)
      return m_vItems[i];
  return 0;
}

// Returns info about the item with the indexed i
DialogItemTemplate* CDialogTemplate::GetItemByIdx(DWORD i) {
  if (i >= m_vItems.size()) return 0;
  return m_vItems[i];
}

// Removes an item
// Returns 1 if removed, 0 otherwise
int CDialogTemplate::RemoveItem(WORD wId) {
  for (unsigned int i = 0; i < m_vItems.size(); i++) {
    if (m_vItems[i]->wId == wId) {
      m_vItems.erase(m_vItems.begin() + i);
      return 1;
    }
  }
  return 0;
}

// Sets the font of the dialog
void CDialogTemplate::SetFont(char* szFaceName, WORD wFontSize) {
  if (strcmp(szFaceName, "MS Shell Dlg")) {
     // not MS Shell Dlg
    m_dwStyle &= ~DS_SHELLFONT;
  }
  else {
    // MS Shell Dlg
    m_dwStyle |= DS_SHELLFONT;
  }
  m_bCharset = DEFAULT_CHARSET;
  m_dwStyle |= DS_SETFONT;
  if (m_szFont) delete [] m_szFont;
  m_szFont = new char[strlen(szFaceName)+1];
  strcpy(m_szFont, szFaceName);
  m_sFontSize = wFontSize;
}

// Adds an item to the dialog
void CDialogTemplate::AddItem(DialogItemTemplate item) {
  DialogItemTemplate* newItem = new DialogItemTemplate;
  CopyMemory(newItem, &item, sizeof(DialogItemTemplate));

  if (item.szClass && !IS_INTRESOURCE(item.szClass)) {
    newItem->szClass = new char[strlen(item.szClass)+1];
    strcpy(newItem->szClass, item.szClass);
  }
  if (item.szTitle && !IS_INTRESOURCE(item.szTitle)) {
    newItem->szTitle = new char[strlen(item.szTitle)+1];
    strcpy(newItem->szTitle, item.szTitle);
  }
  if (item.wCreateDataSize) {
    newItem->szCreationData = new char[item.wCreateDataSize];
    memcpy(newItem->szCreationData, item.szCreationData, item.wCreateDataSize);
  }
  m_vItems.push_back(newItem);
}

// Moves all of the items in the dialog by (x,y)
void CDialogTemplate::MoveAll(short x, short y) {
  for (unsigned int i = 0; i < m_vItems.size(); i++) {
    m_vItems[i]->sX += x;
    m_vItems[i]->sY += y;
  }
}

// Resizes the dialog by (x,y)
void CDialogTemplate::Resize(short x, short y) {
  m_sWidth += x;
  m_sHeight += y;
}

#ifdef _WIN32
// Creates a dummy dialog that is used for converting units
HWND CDialogTemplate::CreateDummyDialog() {
  DWORD dwTemp;
  BYTE* pbDlg = Save(dwTemp);
  HWND hDlg = CreateDialogIndirect(GetModuleHandle(0), (DLGTEMPLATE*)pbDlg, 0, 0);
  delete [] pbDlg;
  if (!hDlg)
    throw runtime_error("Can't create dialog from template!");

  return hDlg;
}

// Converts pixels to this dialog's units
void CDialogTemplate::PixelsToDlgUnits(short& x, short& y) {
  HWND hDlg = CreateDummyDialog();
  RECT r = {0, 0, 10000, 10000};
  MapDialogRect(hDlg, &r);
  DestroyWindow(hDlg);

  x = short(float(x) / (float(r.right)/10000));
  y = short(float(y) / (float(r.bottom)/10000));
}

// Converts pixels to this dialog's units
void CDialogTemplate::DlgUnitsToPixels(short& x, short& y) {
  HWND hDlg = CreateDummyDialog();
  RECT r = {0, 0, 10000, 10000};
  MapDialogRect(hDlg, &r);
  DestroyWindow(hDlg);

  x = short(float(x) * (float(r.right)/10000));
  y = short(float(y) * (float(r.bottom)/10000));
}

// Returns the size of a string in the dialog (in dialog units)
SIZE CDialogTemplate::GetStringSize(WORD id, char *str) {
  HWND hDlg = CreateDummyDialog();

  LOGFONT f;
  GetObject((HFONT)SendMessage(hDlg, WM_GETFONT, 0, 0), sizeof(LOGFONT), &f);

  HDC memDC = CreateCompatibleDC(GetDC(hDlg));
  HFONT font = CreateFontIndirect(&f);
  SelectObject(memDC, font);

  SIZE size;
  GetTextExtentPoint32(memDC, str, strlen(str), &size);

  DestroyWindow(hDlg);
  DeleteObject(font);
  DeleteDC(memDC);

  PixelsToDlgUnits((short&)size.cx, (short&)size.cy);
  
  return size;
}

// Trims the right margins of a control to fit a given text string size.
void CDialogTemplate::RTrimToString(WORD id, char *str, int margins) {
  DialogItemTemplate* item = GetItem(id);
  if (!item) return;

  SIZE size = GetStringSize(id, str);

  size.cx += margins;
  size.cy += 2;

  item->sWidth = short(size.cx);
  item->sHeight = short(size.cy);
}

// Trims the left margins of a control to fit a given text string size.
void CDialogTemplate::LTrimToString(WORD id, char *str, int margins) {
  DialogItemTemplate* item = GetItem(id);
  if (!item) return;

  SIZE size = GetStringSize(id, str);

  size.cx += margins;
  size.cy += 2;

  item->sX += item->sWidth - short(size.cx);
  item->sWidth = short(size.cx);
  item->sHeight = short(size.cy);
}

// Trims the left and right margins of a control to fit a given text string size.
void CDialogTemplate::CTrimToString(WORD id, char *str, int margins) {
  DialogItemTemplate* item = GetItem(id);
  if (!item) return;

  SIZE size = GetStringSize(id, str);

  size.cx += margins;
  size.cy += 2;

  item->sX += item->sWidth/2 - short(size.cx/2);
  item->sWidth = short(size.cx);
  item->sHeight = short(size.cy);
}
#endif

// Moves every item right and gives it the WS_EX_RIGHT extended style
void CDialogTemplate::ConvertToRTL() {
  for (unsigned int i = 0; i < m_vItems.size(); i++) {
    bool addExStyle = false;

    // Button
    if (long(m_vItems[i]->szClass) == 0x80) {
      m_vItems[i]->dwStyle ^= BS_LEFTTEXT;
      m_vItems[i]->dwStyle ^= BS_RIGHT;
      m_vItems[i]->dwStyle ^= BS_LEFT;

      if ((m_vItems[i]->dwStyle & (BS_LEFT|BS_RIGHT)) == (BS_LEFT|BS_RIGHT)) {
        m_vItems[i]->dwStyle ^= BS_LEFT;
        m_vItems[i]->dwStyle ^= BS_RIGHT;
        if (m_vItems[i]->dwStyle & (BS_RADIOBUTTON|BS_CHECKBOX|BS_USERBUTTON)) {
          m_vItems[i]->dwStyle |= BS_RIGHT;
        }
      }
    }
    // Edit
    else if (long(m_vItems[i]->szClass) == 0x81) {
      if ((m_vItems[i]->dwStyle & ES_CENTER) == 0) {
        m_vItems[i]->dwStyle ^= ES_RIGHT;
      }
    }
    // Static
    else if (long(m_vItems[i]->szClass) == 0x82) {
      if ((m_vItems[i]->dwStyle & SS_TYPEMASK) == SS_LEFT || (m_vItems[i]->dwStyle & SS_TYPEMASK) == SS_LEFTNOWORDWRAP)
      {
        m_vItems[i]->dwStyle &= ~SS_TYPEMASK;
        m_vItems[i]->dwStyle |= SS_RIGHT;
      }
      else if ((m_vItems[i]->dwStyle & SS_TYPEMASK) == SS_ICON) {
        m_vItems[i]->dwStyle |= SS_CENTERIMAGE;
      }
    }
    else if (!IS_INTRESOURCE(m_vItems[i]->szClass) && !stricmp(m_vItems[i]->szClass, "RichEdit20A")) {
      if ((m_vItems[i]->dwStyle & ES_CENTER) == 0) {
        m_vItems[i]->dwStyle ^= ES_RIGHT;
      }
    }
    else if (!IS_INTRESOURCE(m_vItems[i]->szClass) && !stricmp(m_vItems[i]->szClass, "SysTreeView32")) {
      m_vItems[i]->dwStyle |= TVS_RTLREADING;
      addExStyle = true;
    }
    else addExStyle = true;

    if (addExStyle)
      m_vItems[i]->dwExtStyle |= WS_EX_RIGHT;

    m_vItems[i]->dwExtStyle |= WS_EX_RTLREADING;

    m_vItems[i]->sX = m_sWidth - m_vItems[i]->sWidth - m_vItems[i]->sX;
  }
  m_dwExtStyle |= WS_EX_RIGHT | WS_EX_RTLREADING;
}

// Saves the dialog in the form of DLGTEMPLATE[EX]
BYTE* CDialogTemplate::Save(DWORD& dwSize) {
  // We need the size first to know how much memory to allocate
  dwSize = GetSize();
  BYTE* pbDlg = new BYTE[dwSize];
  ZeroMemory(pbDlg, dwSize);
  BYTE* seeker = pbDlg;

  if (m_bExtended) {
    DLGTEMPLATEEX dh = {
      0x0001,
      0xFFFF,
      m_dwHelpId,
      m_dwExtStyle,
      m_dwStyle,
      m_vItems.size(),
      m_sX,
      m_sY,
      m_sWidth,
      m_sHeight
    };

    CopyMemory(seeker, &dh, sizeof(DLGTEMPLATEEX));
    seeker += sizeof(DLGTEMPLATEEX);
  }
  else {
    DLGTEMPLATE dh = {
      m_dwStyle,
      m_dwExtStyle,
      m_vItems.size(),
      m_sX,
      m_sY,
      m_sWidth,
      m_sHeight
    };

    CopyMemory(seeker, &dh, sizeof(DLGTEMPLATE));
    seeker += sizeof(DLGTEMPLATE);
  }

  // Write menu variant length array
  WriteStringOrId(m_szMenu);
  // Write class variant length array
  WriteStringOrId(m_szClass);
  // Write title variant length array
  WriteStringOrId(m_szTitle);

  // Write font variant length array, size, and extended info (if needed)
  if (m_dwStyle & DS_SETFONT) {
    *(short*)seeker = m_sFontSize;
    seeker += sizeof(short);
    if (m_bExtended) {
      *(short*)seeker = m_sFontWeight;
      seeker += sizeof(short);
      *(BYTE*)seeker = m_bItalic;
      seeker += sizeof(BYTE);
      *(BYTE*)seeker = m_bCharset;
      seeker += sizeof(BYTE);
    }
    WriteStringOrId(m_szFont);
  }

  // Write all of the items
  for (unsigned int i = 0; i < m_vItems.size(); i++) {
    // DLGITEMTEMPLATE[EX]s must be aligned on DWORD boundry
    if (DWORD(seeker - pbDlg) % sizeof(DWORD))
      seeker += sizeof(WORD);

    if (m_bExtended) {
      DLGITEMTEMPLATEEX dih = {
        m_vItems[i]->dwHelpId,
        m_vItems[i]->dwExtStyle,
        m_vItems[i]->dwStyle,
        m_vItems[i]->sX,
        m_vItems[i]->sY,
        m_vItems[i]->sWidth,
        m_vItems[i]->sHeight,
        m_vItems[i]->wId
      };

      CopyMemory(seeker, &dih, sizeof(DLGITEMTEMPLATEEX));
      seeker += sizeof(DLGITEMTEMPLATEEX);
    }
    else {
      DLGITEMTEMPLATE dih = {
        m_vItems[i]->dwStyle,
        m_vItems[i]->dwExtStyle,
        m_vItems[i]->sX,
        m_vItems[i]->sY,
        m_vItems[i]->sWidth,
        m_vItems[i]->sHeight,
        m_vItems[i]->wId
      };

      CopyMemory(seeker, &dih, sizeof(DLGITEMTEMPLATE));
      seeker += sizeof(DLGITEMTEMPLATE);
    }

    // Write class variant length array
    WriteStringOrId(m_vItems[i]->szClass);
    // Write title variant length array
    WriteStringOrId(m_vItems[i]->szTitle);

    // Write creation data variant length array
    // First write its size
    WORD wCreateDataSize = m_vItems[i]->wCreateDataSize;
    if (m_vItems[i]->wCreateDataSize) wCreateDataSize += sizeof(WORD);
    *(WORD*)seeker = wCreateDataSize;
    seeker += sizeof(WORD);
    // If size is nonzero write the data too
    if (m_vItems[i]->wCreateDataSize) {
      CopyMemory(seeker, m_vItems[i]->szCreationData, m_vItems[i]->wCreateDataSize);
      seeker += m_vItems[i]->wCreateDataSize;
    }
  }

  assert(seeker - pbDlg == dwSize);

  // DONE!
  return pbDlg;
}

// Returns the size that the DLGTEMPLATE[EX] will take when saved
DWORD CDialogTemplate::GetSize() {
  DWORD dwSize = m_bExtended ? sizeof(DLGTEMPLATEEX) : sizeof(DLGTEMPLATE);

  // Menu
  AddStringOrIdSize(m_szMenu);
  // Class
  AddStringOrIdSize(m_szClass);
  // Title
  AddStringOrIdSize(m_szTitle);

  // Font
  if (m_dwStyle & DS_SETFONT) {
    dwSize += sizeof(WORD) + (m_bExtended ? sizeof(short) + 2*sizeof(BYTE) : 0);
    AddStringOrIdSize(m_szFont);
  }

  for (unsigned int i = 0; i < m_vItems.size(); i++) {
    // DLGITEMTEMPLATE[EX]s must be aligned on DWORD boundry
    ALIGN(dwSize, sizeof(DWORD));

    dwSize += m_bExtended ? sizeof(DLGITEMTEMPLATEEX) : sizeof(DLGITEMTEMPLATE);

    // Class
    AddStringOrIdSize(m_vItems[i]->szClass);
    // Title
    AddStringOrIdSize(m_vItems[i]->szTitle);

    dwSize += sizeof(WORD) + m_vItems[i]->wCreateDataSize;
  }

  return dwSize;
}
