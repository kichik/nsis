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

#if !defined(AFX_DIALOGTEMPLATE_H__C5A973AF_0F56_4BEC_814A_79318E2EB4AC__INCLUDED_)
#define AFX_DIALOGTEMPLATE_H__C5A973AF_0F56_4BEC_814A_79318E2EB4AC__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <Windows.h>
#include <Vector>

#include <StdExcept>
using namespace std;

#ifndef IS_INTRESOURCE
#ifndef ULONG_PTR
#define ULONG_PTR DWORD
#endif
#define IS_INTRESOURCE(_r) (((ULONG_PTR)(_r) >> 16) == 0)
#endif

struct DialogItemTemplate {
  DWORD  dwHelpId; // Extended only

  short  sX;
  short  sY;
  short  sWidth;
  short  sHeight;
  DWORD  dwExtStyle;
  DWORD  dwStyle;
  WORD   wId;

  char  *szClass;
  char  *szTitle;
  char  *szCreationData;

  WORD  wCreateDataSize;
};

#pragma pack(push, 1)

typedef struct {
  WORD   dlgVer;
  WORD   signature;
  DWORD  helpID;
  DWORD  exStyle;
  DWORD  style;
  WORD   cDlgItems;
  short  x;
  short  y;
  short  cx;
  short  cy;
} DLGTEMPLATEEX;

typedef struct {
  DWORD  helpID;
  DWORD  exStyle;
  DWORD  style;
  short  x;
  short  y;
  short  cx;
  short  cy;
  WORD   id;
  WORD   _miscrosoft_docs_are_wrong;
} DLGITEMTEMPLATEEX;

#pragma pack(pop)

class CDialogTemplate {
public:
  CDialogTemplate(BYTE* pbData, unsigned int uCodePage=CP_ACP);
  virtual ~CDialogTemplate();

  short GetWidth();
  short GetHeight();
  DialogItemTemplate* GetItem(WORD wId);
  DialogItemTemplate* GetItemByIdx(DWORD i);
  int   RemoveItem(WORD wId);
  void  SetFont(char* szFaceName, WORD wFontSize);
  void  AddItem(DialogItemTemplate item);
  HWND  CreateDummyDialog();
  void  MoveAll(short x, short y);
  void  Resize(short x, short y);
  void  PixelsToDlgUnits(short& x, short& y);
  void  DlgUnitsToPixels(short& x, short& y);
  SIZE  GetStringSize(WORD id, char *str);
  void  RTrimToString(WORD id, char *str, int margins);
  void  LTrimToString(WORD id, char *str, int margins);
  void  CTrimToString(WORD id, char *str, int margins);
  void  ConvertToRTL();
  BYTE* Save(DWORD& dwSize);
  DWORD GetSize();

private:
  bool  m_bExtended;

  DWORD m_dwHelpId; // Extended only

  short m_sX;
  short m_sY;
  short m_sWidth;
  short m_sHeight;
  DWORD m_dwExtStyle;
  DWORD m_dwStyle;

  char* m_szMenu;
  char* m_szClass;
  char* m_szTitle;

   // Only if DS_FONT style is set
  short m_sFontSize;
  short m_sFontWeight; // Extended only
  BYTE  m_bItalic; // Extended only
  BYTE  m_bCharset; // Extended only
  char* m_szFont;

  // For (en/de)coding Unicode
  unsigned int m_uCodePage;

  // Items vector
  vector<DialogItemTemplate*> m_vItems;
};

#endif // !defined(AFX_DIALOGTEMPLATE_H__C5A973AF_0F56_4BEC_814A_79318E2EB4AC__INCLUDED_)
