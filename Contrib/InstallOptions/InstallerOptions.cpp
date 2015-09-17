/*********************************************************
 *
 *  InstallOptions version 2.0 - Plugin for custom pages
 *
 *  See Readme.html for documentation and license
 *
 *  Unicode support by Jim Park -- 08/01/2007
 *
 *********************************************************/

#include <windows.h>
#include <windowsx.h>
#include <shlobj.h>
#include <commdlg.h>
#include <cderr.h>
#include <shellapi.h>
#include "resource.h"

#include <nsis/pluginapi.h> // nsis plugin

#define COUNTOF(a) (sizeof(a)/sizeof(a[0]))
#ifndef min
#include <algorithm>
#define min std::min // mingw64?
#endif

// Use for functions only called from one place to possibly reduce some code
// size.  Allows the source code to remain readable by leaving the function
// intact.
#ifdef _MSC_VER
#define INLINE __forceinline
#else
#define INLINE inline
#endif

void *WINAPI MALLOC(int len) { return (void*)GlobalAlloc(GPTR,len); }
void WINAPI FREE(void *d) { if (d) GlobalFree((HGLOBAL)d); }

#define strcpy(x,y) lstrcpy(x,y)
//#define strncpy(x,y,z) lstrcpyn(x,y,z)
#define strdup(x) STRDUP(x)
#define stricmp(x,y) lstrcmpi(x,y)
//#define abs(x) ((x) < 0 ? -(x) : (x))

TCHAR *WINAPI STRDUP(const TCHAR *c)
{
  TCHAR *t=(TCHAR*)MALLOC((lstrlen(c)+1)*sizeof(TCHAR));
  return lstrcpy(t,c);
}

// Field types
// NB - the order of this list is important - see below

#define FIELD_INVALID      (0)
#define FIELD_HLINE        (1)
#define FIELD_VLINE        (2)
#define FIELD_LABEL        (3)
#define FIELD_ICON         (4)
#define FIELD_BITMAP       (5)
#define FIELD_BROWSEBUTTON (6)
#define FIELD_LINK         (7)
#define FIELD_BUTTON       (8)
#define FIELD_GROUPBOX     (9)
#define FIELD_CHECKBOX     (10)
#define FIELD_RADIOBUTTON  (11)
#define FIELD_TEXT         (12)
#define FIELD_FILEREQUEST  (13)
#define FIELD_DIRREQUEST   (14)
#define FIELD_COMBOBOX     (15)
#define FIELD_LISTBOX      (16)

#define FIELD_SETFOCUS     FIELD_CHECKBOX // First field that qualifies for having the initial keyboard focus
#define FIELD_CHECKLEN     FIELD_TEXT     // First field to have length of state value checked against MinLen/MaxLen

//---------------------------------------------------------------------
// settings
#define IO_ENABLE_LINK

//#define IO_LINK_UNDERLINED // Uncomment to show links text underlined
//---------------------------------------------------------------------

// Flags

// LBS_NOTIFY              0x00000001 // LISTBOX/CHECKBOX/RADIOBUTTON/BUTTON/LINK - Notify NSIS script when control is "activated" (exact meaning depends on the type of control)
// OFN_OVERWRITEPROMPT     0x00000002 // FILEREQUEST
// OFN_HIDEREADONLY        0x00000004 // FILEREQUEST
// LBS_MULTIPLESEL         0x00000008 // LISTBOX
#define FLAG_READONLY      0x00000010 // TEXT/FILEREQUEST/DIRREQUEST
// BS_LEFTTEXT             0x00000020 // CHECKBOX/RADIOBUTTON
#define TRANSPARENT_BMP    0x00000020 // BITMAP
#define FLAG_PASSWORD      0x00000040 // TEXT/FILEREQUEST/DIRREQUEST
#define FLAG_ONLYNUMBERS   0x00000080 // TEXT/FILEREQUEST/DIRREQUEST
#define FLAG_MULTILINE     0x00000100 // TEXT/FILEREQUEST/DIRREQUEST
#define FLAG_NOWORDWRAP    0x00000200 // TEXT/FILEREQUEST/DIRREQUEST - Disable word-wrap in multi-line text boxes
#define FLAG_WANTRETURN    0x00000400 // TEXT/FILEREQUEST/DIRREQUEST
// LBS_EXTENDEDSEL         0x00000800 // LISTBOX
// OFN_PATHMUSTEXIST       0x00000800 // FILEREQUEST
// OFN_FILEMUSTEXIST       0x00001000 // FILEREQUEST
// OFN_CREATEPROMPT        0x00002000 // FILEREQUEST
#define FLAG_DROPLIST      0x00004000 // COMBOBOX
#define FLAG_RESIZETOFIT   0x00008000 // BITMAP
// WS_TABSTOP              0x00010000 // *ALL*
// WS_GROUP                0x00020000 // *ALL*
#define FLAG_SAVEAS        0x00040000 // FILEREQUEST - Show "Save As" instead of "Open" for FileRequest field
// OFN_EXPLORER            0x00080000 // FILEREQUEST
// WS_HSCROLL              0x00100000 // *ALL*
// WS_VSCROLL              0x00200000 // *ALL*
// WS_DISABLED             0x08000000 // *ALL*
#define FLAG_FOCUS         0x10000000 // Controls that can receive focus

struct TableEntry {
  const TCHAR *pszName;
  int   nValue;
};

int WINAPI LookupToken(TableEntry*, TCHAR*);
int WINAPI LookupTokens(TableEntry*, TCHAR*);

void WINAPI ConvertNewLines(TCHAR *str);

// all allocated buffers must be first in the struct
// when adding more allocated buffers to FieldType, don't forget to change this define
#define FIELD_BUFFERS 6
struct FieldType {
  TCHAR  *pszText;
  TCHAR  *pszState;
  TCHAR  *pszRoot;

  TCHAR  *pszListItems;
  TCHAR  *pszFilter;

  TCHAR  *pszValidateText;
  int    nMinLength;
  int    nMaxLength;

  int    nType;
  RECT   rect;

  int    nFlags;

  HWND   hwnd;
  UINT   nControlID;

  INT_PTR    nParentIdx;  // this is used to store original windowproc for LINK
  HANDLE hImage; // this is used by image/icon field to save the handle to the image

  int    nField; // field number in INI file
  const TCHAR  *pszHwndEntry; // "HWND" or "HWND2"

  WNDPROC   wndProc; 
};

// initial buffer size.  buffers will grow as required.
// use a value larger than MAX_PATH to prevent need for excessive growing.
#define BUFFER_SIZE 8192 // 8kb of mem is max char count in multiedit

TCHAR szBrowseButtonCaption[] = _T("...");

HWND hConfigWindow    = NULL;
HWND hMainWindow      = NULL;
HWND hCancelButton    = NULL;
HWND hNextButton      = NULL;
HWND hBackButton      = NULL;

HINSTANCE m_hInstance = NULL;

struct _stack_t *pFilenameStackEntry = NULL;

TCHAR *pszFilename         = NULL;
TCHAR *pszTitle            = NULL;
TCHAR *pszCancelButtonText = NULL;
TCHAR *pszNextButtonText   = NULL;
TCHAR *pszBackButtonText   = NULL;

int bBackEnabled   = FALSE;
int bCancelEnabled = FALSE;   // by ORTIM: 13-August-2002
int bCancelShow    = FALSE;   // by ORTIM: 13-August-2002

int bRTL = FALSE;

FieldType *pFields   = NULL;
#define DEFAULT_RECT 1018
int nRectId          = 0;
int nNumFields       = 0;
int g_done;
int g_NotifyField;  // Field number of notifying control

int WINAPI FindControlIdx(UINT id)
{
  for (int nIdx = 0; nIdx < nNumFields; nIdx++)
    if (id == pFields[nIdx].nControlID)
      return nIdx;
  return -1;
}

LRESULT WINAPI mySendMessage(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
  return SendMessage(hWnd, Msg, wParam, lParam);
}

void WINAPI mySetFocus(HWND hWnd)
{
  mySendMessage(hMainWindow, WM_NEXTDLGCTL, (WPARAM)hWnd, TRUE);
}

void WINAPI mySetWindowText(HWND hWnd, LPCTSTR pszText)
{
  if (pszText)
    SetWindowText(hWnd, pszText);
}

static COLORREF GetLinkColor()
{
  COLORREF clr = GetSysColor(COLOR_HOTLIGHT);
#ifndef _WIN64
  // COLOR_HOTLIGHT is Win98/2000+. GetSysColorBrush is the correct way to
  // detect valid colors but here we just assume nobody uses black.
  if (!clr) clr = RGB(0,0,255);
#endif
  return clr;
}

int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lp, LPARAM pData) {
  static TCHAR szDir[MAX_PATH];

  if (uMsg == BFFM_INITIALIZED &&
      GetWindowText(pFields[(int)pData].hwnd, szDir, MAX_PATH) > 0)
    mySendMessage(hwnd, BFFM_SETSELECTION, TRUE, (LPARAM)szDir);
  return 0;
}


bool INLINE ValidateFields() {
  int nIdx;
  INT_PTR nLength;

  // In the unlikely event we can't allocate memory, go ahead and return true so we can get out of here.
  // May cause problems for the install script, but no memory is problems for us.
  for (nIdx = 0; nIdx < nNumFields; nIdx++) {
    FieldType *pField = pFields + nIdx;
    // this if statement prevents a stupid bug where a min/max length is assigned to a label control
    // where the user obviously has no way of changing what is displayed. (can you say, "infinite loop"?)
    if (pField->nType >= FIELD_CHECKLEN) {
      nLength = mySendMessage(pField->hwnd, WM_GETTEXTLENGTH, 0, 0);

      if (((pField->nMaxLength > 0) && (nLength > pField->nMaxLength)) ||
         ((pField->nMinLength > 0) && (nLength < pField->nMinLength))) {
        if (pField->pszValidateText) {
          TCHAR szTitle[1024];
          GetWindowText(hMainWindow, szTitle, COUNTOF(szTitle));
          MessageBox(hConfigWindow, pField->pszValidateText, szTitle, MB_OK|MB_ICONWARNING);
        }
        mySetFocus(pField->hwnd);
        return false;
      }

    }
  }
  return true;
}

bool WINAPI SaveSettings(void) {
  static TCHAR szField[25];
  int nBufLen = BUFFER_SIZE;
  TCHAR *pszBuffer = (TCHAR*)MALLOC(nBufLen*sizeof(TCHAR));
  if (!pszBuffer) return false;

  int nIdx;
  int CurrField;
  for (nIdx = 0, CurrField = 1; nIdx < nNumFields; nIdx++, CurrField++) {
    FieldType *pField = pFields + nIdx;
    HWND hwnd = pField->hwnd;
    switch (pField->nType) {
      case FIELD_BROWSEBUTTON:
        if (g_NotifyField > CurrField)
          --g_NotifyField;
        --CurrField;
      default:
        continue;

      case FIELD_CHECKBOX:
      case FIELD_RADIOBUTTON:
        wsprintf(pszBuffer, _T("%d"), !!mySendMessage(hwnd, BM_GETCHECK, 0, 0));
        break;

      case FIELD_LISTBOX:
      {
        // Ok, this one requires a bit of work.
        // First, we allocate a buffer long enough to hold every item.
        // Then, we loop through every item and if it's selected we add it to our buffer.
        // If there is already an item in the list, then we prepend a | character before the new item.
        // We could simplify for single-select boxes, but using one piece of code saves some space.
        int nLength = lstrlen(pField->pszListItems) + 10;
        if (nLength > nBufLen) {
          FREE(pszBuffer);
          nBufLen = nLength;
          pszBuffer = (TCHAR*)MALLOC(nBufLen*sizeof(TCHAR));
          if (!pszBuffer) return false;
        }
        TCHAR *pszItem = (TCHAR*)MALLOC(nBufLen*sizeof(TCHAR));
        if (!pszItem) return false;

        *pszBuffer = _T('\0');
        INT_PTR nNumItems = mySendMessage(hwnd, LB_GETCOUNT, 0, 0);
        for (int nIdx2 = 0; nIdx2 < nNumItems; nIdx2++) {
          if (mySendMessage(hwnd, LB_GETSEL, nIdx2, 0) > 0) {
            if (*pszBuffer) lstrcat(pszBuffer, _T("|"));
            mySendMessage(hwnd, LB_GETTEXT, (WPARAM)nIdx2, (LPARAM)pszItem);
            lstrcat(pszBuffer, pszItem);
          }
        }

        FREE(pszItem);
        break;
      }

      case FIELD_TEXT:
      case FIELD_FILEREQUEST:
      case FIELD_DIRREQUEST:
      case FIELD_COMBOBOX:
      {
        int nLength = (int) mySendMessage(pField->hwnd, WM_GETTEXTLENGTH, 0, 0);
        if (nLength > nBufLen) {
          FREE(pszBuffer);
          // add a bit extra so we do this less often
          nBufLen = nLength + 20;
          pszBuffer = (TCHAR*)MALLOC(nBufLen*sizeof(TCHAR));
          if (!pszBuffer) return false;
        }
        *pszBuffer=_T('"');
        GetWindowText(hwnd, pszBuffer+1, nBufLen-1);
        pszBuffer[nLength+1]=_T('"');
        pszBuffer[nLength+2]=_T('\0');

        if (pField->nType == FIELD_TEXT && (pField->nFlags & FLAG_MULTILINE))
        {
          TCHAR *pszBuf2 = (TCHAR*)MALLOC(nBufLen*2*sizeof(TCHAR)); // double the size, consider the worst case, all chars are \r\n
          TCHAR *p1, *p2;
          for (p1 = pszBuffer, p2 = pszBuf2; *p1; p1 = CharNext(p1), p2 = CharNext(p2))
          {
            switch (*p1) {
              case _T('\t'):
                *p2++ = _T('\\');
                *p2   = _T('t');
                break;
              case _T('\n'):
                *p2++ = _T('\\');
                *p2   = _T('n');
                break;
              case _T('\r'):
                *p2++ = _T('\\');
                *p2   = _T('n');
                break;
              case _T('\\'):
                *p2 = _T('\\');
                // Jim Park: used to be p2++ but that's a bug that works because
                // CharNext()'s behavior at terminating null char.  But still
                // definitely, unsafe.
              default:
                lstrcpyn(p2, p1, (int)(CharNext(p1) - p1) + 1);
                break;
            }
          }
          *p2 = 0;
          nBufLen = nBufLen*2;
          FREE(pszBuffer);
          pszBuffer=pszBuf2;
        }
        break;
      }
    }
    wsprintf(szField, _T("Field %d"), CurrField);
    WritePrivateProfileString(szField, _T("State"), pszBuffer, pszFilename);
  }

  // Tell NSIS which control was activated, if any
  wsprintf(pszBuffer, _T("%d"), g_NotifyField);
  WritePrivateProfileString(_T("Settings"), _T("State"), pszBuffer, pszFilename);

  FREE(pszBuffer);

  return true;
}

#define BROWSE_WIDTH 15

static TCHAR szResult[BUFFER_SIZE];
const TCHAR *pszAppName;

DWORD WINAPI myGetProfileString(LPCTSTR lpKeyName)
{
  *szResult = _T('\0');
  return GetPrivateProfileString(pszAppName, lpKeyName, _T(""), szResult, BUFFER_SIZE, pszFilename);
}

TCHAR * WINAPI myGetProfileStringDup(LPCTSTR lpKeyName)
{
  int nSize = myGetProfileString(lpKeyName);
  if (nSize)
    return strdup(szResult);  // uses STRDUP
  else
    return NULL;
}

UINT WINAPI myGetProfileInt(LPCTSTR lpKeyName, INT nDefault)
{
  return GetPrivateProfileInt(pszAppName, lpKeyName, nDefault, pszFilename);
}

int WINAPI ReadSettings(void) {
  static TCHAR szField[25];
  int nIdx, nCtrlIdx;

  pszAppName = _T("Settings");
  pszTitle = myGetProfileStringDup(_T("Title"));
  pszCancelButtonText = myGetProfileStringDup(_T("CancelButtonText"));
  pszNextButtonText = myGetProfileStringDup(_T("NextButtonText"));
  pszBackButtonText = myGetProfileStringDup(_T("BackButtonText"));

  nNumFields = myGetProfileInt(_T("NumFields"), 0);

  nRectId = myGetProfileInt(_T("Rect"), DEFAULT_RECT);

  bBackEnabled = myGetProfileInt(_T("BackEnabled"), -1);
  // by ORTIM: 13-August-2002
  bCancelEnabled = myGetProfileInt(_T("CancelEnabled"), -1);
  bCancelShow = myGetProfileInt(_T("CancelShow"), -1);

  bRTL = myGetProfileInt(_T("RTL"), 0);

  if (nNumFields > 0) {
    // make this twice as large for the worst case that every control is a browse button.
    // the structure is small enough that this won't waste much memory.
    // if the structure gets much larger, we should switch to a linked list.
    pFields = (FieldType *)MALLOC(sizeof(FieldType)*2*nNumFields);
  }

  for (nIdx = 0, nCtrlIdx = 0; nCtrlIdx < nNumFields; nCtrlIdx++, nIdx++) {
    // Control types
    static TableEntry TypeTable[] = {
      { _T("LABEL"),       FIELD_LABEL       },
      { _T("TEXT"),        FIELD_TEXT        },
      { _T("PASSWORD"),    FIELD_TEXT        },
      { _T("LISTBOX"),     FIELD_LISTBOX     },
      { _T("COMBOBOX"),    FIELD_COMBOBOX    },
      { _T("DROPLIST"),    FIELD_COMBOBOX    },
      { _T("FILEREQUEST"), FIELD_FILEREQUEST },
      { _T("DIRREQUEST"),  FIELD_DIRREQUEST  },
      { _T("CHECKBOX"),    FIELD_CHECKBOX    },
      { _T("RADIOBUTTON"), FIELD_RADIOBUTTON },
      { _T("ICON"),        FIELD_ICON        },
      { _T("BITMAP"),      FIELD_BITMAP      },
      { _T("GROUPBOX"),    FIELD_GROUPBOX    },
#ifdef IO_ENABLE_LINK
      { _T("LINK"),        FIELD_LINK        },
#else
      { _T("LINK"),        FIELD_LABEL       },
#endif
      { _T("BUTTON"),      FIELD_BUTTON      },
      { _T("HLINE"),       FIELD_HLINE       },
      { _T("VLINE"),       FIELD_VLINE       },
      { NULL,              0                 }
    };
    // Control flags
    static TableEntry FlagTable[] = {
      { _T("NOTIFY"),            LBS_NOTIFY          },
      { _T("WARN_IF_EXIST"),     OFN_OVERWRITEPROMPT },
      { _T("FILE_HIDEREADONLY"), OFN_HIDEREADONLY    },
      { _T("MULTISELECT"),       LBS_MULTIPLESEL     },
      { _T("READONLY"),          FLAG_READONLY       },
      { _T("RIGHT"),             BS_LEFTTEXT         },
      { _T("PASSWORD"),          FLAG_PASSWORD       },
      { _T("ONLY_NUMBERS"),      FLAG_ONLYNUMBERS    },
      { _T("MULTILINE"),         FLAG_MULTILINE      },
      { _T("NOWORDWRAP"),        FLAG_NOWORDWRAP     },
      { _T("WANTRETURN"),        FLAG_WANTRETURN     },
      { _T("EXTENDEDSELCT"),     LBS_EXTENDEDSEL     },
      { _T("PATH_MUST_EXIST"),   OFN_PATHMUSTEXIST   },
      { _T("FILE_MUST_EXIST"),   OFN_FILEMUSTEXIST   },
      { _T("PROMPT_CREATE"),     OFN_CREATEPROMPT    },
      { _T("DROPLIST"),          FLAG_DROPLIST       },
      { _T("RESIZETOFIT"),       FLAG_RESIZETOFIT    },
      { _T("NOTABSTOP"),         WS_TABSTOP          },
      { _T("GROUP"),             WS_GROUP            },
      { _T("REQ_SAVE"),          FLAG_SAVEAS         },
      { _T("FILE_EXPLORER"),     OFN_EXPLORER        },
      { _T("HSCROLL"),           WS_HSCROLL          },
      { _T("VSCROLL"),           WS_VSCROLL          },
      { _T("DISABLED"),          WS_DISABLED         },
      { _T("TRANSPARENT"),       TRANSPARENT_BMP     },
      { _T("FOCUS"),             FLAG_FOCUS          },
      { NULL,                    0                   }
    };
    FieldType *pField = pFields + nIdx;

    pField->nField = nCtrlIdx + 1;
    pField->pszHwndEntry = _T("HWND");

    wsprintf(szField, _T("Field %d"), nCtrlIdx + 1);
    pszAppName = szField;

    // Get the control type
    myGetProfileString(_T("TYPE"));
    pField->nType = LookupToken(TypeTable, szResult);
    if (pField->nType == FIELD_INVALID)
      continue;

    // Lookup flags associated with the control type
    pField->nFlags = LookupToken(FlagTable, szResult);
    myGetProfileString(_T("Flags"));
    pField->nFlags |= LookupTokens(FlagTable, szResult);

    // pszState must not be NULL!
    myGetProfileString(_T("State"));
    pField->pszState = strdup(szResult); // uses STRDUP

    // ListBox items list
    {
      int nResult = myGetProfileString(_T("ListItems"));
      if (nResult) {
        // add an extra | character to the end to simplify the loop where we add the items.
        pField->pszListItems = (TCHAR*)MALLOC((nResult + 2)*sizeof(TCHAR));
        lstrcpy(pField->pszListItems, szResult);
        pField->pszListItems[nResult] = _T('|');
        pField->pszListItems[nResult + 1] = _T('\0');
      }
    }

    // Label Text - convert newline
    pField->pszText = myGetProfileStringDup(_T("TEXT"));
    if (pField->nType == FIELD_LABEL || pField->nType == FIELD_LINK)
      ConvertNewLines(pField->pszText);

    // Dir request - root folder
    pField->pszRoot = myGetProfileStringDup(_T("ROOT"));

    // ValidateText - convert newline
    pField->pszValidateText = myGetProfileStringDup(_T("ValidateText"));
    ConvertNewLines(pField->pszValidateText);

    {
      int nResult = GetPrivateProfileString(szField, _T("Filter"), _T("All Files|*.*"), szResult, COUNTOF(szResult), pszFilename);
      if (nResult) {
        // Convert the filter to the format required by Windows: NULL after each
        // item followed by a terminating NULL
        pField->pszFilter = (TCHAR*)MALLOC((nResult + 2)*sizeof(TCHAR));
        lstrcpy(pField->pszFilter, szResult);
        TCHAR *pszPos = pField->pszFilter;
        while (*pszPos)
        {
          if (*pszPos == _T('|'))
            *pszPos++ = 0;
          else
            pszPos = CharNext(pszPos);
        }
      }
    }

    pField->rect.left = myGetProfileInt(_T("LEFT"), 0);
    pField->rect.top = myGetProfileInt(_T("TOP"), 0);
    pField->rect.right = myGetProfileInt(_T("RIGHT"), 0);
    pField->rect.bottom = myGetProfileInt(_T("BOTTOM"), 0);
    pField->nMinLength = myGetProfileInt(_T("MinLen"), 0);
    pField->nMaxLength = myGetProfileInt(_T("MaxLen"), 0);

    // Text color for LINK control, default is the system default link color
    pField->hImage = (HANDLE)(UINT_PTR) myGetProfileInt(_T("TxtColor"), GetLinkColor());

    pField->nControlID = 1200 + nIdx;
    if (pField->nType == FIELD_FILEREQUEST || pField->nType == FIELD_DIRREQUEST)
    {
      FieldType *pNewField = &pFields[nIdx+1];
      pNewField->nControlID = 1200 + nIdx + 1;
      pNewField->nType = FIELD_BROWSEBUTTON;
      pNewField->nFlags = pField->nFlags & (WS_DISABLED | WS_TABSTOP);
      pNewField->pszText = STRDUP(szBrowseButtonCaption); // needed for generic FREE
      pNewField->rect.right  = pField->rect.right;
      pNewField->rect.left   = pNewField->rect.right - BROWSE_WIDTH;
      pNewField->rect.bottom = pField->rect.bottom;
      pNewField->rect.top    = pField->rect.top;
      pField->rect.right = pNewField->rect.left - 3;
      pNewField->nField = nCtrlIdx + 1;
      pNewField->pszHwndEntry = _T("HWND2");
      nNumFields++;
      nIdx++;
    }
  }

  return nNumFields;
}

LRESULT WINAPI WMCommandProc(HWND hWnd, UINT id, HWND hwndCtl, UINT codeNotify) {
  int nIdx = FindControlIdx(id);
  // Ignore if the dialog is in the process of being created
  if (g_done || nIdx < 0)
    return 0;

  switch (pFields[nIdx].nType)
  {
    case FIELD_BROWSEBUTTON:
      --nIdx;
    case FIELD_LINK:
    case FIELD_BUTTON:
    case FIELD_CHECKBOX:
    case FIELD_RADIOBUTTON:
      if (codeNotify != BN_CLICKED)
        return 0;
      break;
    case FIELD_COMBOBOX:
    case FIELD_LISTBOX:
      if (codeNotify != LBN_SELCHANGE) // LBN_SELCHANGE == CBN_SELCHANGE
        return 0;
      break;
    default:
      return 0;
  }

  FieldType *pField = pFields + nIdx;

  TCHAR szBrowsePath[MAX_PATH];

  switch (pField->nType) {
    case FIELD_FILEREQUEST: {
      OPENFILENAME ofn={0,};

      ofn.lStructSize = sizeof(ofn);
      ofn.hwndOwner = hConfigWindow;
      ofn.lpstrFilter = pField->pszFilter;
      ofn.lpstrFile = szBrowsePath;
      ofn.nMaxFile  = COUNTOF(szBrowsePath);
      ofn.Flags = pField->nFlags & (OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_CREATEPROMPT | OFN_EXPLORER);

      GetWindowText(pField->hwnd, szBrowsePath, COUNTOF(szBrowsePath));

    tryagain:
      GetCurrentDirectory(BUFFER_SIZE, szResult); // save working dir
      if ((pField->nFlags & FLAG_SAVEAS) ? GetSaveFileName(&ofn) : GetOpenFileName(&ofn)) {
        mySetWindowText(pField->hwnd, szBrowsePath);
        SetCurrentDirectory(szResult); // restore working dir
                                       // OFN_NOCHANGEDIR doesn't always work (see MSDN)
        break;
      }
      else if (szBrowsePath[0] && CommDlgExtendedError() == FNERR_INVALIDFILENAME) {
        szBrowsePath[0] = _T('\0');
        goto tryagain;
      }

      break;
    }

    case FIELD_DIRREQUEST: {
      BROWSEINFO bi;

      bi.hwndOwner = hConfigWindow;
      bi.pidlRoot = NULL;
      bi.pszDisplayName = szBrowsePath;
      bi.lpszTitle = pField->pszText;
#ifndef BIF_NEWDIALOGSTYLE
#define BIF_NEWDIALOGSTYLE 0x0040
#endif
      bi.ulFlags = BIF_STATUSTEXT | BIF_RETURNONLYFSDIRS | BIF_NEWDIALOGSTYLE;
      bi.lpfn = BrowseCallbackProc;
      bi.lParam = nIdx;
      bi.iImage = 0;

      if (pField->pszRoot) {
        LPSHELLFOLDER sf;
        ULONG eaten;
        LPITEMIDLIST root;
        SHGetDesktopFolder(&sf);
#ifdef _UNICODE
        sf->ParseDisplayName(hConfigWindow, NULL, pField->pszRoot, &eaten, &root, NULL);
#else
        int ccRoot = (lstrlen(pField->pszRoot) * 2) + 2;
        LPWSTR pwszRoot = (LPWSTR) MALLOC(ccRoot);
        MultiByteToWideChar(CP_ACP, 0, pField->pszRoot, -1, pwszRoot, ccRoot);
        sf->ParseDisplayName(hConfigWindow, NULL, pwszRoot, &eaten, &root, NULL);
        FREE(pwszRoot);
#endif
        bi.pidlRoot = root;
        sf->Release();
      }
      //CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
      LPITEMIDLIST pResult = SHBrowseForFolder(&bi);
      if (!pResult)
        break;

      if (SHGetPathFromIDList(pResult, szBrowsePath)) {
        mySetWindowText(pField->hwnd, szBrowsePath);
      }

      CoTaskMemFree(pResult);

      break;
    }

    case FIELD_LINK:
    case FIELD_BUTTON:
      // Allow the state to be empty - this might be useful in conjunction
      // with the NOTIFY flag
      if (*pField->pszState)
        ShellExecute(hMainWindow, NULL, pField->pszState, NULL, NULL, SW_SHOWDEFAULT);
      break;
  }

  if (pField->nFlags & LBS_NOTIFY) {
    // Remember which control was activated then pretend the user clicked Next
    g_NotifyField = nIdx + 1;
    mySendMessage(hMainWindow, WM_NOTIFY_OUTER_NEXT, 1, 0);
  }

  return 0;
}


static WNDPROC lpWndProcOld;

int g_is_cancel,g_is_back;

INT_PTR CALLBACK ParentWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  INT_PTR bRes;
  if (message == WM_NOTIFY_OUTER_NEXT && wParam == 1)
  {
    // Don't call leave function if fields aren't valid
    if (!g_NotifyField && !ValidateFields())
      return 0;
    // Get the settings ready for the leave function verification
    SaveSettings();
    // Reset the record of activated control
    g_NotifyField = 0;
  }
  bRes = CallWindowProc(lpWndProcOld,hwnd,message,wParam,lParam);
  if (message == WM_NOTIFY_OUTER_NEXT && !bRes)
  {
    // if leave function didn't abort (bRes != 0 in that case)
    if (wParam == (WPARAM)-1)
      g_is_back++;
    if (wParam == NOTIFY_BYE_BYE)
      g_is_cancel++;
    g_done++;
    PostMessage(hConfigWindow,WM_CLOSE,0,0);
  }
  return bRes;
}

INT_PTR CALLBACK cfgDlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
    HANDLE_MSG(hwndDlg, WM_COMMAND, WMCommandProc);
    case WM_DRAWITEM:
    {
      DRAWITEMSTRUCT* lpdis = (DRAWITEMSTRUCT*)lParam;
      int nIdx = FindControlIdx(lpdis->CtlID);
#ifdef IO_LINK_UNDERLINED
      HFONT OldFont;
      LOGFONT lf;
#endif

      if (nIdx < 0)
        break;
      FieldType *pField = pFields + nIdx;

#ifdef IO_LINK_UNDERLINED
      GetObject(GetCurrentObject(lpdis->hDC, OBJ_FONT), sizeof(lf), &lf);
      lf.lfUnderline = TRUE;
      OldFont = (HFONT)SelectObject(lpdis->hDC, CreateFontIndirect(&lf));
#endif

      // We need lpdis->rcItem later
      RECT rc = lpdis->rcItem;

      // Calculate needed size of the control
      DrawText(lpdis->hDC, pField->pszText, -1, &rc, DT_VCENTER | DT_WORDBREAK | DT_CALCRECT);

      // Make some more room so the focus rect won't cut letters off
      rc.right = min(rc.right + 2, lpdis->rcItem.right);

      // Move rect to right if in RTL mode
      if (bRTL)
      {
        rc.left += lpdis->rcItem.right - rc.right;
        rc.right += lpdis->rcItem.right - rc.right;
      }

      if (lpdis->itemAction & ODA_DRAWENTIRE)
      {
        // Get TxtColor unless the user has set another using SetCtlColors
        if (!GetWindowLongPtr(lpdis->hwndItem, GWLP_USERDATA))
          SetTextColor(lpdis->hDC, (COLORREF)(INT_PTR) pField->hImage);

        // Draw the text
        DrawText(lpdis->hDC, pField->pszText, -1, &rc, DT_CENTER | DT_VCENTER | DT_WORDBREAK | (bRTL ? DT_RTLREADING : 0));
      }

      // Draw the focus rect if needed
      if (((lpdis->itemState & ODS_FOCUS) && (lpdis->itemAction & ODA_DRAWENTIRE)) || (lpdis->itemAction & ODA_FOCUS))
      {
        // NB: when not in DRAWENTIRE mode, this will actually toggle the focus
        // rectangle since it's drawn in a XOR way
        DrawFocusRect(lpdis->hDC, &rc);
      }

      pField->rect = rc;

#ifdef IO_LINK_UNDERLINED
      DeleteObject(SelectObject(lpdis->hDC, OldFont));
#endif
      break;
    }
    case WM_CTLCOLORSTATIC:
    case WM_CTLCOLOREDIT:
    case WM_CTLCOLORDLG:
    case WM_CTLCOLORBTN:
    case WM_CTLCOLORLISTBOX:
      // let the NSIS window handle colors, it knows best
      return mySendMessage(hMainWindow, uMsg, wParam, lParam);
  }
  return 0;
}

#ifdef IO_ENABLE_LINK

#ifndef IDC_HAND
#define IDC_HAND MAKEINTRESOURCE(32649)
#endif

#ifndef BS_TYPEMASK
#define BS_TYPEMASK 0x0000000FL
#endif

// pFields[nIdx].nParentIdx is used to store original windowproc
LRESULT CALLBACK StaticLINKWindowProc(HWND hWin, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  int StaticField = FindControlIdx(GetDlgCtrlID(hWin));
  if (StaticField < 0)
    return 0;
  FieldType *pField = pFields + StaticField;

  switch(uMsg)
  {
    case WM_GETDLGCODE:
      // Pretend we are a normal button/default button as appropriate
      return DLGC_BUTTON | ((pField->nFlags & FLAG_WANTRETURN) ? DLGC_DEFPUSHBUTTON : DLGC_UNDEFPUSHBUTTON);

    case BM_SETSTYLE:
      // Detect when we are becoming the default button but don't lose the owner-draw style
      if ((wParam & BS_TYPEMASK) == BS_DEFPUSHBUTTON)
        pField->nFlags |= FLAG_WANTRETURN;  // Hijack this flag to indicate default button status
      else
        pField->nFlags &= ~FLAG_WANTRETURN;
      wParam = (wParam & ~BS_TYPEMASK) | BS_OWNERDRAW;
      break;

    case WM_NCHITTEST:
    {
      POINT pt = {GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam)};
      MapWindowPoints(0, hWin, &pt, 1);
      if (PtInRect(&pField->rect, pt))
        return HTCLIENT;
      else
        return HTNOWHERE;
    }

    case WM_SETCURSOR:
    {
      if ((HWND)wParam == hWin && LOWORD(lParam) == HTCLIENT)
      {
        HCURSOR hCur = LoadCursor(NULL, IDC_HAND);
        if (hCur)
        {
          SetCursor(hCur);
          return 1; // halt further processing
        }
      }
    }
  }
  return CallWindowProc((WNDPROC)pField->nParentIdx, hWin, uMsg, wParam, lParam);
}
#endif

LRESULT CALLBACK NumbersOnlyPasteWndProc(HWND hWin, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  int nIdx = FindControlIdx(GetDlgCtrlID(hWin));
  if (nIdx < 0)
    return 0;

  FieldType *pField = pFields + nIdx;

  if (uMsg == WM_PASTE)
  {
    if (OpenClipboard(hWin))
    {
#ifdef _UNICODE
      HGLOBAL hData = GetClipboardData(CF_UNICODETEXT);
#else
      HGLOBAL hData = GetClipboardData(CF_TEXT);
#endif
      
      if (hData)
      {
        TCHAR *lpData = (TCHAR *) GlobalLock(hData);
        if (lpData)
        {
          int iLen = lstrlen(lpData);
          TCHAR *lpFilteredData = (TCHAR *) MALLOC((iLen + 1)*sizeof(TCHAR));

          if (lpFilteredData)
          {
            for (int i = 0, j = 0; i < iLen; i++)
            {
              if (lpData[i] >= _T('0') && lpData[i] <= _T('9'))
              {
                lpFilteredData[j] = lpData[i];
                j++;
              }
              lpFilteredData[j] = 0;
            }

            SendMessage(hWin, EM_REPLACESEL, TRUE, (LPARAM) lpFilteredData);
            FREE(lpFilteredData);
          }

          GlobalUnlock(hData);
        }
      }

      CloseClipboard();

      return 0;
    }
  }

  return CallWindowProc(pField->wndProc, hWin, uMsg, wParam, lParam);
}

int old_cancel_visible;

int WINAPI createCfgDlg()
{
  g_is_back=0;
  g_is_cancel=0;

  HWND mainwnd = hMainWindow;
  if (!mainwnd)
  {
    popstring(NULL);
    pushstring(_T("error finding mainwnd"));
    return 1; // cannot be used in silent mode unfortunately.
  }

  if (!g_stacktop || !*g_stacktop || !(pszFilename = (*g_stacktop)->text) || !pszFilename[0] || !ReadSettings())
  {
    popstring(NULL);
    pushstring(_T("error finding config"));
    return 1;
  }

  HWND childwnd=GetDlgItem(mainwnd,nRectId);
  if (!childwnd)
  {
    popstring(NULL);
    pushstring(_T("error finding childwnd"));
    return 1;
  }

  hCancelButton = GetDlgItem(mainwnd,IDCANCEL);
  hNextButton = GetDlgItem(mainwnd,IDOK);
  hBackButton = GetDlgItem(mainwnd,3);

  mySetWindowText(hCancelButton,pszCancelButtonText);
  mySetWindowText(hNextButton,pszNextButtonText);
  mySetWindowText(hBackButton,pszBackButtonText);

  if (bBackEnabled!=-1) EnableWindow(hBackButton,bBackEnabled);
  if (bCancelEnabled!=-1)
  {
    EnableWindow(hCancelButton,bCancelEnabled);
    if (bCancelEnabled)
      EnableMenuItem(GetSystemMenu(mainwnd, FALSE), SC_CLOSE, MF_BYCOMMAND | MF_ENABLED);
    else
      EnableMenuItem(GetSystemMenu(mainwnd, FALSE), SC_CLOSE, MF_BYCOMMAND | MF_GRAYED);
  }
  if (bCancelShow!=-1) old_cancel_visible=ShowWindow(hCancelButton,bCancelShow?SW_SHOWNA:SW_HIDE);

  HFONT hFont = (HFONT)mySendMessage(mainwnd, WM_GETFONT, 0, 0);

  // Prevent WM_COMMANDs from being processed while we are building
  g_done = 1;

  int mainWndWidth, mainWndHeight;
  hConfigWindow=CreateDialog(m_hInstance,MAKEINTRESOURCE(IDD_DIALOG1),mainwnd,cfgDlgProc);
  if (hConfigWindow)
  {
    RECT dialog_r;
    GetWindowRect(childwnd,&dialog_r);
    MapWindowPoints(0, mainwnd, (LPPOINT) &dialog_r, 2);
    mainWndWidth = dialog_r.right - dialog_r.left;
    mainWndHeight = dialog_r.bottom - dialog_r.top;
    SetWindowPos(
      hConfigWindow,
      0,
      dialog_r.left,
      dialog_r.top,
      mainWndWidth,
      mainWndHeight,
      SWP_NOZORDER|SWP_NOACTIVATE
    );
    // Sets the font of IO window to be the same as the main window
    mySendMessage(hConfigWindow, WM_SETFONT, (WPARAM)hFont, TRUE);
  }
  else
  {
    popstring(NULL);
    pushstring(_T("error creating dialog"));
    return 1;
  }

  BOOL fFocused = FALSE;
  BOOL fFocusedByFlag = FALSE;

#define DEFAULT_STYLES (WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS)
#define RTL_EX_STYLES (WS_EX_RTLREADING | WS_EX_LEFTSCROLLBAR)

  for (int nIdx = 0; nIdx < nNumFields; nIdx++) {
    static struct {
      const TCHAR* pszClass;
      DWORD dwStyle;
      DWORD dwRTLStyle;
      DWORD dwExStyle;
      DWORD dwRTLExStyle;
    } ClassTable[] = {
      { _T("STATIC"),       // FIELD_HLINE
        DEFAULT_STYLES | SS_ETCHEDHORZ | SS_SUNKEN,
        DEFAULT_STYLES | SS_ETCHEDHORZ | SS_SUNKEN,
        WS_EX_TRANSPARENT,
        WS_EX_TRANSPARENT | RTL_EX_STYLES },
      { _T("STATIC"),       // FIELD_VLINE
        DEFAULT_STYLES | SS_ETCHEDVERT | SS_SUNKEN,
        DEFAULT_STYLES | SS_ETCHEDVERT | SS_SUNKEN,
        WS_EX_TRANSPARENT,
        WS_EX_TRANSPARENT | RTL_EX_STYLES },
      { _T("STATIC"),       // FIELD_LABEL
        DEFAULT_STYLES,
        DEFAULT_STYLES | SS_RIGHT,
        WS_EX_TRANSPARENT,
        WS_EX_TRANSPARENT | RTL_EX_STYLES },
      { _T("STATIC"),       // FIELD_ICON
        DEFAULT_STYLES | SS_ICON,
        DEFAULT_STYLES | SS_ICON,
        0,
        RTL_EX_STYLES },
      { _T("STATIC"),       // FIELD_BITMAP
        DEFAULT_STYLES | SS_BITMAP,
        DEFAULT_STYLES | SS_BITMAP,
        0,
        RTL_EX_STYLES },
      { _T("BUTTON"),       // FIELD_BROWSEBUTTON
        DEFAULT_STYLES | WS_TABSTOP,
        DEFAULT_STYLES | WS_TABSTOP,
        0,
        RTL_EX_STYLES },
      { _T("BUTTON"),       // FIELD_LINK
        DEFAULT_STYLES | WS_TABSTOP | BS_OWNERDRAW,
        DEFAULT_STYLES | WS_TABSTOP | BS_OWNERDRAW | BS_RIGHT,
        0,
        RTL_EX_STYLES },
      { _T("BUTTON"),       // FIELD_BUTTON
        DEFAULT_STYLES | WS_TABSTOP,
        DEFAULT_STYLES | WS_TABSTOP,
        0,
        RTL_EX_STYLES },
      { _T("BUTTON"),       // FIELD_GROUPBOX
        DEFAULT_STYLES | BS_GROUPBOX,
        DEFAULT_STYLES | BS_GROUPBOX | BS_RIGHT,
        WS_EX_TRANSPARENT,
        WS_EX_TRANSPARENT | RTL_EX_STYLES },
      { _T("BUTTON"),       // FIELD_CHECKBOX
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX | BS_MULTILINE,
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX | BS_MULTILINE | BS_RIGHT | BS_LEFTTEXT,
        0,
        RTL_EX_STYLES },
      { _T("BUTTON"),       // FIELD_RADIOBUTTON
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON | BS_MULTILINE,
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON | BS_MULTILINE | BS_RIGHT | BS_LEFTTEXT,
        0,
        RTL_EX_STYLES },
      { _T("EDIT"),         // FIELD_TEXT
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL,
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL | ES_RIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | RTL_EX_STYLES },
      { _T("EDIT"),         // FIELD_FILEREQUEST
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL,
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL | ES_RIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | RTL_EX_STYLES },
      { _T("EDIT"),         // FIELD_DIRREQUEST
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL,
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL | ES_RIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | RTL_EX_STYLES },
      { _T("COMBOBOX"),     // FIELD_COMBOBOX
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | CBS_AUTOHSCROLL | CBS_HASSTRINGS,
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | CBS_AUTOHSCROLL | CBS_HASSTRINGS,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | WS_EX_RIGHT | RTL_EX_STYLES },
      { _T("LISTBOX"),      // FIELD_LISTBOX
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT,
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | WS_EX_RIGHT | RTL_EX_STYLES }
    };

    FieldType *pField = pFields + nIdx;

#undef DEFAULT_STYLES

    if (pField->nType < 1 || pField->nType > (int)(COUNTOF(ClassTable)))
      continue;

    DWORD dwStyle, dwExStyle;
    if (bRTL) {
      dwStyle = ClassTable[pField->nType - 1].dwRTLStyle;
      dwExStyle = ClassTable[pField->nType - 1].dwRTLExStyle;
    }
    else {
      dwStyle = ClassTable[pField->nType - 1].dwStyle;
      dwExStyle = ClassTable[pField->nType - 1].dwExStyle;
    }

    // Convert from dialog units

    RECT rect = pField->rect;
    // MapDialogRect uses the font used when a dialog is created, and ignores
    // any subsequent WM_SETFONT messages (like we used above); so use the main
    // NSIS window for the conversion, instead of this one.
    MapDialogRect(mainwnd, &rect);

    if (pField->rect.left < 0)
      rect.left += mainWndWidth;
    if (pField->rect.right < 0)
      rect.right += mainWndWidth;
    if (pField->rect.top < 0)
      rect.top += mainWndHeight;
    if (pField->rect.bottom < 0)
      rect.bottom += mainWndHeight;

    if (bRTL) {
      int right = rect.right;
      rect.right = mainWndWidth - rect.left;
      rect.left = mainWndWidth - right;
    }

    TCHAR *title = pField->pszText;
    switch (pField->nType) {
      case FIELD_ICON:
      case FIELD_BITMAP:
        title = NULL; // otherwise it is treated as the name of a resource
        break;
      case FIELD_CHECKBOX:
      case FIELD_RADIOBUTTON:
        dwStyle ^= pField->nFlags & BS_LEFTTEXT;
        break;
      case FIELD_TEXT:
      case FIELD_FILEREQUEST:
      case FIELD_DIRREQUEST:
        if (pField->nFlags & FLAG_PASSWORD)
          dwStyle |= ES_PASSWORD;
        if (pField->nFlags & FLAG_ONLYNUMBERS)
          dwStyle |= ES_NUMBER;
        if (pField->nFlags & FLAG_WANTRETURN)
          dwStyle |= ES_WANTRETURN;
        if (pField->nFlags & FLAG_READONLY)
          dwStyle |= ES_READONLY;
        title = pField->pszState;
        if (pField->nFlags & FLAG_MULTILINE)
        {
          dwStyle |= ES_MULTILINE | ES_AUTOVSCROLL;
          // Enable word-wrap unless we have a horizontal scroll bar
          // or it has been explicitly disallowed
          if (!(pField->nFlags & (WS_HSCROLL | FLAG_NOWORDWRAP)))
            dwStyle &= ~ES_AUTOHSCROLL;
          ConvertNewLines(pField->pszState);
          // If multiline-readonly then hold the text back until after the
          // initial focus has been set. This is so the text is not initially
          // selected - useful for License Page look-a-likes.
          if (pField->nFlags & FLAG_READONLY)
            title = NULL;
        }
        break;
      case FIELD_COMBOBOX:
        dwStyle |= (pField->nFlags & FLAG_DROPLIST) ? CBS_DROPDOWNLIST : CBS_DROPDOWN;
        title = pField->pszState;
        break;
      case FIELD_LISTBOX:
        dwStyle |= pField->nFlags & (LBS_NOTIFY | LBS_MULTIPLESEL | LBS_EXTENDEDSEL);
        break;
    }

    dwStyle |= pField->nFlags & (WS_GROUP | WS_HSCROLL | WS_VSCROLL | WS_DISABLED);
    if (pField->nFlags & WS_TABSTOP) dwStyle &= ~WS_TABSTOP;

    HWND hwCtrl = pField->hwnd = CreateWindowEx(
      dwExStyle,
      ClassTable[pField->nType - 1].pszClass,
      title,
      dwStyle,
      rect.left,
      rect.top,
      rect.right - rect.left,
      rect.bottom - rect.top,
      hConfigWindow,
      (HMENU)(UINT_PTR) pField->nControlID,
      m_hInstance,
      NULL
    );

    {
      TCHAR szField[64];
      TCHAR szHwnd[64];
      wsprintf(szField, _T("Field %d"), pField->nField);
      wsprintf(szHwnd, _T("%d"), hwCtrl);
      WritePrivateProfileString(szField, pField->pszHwndEntry, szHwnd, pszFilename);
    }

    if (hwCtrl) {
      // Sets the font of IO window to be the same as the main window
      mySendMessage(hwCtrl, WM_SETFONT, (WPARAM)hFont, TRUE);
      // make sure we created the window, then set additional attributes
      switch (pField->nType) {
        case FIELD_TEXT:
        case FIELD_FILEREQUEST:
        case FIELD_DIRREQUEST:
          mySendMessage(hwCtrl, EM_LIMITTEXT, (WPARAM)pField->nMaxLength, (LPARAM)0);
          if (dwStyle & ES_NUMBER)
          {
            pField->wndProc = (WNDPROC) GetWindowLongPtr(hwCtrl, GWLP_WNDPROC);
            SetWindowLongPtr(hwCtrl, GWLP_WNDPROC, (LONG_PTR) NumbersOnlyPasteWndProc);
          }
          break;

        case FIELD_CHECKBOX:
        case FIELD_RADIOBUTTON:
          if (pField->pszState[0] == _T('1'))
            mySendMessage(hwCtrl, BM_SETCHECK, (WPARAM)BST_CHECKED, 0);
          break;

        case FIELD_COMBOBOX:
        case FIELD_LISTBOX:
          // if this is a listbox or combobox, we need to add the list items.
          if (pField->pszListItems) {
            UINT nAddMsg, nFindMsg, nSetSelMsg;
            if (pField->nType == FIELD_COMBOBOX) {
              nAddMsg = CB_ADDSTRING;
              nFindMsg = CB_FINDSTRINGEXACT;
              nSetSelMsg = CB_SETCURSEL;
            }
            else {
              nAddMsg = LB_ADDSTRING;
              nFindMsg = LB_FINDSTRINGEXACT;
              nSetSelMsg = LB_SETCURSEL;
            }
            TCHAR *pszStart, *pszEnd, *pszList;
            pszStart = pszEnd = pszList = STRDUP(pField->pszListItems);
            // pszListItems has a trailing pipe
            while (*pszEnd) {
              if (*pszEnd == _T('|')) {
                *pszEnd = _T('\0');
                if (*pszStart)
                  mySendMessage(hwCtrl, nAddMsg, 0, (LPARAM) pszStart);
                pszStart = ++pszEnd;
              }
              else
                pszEnd = CharNext(pszEnd);
            }
            FREE(pszList);
            if (pField->pszState) {
              if (pField->nFlags & (LBS_MULTIPLESEL|LBS_EXTENDEDSEL) && nFindMsg == LB_FINDSTRINGEXACT) {
                mySendMessage(hwCtrl, LB_SETSEL, FALSE, (LPARAM)-1);
                pszStart = pszEnd = pField->pszState;
                for (;;) {
                  TCHAR c = *pszEnd;
                  if (c == _T('|') || c == _T('\0')) {
                    *pszEnd = _T('\0');
                    if (*pszStart)
                    {
                      INT_PTR nItem = mySendMessage(hwCtrl, LB_FINDSTRINGEXACT, (WPARAM)-1, (LPARAM)pszStart);
                      if (nItem != LB_ERR)
                        mySendMessage(hwCtrl, LB_SETSEL, TRUE, nItem);
                    }
                    if (!c)
                      break;
                    pszStart = ++pszEnd;
                  }
                  else
                    pszEnd = CharNext(pszEnd);
                }
              }
              else {
                INT_PTR nItem = mySendMessage(hwCtrl, nFindMsg, (WPARAM)-1, (LPARAM)pField->pszState);
                if (nItem != CB_ERR) { // CB_ERR == LB_ERR == -1
                  mySendMessage(hwCtrl, nSetSelMsg, nItem, 0);
                }
              }
            }
          }
          break;

        case FIELD_ICON:
        case FIELD_BITMAP:
        {
          UINT nImageType = pField->nType == FIELD_BITMAP ? IMAGE_BITMAP : IMAGE_ICON;
          LPARAM nImage = 0;

          if (pField->pszText) {
            pField->hImage = LoadImage(
              m_hInstance,
              pField->pszText,
              nImageType,
              (pField->nFlags & FLAG_RESIZETOFIT)
                ? (rect.right - rect.left)
                : 0,
              (pField->nFlags & FLAG_RESIZETOFIT)
                ? (rect.bottom - rect.top)
                : 0,
              LR_LOADFROMFILE
            );
            nImage = (LPARAM)pField->hImage;
          }
          else
            nImage = (LPARAM)LoadIcon(GetModuleHandle(0), MAKEINTRESOURCE(103));

          if ((pField->nFlags & TRANSPARENT_BMP) && nImageType == IMAGE_BITMAP)
          {
            // based on AdvSplash's SetTransparentRegion
            BITMAP bm;
            HBITMAP hBitmap = (HBITMAP) nImage;

            if (GetObject(hBitmap, sizeof(bm), &bm))
            {
              HDC dc;
              int x, y;
              HRGN region, cutrgn;
              BITMAPINFO bmi;
              int size = bm.bmWidth * bm.bmHeight * sizeof(int);
              int *bmp = (int *) MALLOC(size);
              if (bmp)
              {
                bmi.bmiHeader.biBitCount = 32;
                bmi.bmiHeader.biCompression = BI_RGB;
                bmi.bmiHeader.biHeight = bm.bmHeight;
                bmi.bmiHeader.biPlanes = 1;
                bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
                bmi.bmiHeader.biWidth = bm.bmWidth;
                bmi.bmiHeader.biClrUsed = 0;
                bmi.bmiHeader.biClrImportant = 0;

                dc = CreateCompatibleDC(NULL);
                SelectObject(dc, hBitmap);

                x = GetDIBits(dc, hBitmap, 0, bm.bmHeight, bmp, &bmi, DIB_RGB_COLORS);

                region = CreateRectRgn(0, 0, bm.bmWidth, bm.bmHeight);

                int keycolor = *bmp & 0xFFFFFF;

                // Search for transparent pixels 
                for (y = bm.bmHeight - 1; y >= 0; y--) {
                  for (x = 0; x < bm.bmWidth;) {
                    if ((*bmp & 0xFFFFFF) == keycolor) {
                      int j = x;
                      while ((x < bm.bmWidth) && ((*bmp & 0xFFFFFF) == keycolor)) {
                        bmp++, x++;
                      }

                      // Cut transparent pixels from the original region
                      cutrgn = CreateRectRgn(j, y, x, y + 1);
                      CombineRgn(region, region, cutrgn, RGN_XOR);
                      DeleteObject(cutrgn);
                    } else {
                      bmp++, x++;
                    }
                  }
                }

                // Set resulting region.
                SetWindowRgn(hwCtrl, region, TRUE);
                DeleteObject(region);
                DeleteObject(dc);
                FREE(bmp);
              }
            }
          }

          mySendMessage(
            hwCtrl,
            STM_SETIMAGE,
            nImageType,
            nImage
          );

          if (pField->nType == FIELD_BITMAP)
          {
            // Centre the image in the requested space.
            // Cannot use SS_CENTERIMAGE because it behaves differently on XP to
            // everything else.  (Thank you Microsoft.)
            RECT  bmp_rect;
            GetClientRect(hwCtrl, &bmp_rect);
            bmp_rect.left = (rect.left + rect.right - bmp_rect.right) / 2;
            bmp_rect.top = (rect.top + rect.bottom - bmp_rect.bottom) / 2;
            SetWindowPos(hwCtrl, NULL, bmp_rect.left, bmp_rect.top, 0, 0,
                         SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOZORDER);
          }

          break;
        }

#ifdef IO_ENABLE_LINK
        case FIELD_LINK:
          pField->nParentIdx = (INT_PTR) SetWindowLongPtr(hwCtrl, GWLP_WNDPROC, (LONG_PTR)StaticLINKWindowProc);
          break;
#endif
      }

      // Set initial focus to the first appropriate field ( with FOCUS flag)
      if (!fFocusedByFlag && (dwStyle & (WS_TABSTOP | WS_DISABLED)) == WS_TABSTOP && pField->nType >= FIELD_SETFOCUS) {
        if (pField->nFlags & FLAG_FOCUS) {
          fFocusedByFlag = TRUE;
        }
        if (!fFocused || fFocusedByFlag) {
          fFocused = TRUE;
          mySetFocus(hwCtrl);
        }
      }

      // If multiline-readonly then hold the text back until after the
      // initial focus has been set. This is so the text is not initially
      // selected - useful for License Page look-a-likes.
      if ((pField->nFlags & (FLAG_MULTILINE | FLAG_READONLY)) == (FLAG_MULTILINE | FLAG_READONLY))
        mySetWindowText(hwCtrl, pField->pszState);
    }
  }

  if (!fFocused)
    mySetFocus(hNextButton);

  mySetWindowText(mainwnd,pszTitle);
  pFilenameStackEntry = *g_stacktop;
  *g_stacktop = (*g_stacktop)->next;
  static TCHAR tmp[32];
  wsprintf(tmp,_T("%d"),hConfigWindow);
  pushstring(tmp);
  return 0;
}

void WINAPI showCfgDlg()
{
  lpWndProcOld = (WNDPROC) SetWindowLongPtr(hMainWindow,DWLP_DLGPROC,(LONG_PTR)ParentWndProc);

  // Tell NSIS to remove old inner dialog and pass handle of the new inner dialog
  mySendMessage(hMainWindow, WM_NOTIFY_CUSTOM_READY, (WPARAM)hConfigWindow, 0);
  ShowWindow(hConfigWindow, SW_SHOWNA);

  g_done = g_NotifyField = 0;

  while (!g_done) {
    MSG msg;
    GetMessage(&msg, NULL, 0, 0);
    if (!IsDialogMessage(hConfigWindow,&msg) && !IsDialogMessage(hMainWindow,&msg))
    {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }

  // we don't save settings on cancel since that means your installer will likely
  // quit soon, which means the ini might get flushed late and cause delete issues?
  if (!g_is_cancel) SaveSettings();

  SetWindowLongPtr(hMainWindow,DWLP_DLGPROC,(LONG_PTR)lpWndProcOld);
  DestroyWindow(hConfigWindow);

  // by ORTIM: 13-August-2002
  if (bCancelShow!=-1) ShowWindow(hCancelButton,old_cancel_visible?SW_SHOWNA:SW_HIDE);

  FREE(pFilenameStackEntry);
  FREE(pszTitle);
  FREE(pszCancelButtonText);
  FREE(pszNextButtonText);
  FREE(pszBackButtonText);

  int i = nNumFields;
  while (i--) {
    FieldType *pField = pFields + i;

    int j = FIELD_BUFFERS;
    while (j--)
      FREE(((TCHAR **) pField)[j]);

    if (pField->nType == FIELD_BITMAP) {
      DeleteObject(pField->hImage);
    }
    if (pField->nType == FIELD_ICON) {
      DestroyIcon((HICON)pField->hImage);
    }
  }
  FREE(pFields);

  pushstring(g_is_cancel?_T("cancel"):g_is_back?_T("back"):_T("success"));
}

int initCalled;

#ifdef _UNICODE
// convert ini file to Unicode so that WritePrivateProfileString can write Unicode strings in it
extern "C" void __declspec(dllexport) make_unicode(HWND hwndParent, int string_size,
                                      TCHAR *variables, stack_t **stacktop)
{
  EXDLL_INIT();
  TCHAR filename[MAX_PATH];
  popstring(filename);
  HANDLE hFile = CreateFile(filename, GENERIC_READ|GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
  if (hFile != INVALID_HANDLE_VALUE)
  {
    DWORD fSize = GetFileSize(hFile, NULL);
    LPSTR lpBuffer = (LPSTR) MALLOC(fSize);
    if (lpBuffer)
    {
        DWORD bytes;
        ReadFile(hFile, lpBuffer, fSize, &bytes, NULL);
        if ((bytes < 2) || (lpBuffer[0] != '\xFF') || (lpBuffer[1] != '\xFE')) // file is not already Unicode
        {
            LPWSTR lpWide = (LPWSTR) MALLOC((bytes+1)*2);
            if (lpWide)
            {
                int cch = MultiByteToWideChar(CP_ACP, 0, lpBuffer, bytes, lpWide, bytes+1);
                if (cch)
                {
                    SetFilePointer(hFile, 0, NULL, FILE_BEGIN);
                    WriteFile(hFile, "\xFF\xFE", 2, &bytes, NULL); // write Unicode BOM
                    WriteFile(hFile, lpWide, cch*2, &bytes, NULL);
                    SetEndOfFile(hFile);
                }
                FREE(lpWide);
            }
        }
        FREE(lpBuffer);
    }
    CloseHandle(hFile);
  }
}
#endif

extern "C" void __declspec(dllexport) dialog(HWND hwndParent, int string_size,
                                      TCHAR *variables, stack_t **stacktop)
{
  hMainWindow=hwndParent;
  EXDLL_INIT();
  if (initCalled) {
    pushstring(_T("error"));
    return;
  }
  if (createCfgDlg())
    return;
  popstring(NULL);
  showCfgDlg();
}

static UINT_PTR PluginCallback(enum NSPIM msg)
{
  return 0;
}

extern "C" void __declspec(dllexport) initDialog(HWND hwndParent, int string_size,
                                      TCHAR *variables, stack_t **stacktop,
                                      extra_parameters *extra)
{
  hMainWindow=hwndParent;
  EXDLL_INIT();

  extra->RegisterPluginCallback(m_hInstance, PluginCallback);

  if (initCalled) {
    pushstring(_T("error"));
    return;
  }
  if (createCfgDlg())
    return;
  initCalled++;
}

extern "C" void __declspec(dllexport) show(HWND hwndParent, int string_size,
                                      TCHAR *variables, stack_t **stacktop)
{
  EXDLL_INIT();
  if (!initCalled) {
    pushstring(_T("error"));
    return;
  }
  initCalled--;
  showCfgDlg();
}

extern "C" BOOL WINAPI DllMain(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  m_hInstance=hInst;
  return TRUE;
}


/**
 * Looks up a single token in the psTable_ and returns its mapped numerical value.
 *
 * @param psTable_ The lookup table.
 * @param pszToken_ The token to lookup.
 * @return The integer value related to the token, otherwise 0.
 */
int WINAPI LookupToken(TableEntry* psTable_, TCHAR* pszToken_)
{
  for (int i = 0; psTable_[i].pszName; i++)
    if (!lstrcmpi(pszToken_, psTable_[i].pszName))
      return psTable_[i].nValue;
  return 0;
}

/**
 * In a string of tokens separated by vertical bars '|', look them up in the
 * Lookup Table psTable and return their logical OR of their subsequent
 * integer values.
 * 
 * @param psTable_ The lookup table to search in.
 * @param pszToken String of tokens separated by '|' whose values are to be
 * ORed together.
 * @return The ORed value of the token values.  If no tokens were found, it
 * will return 0.
 */
int WINAPI LookupTokens(TableEntry* psTable_, TCHAR* pszTokens_)
{
  int n = 0;
  TCHAR *pszStart = pszTokens_;
  TCHAR *pszEnd = pszTokens_;
  for (;;) {
    TCHAR c = *pszEnd;
    if (c == _T('|') || c == _T('\0')) {
      *pszEnd = _T('\0');
      n |= LookupToken(psTable_, pszStart);
      *pszEnd = c;
      if (!c)
        break;
      pszStart = ++pszEnd;
    }
    else
      pszEnd = CharNext(pszEnd);
  }
  return n;
}

/**
 * ConvertNewLines takes a string and turns escape sequences written
 * as separate chars e.g. "\\t" into the special char they represent
 * '\t'.  The transformation is done in place.
 *
 * @param str [in/out] The string to convert.
 */
void WINAPI ConvertNewLines(TCHAR *str) {
  TCHAR *p1, *p2, *p3;
  TCHAR tch0, tch1, nch;

  if (!str)
    return;

  p1 = p2 = str;

  while ((tch0 = *p1) != 0)
  {
    nch = 0;  // new translated char
    if (tch0 == _T('\\'))
    {
      tch1 = *(p1+1);
      
      if      (tch1 == _T('t'))  nch = _T('\t');
      else if (tch1 == _T('n'))  nch = _T('\n');
      else if (tch1 == _T('r'))  nch = _T('\r');
      else if (tch1 == _T('\\')) nch = _T('\\');
    }

    // Was it a special char?
    if (nch)
    {
      *p2++ = nch;
      p1   += 2;
    }
    else
    {
      // For MBCS
      p3 = CharNext(p1);
      while (p1 < p3)
        *p2++ = *p1++;
    }
  }

  *p2 = 0;
}
