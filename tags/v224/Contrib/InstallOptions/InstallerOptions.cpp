/*********************************************************
 *
 *  InstallOptions version 2.0 - Plugin for custom pages
 *
 *  See Readme.html for documentation and license
 *
 *********************************************************/

#include <windows.h>
#include <windowsx.h>
#include <shlobj.h>
#include <commdlg.h>
#include <cderr.h>
#include "resource.h"
#include "shellapi.h"

#define popstring dontuseme
#include "../ExDLL/exdll.h"
#undef popstring

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

void WINAPI popstring(char *str)
{
  if (g_stacktop && *g_stacktop)
  {
    stack_t *th = *g_stacktop;
    *g_stacktop = th->next;
    if (str)
      lstrcpy(str, th->text);
    FREE(th);
  }
}

#define strcpy(x,y) lstrcpy(x,y)
//#define strncpy(x,y,z) lstrcpyn(x,y,z)
#define strdup(x) STRDUP(x)
#define stricmp(x,y) lstrcmpi(x,y)
//#define abs(x) ((x) < 0 ? -(x) : (x))

char *WINAPI STRDUP(const char *c)
{
  char *t=(char*)MALLOC(lstrlen(c)+1);
  return lstrcpy(t,c);
}

// Turn a pair of chars into a word
// Turn four chars into a dword
#ifdef __BIG_ENDIAN__ // Not very likely, but, still...
#define CHAR2_TO_WORD(a,b) (((WORD)(b))|((a)<<8))
#define CHAR4_TO_DWORD(a,b,c,d) (((DWORD)CHAR2_TO_WORD(c,d))|(CHAR2_TO_WORD(a,b)<<16))
#else
#define CHAR2_TO_WORD(a,b) (((WORD)(a))|((b)<<8))
#define CHAR4_TO_DWORD(a,b,c,d) (((DWORD)CHAR2_TO_WORD(a,b))|(CHAR2_TO_WORD(c,d)<<16))
#endif

// Field types
// NB - the order of this list is important - see below

#define FIELD_INVALID      (0)
#define FIELD_LABEL        (1)
#define FIELD_ICON         (2)
#define FIELD_BITMAP       (3)
#define FIELD_BROWSEBUTTON (4)
#define FIELD_LINK         (5)
#define FIELD_BUTTON       (6)
#define FIELD_GROUPBOX     (7)
#define FIELD_CHECKBOX     (8)
#define FIELD_RADIOBUTTON  (9)
#define FIELD_TEXT         (10)
#define FIELD_FILEREQUEST  (11)
#define FIELD_DIRREQUEST   (12)
#define FIELD_COMBOBOX     (13)
#define FIELD_LISTBOX      (14)

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
  char *pszName;
  int   nValue;
};

int WINAPI LookupToken(TableEntry*, char*);
int WINAPI LookupTokens(TableEntry*, char*);

void WINAPI ConvertNewLines(char *str);

// all allocated buffers must be first in the struct
// when adding more allocated buffers to FieldType, don't forget to change this define
#define FIELD_BUFFERS 6
struct FieldType {
  char  *pszText;
  char  *pszState;
  char  *pszRoot;

  char  *pszListItems;
  char  *pszFilter;

  char   *pszValidateText;
  int    nMinLength;
  int    nMaxLength;

  int    nType;
  RECT   rect;

  int    nFlags;

  HWND   hwnd;
  UINT   nControlID;

  int    nParentIdx;  // this is used to store original windowproc for LINK
  HANDLE hImage; // this is used by image/icon field to save the handle to the image

  int    nField; // field number in INI file
  char  *pszHwndEntry; // "HWND" or "HWND2"

  long   wndProc; 
};

// initial buffer size.  buffers will grow as required.
// use a value larger than MAX_PATH to prevent need for excessive growing.
#define BUFFER_SIZE 8192 // 8kb of mem is max char count in multiedit

char szBrowseButtonCaption[] = "...";

HWND hConfigWindow    = NULL;
HWND hMainWindow      = NULL;
HWND hCancelButton    = NULL;
HWND hNextButton      = NULL;
HWND hBackButton      = NULL;

HINSTANCE m_hInstance = NULL;

struct _stack_t *pFilenameStackEntry = NULL;

char *pszFilename         = NULL;
char *pszTitle            = NULL;
char *pszCancelButtonText = NULL;
char *pszNextButtonText   = NULL;
char *pszBackButtonText   = NULL;

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

int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lp, LPARAM pData) {
  static TCHAR szDir[MAX_PATH];

  if (uMsg == BFFM_INITIALIZED &&
      GetWindowText(pFields[(int)pData].hwnd, szDir, MAX_PATH) > 0)
    mySendMessage(hwnd, BFFM_SETSELECTION, TRUE, (LPARAM)szDir);
  return 0;
}


bool INLINE ValidateFields() {
  int nIdx;
  int nLength;

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
          MessageBox(hConfigWindow, pField->pszValidateText, NULL, MB_OK|MB_ICONWARNING);
        }
        mySetFocus(pField->hwnd);
        return false;
      }

    }
  }
  return true;
}

bool WINAPI SaveSettings(void) {
  static char szField[25];
  int nBufLen = BUFFER_SIZE;
  char *pszBuffer = (char*)MALLOC(nBufLen);
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
        wsprintf(pszBuffer, "%d", !!mySendMessage(hwnd, BM_GETCHECK, 0, 0));
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
          pszBuffer = (char*)MALLOC(nBufLen);
          if (!pszBuffer) return false;
        }
        char *pszItem = (char*)MALLOC(nBufLen);
        if (!pszItem) return false;

        *pszBuffer = '\0';
        int nNumItems = mySendMessage(hwnd, LB_GETCOUNT, 0, 0);
        for (int nIdx2 = 0; nIdx2 < nNumItems; nIdx2++) {
          if (mySendMessage(hwnd, LB_GETSEL, nIdx2, 0) > 0) {
            if (*pszBuffer) lstrcat(pszBuffer, "|");
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
        int nLength = mySendMessage(pField->hwnd, WM_GETTEXTLENGTH, 0, 0);
        if (nLength > nBufLen) {
          FREE(pszBuffer);
          // add a bit extra so we do this less often
          nBufLen = nLength + 20;
          pszBuffer = (char*)MALLOC(nBufLen);
          if (!pszBuffer) return false;
        }
        *pszBuffer='"';
        GetWindowText(hwnd, pszBuffer+1, nBufLen-1);
        pszBuffer[nLength+1]='"';
        pszBuffer[nLength+2]='\0';

        if (pField->nType == FIELD_TEXT && (pField->nFlags & FLAG_MULTILINE))
        {
          char *pszBuf2 = (char*)MALLOC(nBufLen*2); // double the size, consider the worst case, all chars are \r\n
          char *p1, *p2;
          for (p1 = pszBuffer, p2 = pszBuf2; *p1; p1 = CharNext(p1), p2 = CharNext(p2))
          {
            switch (*p1) {
              case '\t':
                *(LPWORD)p2 = CHAR2_TO_WORD('\\', 't');
                p2++;
                break;
              case '\n':
                *(LPWORD)p2 = CHAR2_TO_WORD('\\', 'n');
                p2++;
                break;
              case '\r':
                *(LPWORD)p2 = CHAR2_TO_WORD('\\', 'r');
                p2++;
                break;
              case '\\':
                *p2++ = '\\';
              default:
                lstrcpyn(p2, p1, CharNext(p1) - p1 + 1);
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
    wsprintf(szField, "Field %d", CurrField);
    WritePrivateProfileString(szField, "State", pszBuffer, pszFilename);
  }

  // Tell NSIS which control was activated, if any
  wsprintf(pszBuffer, "%d", g_NotifyField);
  WritePrivateProfileString("Settings", "State", pszBuffer, pszFilename);

  FREE(pszBuffer);

  return true;
}

#define BROWSE_WIDTH 15

static char szResult[BUFFER_SIZE];
char *pszAppName;

DWORD WINAPI myGetProfileString(LPCTSTR lpKeyName)
{
  *szResult = '\0';
  return GetPrivateProfileString(pszAppName, lpKeyName, "", szResult, BUFFER_SIZE, pszFilename);
}

char * WINAPI myGetProfileStringDup(LPCTSTR lpKeyName)
{
  int nSize = myGetProfileString(lpKeyName);
  if (nSize)
    return strdup(szResult);
  else
    return NULL;
}

UINT WINAPI myGetProfileInt(LPCTSTR lpKeyName, INT nDefault)
{
  return GetPrivateProfileInt(pszAppName, lpKeyName, nDefault, pszFilename);
}

int WINAPI ReadSettings(void) {
  static char szField[25];
  int nIdx, nCtrlIdx;

  pszAppName = "Settings";
  pszTitle = myGetProfileStringDup("Title");
  pszCancelButtonText = myGetProfileStringDup("CancelButtonText");
  pszNextButtonText = myGetProfileStringDup("NextButtonText");
  pszBackButtonText = myGetProfileStringDup("BackButtonText");

  nNumFields = myGetProfileInt("NumFields", 0);

  nRectId = myGetProfileInt("Rect", DEFAULT_RECT);

  bBackEnabled = myGetProfileInt("BackEnabled", -1);
  // by ORTIM: 13-August-2002
  bCancelEnabled = myGetProfileInt("CancelEnabled", -1);
  bCancelShow = myGetProfileInt("CancelShow", -1);

  bRTL = myGetProfileInt("RTL", 0);

  if (nNumFields > 0) {
    // make this twice as large for the worst case that every control is a browse button.
    // the structure is small enough that this won't waste much memory.
    // if the structure gets much larger, we should switch to a linked list.
    pFields = (FieldType *)MALLOC(sizeof(FieldType)*2*nNumFields);
  }

  for (nIdx = 0, nCtrlIdx = 0; nCtrlIdx < nNumFields; nCtrlIdx++, nIdx++) {
    // Control types
    static TableEntry TypeTable[] = {
      { "LABEL",       FIELD_LABEL       },
      { "TEXT",        FIELD_TEXT        },
      { "PASSWORD",    FIELD_TEXT        },
      { "LISTBOX",     FIELD_LISTBOX     },
      { "COMBOBOX",    FIELD_COMBOBOX    },
      { "DROPLIST",    FIELD_COMBOBOX    },
      { "FILEREQUEST", FIELD_FILEREQUEST },
      { "DIRREQUEST",  FIELD_DIRREQUEST  },
      { "CHECKBOX",    FIELD_CHECKBOX    },
      { "RADIOBUTTON", FIELD_RADIOBUTTON },
      { "ICON",        FIELD_ICON        },
      { "BITMAP",      FIELD_BITMAP      },
      { "GROUPBOX",    FIELD_GROUPBOX    },
#ifdef IO_ENABLE_LINK
      { "LINK",        FIELD_LINK        },
#else
      { "LINK",        FIELD_LABEL       },
#endif
      { "BUTTON",      FIELD_BUTTON      },
      { NULL,          0                 }
    };
    // Control flags
    static TableEntry FlagTable[] = {
      { "NOTIFY",            LBS_NOTIFY          },
      { "WARN_IF_EXIST",     OFN_OVERWRITEPROMPT },
      { "FILE_HIDEREADONLY", OFN_HIDEREADONLY    },
      { "MULTISELECT",       LBS_MULTIPLESEL     },
      { "READONLY",          FLAG_READONLY       },
      { "RIGHT",             BS_LEFTTEXT         },
      { "PASSWORD",          FLAG_PASSWORD       },
      { "ONLY_NUMBERS",      FLAG_ONLYNUMBERS    },
      { "MULTILINE",         FLAG_MULTILINE      },
      { "NOWORDWRAP",        FLAG_NOWORDWRAP     },
      { "WANTRETURN",        FLAG_WANTRETURN     },
      { "EXTENDEDSELCT",     LBS_EXTENDEDSEL     },
      { "PATH_MUST_EXIST",   OFN_PATHMUSTEXIST   },
      { "FILE_MUST_EXIST",   OFN_FILEMUSTEXIST   },
      { "PROMPT_CREATE",     OFN_CREATEPROMPT    },
      { "DROPLIST",          FLAG_DROPLIST       },
      { "RESIZETOFIT",       FLAG_RESIZETOFIT    },
      { "NOTABSTOP",         WS_TABSTOP          },
      { "GROUP",             WS_GROUP            },
      { "REQ_SAVE",          FLAG_SAVEAS         },
      { "FILE_EXPLORER",     OFN_EXPLORER        },
      { "HSCROLL",           WS_HSCROLL          },
      { "VSCROLL",           WS_VSCROLL          },
      { "DISABLED",          WS_DISABLED         },
      { "TRANSPARENT",       TRANSPARENT_BMP     },
      { "FOCUS",             FLAG_FOCUS          },
      { NULL,                0                   }
    };
    FieldType *pField = pFields + nIdx;

    pField->nField = nCtrlIdx + 1;
    pField->pszHwndEntry = "HWND";

    wsprintf(szField, "Field %d", nCtrlIdx + 1);
    pszAppName = szField;

    // Get the control type
    myGetProfileString("TYPE");
    pField->nType = LookupToken(TypeTable, szResult);
    if (pField->nType == FIELD_INVALID)
      continue;

    // Lookup flags associated with the control type
    pField->nFlags = LookupToken(FlagTable, szResult);
    myGetProfileString("Flags");
    pField->nFlags |= LookupTokens(FlagTable, szResult);

    // pszState must not be NULL!
    myGetProfileString("State");
    pField->pszState = strdup(szResult);

    // ListBox items list
    {
      int nResult = myGetProfileString("ListItems");
      if (nResult) {
        // add an extra | character to the end to simplify the loop where we add the items.
        pField->pszListItems = (char*)MALLOC(nResult + 2);
        strcpy(pField->pszListItems, szResult);
        pField->pszListItems[nResult] = '|';
        pField->pszListItems[nResult + 1] = '\0';
      }
    }

    // Label Text - convert newline
    pField->pszText = myGetProfileStringDup("TEXT");
    if (pField->nType == FIELD_LABEL)
      ConvertNewLines(pField->pszText);

    // Dir request - root folder
    pField->pszRoot = myGetProfileStringDup("ROOT");

    // ValidateText - convert newline
    pField->pszValidateText = myGetProfileStringDup("ValidateText");
    ConvertNewLines(pField->pszValidateText);

    {
      int nResult = GetPrivateProfileString(szField, "Filter", "All Files|*.*", szResult, sizeof(szResult), pszFilename);
      if (nResult) {
        // Convert the filter to the format required by Windows: NULL after each
        // item followed by a terminating NULL
        pField->pszFilter = (char*)MALLOC(nResult + 2);
        strcpy(pField->pszFilter, szResult);
        char *pszPos = pField->pszFilter;
        while (*pszPos)
        {
          if (*pszPos == '|')
            *pszPos++ = 0;
          else
            pszPos = CharNext(pszPos);
        }
      }
    }

    pField->rect.left = myGetProfileInt("LEFT", 0);
    pField->rect.top = myGetProfileInt("TOP", 0);
    pField->rect.right = myGetProfileInt("RIGHT", 0);
    pField->rect.bottom = myGetProfileInt("BOTTOM", 0);
    pField->nMinLength = myGetProfileInt("MinLen", 0);
    pField->nMaxLength = myGetProfileInt("MaxLen", 0);

    // Text color for LINK control, default is pure blue
    pField->hImage = (HANDLE)myGetProfileInt("TxtColor", RGB(0,0,255));

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
      pNewField->pszHwndEntry = "HWND2";
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

  char szBrowsePath[MAX_PATH];

  switch (pField->nType) {
    case FIELD_FILEREQUEST: {
      OPENFILENAME ofn={0,};

      ofn.lStructSize = sizeof(ofn);
      ofn.hwndOwner = hConfigWindow;
      ofn.lpstrFilter = pField->pszFilter;
      ofn.lpstrFile = szBrowsePath;
      ofn.nMaxFile  = sizeof(szBrowsePath);
      ofn.Flags = pField->nFlags & (OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_CREATEPROMPT | OFN_EXPLORER);

      GetWindowText(pField->hwnd, szBrowsePath, sizeof(szBrowsePath));

    tryagain:
      GetCurrentDirectory(BUFFER_SIZE, szResult); // save working dir
      if ((pField->nFlags & FLAG_SAVEAS) ? GetSaveFileName(&ofn) : GetOpenFileName(&ofn)) {
        mySetWindowText(pField->hwnd, szBrowsePath);
        SetCurrentDirectory(szResult); // restore working dir
                                       // OFN_NOCHANGEDIR doesn't always work (see MSDN)
        break;
      }
      else if (szBrowsePath[0] && CommDlgExtendedError() == FNERR_INVALIDFILENAME) {
        szBrowsePath[0] = '\0';
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
        int ccRoot = (lstrlen(pField->pszRoot) * 2) + 2;
        LPWSTR pwszRoot = (LPWSTR) MALLOC(ccRoot);
        MultiByteToWideChar(CP_ACP, 0, pField->pszRoot, -1, pwszRoot, ccRoot);
        SHGetDesktopFolder(&sf);
        sf->ParseDisplayName(hConfigWindow, NULL, pwszRoot, &eaten, &root, NULL);
        bi.pidlRoot = root;
        sf->Release();
        FREE(pwszRoot);
      }
      //CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
      LPITEMIDLIST pResult = SHBrowseForFolder(&bi);
      if (!pResult)
        break;

      if (SHGetPathFromIDList(pResult, szBrowsePath)) {
        mySetWindowText(pField->hwnd, szBrowsePath);
      }

      LPMALLOC pMalloc;
      if (!SHGetMalloc(&pMalloc)) {
        pMalloc->Free(pResult);
      }

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


static void *lpWndProcOld;

int g_is_cancel,g_is_back;

BOOL CALLBACK ParentWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  BOOL bRes;
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
  bRes = CallWindowProc((long (__stdcall *)(struct HWND__ *,unsigned int,unsigned int,long))lpWndProcOld,hwnd,message,wParam,lParam);
  if (message == WM_NOTIFY_OUTER_NEXT && !bRes)
  {
    // if leave function didn't abort (bRes != 0 in that case)
    if (wParam == -1)
      g_is_back++;
    if (wParam == NOTIFY_BYE_BYE)
      g_is_cancel++;
    g_done++;
    PostMessage(hConfigWindow,WM_CLOSE,0,0);
  }
  return bRes;
}

BOOL CALLBACK cfgDlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
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
      DrawText(lpdis->hDC, pField->pszText, -1, &rc, DT_VCENTER | DT_SINGLELINE | DT_CALCRECT);

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
        if (!GetWindowLong(lpdis->hwndItem, GWL_USERDATA))
          SetTextColor(lpdis->hDC, (COLORREF) pField->hImage);

        // Draw the text
        DrawText(lpdis->hDC, pField->pszText, -1, &rc, DT_CENTER | DT_VCENTER | DT_SINGLELINE | (bRTL ? DT_RTLREADING : 0));
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
int WINAPI StaticLINKWindowProc(HWND hWin, UINT uMsg, WPARAM wParam, LPARAM lParam)
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

int WINAPI NumbersOnlyPasteWndProc(HWND hWin, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  int nIdx = FindControlIdx(GetDlgCtrlID(hWin));
  if (nIdx < 0)
    return 0;

  FieldType *pField = pFields + nIdx;

  if (uMsg == WM_PASTE)
  {
    if (OpenClipboard(hWin))
    {
      HGLOBAL hData = GetClipboardData(CF_TEXT);
      
      if (hData)
      {
        char *lpData = (char *) GlobalLock(hData);
        if (lpData)
        {
          int iLen = lstrlen(lpData);
          char *lpFilteredData = (char *) MALLOC(iLen + 1);

          if (lpFilteredData)
          {
            for (int i = 0, j = 0; i < iLen; i++)
            {
              if (lpData[i] >= '0' && lpData[i] <= '9')
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

  return CallWindowProc((WNDPROC) pField->wndProc, hWin, uMsg, wParam, lParam);
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
    pushstring("error finding mainwnd");
    return 1; // cannot be used in silent mode unfortunately.
  }

  if (!g_stacktop || !*g_stacktop || !(pszFilename = (*g_stacktop)->text) || !pszFilename[0] || !ReadSettings())
  {
    popstring(NULL);
    pushstring("error finding config");
    return 1;
  }

  HWND childwnd=GetDlgItem(mainwnd,nRectId);
  if (!childwnd)
  {
    popstring(NULL);
    pushstring("error finding childwnd");
    return 1;
  }

  hCancelButton = GetDlgItem(mainwnd,IDCANCEL);
  hNextButton = GetDlgItem(mainwnd,IDOK);
  hBackButton = GetDlgItem(mainwnd,3);

  mySetWindowText(hCancelButton,pszCancelButtonText);
  mySetWindowText(hNextButton,pszNextButtonText);
  mySetWindowText(hBackButton,pszBackButtonText);

  if (bBackEnabled!=-1) EnableWindow(hBackButton,bBackEnabled);
  if (bCancelEnabled!=-1) EnableWindow(hCancelButton,bCancelEnabled);
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
    pushstring("error creating dialog");
    return 1;
  }

  BOOL fFocused = FALSE;
  BOOL fFocusedByFlag = FALSE;

#define DEFAULT_STYLES (WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS)
#define RTL_EX_STYLES (WS_EX_RTLREADING | WS_EX_LEFTSCROLLBAR)

  for (int nIdx = 0; nIdx < nNumFields; nIdx++) {
    static struct {
      char* pszClass;
      DWORD dwStyle;
      DWORD dwRTLStyle;
      DWORD dwExStyle;
      DWORD dwRTLExStyle;
    } ClassTable[] = {
      { "STATIC",       // FIELD_LABEL
        DEFAULT_STYLES,
        DEFAULT_STYLES | SS_RIGHT,
        WS_EX_TRANSPARENT,
        WS_EX_TRANSPARENT | RTL_EX_STYLES },
      { "STATIC",       // FIELD_ICON
        DEFAULT_STYLES | SS_ICON,
        DEFAULT_STYLES | SS_ICON,
        0,
        RTL_EX_STYLES },
      { "STATIC",       // FIELD_BITMAP
        DEFAULT_STYLES | SS_BITMAP,
        DEFAULT_STYLES | SS_BITMAP,
        0,
        RTL_EX_STYLES },
      { "BUTTON",       // FIELD_BROWSEBUTTON
        DEFAULT_STYLES | WS_TABSTOP,
        DEFAULT_STYLES | WS_TABSTOP,
        0,
        RTL_EX_STYLES },
      { "BUTTON",       // FIELD_LINK
        DEFAULT_STYLES | WS_TABSTOP | BS_OWNERDRAW,
        DEFAULT_STYLES | WS_TABSTOP | BS_OWNERDRAW | BS_RIGHT,
        0,
        RTL_EX_STYLES },
      { "BUTTON",       // FIELD_BUTTON
        DEFAULT_STYLES | WS_TABSTOP,
        DEFAULT_STYLES | WS_TABSTOP,
        0,
        RTL_EX_STYLES },
      { "BUTTON",       // FIELD_GROUPBOX
        DEFAULT_STYLES | BS_GROUPBOX,
        DEFAULT_STYLES | BS_GROUPBOX | BS_RIGHT,
        WS_EX_TRANSPARENT,
        WS_EX_TRANSPARENT | RTL_EX_STYLES },
      { "BUTTON",       // FIELD_CHECKBOX
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX | BS_MULTILINE,
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX | BS_MULTILINE | BS_RIGHT | BS_LEFTTEXT,
        0,
        RTL_EX_STYLES },
      { "BUTTON",       // FIELD_RADIOBUTTON
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON | BS_MULTILINE,
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON | BS_MULTILINE | BS_RIGHT | BS_LEFTTEXT,
        0,
        RTL_EX_STYLES },
      { "EDIT",         // FIELD_TEXT
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL,
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL | ES_RIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | RTL_EX_STYLES },
      { "EDIT",         // FIELD_FILEREQUEST
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL,
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL | ES_RIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | RTL_EX_STYLES },
      { "EDIT",         // FIELD_DIRREQUEST
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL,
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL | ES_RIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | RTL_EX_STYLES },
      { "COMBOBOX",     // FIELD_COMBOBOX
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | CBS_AUTOHSCROLL | CBS_HASSTRINGS,
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | CBS_AUTOHSCROLL | CBS_HASSTRINGS,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | WS_EX_RIGHT | RTL_EX_STYLES },
      { "LISTBOX",      // FIELD_LISTBOX
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT,
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | WS_EX_RIGHT | RTL_EX_STYLES }
    };

    FieldType *pField = pFields + nIdx;

#undef DEFAULT_STYLES

    if (pField->nType < 1 || pField->nType > (sizeof(ClassTable) / sizeof(ClassTable[0])))
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

    char *title = pField->pszText;
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
      (HMENU)pField->nControlID,
      m_hInstance,
      NULL
    );

    {
      char szField[64];
      char szHwnd[64];
      wsprintf(szField, "Field %d", pField->nField);
      wsprintf(szHwnd, "%d", hwCtrl);
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
            pField->wndProc = GetWindowLong(hwCtrl, GWL_WNDPROC);
            SetWindowLong(hwCtrl, GWL_WNDPROC, (long) NumbersOnlyPasteWndProc);
          }
          break;

        case FIELD_CHECKBOX:
        case FIELD_RADIOBUTTON:
          if (pField->pszState[0] == '1')
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
            char *pszStart, *pszEnd, *pszList;
            pszStart = pszEnd = pszList = STRDUP(pField->pszListItems);
            // pszListItems has a trailing pipe
            while (*pszEnd) {
              if (*pszEnd == '|') {
                *pszEnd = '\0';
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
                mySendMessage(hwCtrl, LB_SETSEL, FALSE, -1);
                pszStart = pszEnd = pField->pszState;
                for (;;) {
                  char c = *pszEnd;
                  if (c == '|' || c == '\0') {
                    *pszEnd = '\0';
                    if (*pszStart)
                    {
                      int nItem = mySendMessage(hwCtrl, LB_FINDSTRINGEXACT, -1, (LPARAM)pszStart);
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
                int nItem = mySendMessage(hwCtrl, nFindMsg, -1, (LPARAM)pField->pszState);
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
          WPARAM nImageType = pField->nType == FIELD_BITMAP ? IMAGE_BITMAP : IMAGE_ICON;
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
          pField->nParentIdx = SetWindowLong(hwCtrl, GWL_WNDPROC, (long)StaticLINKWindowProc);
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
  static char tmp[32];
  wsprintf(tmp,"%d",hConfigWindow);
  pushstring(tmp);
  return 0;
}

void WINAPI showCfgDlg()
{
  lpWndProcOld = (void *) SetWindowLong(hMainWindow,DWL_DLGPROC,(long)ParentWndProc);

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
  // quit soon, which means the ini might get flushed late and cause crap. :) anwyay.
  if (!g_is_cancel) SaveSettings();

  SetWindowLong(hMainWindow,DWL_DLGPROC,(long)lpWndProcOld);
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
      FREE(((char **) pField)[j]);

    if (pField->nType == FIELD_BITMAP) {
      DeleteObject(pField->hImage);
    }
    if (pField->nType == FIELD_ICON) {
      DestroyIcon((HICON)pField->hImage);
    }
  }
  FREE(pFields);

  pushstring(g_is_cancel?"cancel":g_is_back?"back":"success");
}

int initCalled;

extern "C" void __declspec(dllexport) dialog(HWND hwndParent, int string_size,
                                      char *variables, stack_t **stacktop)
{
  hMainWindow=hwndParent;
  EXDLL_INIT();
  if (initCalled) {
    pushstring("error");
    return;
  }
  if (createCfgDlg())
    return;
  popstring(NULL);
  showCfgDlg();
}

extern "C" void __declspec(dllexport) initDialog(HWND hwndParent, int string_size,
                                      char *variables, stack_t **stacktop)
{
  hMainWindow=hwndParent;
  EXDLL_INIT();
  if (initCalled) {
    pushstring("error");
    return;
  }
  if (createCfgDlg())
    return;
  initCalled++;
}

extern "C" void __declspec(dllexport) show(HWND hwndParent, int string_size,
                                      char *variables, stack_t **stacktop)
{
  EXDLL_INIT();
  if (!initCalled) {
    pushstring("error");
    return;
  }
  initCalled--;
  showCfgDlg();
}

extern "C" BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  m_hInstance=(HINSTANCE) hInst;
  return TRUE;
}


int WINAPI LookupToken(TableEntry* psTable_, char* pszToken_)
{
  for (int i = 0; psTable_[i].pszName; i++)
    if (!stricmp(pszToken_, psTable_[i].pszName))
      return psTable_[i].nValue;
  return 0;
}

int WINAPI LookupTokens(TableEntry* psTable_, char* pszTokens_)
{
  int n = 0;
  char *pszStart = pszTokens_;
  char *pszEnd = pszTokens_;
  for (;;) {
    char c = *pszEnd;
    if (c == '|' || c == '\0') {
      *pszEnd = '\0';
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

void WINAPI ConvertNewLines(char *str) {
  char *p1, *p2, *p3;

  if (!str)
    return;

  p1 = p2 = str;

  while (*p1)
  {
    switch (*(LPWORD)p1)
    {
    case CHAR2_TO_WORD('\\', 't'):
      *p2 = '\t';
      p1 += 2;
      p2++;
      break;
    case CHAR2_TO_WORD('\\', 'n'):
      *p2 = '\n';
      p1 += 2;
      p2++;
      break;
    case CHAR2_TO_WORD('\\', 'r'):
      *p2 = '\r';
      p1 += 2;
      p2++;
      break;
    case CHAR2_TO_WORD('\\', '\\'):
      *p2 = '\\';
      p1 += 2;
      p2++;
      break;
    default:
      p3 = CharNext(p1);
      while (p1 < p3)
        *p2++ = *p1++;
      break;
    }
  }

  *p2 = 0;
}
