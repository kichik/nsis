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
#include "Shellapi.h"

#define popstring dontuseme
#include "../exdll/exdll.h"
#undef popstring

void *WINAPI MALLOC(int len) { return (void*)GlobalAlloc(GPTR,len); }
void WINAPI FREE(void *d) { if (d) GlobalFree((HGLOBAL)d); }

static int WINAPI popstring(char *str)
{
  stack_t *th;
  if (!g_stacktop || !*g_stacktop) return 1;
  th=(*g_stacktop);
  if (str) lstrcpy(str,th->text);
  *g_stacktop = th->next;
  FREE(th);
  return 0;
}

#define CC_TEXT 1
#define CC_BK 4

typedef struct {
  COLORREF text;
  LOGBRUSH bk;
  HBRUSH bkb;
  int bkmode;
  int flags;
} ctlcolors;

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

#define FIELD_INVALID      (0)
#define FIELD_LABEL        (1)
#define FIELD_ICON         (2)
#define FIELD_BITMAP       (3)
#define FIELD_BROWSEBUTTON (4)
#define FIELD_CHECKBOX     (5)
#define FIELD_RADIOBUTTON  (6)
#define FIELD_TEXT         (7)
#define FIELD_FILEREQUEST  (8)
#define FIELD_DIRREQUEST   (9)
#define FIELD_COMBOBOX     (10)
#define FIELD_LISTBOX      (11)
#define FIELD_GROUPBOX     (12)
#define FIELD_LINK         (13)
#define FIELD_BUTTON       (14)

//---------------------------------------------------------------------
// settings
// crashes on windows 98 - #define IO_ENABLE_LINK
#define IO_ENABLE_LINK

//#define IO_LINK_UNDERLINED // Uncomment to show links text underlined
//---------------------------------------------------------------------

// general flags
#define FLAG_RIGHT         0x00000001

// OFN_OVERWRITEPROMPT     0x00000002
// OFN_HIDEREADONLY        0x00000004

#define FLAG_DISABLED      0x00000008
#define FLAG_GROUP         0x00000010
#define FLAG_NOTABSTOP     0x00000020

// text box flags
#define FLAG_PASSWORD      0x00000040
#define FLAG_ONLYNUMBERS   0x00000080
#define FLAG_MULTILINE     0x00000100

// listbox flags
#define FLAG_MULTISELECT   0x00000200
#define FLAG_EXTENDEDSEL   0x00000400

// OFN_PATHMUSTEXIST       0x00000800
// OFN_FILEMUSTEXIST       0x00001000
// OFN_CREATEPROMPT        0x00002000

// combobox flags
#define FLAG_DROPLIST      0x00004000

// bitmap flags
#define FLAG_RESIZETOFIT   0x00008000

// general flags
#define FLAG_NOTIFY        0x00010000 // Notify NSIS script when control is "activated" (exact meaning depends on the type of control)

// browse flags
#define FLAG_SAVEAS        0x00020000 // Show "Save As" instead of "Open" for FileRequest field

// text box flags
#define FLAG_NOWORDWRAP    0x00040000 // Disable word-wrap in multi-line text boxes

// OFN_EXPLORER            0x00080000

// more text box flags
#define FLAG_WANTRETURN    0x00100000
#define FLAG_VSCROLL       0x00200000
#define FLAG_HSCROLL       0x00400000
#define FLAG_READONLY      0x00800000

struct TableEntry {
  char *pszName;
  int   nValue;
};

int WINAPI LookupToken(TableEntry*, char*);
int WINAPI LookupTokens(TableEntry*, char*);

void WINAPI ConvertNewLines(char *str);

struct FieldType {
  char  *pszText;
  char  *pszState;
  char  *pszRoot;

  char  *pszListItems;
  char  *pszFilter;

  int    nType;
  RECT   rect;

  int    nMinLength;
  int    nMaxLength;
  char   *pszValidateText;

  int    nFlags;

  HWND   hwnd;
  UINT   nControlID;

  int    nParentIdx;  // this is used to store original windowproc for LINK
  HANDLE hImage; // this is used by image/icon field to save the handle to the image
};

// initial buffer size.  buffers will grow as required.
// use a value larger than MAX_PATH to prevent need for excessive growing.
#define MAX_BUFFER_LENGTH (300)

char szBrowseButtonCaption[] = "...";

HWND hConfigWindow    = NULL;
HWND hMainWindow      = NULL;
HWND hCancelButton    = NULL;
HWND hNextButton      = NULL;
HWND hBackButton      = NULL;
HWND hInitialFocus    = NULL;

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


int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lp, LPARAM pData) {
   static TCHAR szDir[MAX_PATH];

   if (uMsg == BFFM_INITIALIZED) {
      if (GetWindowText(pFields[(int)pData].hwnd, szDir, MAX_PATH) > 0) {
        mySendMessage(hwnd, BFFM_SETSELECTION, TRUE, (LPARAM)szDir);
      }
    }
   return 0;
}


bool WINAPI ValidateFields() {
  int nIdx;
  int nLength;

  // In the unlikely event we can't allocate memory, go ahead and return true so we can get out of here.
  // May cause problems for the install script, but no memory is problems for us.
  for (nIdx = 0; nIdx < nNumFields; nIdx++) {
    FieldType *pField = pFields + nIdx;
    // this if statement prevents a stupid bug where a min/max length is assigned to a label control
    // where the user obviously has no way of changing what is displayed. (can you say, "infinite loop"?)
    if (pField->nType >= FIELD_TEXT) {
      nLength = mySendMessage(pField->hwnd, WM_GETTEXTLENGTH, 0, 0);

      if (((pField->nMaxLength > 0) && (nLength > pField->nMaxLength)) ||
         ((pField->nMinLength > 0) && (nLength < pField->nMinLength))) {
        if (pField->pszValidateText) {
          MessageBox(hConfigWindow, pField->pszValidateText, NULL, MB_OK|MB_ICONWARNING);
        }
        SetFocus(pField->hwnd);
        return false;
      }

    }
  }
  return true;
}

bool WINAPI SaveSettings(void) {
  static char szField[25];
  int nIdx;
  int nBufLen = MAX_BUFFER_LENGTH;
  char *pszBuffer = (char*)MALLOC(nBufLen);
  if (!pszBuffer) return false;

  int CurrField = 1;
  for (nIdx = 0; nIdx < nNumFields; nIdx++) {
    FieldType *pField = pFields + nIdx;
    HWND hwnd = pField->hwnd;
    switch (pField->nType) {
      case FIELD_BROWSEBUTTON:
        if (g_NotifyField > CurrField)
          --g_NotifyField;
        continue;
      case FIELD_INVALID:
      case FIELD_LABEL:
      case FIELD_BUTTON:
        *pszBuffer=0;
        break;
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
      default:
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

          if ( pField->nType == FIELD_TEXT && pField->nFlags & FLAG_MULTILINE )
          {
            char *pszBuf2 = (char*)MALLOC(nBufLen*2); // double the size, consider the worst case, all chars are \r\n
            char *p1, *p2;
            for (p1=pszBuffer,p2=pszBuf2; *p1; p1++, p2++) {
              switch (*p1) {
                case '\r':
                  *p2++ = '\\';
                  *p2 = 'r';
                  break;
                case '\n':
                  *p2++ = '\\';
                  *p2 = 'n';
                  break;
                case '\t':
                  *p2++ = '\\';
                  *p2 = 't';
                  break;
                case '\\':
                  *p2++ = '\\';
                default:
                  *p2=*p1;
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
    CurrField++;
  }

  // Tell NSIS which control was activated, if any
  wsprintf(pszBuffer, "%d", g_NotifyField);
  WritePrivateProfileString("Settings", "State", pszBuffer, pszFilename);

  FREE(pszBuffer);

  return true;
}

#define BROWSE_WIDTH 15

#define BUFFER_SIZE 8192 // 8kb of mem is max char count in multiedit
static char szResult[BUFFER_SIZE];

DWORD WINAPI myGetProfileString(LPCTSTR lpAppName, LPCTSTR lpKeyName)
{
  *szResult = '\0';
  return GetPrivateProfileString(lpAppName, lpKeyName, "", szResult, BUFFER_SIZE, pszFilename);
}

char * WINAPI myGetProfileStringDup(LPCTSTR lpAppName, LPCTSTR lpKeyName)
{
  int nSize = myGetProfileString(lpAppName, lpKeyName);
  if ( nSize )
    return strdup(szResult);
  else
    return NULL;
}

UINT WINAPI myGetProfileInt(LPCTSTR lpAppName, LPCTSTR lpKeyName, INT nDefault)
{
  return GetPrivateProfileInt(lpAppName, lpKeyName, nDefault, pszFilename);
}

int WINAPI ReadSettings(void) {
  static char szField[25];
  int nIdx, nCtrlIdx;

  pszTitle = myGetProfileStringDup("Settings", "Title");
  pszCancelButtonText = myGetProfileStringDup("Settings", "CancelButtonText");
  pszNextButtonText = myGetProfileStringDup("Settings", "NextButtonText");
  pszBackButtonText = myGetProfileStringDup("Settings", "BackButtonText");

  nNumFields = myGetProfileInt("Settings", "NumFields", 0);

  nRectId = myGetProfileInt("Settings", "Rect", DEFAULT_RECT);

  bBackEnabled = myGetProfileInt("Settings", "BackEnabled", -1);
  // by ORTIM: 13-August-2002
  bCancelEnabled = myGetProfileInt("Settings", "CancelEnabled", -1);
  bCancelShow = myGetProfileInt("Settings", "CancelShow", -1);

  bRTL = myGetProfileInt("Settings", "RTL", 0);

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
      { "RIGHT",             FLAG_RIGHT          },
      { "WARN_IF_EXIST",     OFN_OVERWRITEPROMPT },
      { "FILE_HIDEREADONLY", OFN_HIDEREADONLY    },
      { "DISABLED",          FLAG_DISABLED       },
      { "GROUP",             FLAG_GROUP          },
      { "NOTABSTOP",         FLAG_NOTABSTOP      },
      { "PASSWORD",          FLAG_PASSWORD       },
      { "ONLY_NUMBERS",      FLAG_ONLYNUMBERS    },
      { "MULTILINE",         FLAG_MULTILINE      },
      { "MULTISELECT",       FLAG_MULTISELECT    },
      { "EXTENDEDSELCT",     FLAG_EXTENDEDSEL    },
      { "FILE_MUST_EXIST",   OFN_FILEMUSTEXIST   },
      { "PATH_MUST_EXIST",   OFN_PATHMUSTEXIST   },
      { "PROMPT_CREATE",     OFN_CREATEPROMPT    },
      { "DROPLIST",          FLAG_DROPLIST       },
      { "RESIZETOFIT",       FLAG_RESIZETOFIT    },
      { "NOTIFY",            FLAG_NOTIFY         },
      { "REQ_SAVE",          FLAG_SAVEAS         },
      { "NOWORDWRAP",        FLAG_NOWORDWRAP     },
      { "FILE_EXPLORER",     OFN_EXPLORER        },
      { "WANTRETURN",        FLAG_WANTRETURN     },
      { "VSCROLL",           FLAG_VSCROLL        },
      { "HSCROLL",           FLAG_HSCROLL        },
      { "READONLY",          FLAG_READONLY       },
      { NULL,                0                   }
    };
    FieldType *pField = pFields + nIdx;

    wsprintf(szField, "Field %d", nCtrlIdx + 1);
    myGetProfileString(szField, "TYPE");

    // Get the control type
    pField->nType = LookupToken(TypeTable, szResult);
    if (pField->nType == FIELD_INVALID)
      continue;

    // Lookup flags associated with the control type
    pField->nFlags |= LookupToken(FlagTable, szResult);

    pField->pszText = myGetProfileStringDup(szField, "TEXT");

    // Label Text - convert newline

    if (pField->nType == FIELD_LABEL) {
      ConvertNewLines(pField->pszText);
    }

    // pszState must not be NULL!
    myGetProfileString(szField, "State");
    pField->pszState = strdup(szResult);

    pField->pszRoot = myGetProfileStringDup(szField, "ROOT");

    {
      int nResult = myGetProfileString(szField, "ListItems");
      if (nResult) {
        // add an extra | character to the end to simplify the loop where we add the items.
        pField->pszListItems = (char*)MALLOC(nResult + 2);
        strcpy(pField->pszListItems, szResult);
        pField->pszListItems[nResult] = '|';
        pField->pszListItems[nResult + 1] = '\0';
      }
    }
    pField->nMaxLength = myGetProfileInt(szField, "MaxLen", 0);
    pField->nMinLength = myGetProfileInt(szField, "MinLen", 0);

    pField->pszValidateText = myGetProfileStringDup(szField, "ValidateText");

    // ValidateText - convert newline

    if (pField->pszValidateText) {
      ConvertNewLines(pField->pszValidateText);
    }

    {
      int nResult = GetPrivateProfileString(szField, "Filter", "All Files|*.*", szResult, sizeof(szResult), pszFilename);
      if (nResult) {
        // add an extra | character to the end to simplify the loop where we add the items.
        pField->pszFilter = (char*)MALLOC(nResult + 2);
        strcpy(pField->pszFilter, szResult);
        char *pszPos = pField->pszFilter;
        while (*pszPos) {
          if (*pszPos == '|') *pszPos = '\0';
          pszPos++;
        }
      }
    }

    pField->rect.left = myGetProfileInt(szField, "LEFT", 0);
    pField->rect.right = myGetProfileInt(szField, "RIGHT", 0);
    pField->rect.top = myGetProfileInt(szField, "TOP", 0);
    pField->rect.bottom = myGetProfileInt(szField, "BOTTOM", 0);

    myGetProfileString(szField, "Flags");
    pField->nFlags |= LookupTokens(FlagTable, szResult);

    // Text color for LINK control, default is pure blue
    //if (pField->nType == FIELD_LINK)
    pField->hImage = (HANDLE)myGetProfileInt(szField, "TxtColor", RGB(0,0,255));

    pField->nControlID = 1200 + nIdx;
    if (pField->nType == FIELD_FILEREQUEST || pField->nType == FIELD_DIRREQUEST)
    {
      FieldType *pNewField = &pFields[nIdx+1];
      pNewField->nControlID = 1200 + nIdx + 1;
      pNewField->nType = FIELD_BROWSEBUTTON;
      pNewField->nFlags = pField->nFlags & (FLAG_DISABLED | FLAG_NOTABSTOP);
      pNewField->pszText = STRDUP(szBrowseButtonCaption); // needed for generic FREE
      pNewField->rect.right  = pField->rect.right;
      pNewField->rect.left   = pNewField->rect.right - BROWSE_WIDTH;
      pNewField->rect.bottom = pField->rect.bottom;
      pNewField->rect.top    = pField->rect.top;
      pField->rect.right = pNewField->rect.left - 3;
      nNumFields++;
      nIdx++;
    }
  }

  return nNumFields;
}


LRESULT WINAPI WMCommandProc(HWND hWnd, UINT id, HWND hwndCtl, UINT codeNotify) {
  switch (codeNotify) {
    case BN_CLICKED:
    {
      char szBrowsePath[MAX_PATH];
      int nIdx = FindControlIdx(id);
      if (nIdx < 0)
        break;
      if (pFields[nIdx].nType == FIELD_BROWSEBUTTON)
        --nIdx;
      FieldType *pField = pFields + nIdx;
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
          if ((pField->nFlags & FLAG_SAVEAS) ? GetSaveFileName(&ofn) : GetOpenFileName(&ofn)) {
            SetWindowText(pField->hwnd, szBrowsePath);
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
//          CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
          LPITEMIDLIST pResult = SHBrowseForFolder(&bi);
          if (!pResult)
            break;

          if (SHGetPathFromIDList(pResult, szBrowsePath)) {
            SetWindowText(pField->hwnd, szBrowsePath);
          }

          LPMALLOC pMalloc;
          if (!SHGetMalloc(&pMalloc)) {
            pMalloc->Free(pResult);
          }

          break;
        }

        case FIELD_LINK:
          ShellExecute(hMainWindow, NULL, pField->pszState, NULL, NULL, SW_SHOWDEFAULT);
          break;
      }

      if (pField->nFlags & FLAG_NOTIFY) {
        // Remember which control was activated then pretend the user clicked Next
        g_NotifyField = nIdx + 1;
        FORWARD_WM_COMMAND(hMainWindow, IDOK, hNextButton, codeNotify, mySendMessage);
      }
    }
    break;
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
    if (!ValidateFields())
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

      // Get TxtColor unless the user has set another using SetCtlColors
      if (!GetWindowLong(lpdis->hwndItem, GWL_USERDATA))
        SetTextColor(lpdis->hDC, (COLORREF) pField->hImage);

      // Calculate needed size of the control
      DrawText(lpdis->hDC, pField->pszText, -1, &rc, DT_VCENTER | DT_SINGLELINE | DT_CALCRECT);
      
      // Move rect to right if in RTL mode
      if (bRTL)
      {
        rc.left += lpdis->rcItem.right - rc.right;
        rc.right += lpdis->rcItem.right - rc.right;
      }

      // Make some more room so the focus rect won't cut letters off
      rc.left = max(rc.left - 2, lpdis->rcItem.left);
      rc.right = min(rc.right + 2, lpdis->rcItem.right);
      /*rc.top = max(rc.top - 2, lpdis->rcItem.top);
      rc.bottom = min(rc.bottom + 2, lpdis->rcItem.bottom);*/

      // Draw the text
      DrawText(lpdis->hDC, pField->pszText, -1, &rc, DT_CENTER | DT_VCENTER | DT_SINGLELINE | (bRTL ? DT_RTLREADING : 0));

      // Draw the focus rect if needed
      if (((lpdis->itemState & ODS_FOCUS) && (lpdis->itemAction & ODA_DRAWENTIRE)) || (lpdis->itemAction & ODA_FOCUS) || (lpdis->itemAction & ODA_SELECT))
      {
        DrawFocusRect(lpdis->hDC, &rc);
      }

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
    {
      ctlcolors *c = (ctlcolors *) GetWindowLong((HWND) lParam, GWL_USERDATA);

      if (c) {
        SetBkMode((HDC)wParam, c->bkmode);
        if (c->flags & CC_BK)
          SetBkColor((HDC)wParam, c->bk.lbColor);
        if (c->flags & CC_TEXT)
          SetTextColor((HDC)wParam, c->text);

        return (BOOL)c->bkb;
      }
    }
  }
  return 0;
}

#ifdef IO_ENABLE_LINK
// pFields[nIdx].nParentIdx is used to store original windowproc
int WINAPI StaticLINKWindowProc(HWND hWin, UINT uMsg, LPARAM wParam, WPARAM lParam)
{
  int StaticField = FindControlIdx(GetDlgCtrlID(hWin));
  if (StaticField < 0)
    return 0;
  FieldType *pField = pFields + StaticField;

  switch(uMsg)
  {
  case WM_GETDLGCODE:
    return DLGC_BUTTON|DLGC_WANTALLKEYS;
  case WM_KEYDOWN:
    {
      if ( wParam == VK_RETURN )
        WMCommandProc(hMainWindow, pField->nControlID, pField->hwnd, BN_CLICKED);
      else if ( wParam == VK_TAB )
        mySendMessage(hMainWindow, WM_NEXTDLGCTL, GetKeyState(VK_SHIFT) & 0x8000, FALSE);
    }
    break;
  case WM_SETCURSOR:
    {
      if ( (HWND)wParam == hWin && LOWORD(lParam) == HTCLIENT )
      {
        HCURSOR hCur = LoadCursor(NULL, IDC_HAND);
        if ( hCur )
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
  hInitialFocus = hNextButton = GetDlgItem(mainwnd,IDOK);
  hBackButton = GetDlgItem(mainwnd,3);

  if (pszCancelButtonText) SetWindowText(hCancelButton,pszCancelButtonText);
  if (pszNextButtonText) SetWindowText(hNextButton,pszNextButtonText);
  if (pszBackButtonText) SetWindowText(hBackButton,pszBackButtonText);

  if (bBackEnabled!=-1) EnableWindow(hBackButton,bBackEnabled);
  if (bCancelEnabled!=-1) EnableWindow(hCancelButton,bCancelEnabled);
  if (bCancelShow!=-1) old_cancel_visible=ShowWindow(hCancelButton,bCancelShow?SW_SHOWNA:SW_HIDE);

  HFONT hFont = (HFONT)mySendMessage(mainwnd, WM_GETFONT, 0, 0);

  RECT dialog_r;
  int width;
  hConfigWindow=CreateDialog(m_hInstance,MAKEINTRESOURCE(IDD_DIALOG1),mainwnd,cfgDlgProc);
  if (hConfigWindow)
  {
    GetWindowRect(childwnd,&dialog_r);
    ScreenToClient(mainwnd,(LPPOINT)&dialog_r);
    ScreenToClient(mainwnd,((LPPOINT)&dialog_r)+1);
    width = dialog_r.right-dialog_r.left;
    SetWindowPos(
      hConfigWindow,
      0,
      dialog_r.left,
      dialog_r.top,
      width,
      dialog_r.bottom-dialog_r.top,
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

  // Init dialog unit conversion

  HDC memDC = CreateCompatibleDC(GetDC(hConfigWindow));
  SelectObject(memDC, hFont);

  TEXTMETRIC tm;
  GetTextMetrics(memDC, &tm);
  int baseUnitY = tm.tmHeight;

  SIZE size;
  GetTextExtentPoint32(memDC,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", 52, &size);
  int baseUnitX = (size.cx / 26 + 1) / 2;

  DeleteDC(memDC);

#define DEFAULT_STYLES (WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS)

  for (int nIdx = 0; nIdx < nNumFields; nIdx++) {
    static struct {
      char* pszClass;
      DWORD dwStyle;
      DWORD dwRTLStyle;
      DWORD dwExStyle;
      DWORD dwRTLExStyle;
    } ClassTable[] = {
      { "STATIC",       // FIELD_LABEL
        DEFAULT_STYLES /*| WS_TABSTOP*/,
        DEFAULT_STYLES | SS_RIGHT /*| WS_TABSTOP*/,
        WS_EX_TRANSPARENT,
        WS_EX_TRANSPARENT | WS_EX_RTLREADING },
      { "STATIC",       // FIELD_ICON
        DEFAULT_STYLES /*| WS_TABSTOP*/ | SS_ICON,
        DEFAULT_STYLES /*| WS_TABSTOP*/ | SS_ICON,
        0,
        WS_EX_RTLREADING },
      { "STATIC",       // FIELD_BITMAP
        DEFAULT_STYLES /*| WS_TABSTOP*/ | SS_BITMAP | SS_CENTERIMAGE,
        DEFAULT_STYLES /*| WS_TABSTOP*/ | SS_BITMAP | SS_CENTERIMAGE,
        0,
        WS_EX_RTLREADING },
      { "BUTTON",       // FIELD_BROWSEBUTTON
        DEFAULT_STYLES | WS_TABSTOP,
        DEFAULT_STYLES | WS_TABSTOP,
        0,
        WS_EX_RTLREADING },
      { "BUTTON",       // FIELD_CHECKBOX
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX | BS_MULTILINE,
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX | BS_MULTILINE | BS_RIGHT | BS_LEFTTEXT,
        0,
        WS_EX_RTLREADING },
      { "BUTTON",       // FIELD_RADIOBUTTON
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON | BS_MULTILINE,
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON | BS_MULTILINE | BS_RIGHT | BS_LEFTTEXT,
        0,
        WS_EX_RTLREADING },
      { "EDIT",         // FIELD_TEXT
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL,
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL | ES_RIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | WS_EX_RTLREADING },
      { "EDIT",         // FIELD_FILEREQUEST
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL,
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL | ES_RIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | WS_EX_RTLREADING },
      { "EDIT",         // FIELD_DIRREQUEST
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL,
        DEFAULT_STYLES | WS_TABSTOP | ES_AUTOHSCROLL | ES_RIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | WS_EX_RTLREADING },
      { "COMBOBOX",     // FIELD_COMBOBOX
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | CBS_AUTOHSCROLL | CBS_HASSTRINGS,
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | CBS_AUTOHSCROLL | CBS_HASSTRINGS,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | WS_EX_RIGHT | WS_EX_RTLREADING },
      { "LISTBOX",      // FIELD_LISTBOX
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT,
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE | WS_EX_RIGHT | WS_EX_RTLREADING },
      { "BUTTON",       // FIELD_GROUPBOX
        DEFAULT_STYLES | BS_GROUPBOX,
        DEFAULT_STYLES | BS_GROUPBOX | BS_RIGHT,
        WS_EX_TRANSPARENT,
        WS_EX_TRANSPARENT | WS_EX_RTLREADING },
      { "BUTTON",       // FIELD_LINK
        DEFAULT_STYLES | WS_TABSTOP | BS_OWNERDRAW,
        DEFAULT_STYLES | WS_TABSTOP | BS_OWNERDRAW | BS_RIGHT,
        0,
        WS_EX_RTLREADING },
      { "BUTTON",       // FIELD_BUTTON
        DEFAULT_STYLES | WS_TABSTOP,
        DEFAULT_STYLES | WS_TABSTOP,
        0,
        WS_EX_RTLREADING }
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

    RECT rect;

    rect.left = MulDiv(pField->rect.left, baseUnitX, 4);
    rect.right = MulDiv(pField->rect.right, baseUnitX, 4);
    rect.top = MulDiv(pField->rect.top, baseUnitY, 8);
    rect.bottom = MulDiv(pField->rect.bottom, baseUnitY, 8);

    if (pField->rect.left < 0)
      rect.left += dialog_r.right - dialog_r.left;
    if (pField->rect.right < 0)
      rect.right += dialog_r.right - dialog_r.left;
    if (pField->rect.top < 0)
      rect.top += dialog_r.bottom - dialog_r.top;
    if (pField->rect.bottom < 0)
      rect.bottom += dialog_r.bottom - dialog_r.top;

    if (bRTL) {
      int right = rect.right;
      rect.right = width - rect.left;
      rect.left = width - right;
    }

    char *title = pField->pszText;
    switch (pField->nType) {
      case FIELD_ICON:
      case FIELD_BITMAP:
        title = NULL; // otherwise it is treated as the name of a resource
        break;
      case FIELD_CHECKBOX:
      case FIELD_RADIOBUTTON:
      case FIELD_BUTTON:
        if (pField->nFlags & FLAG_RIGHT)
          dwStyle |= BS_RIGHTBUTTON;
        break;
      case FIELD_TEXT:
      case FIELD_FILEREQUEST:
      case FIELD_DIRREQUEST:
        if (pField->nFlags & FLAG_PASSWORD)
          dwStyle |= ES_PASSWORD;
        if (pField->nFlags & FLAG_ONLYNUMBERS)
          dwStyle |= ES_NUMBER;
        if (pField->nFlags & FLAG_MULTILINE)
        {
          dwStyle |= ES_MULTILINE | ES_AUTOVSCROLL;
          // Enable word-wrap unless we have a horizontal scroll bar
          // or it has been explicitly disallowed
          if (!(pField->nFlags & (FLAG_HSCROLL | FLAG_NOWORDWRAP)))
            dwStyle &= ~ES_AUTOHSCROLL;
          ConvertNewLines(pField->pszState);
        }
        if (pField->nFlags & FLAG_WANTRETURN)
          dwStyle |= ES_WANTRETURN;
        if (pField->nFlags & FLAG_VSCROLL)
          dwStyle |= WS_VSCROLL;
        if (pField->nFlags & FLAG_HSCROLL)
          dwStyle |= WS_HSCROLL;
        if (pField->nFlags & FLAG_READONLY)
          dwStyle |= ES_READONLY;
        title = pField->pszState;
        break;
      case FIELD_COMBOBOX:
        dwStyle |= (pField->nFlags & FLAG_DROPLIST) ? CBS_DROPDOWNLIST : CBS_DROPDOWN;
        title = pField->pszState;
        break;
      case FIELD_LISTBOX:
        if (pField->nFlags & FLAG_EXTENDEDSEL)
          dwStyle |= LBS_EXTENDEDSEL;
        if (pField->nFlags & FLAG_MULTISELECT)
          dwStyle |= LBS_MULTIPLESEL;
        break;
    }

    if (pField->nFlags & FLAG_DISABLED) dwStyle |= WS_DISABLED;
    if (pField->nFlags & FLAG_GROUP) dwStyle |= WS_GROUP;
    if (pField->nFlags & FLAG_NOTABSTOP) dwStyle &= ~WS_TABSTOP;

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

    if (hwCtrl) {
      // Sets the font of IO window to be the same as the main window
      mySendMessage(hwCtrl, WM_SETFONT, (WPARAM)hFont, TRUE);
      // Set initial focus to the first appropriate field
      if ((hInitialFocus == hNextButton) && (dwStyle & WS_TABSTOP))
        hInitialFocus = hwCtrl;
      // make sure we created the window, then set additional attributes
      switch (pField->nType) {
        case FIELD_TEXT:
          mySendMessage(hwCtrl, WM_SETTEXT, 0, (LPARAM)title);
          // no break;
        case FIELD_FILEREQUEST:
        case FIELD_DIRREQUEST:
          mySendMessage(hwCtrl, EM_LIMITTEXT, (WPARAM)pField->nMaxLength, (LPARAM)0);
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
            while ((*pszEnd) && (*pszStart)) {
              if (*pszEnd == '|') {
                *pszEnd = '\0';
                if (pszEnd > pszStart) {
                  mySendMessage(hwCtrl, nAddMsg, 0, (LPARAM)pszStart);
                }
                // jump to the next item, skip any redundant | characters
                do { pszEnd++; } while (*pszEnd == '|');
                pszStart = pszEnd;
              }
              pszEnd++;
            }
            FREE(pszList);
            if (pField->pszState) {
              if (pField->nFlags & (FLAG_MULTISELECT|FLAG_EXTENDEDSEL) && nFindMsg == LB_FINDSTRINGEXACT) {
                mySendMessage(hwCtrl, LB_SETSEL, FALSE, -1);
                pszStart = pszEnd = pField->pszState;
                while (*pszStart) {
                  char cLast = *pszEnd;
                  if (*pszEnd == '|') *pszEnd = '\0';
                  if (!*pszEnd) {
                    if (pszEnd > pszStart) {
                      int nItem = mySendMessage(hwCtrl, nFindMsg, -1, (LPARAM)pszStart);
                      if (nItem != CB_ERR) { // CB_ERR == LB_ERR == -1
                        mySendMessage(hwCtrl, LB_SETSEL, TRUE, nItem);
                      }
                    }
                    if (cLast) {
                      do {
                        pszEnd++;
                      } while (*pszEnd == '|');
                    }
                    pszStart = pszEnd;
                  }
                  pszEnd++;
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
          mySendMessage(
            hwCtrl,
            STM_SETIMAGE,
            nImageType,
            nImage
          );
          break;
        }

#ifdef IO_ENABLE_LINK
        case FIELD_LINK:
          pField->nParentIdx = SetWindowLong(hwCtrl, GWL_WNDPROC, (long)StaticLINKWindowProc);
          break;
#endif
      }
    }
  }

  if (pszTitle)
    SetWindowText(mainwnd,pszTitle);
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
  SetFocus(hInitialFocus);

  g_done = g_NotifyField = 0;

  while (!g_done) {
    MSG msg;
    int nResult = GetMessage(&msg, NULL, 0, 0);
    if (!IsDialogMessage(hConfigWindow,&msg) && !IsDialogMessage(hMainWindow,&msg) && !TranslateMessage(&msg))
      DispatchMessage(&msg);
  }

  // we don't save settings on cancel since that means your installer will likely
  // quit soon, which means the ini might get flushed late and cause crap. :) anwyay.
  if (!g_is_cancel) SaveSettings();

  if (lpWndProcOld)
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
    FREE(pField->pszText);
    FREE(pField->pszState);
    FREE(pField->pszListItems);
    FREE(pField->pszFilter);
    FREE(pField->pszRoot);
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
  if (createCfgDlg()) {
    return;
  }
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
  initCalled++;
  createCfgDlg();
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
  if (ul_reason_for_call == DLL_THREAD_DETACH || ul_reason_for_call == DLL_PROCESS_DETACH)
    DestroyWindow(hConfigWindow);
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
    if (*pszEnd == '\0') {
      n |= LookupToken(psTable_, pszStart);
      break;
    }
    if (*pszEnd == '|') {
      *pszEnd = '\0';
      n |= LookupToken(psTable_, pszStart);
      *pszEnd = '|';
      pszStart = pszEnd + 1;
    }
    pszEnd++;
  }
  return n;
}

void WINAPI ConvertNewLines(char *str) {
  char *p1, *p2;
  if (!str) return;
  for (p1=p2=str; *p1; p1++, p2++) {
    if (*p1 == '\\') {
      switch (p1[1]) {
        case 'n':
          *p2 = '\n';
          break;
        case 'r':
          *p2 = '\r';
          break;
        case 't':
          *p2 = '\t';
          break;
        case '\\':
          *p2 = '\\';
          break;
        default:
          p1--;
          p2--;
          break;
      }
      p1++;
    }
    else *p2 = *p1;
  }
  *p2 = 0;
}

