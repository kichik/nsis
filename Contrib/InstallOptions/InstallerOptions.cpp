/*********************************************************************************
 *
 *  Original version by Michael Bishop:
 *  InstallerOptions/DLL version 2.0
 *
 *  highly modified by justin frankel to go in as dll, subclass, be sexy, and whatnot.
 *
 *  key changes
 *   - jf> updated with new exdll.h
 *   - no longer need parentwnd ini writing shit
 *   - to call now, use:
 *        Push $TEMP\inst.ini
 *        CallInstDLL $TEMP\mydll.dll dialog
 *        Pop $0
 *         ($0 would be "success" "cancel" "back" or some other value on error.
 *   - new INI entries: [settings]\cancelconfirm (text to confirm cancel on cancel button click)
 *   - fixed some flag related bugs (multiple of them at 0x100 etc)
 *   - made it so you can specify positions in negative, for distance from the right/bottom edges.
 *   - made it so that the file/dir requests automatically size the browse button in
 *   - removed a lot of code for the old style integration
 *   - removed support for silent installers (it seems the old version would bring up it's own dialog)
 *
 *   - keyboard integration fixed
 *   - fixed issues with file open dialog too
 *
 *   - added BackEnabled, fixed more (getting it ready to use with closer integration to nsis 1.90)
 *
 *   - results are now read differently from the .ini file. Instead of [Results]\<number>,
 *     use [Field <number>]\State
 *
 *   - The state of checkboxes and radioboxes is now defined by State=. State=1 is checked,
 *     State=0 (or no State=) is unchecked.
 *
 *   - The initial contents of edit controls and file/dir request controls is now defined by
 *     State= instead of Text=.
 *
 *   - Font is now taken from the main NSIS window (by Amir Szekely 22nd July 2002)
 *
 *   - Added CancelEnabled (by ORTIM: 13-August-2002)
 *   - Added CancelShow (by ORTIM: 13-August 2002)
 *   - Added pixel transformation for widgets (by ORTIM: 14-August-2002)
 *
 *   - Added CancelConfirmCaption and CancelConfirmFlags (by Amir Szekely & Dave Laundon)
 *
 *   - Added Icon and Bitmap controls (by Amir Szekely 4th September 2002)
 *
 *   - Added initDialog and show for support with NSIS's new CreateFont and SetStaticBkColor
 *
 *   Version 2.0 - Changes by Joost Verburg
 *
 *   - Works with custom font and DPI settings
 *
 *   - INI files should contain dialog units now, no pixels
 *
 *  Copyright (C) 2001 Michael Bishop
 *  Portions Copyright (C) 2001 Nullsoft, Inc.
 *
 *  This software is provided 'as-is', without any express or implied
 *  warranty.  In no event will the authors be held liable for any damages
 *  arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented; you must not
 *     claim that you wrote the original software. If you use this software
 *     in a product, an acknowledgment in the product documentation would be
 *     appreciated but is not required.
 *  2. Altered source versions must be plainly marked as such, and must not be
 *     misrepresented as being the original software.
 *  3. This notice may not be removed or altered from any source distribution.
 *
 **********************************************************************************/

#include <windows.h>
#include <windowsx.h>
#include <shlobj.h>
#include <atlbase.h>
#include <stdio.h>
#include <commdlg.h>
#include "resource.h"

#include "../exdll/exdll.h"

#define strcpy(x,y) lstrcpy(x,y)
#define strncpy(x,y,z) lstrcpyn(x,y,z)
#define strdup(x) STRDUP(x)
#define stricmp(x,y) lstrcmpi(x,y)
//#define abs(x) ((x) < 0 ? -(x) : (x))

void *MALLOC(int len) { return (void*)GlobalAlloc(GPTR,len); }
void FREE(void *d) { if (d) GlobalFree((HGLOBAL)d); }

char *STRDUP(const char *c)
{
  char *t=(char*)MALLOC(lstrlen(c)+1);
  lstrcpy(t,c);
  return t;
}

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

// general flags
#define FLAG_BOLD          (1)
#define FLAG_RIGHT         (2)
#define FLAG_DISABLED      (4)
#define FLAG_GROUP         (8)
#define FLAG_NOTABSTOP     (16)

// text box flags
#define FLAG_PASSWORD      (32)

// listbox flags
#define FLAG_MULTISELECT   (64)

// combobox flags
#define FLAG_DROPLIST      (128)

// bitmap flags
#define FLAG_RESIZETOFIT   (256)

struct TableEntry {
  char *pszName;
  int   nValue;
};

int LookupToken(TableEntry*, char*);
int LookupTokens(TableEntry*, char*);

struct FieldType {
  char *pszText;
  char *pszState;
  char *pszRoot;

  char *pszListItems;
  char *pszFilter;

  int   nType;
  RECT  rect;

  int   nMinLength;
  int   nMaxLength;
  char  *pszValidateText;

  int   nFlags;
  bool  bSaveDlg;

  HWND  hwnd;
  UINT  nControlID;

  int   nParentIdx;  // this is used by the filerequest and dirrequest controls
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

HINSTANCE m_hInstance = NULL;

char *pszFilename         = NULL;
char *pszTitle            = NULL;
char *pszCancelButtonText = NULL;
char *pszNextButtonText   = NULL;
char *pszBackButtonText   = NULL;

int bBackEnabled   = FALSE;
int bCancelEnabled = FALSE;   // by ORTIM: 13-August-2002
int bCancelShow    = FALSE;   // by ORTIM: 13-August-2002

FieldType *pFields   = NULL;
#define DEFAULT_RECT 1018
int nRectId          = 0;
int nNumFields       = 0;
int g_done;


// array of HWNDs and window styles used to make the main NSIS controls invisible while this program runs.

bool BrowseForFile(int nControlIdx) {
  OPENFILENAME ofn={0,};
  HWND hControl;
  BOOL bResult;
  FieldType *pThisField = &pFields[nControlIdx];

  hControl = pThisField->hwnd;

  ofn.Flags = pThisField->nFlags;

//  ofn.hInstance = m_hInstance;  // no templates so we can leave this at NULL;
  ofn.hwndOwner = hConfigWindow;
//  ofn.lCustData = NULL;
//  ofn.lpfnHook  = NULL;
//  ofn.lpstrCustomFilter = NULL;
//  ofn.lpstrDefExt = NULL;
  ofn.nMaxFile  = MAX_PATH;
  ofn.lpstrFile = (char*)MALLOC(MAX_PATH);

//  ofn.nMaxFileTitle = MAX_PATH;  // we ignore this for simplicity, leave lpstrFileTitle at NULL
//  ofn.lpstrFileTitle = new char [ofn.nMaxFileTitle];

  ofn.lpstrFilter = pThisField->pszFilter;      // TODO: implement this
//  ofn.lpstrInitialDir = NULL;  // for now, just use the default initial directory.
//  ofn.lpstrTitle = NULL;      // TODO: implement this
//  ofn.lpTemplateName = NULL;
  ofn.lStructSize = sizeof(ofn);
//  ofn.nFileExtension     // this is output variable, leave it to 0 for now.
//  ofn.nFileOffset        // this is output variable, leave it to 0 for now.
//  ofn.nFilterIndex = 1;  // since we use no custom filters, leaving it at 0 means use the first.
//  ofn.nMaxCustFilter = 0;

  GetWindowText(hControl, ofn.lpstrFile, MAX_PATH);

  //for(;;) {
    if (pThisField->bSaveDlg) {
      bResult = GetSaveFileName(&ofn);
    } else {
      bResult = GetOpenFileName(&ofn);
    }
    if (bResult) {
      SetWindowText(hControl, ofn.lpstrFile);
      return true;
    }
    // check this because the dialog will sometimes return this error just because a directory is specified
    // instead of a filename.  in this case, try it without the initial filename and see if that works.
//    if (*(ofn.lpstrFile)) { //&& (CommDlgExtendedError() == FNERR_INVALIDFILENAME)) {
  //    *(ofn.lpstrFile) = '\0';
   // } else {
      //break;
   // }
  //}

  return false;
}


int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lp, LPARAM pData) {
   TCHAR szDir[MAX_PATH];

   if (uMsg == BFFM_INITIALIZED) {
      if (GetWindowText(pFields[(int)pData].hwnd, szDir, MAX_PATH) > 0) {
        SendMessage(hwnd, BFFM_SETSELECTION, TRUE, (LPARAM)szDir);
      }
    }
   return 0;
}


bool BrowseForFolder(int nControlIdx) {
  BROWSEINFO bi;

  bi.hwndOwner = hConfigWindow;
  bi.pidlRoot = NULL;
  bi.pszDisplayName = (char*)MALLOC(MAX_PATH);
  bi.lpszTitle = pFields[nControlIdx].pszText;
  bi.ulFlags = BIF_STATUSTEXT;
  bi.lpfn = BrowseCallbackProc;
  bi.lParam = nControlIdx;
  bi.iImage = 0;

  if (pFields[nControlIdx].pszRoot) {
	  LPSHELLFOLDER sf;
	  ULONG eaten;
	  LPITEMIDLIST root;
#define _alloca MALLOC
	  USES_CONVERSION;
	  LPOLESTR s = A2OLE(pFields[nControlIdx].pszRoot);
#undef _alloca
	  SHGetDesktopFolder(&sf);
	  sf->ParseDisplayName(hConfigWindow, NULL, (unsigned short *)s, &eaten, &root, NULL);
	  bi.pidlRoot = root;
	  sf->Release();
	  FREE(s);
  }
//  CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
  LPITEMIDLIST pResult = SHBrowseForFolder(&bi);
  if (!pResult) {
    FREE(bi.pszDisplayName);
    return false;
  }

  char *pszFolder = (char*)MALLOC(MAX_PATH);
  if (SHGetPathFromIDList(pResult, pszFolder)) {
    SetWindowText(pFields[nControlIdx].hwnd, pszFolder);
  }

  LPMALLOC pMalloc;
  if (!SHGetMalloc(&pMalloc)) {
    pMalloc->Free(pResult);
  }

  FREE(bi.pszDisplayName);
  FREE(pszFolder);

  return true;
}

bool ValidateFields() {
  int nIdx;
  int nLength;

  // In the unlikely event we can't allocate memory, go ahead and return true so we can get out of here.
  // May cause problems for the install script, but no memory is problems for us.
  for (nIdx = 0; nIdx < nNumFields; nIdx++) {
    // this if statement prevents a stupid bug where a min/max length is assigned to a label control
    //   where the user obviously has no way of changing what is displayed. (can you say, "infinite loop"?)
    if (pFields[nIdx].nType >= FIELD_TEXT) {
      nLength = SendMessage(pFields[nIdx].hwnd, WM_GETTEXTLENGTH, 0, 0);

      if (((pFields[nIdx].nMaxLength > 0) && (nLength > pFields[nIdx].nMaxLength)) ||
         ((pFields[nIdx].nMinLength > 0) && (nLength < pFields[nIdx].nMinLength))) {
        if (pFields[nIdx].pszValidateText) {
          MessageBox(hConfigWindow, pFields[nIdx].pszValidateText, NULL, MB_OK);
        }
        SetFocus(pFields[nIdx].hwnd);
        return false;
      }

    }
  }
  return true;
}

bool SaveSettings(void) {
  static char szField[25];
  int nIdx;
  HWND hwnd;
  int nBufLen = MAX_BUFFER_LENGTH;
  char *pszBuffer = (char*)MALLOC(nBufLen);

  if (!pszBuffer) return false;
  for(nIdx = 0; nIdx < nNumFields; nIdx++) {
    hwnd = pFields[nIdx].hwnd;
    wsprintf(szField, "Field %d", nIdx + 1);
    switch(pFields[nIdx].nType) {
      case FIELD_CHECKBOX:
      case FIELD_RADIOBUTTON:
        {
          wsprintf(pszBuffer, "%d", !!SendMessage(hwnd, BM_GETCHECK, 0, 0));
          break;
        }
      case FIELD_LISTBOX:
        {
          // Ok, this one requires a bit of work.
          // First, we allocate a buffer long enough to hold every item.
          // Then, we loop through every item and if it's selected we add it to our buffer.
          // If there is already an item in the list, then we prepend a | character before the new item.
          // We could simplify for single-select boxes, but using one piece of code saves some space.
          int nLength = lstrlen(pFields[nIdx].pszListItems) + 10;
          if (nLength > nBufLen) {
            FREE(pszBuffer);
            nBufLen = nLength;
            pszBuffer = (char*)MALLOC(nBufLen);
            if (!pszBuffer) return false;
          }
          char *pszItem = (char*)MALLOC(nBufLen);

          *pszBuffer = '\0';
          int nNumItems = SendMessage(hwnd, LB_GETCOUNT, 0, 0);
          for(int nIdx2 = 0; nIdx2 < nNumItems; nIdx2++) {
            if (SendMessage(hwnd, LB_GETSEL, nIdx2, 0) > 0) {
              if (*pszBuffer) lstrcat(pszBuffer, "|");
              SendMessage(hwnd, LB_GETTEXT, (WPARAM)nIdx2, (LPARAM)pszItem);
              lstrcat(pszBuffer, pszItem);
            }
          }

          FREE(pszItem);
          break;
        }
      default:
        {
          int nLength = SendMessage(pFields[nIdx].hwnd, WM_GETTEXTLENGTH, 0, 0);
          if (nLength > nBufLen) {
            FREE(pszBuffer);
            // add a bit extra so we do this less often
            nBufLen = nLength + 20;
            pszBuffer = (char*)MALLOC(nBufLen);
            if (!pszBuffer) return false;
          }
          GetWindowText(hwnd, pszBuffer, nBufLen);
          break;
        }
    }
    WritePrivateProfileString(szField, "STATE", pszBuffer, pszFilename);
  }

  FREE(pszBuffer);

  return true;
}

void AddBrowseButtons() {
  // this function loops through all the controls and if a filerequest or dirrequest
  // control is found, then it adds the corresponding browse button.
  // NOTE: this also resizes the text box created to make room for the button.
  int nIdx;
  int nWidth = 15;
  FieldType *pNewField;

  for (nIdx = nNumFields - 1; nIdx >= 0; nIdx--) {
    // we loop down so we don't run into the newly added fields.
    switch (pFields[nIdx].nType) {
      case FIELD_FILEREQUEST:
      case FIELD_DIRREQUEST:
        pNewField = &pFields[nNumFields];
        // nNumFields functions as the index of the new control, increment at *end* of loop
        pNewField->nControlID = 1200 + nNumFields;
        pNewField->nParentIdx = nIdx;
        pNewField->nType = FIELD_BROWSEBUTTON;
        //pNewField->pszListItems = NULL;
        //pNewField->nMaxLength = 0;
        //pNewField->nMinLength = 0;
        pNewField->pszText = szBrowseButtonCaption; //STRDUP("...");
        //pNewField->pszValidateText = NULL;

        pNewField->rect.right  = pFields[nIdx].rect.right;
        pNewField->rect.left   = pNewField->rect.right - nWidth;
        pNewField->rect.bottom = pFields[nIdx].rect.bottom;
        pNewField->rect.top    = pFields[nIdx].rect.top;

        pFields[nIdx].rect.right = pNewField->rect.left - 3;

        nNumFields++;
        break;
    }
  }
}

static char szResult[4096];

DWORD WINAPI myGetProfileString(LPCTSTR lpAppName, LPCTSTR lpKeyName)
{
  *szResult = '\0';
  return GetPrivateProfileString(lpAppName, lpKeyName, "", szResult, sizeof(szResult), pszFilename);
}

char * WINAPI myGetProfileStringDup(LPCTSTR lpAppName, LPCTSTR lpKeyName)
{
  if (myGetProfileString(lpAppName, lpKeyName))
    return strdup(szResult);
  else
    return NULL;
}

bool ReadSettings(void) {
  static char szField[25];
  int nIdx;

  pszTitle = myGetProfileStringDup("Settings", "Title");
  pszCancelButtonText = myGetProfileStringDup("Settings", "CancelButtonText");
  pszNextButtonText = myGetProfileStringDup("Settings", "NextButtonText");
  pszBackButtonText = myGetProfileStringDup("Settings", "BackButtonText");

  nNumFields = GetPrivateProfileInt("Settings", "NumFields", 0, pszFilename);

  nRectId = GetPrivateProfileInt("Settings", "Rect", DEFAULT_RECT, pszFilename);

  bBackEnabled = GetPrivateProfileInt("Settings", "BackEnabled", 0xFFFF0000, pszFilename);
  // by ORTIM: 13-August-2002
  bCancelEnabled = GetPrivateProfileInt("Settings", "CancelEnabled", 0xFFFF0000, pszFilename);
  bCancelShow = GetPrivateProfileInt("Settings", "CancelShow", 0xFFFF0000, pszFilename);

  if (nNumFields > 0) {
    // make this twice as large for the worst case that every control is a browse button.
    // the structure is small enough that this won't waste much memory.
    // if the structure gets much larger, we should switch to a linked list.
    pFields = (FieldType *)MALLOC(sizeof(FieldType)*2*nNumFields);
  }

  for(nIdx = 0; nIdx < nNumFields; nIdx++) {
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
      { NULL,          0                 }
    };
    // Control flags
    static TableEntry FlagTable[] = {
      { "FILE_MUST_EXIST",   OFN_FILEMUSTEXIST   },
      { "PATH_MUST_EXIST",   OFN_PATHMUSTEXIST   },
      { "WARN_IF_EXIST",     OFN_OVERWRITEPROMPT },
      { "PROMPT_CREATE",     OFN_CREATEPROMPT    },
      { "RIGHT",             FLAG_RIGHT          },
      { "PASSWORD",          FLAG_PASSWORD       },
      { "DROPLIST",          FLAG_DROPLIST       },
      { "MULTISELECT",       FLAG_MULTISELECT    },
      { "FILE_EXPLORER",     OFN_EXPLORER        },
      { "FILE_HIDEREADONLY", OFN_HIDEREADONLY    },
      { "RESIZETOFIT",       FLAG_RESIZETOFIT    },
      { "GROUP",             FLAG_GROUP          },
      { "DISABLED",          FLAG_DISABLED       },
      { "NOTABSTOP",         FLAG_NOTABSTOP      },
/*
      { "NO_ALPHA",          0                   },
      { "NO_NUMBERS",        0                   },
      { "NO_SYMBOLS",        0                   },
      { "BOLD",              FLAG_BOLD           },
*/
      { NULL,                0                   }
    };

    wsprintf(szField, "Field %d", nIdx + 1);
    myGetProfileString(szField, "TYPE");

    // Get the control type
    pFields[nIdx].nType = LookupToken(TypeTable, szResult);
    if (!pFields[nIdx].nType)
      continue;

    // Lookup flags associated with the control type
    pFields[nIdx].nFlags |= LookupToken(FlagTable, szResult);

    pFields[nIdx].pszText = myGetProfileStringDup(szField, "TEXT");
    if (pFields[nIdx].nType == FIELD_LABEL) {
      char *p1, *p2;
      for (p1=p2=pFields[nIdx].pszText; *p1; p1++, p2++) {
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

    // pszState cannot be NULL (?)
    myGetProfileString(szField, "STATE");
    pFields[nIdx].pszState = STRDUP(szResult);

    pFields[nIdx].pszRoot = myGetProfileStringDup(szField, "ROOT");

    {
      int nResult = myGetProfileString(szField, "ListItems");
      if (nResult) {
        // add an extra | character to the end to simplify the loop where we add the items.
        pFields[nIdx].pszListItems = (char*)MALLOC(nResult + 2);
        wsprintf(pFields[nIdx].pszListItems, "%s|", szResult);
      }
    }
    pFields[nIdx].nMaxLength = GetPrivateProfileInt(szField, "MaxLen", 0, pszFilename);
    pFields[nIdx].nMinLength = GetPrivateProfileInt(szField, "MinLen", 0, pszFilename);

    pFields[nIdx].pszValidateText = myGetProfileStringDup(szField, "ValidateText");
    if (pFields[nIdx].pszValidateText) {
      // translate backslash-n in the input into actual carriage-return/line-feed characters.
      for (char *pPos = pFields[nIdx].pszValidateText; *pPos; pPos++) {
        if (*pPos == '\\') {
          if ((*(pPos + 1) == 'n') || (*(pPos + 1) == 'N')) {
            *pPos = '\r';
            *(pPos + 1) = '\n';
/*
          } else if (*(pPos + 1) == '\\') {
            // if it's 2 backslash in a row, then skip the second.
            pPos++;
*/
          }
        }
      }
    }

    {
      int nResult = GetPrivateProfileString(szField, "Filter", "All Files|*.*", szResult, sizeof(szResult), pszFilename);
      if (nResult) {
        // add an extra | character to the end to simplify the loop where we add the items.
        pFields[nIdx].pszFilter = (char*)MALLOC(nResult + 2);
        strcpy(pFields[nIdx].pszFilter, szResult);
        char *pszPos = pFields[nIdx].pszFilter;
        while (*pszPos) {
          if (*pszPos == '|') *pszPos = '\0';
          pszPos++;
        }
      }
    }

    pFields[nIdx].rect.left = GetPrivateProfileInt(szField, "LEFT", 0, pszFilename);
    pFields[nIdx].rect.right = GetPrivateProfileInt(szField, "RIGHT", 0, pszFilename);
    pFields[nIdx].rect.top = GetPrivateProfileInt(szField, "TOP", 0, pszFilename);
    pFields[nIdx].rect.bottom = GetPrivateProfileInt(szField, "BOTTOM", 0, pszFilename);

    if (myGetProfileString(szField, "Flags")) {
      // append the | to make parsing a bit easier
      if (lstrlen(szResult)<sizeof(szResult)-1) lstrcat(szResult, "|");
      // parse the flags text
      char *pszStart, *pszEnd;
      pszStart = pszEnd = szResult;
      while ((*pszEnd) && (*pszStart)) {
        if (*pszEnd == '|') {
          *pszEnd = '\0';
          if (pszEnd > pszStart) {
            if (!stricmp("REQ_SAVE", pszStart)) {
              pFields[nIdx].bSaveDlg = true;
            } else {
              // v1.3 converted this to a table lookup.
              // I think it's a bit larger now, but we can
              //   add new flags with very little overhead.
              pFields[nIdx].nFlags |= LookupToken(FlagTable, pszStart);
            }
          }
          // jump to the next item, skip any redundant | characters
          do { pszEnd++; } while (*pszEnd == '|');
          pszStart = pszEnd;
        }
        pszEnd++;
      }
    }

    pFields[nIdx].nControlID = 1200 + nIdx;
  }

  AddBrowseButtons();

  return true;
}


LRESULT WMCommandProc(HWND hWnd, UINT id, HWND hwndCtl, UINT codeNotify) {
	switch (codeNotify) {
		case BN_CLICKED:
      {
        for (int nIdx = 0; nIdx < nNumFields; nIdx++) {
          if (pFields[nIdx].nType == FIELD_BROWSEBUTTON) {
            if (id == pFields[nIdx].nControlID) {
              int nParentIdx = pFields[nIdx].nParentIdx;

              switch(pFields[nParentIdx].nType) {
                case FIELD_FILEREQUEST:
                  BrowseForFile(nParentIdx);
                  break;
                case FIELD_DIRREQUEST:
                  BrowseForFolder(nParentIdx);
                  break;
              }
              break;
            }
          }
        }
      }
			break;
	}
	return 0;
}


static void *lpWndProcOld;

static LRESULT CALLBACK ParentWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  if (message == WM_NOTIFY_OUTER_NEXT)
  {
    PostMessage(hConfigWindow,WM_USER+666,wParam,0);
  }
  return CallWindowProc((long (__stdcall *)(struct HWND__ *,unsigned int,unsigned int,long))lpWndProcOld,hwnd,message,wParam,lParam);
}

int g_is_cancel,g_is_back;


BOOL CALLBACK cfgDlgProc(HWND   hwndDlg,
								 UINT   uMsg,
								 WPARAM wParam,
								 LPARAM lParam)
{
  switch (uMsg)
  {
    HANDLE_MSG(hwndDlg, WM_COMMAND, WMCommandProc);
    return 0;
    case WM_USER+666:
      if (wParam == NOTIFY_BYE_BYE || wParam == -1 || ValidateFields()) {
        if (wParam == -1) g_is_back++;
        if (wParam == NOTIFY_BYE_BYE) g_is_cancel++;
        g_done++;
        PostMessage(hwndDlg,WM_CLOSE,0,0);
      }
    break;
    case WM_CTLCOLORSTATIC:
    case WM_CTLCOLORDLG:
    {
      COLORREF color = GetWindowLong((HWND)lParam, GWL_USERDATA);
      if (color) {
        LOGBRUSH b={BS_SOLID, color-1, 0};
        SetBkColor((HDC)wParam, b.lbColor);
        return (BOOL)CreateBrushIndirect(&b);
      }
    }
  }
	return 0;
}

int nIdx;
HWND childwnd;
int cw_vis;
int was_ok_enabled;
char old_cancel[256];
char old_ok[256];
char old_back[256];
int old_cancel_enabled;
int old_cancel_visible;
char old_title[1024];

int createCfgDlg()
{
  UINT nAddMsg;

  g_is_back=0;
  g_is_cancel=0;

  if (!hMainWindow)
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

  childwnd=FindWindowEx(hMainWindow,NULL,"#32770",NULL); // find window to replace
  if (!childwnd || nRectId != DEFAULT_RECT) childwnd=GetDlgItem(hMainWindow,nRectId);
  if (!childwnd)
  {
    popstring(NULL);
    pushstring("error finding childwnd");
    return 1;
  }

  cw_vis=IsWindowVisible(childwnd);
  if (cw_vis) ShowWindow(childwnd,SW_HIDE);

  hCancelButton = GetDlgItem(hMainWindow,IDCANCEL);
  hNextButton = GetDlgItem(hMainWindow,IDOK);
  hBackButton = GetDlgItem(hMainWindow,3);

  was_ok_enabled=EnableWindow(hNextButton,1);
  GetWindowText(hCancelButton,old_cancel,sizeof(old_cancel));
  if (pszCancelButtonText) SetWindowText(hCancelButton,pszCancelButtonText);
  GetWindowText(hNextButton,old_ok,sizeof(old_ok));
  if (pszNextButtonText) SetWindowText(hNextButton,pszNextButtonText);
  GetWindowText(hBackButton,old_back,sizeof(old_back));
  if (pszBackButtonText) SetWindowText(hBackButton,pszBackButtonText);

  if (bBackEnabled!=0xFFFF0000) EnableWindow(hBackButton,bBackEnabled);
  // by ORTIM: 13-August-2002
  if (bCancelEnabled!=0xFFFF0000) old_cancel_enabled=!EnableWindow(hCancelButton,bCancelEnabled);
  if (bCancelShow!=0xFFFF0000) old_cancel_visible=ShowWindow(hCancelButton,bCancelShow?SW_SHOWNA:SW_HIDE);

  // Added by Amir Szekely 22nd July 2002
  HFONT hFont = (HFONT)SendMessage(hMainWindow, WM_GETFONT, 0, 0);

  RECT dialog_r;
  hConfigWindow=CreateDialog(m_hInstance,MAKEINTRESOURCE(IDD_DIALOG1),hMainWindow,cfgDlgProc);
  if (hConfigWindow)
  {
    GetWindowRect(childwnd,&dialog_r);
    ScreenToClient(hMainWindow,(LPPOINT)&dialog_r);
    ScreenToClient(hMainWindow,((LPPOINT)&dialog_r)+1);
    SetWindowPos(hConfigWindow,0,dialog_r.left,dialog_r.top,dialog_r.right-dialog_r.left,dialog_r.bottom-dialog_r.top,SWP_NOZORDER|SWP_NOACTIVATE);
    // Added by Amir Szekely 22nd July 2002
    // Sets the font of IO window to be the same as the main window
    SendMessage(hConfigWindow, WM_SETFONT, (WPARAM)hFont, TRUE);
  }
  else
  {
    popstring(NULL);
    pushstring("error creating dialog");
    return 1;
  }

  // By Joost Verburg 14th December 2002
  // Works with custom font & DPI settings
  // INI files should contain dialog units now

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

  for (nIdx = 0; nIdx < nNumFields; nIdx++) {
    static struct {
      char* pszClass;
      DWORD dwStyle;
      DWORD dwExStyle;
    } ClassTable[] = {
      { "STATIC",       // FIELD_LABEL
        DEFAULT_STYLES /*| WS_TABSTOP*/,
        WS_EX_TRANSPARENT },
      { "STATIC",       // FIELD_ICON
        DEFAULT_STYLES /*| WS_TABSTOP*/ | SS_ICON,
        0 },
      { "STATIC",       // FIELD_BITMAP
        DEFAULT_STYLES /*| WS_TABSTOP*/ | SS_BITMAP,
        0 },
      { "BUTTON",       // FIELD_BROWSEBUTTON
        DEFAULT_STYLES | WS_TABSTOP,
        0 },
      { "BUTTON",       // FIELD_CHECKBOX
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX,
        0 },
      { "BUTTON",       // FIELD_RADIOBUTTON
        DEFAULT_STYLES | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON,
        0 },
      { "EDIT",         // FIELD_TEXT
        DEFAULT_STYLES | WS_TABSTOP | WS_BORDER | ES_AUTOHSCROLL,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE },
      { "EDIT",         // FIELD_FILEREQUEST
        DEFAULT_STYLES | WS_TABSTOP | WS_BORDER | ES_AUTOHSCROLL,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE },
      { "EDIT",         // FIELD_DIRREQUEST
        DEFAULT_STYLES | WS_TABSTOP | WS_BORDER | ES_AUTOHSCROLL,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE },
      { "COMBOBOX",     // FIELD_COMBOBOX
        DEFAULT_STYLES | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | CBS_AUTOHSCROLL | CBS_HASSTRINGS,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE },
      { "LISTBOX",      // FIELD_LISTBOX
        DEFAULT_STYLES | WS_TABSTOP | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE },
      { "BUTTON",       // FIELD_GROUPBOX
        DEFAULT_STYLES | BS_GROUPBOX,
        WS_EX_TRANSPARENT }
    };

#undef DEFAULT_STYLES

    if (pFields[nIdx].nType < 1 || pFields[nIdx].nType > (sizeof(ClassTable) / sizeof(ClassTable[0])))
      continue;

    DWORD dwStyle = ClassTable[pFields[nIdx].nType - 1].dwStyle;
    DWORD dwExStyle = ClassTable[pFields[nIdx].nType - 1].dwExStyle;

    // By Joost Verburg 14th December 2002
    // Works with custom font & DPI settings
    // INI files should contain dialog units now

    RECT rect;

    rect.left = MulDiv(pFields[nIdx].rect.left, baseUnitX, 4);
    rect.right = MulDiv(pFields[nIdx].rect.right, baseUnitX, 4);
    rect.top = MulDiv(pFields[nIdx].rect.top, baseUnitY, 8);
    rect.bottom = MulDiv(pFields[nIdx].rect.bottom, baseUnitY, 8);

    if (pFields[nIdx].rect.left < 0)
      rect.left += dialog_r.right - dialog_r.left;
    if (pFields[nIdx].rect.right < 0)
      rect.right += dialog_r.right - dialog_r.left;
    if (pFields[nIdx].rect.top < 0)
      rect.top += dialog_r.bottom - dialog_r.top;
    if (pFields[nIdx].rect.bottom < 0)
      rect.bottom += dialog_r.bottom - dialog_r.top;

    char *title = pFields[nIdx].pszText;
    switch (pFields[nIdx].nType) {
      case FIELD_CHECKBOX:
      case FIELD_RADIOBUTTON:
        if (pFields[nIdx].nFlags & FLAG_RIGHT)
          dwStyle |= BS_RIGHTBUTTON;
        break;
      case FIELD_FILEREQUEST:
      case FIELD_DIRREQUEST:
      case FIELD_TEXT:
        if (pFields[nIdx].nFlags & FLAG_PASSWORD)
          dwStyle |= ES_PASSWORD;
        title = pFields[nIdx].pszState;
        break;
      case FIELD_COMBOBOX:
        dwStyle |= (pFields[nIdx].nFlags & FLAG_DROPLIST) ? CBS_DROPDOWNLIST : CBS_DROPDOWN;
        title = pFields[nIdx].pszState;
        break;
      case FIELD_LISTBOX:
        if (pFields[nIdx].nFlags & FLAG_MULTISELECT)
          dwStyle |= LBS_EXTENDEDSEL;
        break;
    }

    if (pFields[nIdx].nFlags & FLAG_DISABLED) dwStyle |= WS_DISABLED;
    if (pFields[nIdx].nFlags & FLAG_GROUP) dwStyle |= WS_GROUP;
    if (pFields[nIdx].nFlags & FLAG_NOTABSTOP) dwStyle &= ~WS_TABSTOP;

    HWND hwCtrl = pFields[nIdx].hwnd = CreateWindowEx(
      dwExStyle,
      ClassTable[pFields[nIdx].nType - 1].pszClass,
      title,
      dwStyle,
      rect.left,
      rect.top,
      rect.right - rect.left,
      rect.bottom - rect.top,
      hConfigWindow,
      (HMENU)pFields[nIdx].nControlID,
      m_hInstance,
      NULL
    );

    if (hwCtrl) {
      // Changed by Amir Szekely 22nd July 2002
      // Sets the font of IO window to be the same as the main window
      SendMessage(hwCtrl, WM_SETFONT, (WPARAM)hFont, TRUE);
      // make sure we created the window, then set additional attributes
      if (pFields[nIdx].nMaxLength > 0) {
        switch (pFields[nIdx].nType) {
          case FIELD_TEXT:
          case FIELD_DIRREQUEST:
          case FIELD_FILEREQUEST:
            SendMessage(hwCtrl, EM_LIMITTEXT, (WPARAM)pFields[nIdx].nMaxLength, (LPARAM)0);
            break;
        }
      }
      if ((pFields[nIdx].nType == FIELD_CHECKBOX) || (pFields[nIdx].nType == FIELD_RADIOBUTTON)) {
        if (pFields[nIdx].pszState[0] == '1')
        {
          SendMessage(hwCtrl, BM_SETCHECK, (WPARAM)BST_CHECKED, 0);
        }
      } else if (
                 ((pFields[nIdx].nType == FIELD_COMBOBOX) && (nAddMsg = CB_ADDSTRING)) ||
                 ((pFields[nIdx].nType == FIELD_LISTBOX ) && (nAddMsg = LB_ADDSTRING))
                 ) {
        // if this is a listbox or combobox, we need to add the list items.
        char *pszStart, *pszEnd;
        pszStart = pszEnd = pFields[nIdx].pszListItems;
        while ((*pszEnd) && (*pszStart)) {
          if (*pszEnd == '|') {
            *pszEnd = '\0';
            if (pszEnd > pszStart) {
              SendMessage(hwCtrl, nAddMsg, 0, (LPARAM)pszStart);
            }
            // jump to the next item, skip any redundant | characters
            do { pszEnd++; } while (*pszEnd == '|');
            pszStart = pszEnd;
          }
          pszEnd++;
        }
        if (pFields[nIdx].pszState) {
          int nItem = SendMessage(hwCtrl, CB_FINDSTRINGEXACT, -1, (LPARAM)pFields[nIdx].pszState);
          if (nItem != CB_ERR) {
            SendMessage(hwCtrl, CB_SETCURSEL, nItem, 0);
          }
        }
      } else if (pFields[nIdx].nType == FIELD_BITMAP || pFields[nIdx].nType == FIELD_ICON) {
        WPARAM nImageType = pFields[nIdx].nType == FIELD_BITMAP ? IMAGE_BITMAP : IMAGE_ICON;
        SendMessage(
          hwCtrl,
          STM_SETIMAGE,
          nImageType,
          pFields[nIdx].pszText?
          (LPARAM)LoadImage(
            0,
            pFields[nIdx].pszText,
            nImageType,
            // Scaling an icon/bitmap in relation to dialog units usually looks crap, so
            // take the size originally specified as pixels, *unless* it seems likely the
            // image is required to span the whole dialog.
            (pFields[nIdx].rect.right - pFields[nIdx].rect.left > 0 && !(pFields[nIdx].nFlags & FLAG_RESIZETOFIT))
              ? (pFields[nIdx].rect.right - pFields[nIdx].rect.left)
              : (rect.right - rect.left),
            (pFields[nIdx].rect.right - pFields[nIdx].rect.left > 0 && !(pFields[nIdx].nFlags & FLAG_RESIZETOFIT))
              ? (pFields[nIdx].rect.bottom - pFields[nIdx].rect.top)
              : (rect.bottom - rect.top),
            LR_LOADFROMFILE
          ):(LPARAM)LoadIcon(GetModuleHandle(0), MAKEINTRESOURCE(103))
        );
      }
    }
  }

  if (pszTitle)
  {
    GetWindowText(hMainWindow,old_title,sizeof(old_title));
    SetWindowText(hMainWindow,pszTitle);
  }
  char tmp[32];
  wsprintf(tmp,"%d",hConfigWindow);
  pushstring(tmp);
  return 0;
}

void showCfgDlg()
{
  lpWndProcOld = (void *) SetWindowLong(hMainWindow,GWL_WNDPROC,(long)ParentWndProc);

  SendMessage(hMainWindow, WM_NOTIFY_CUSTOM_READY, 0, 0);
  ShowWindow(hConfigWindow, SW_SHOWNA);
  SetFocus(hNextButton);

  g_done=0;

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
    SetWindowLong(hMainWindow,GWL_WNDPROC,(long)lpWndProcOld);
  DestroyWindow(hConfigWindow);
  if (was_ok_enabled) EnableWindow(hNextButton,0);
  SetWindowText(hCancelButton,old_cancel);
  SetWindowText(hNextButton,old_ok);
  SetWindowText(hBackButton,old_back);

  // by ORTIM: 13-August-2002
  if (bCancelEnabled!=0xFFFF0000) EnableWindow(hCancelButton,old_cancel_enabled);
  if (bCancelShow!=0xFFFF0000) ShowWindow(hCancelButton,old_cancel_visible?SW_SHOWNA:SW_HIDE);

  if (pszTitle) SetWindowText(hMainWindow,old_title);

  if (cw_vis) ShowWindow(childwnd,SW_SHOWNA);

  FREE(pszTitle);
  FREE(pszCancelButtonText);
  FREE(pszNextButtonText);
  FREE(pszBackButtonText);
  for (nIdx = 0; nIdx < nNumFields; nIdx++) {
    FREE(pFields[nIdx].pszText);
    FREE(pFields[nIdx].pszState);
    FREE(pFields[nIdx].pszListItems);
    FREE(pFields[nIdx].pszFilter);
    FREE(pFields[nIdx].pszRoot);
  }
  FREE(pFields);

  popstring(NULL);
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

#ifdef DEBUG
extern "C" BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
#else
extern "C" BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
#endif
{
  m_hInstance=(HINSTANCE) hInst;
  if (ul_reason_for_call == DLL_THREAD_DETACH || ul_reason_for_call == DLL_PROCESS_DETACH)
    DestroyWindow(hConfigWindow);
	return TRUE;
}


int LookupToken(TableEntry* psTable_, char* pszToken_)
{
  for (int i = 0; psTable_[i].pszName; i++)
    if (!stricmp(pszToken_, psTable_[i].pszName))
      return psTable_[i].nValue;
  return 0;
}

int LookupTokens(TableEntry* psTable_, char* pszTokens_)
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
