/*********************************************************************************
 *
 *  InstallerOptions by Michael Bishop:
 *  InstallerOptions/DLL Version 1.2 beta
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
 *   - Added CancelConfirmCaption and CancelConfirmIcon (by Amir Szekely)
 *
 *   - Added Icon and Bitmap controls (by Amir Szekely 4th September 2002)
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

// general flags
#define FLAG_BOLD          (0x1)
#define FLAG_RIGHT         (0x2)

// text box flags
#define FLAG_PASSWORD      (0x100)

// listbox flags
#define FLAG_MULTISELECT   (0x200)

// combobox flags
#define FLAG_DROPLIST      (0x400)

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
HINSTANCE m_hInstance = NULL;

char *pszFilename = NULL;
char *pszTitle = NULL;
char *pszCancelQuestion = NULL;
char *pszCancelQuestionCaption = NULL;
char *pszCancelButtonText = NULL;
char *pszNextButtonText = NULL;
char *pszBackButtonText = NULL;
unsigned int nCancelConfirmFlags=0;
BOOL bBackEnabled=FALSE;

BOOL bCancelEnabled=TRUE;  // by ORTIM: 13-August-2002
int  bCancelShow=1;        // by ORTIM: 13-August-2002

FieldType *pFields   = NULL;
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
  ofn.lpstrFile = (char*)MALLOC(ofn.nMaxFile);

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

  GetWindowText(hControl, ofn.lpstrFile, ofn.nMaxFile);

  for(;;) {
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
      break;
   // }
  }

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
  BROWSEINFO bi={0,};
  HWND hControl;

  hControl = pFields[nControlIdx].hwnd;

  bi.hwndOwner = hConfigWindow;
  // bi.pidlRoot = NULL;
  bi.pszDisplayName = (char*)MALLOC(MAX_PATH);
  //LPCTSTR lpszTitle = NULL;
  bi.ulFlags = BIF_STATUSTEXT;
  bi.lpfn = BrowseCallbackProc;
  bi.lParam = nControlIdx;
  //bi.iImage = 0;

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
    SetWindowText(hControl, pszFolder);
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
      nLength = GetWindowTextLength(pFields[nIdx].hwnd);

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
          int nLength = GetWindowTextLength(pFields[nIdx].hwnd);
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
  int nWidth = 22;
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

static char szResult[1000];

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
  // Messagebox flags
  static TableEntry MBFlagTable[] = {
    { "MB_ICONEXCLAMATION", MB_ICONEXCLAMATION },
//  { "MB_ICONWARNING",     MB_ICONWARNING     }, // same as above
    { "MB_ICONINFORMATION", MB_ICONINFORMATION },
//  { "MB_ICONASTERISK",    MB_ICONASTERISK    }, // same as above
    { "MB_ICONQUESTION",    MB_ICONQUESTION    },
    { "MB_ICONSTOP",        MB_ICONSTOP        },
//  { "MB_ICONERROR",       MB_ICONERROR       }, // same as above
//  { "MB_ICONHAND",        MB_ICONHAND        }, // same as above
    { "MB_TOPMOST",         MB_TOPMOST         },
    { "MB_SETFOREGROUND",   MB_SETFOREGROUND   },
    { "MB_RIGHT",           MB_RIGHT           },
    { "MB_DEFBUTTON1",      MB_DEFBUTTON1      },
    { "MB_DEFBUTTON2",      MB_DEFBUTTON2      },
//  { "MB_DEFBUTTON3",      MB_DEFBUTTON3      }, // useless, as there are only two buttons
//  { "MB_DEFBUTTON4",      MB_DEFBUTTON4      }, // useless, as there are only two buttons
    { NULL,                 0                  }
  };

  pszTitle = myGetProfileStringDup("Settings", "Title");
  pszCancelQuestion = myGetProfileStringDup("Settings", "CancelConfirm");
  pszCancelQuestionCaption = myGetProfileStringDup("Settings", "CancelConfirmCaption");
  pszCancelButtonText = myGetProfileStringDup("Settings", "CancelButtonText");
  pszNextButtonText = myGetProfileStringDup("Settings", "NextButtonText");
  pszBackButtonText = myGetProfileStringDup("Settings", "BackButtonText");

  myGetProfileString("Settings", "CancelConfirmFlags");
  nCancelConfirmFlags = LookupTokens(MBFlagTable, szResult);

  nNumFields = GetPrivateProfileInt("Settings", "NumFields", 0, pszFilename);
  bBackEnabled = GetPrivateProfileInt("Settings", "BackEnabled", 0, pszFilename);

  bCancelEnabled = GetPrivateProfileInt("Settings", "CancelEnabled", 1, pszFilename);  // by ORTIM: 13-August-2002
  bCancelShow = GetPrivateProfileInt("Settings", "CancelShow", 1, pszFilename);        // by ORTIM: 13-August-2002

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
	if (message == WM_COMMAND && (LOWORD(wParam) == IDCANCEL || LOWORD(wParam) == IDOK || LOWORD(wParam) == 3))
  {
		PostMessage(hConfigWindow,WM_USER+666,0,LOWORD(wParam));
    return 0;
  }
  if (message == WM_CLOSE)
  {
    PostMessage(hConfigWindow,WM_USER+666,0,IDCANCEL);
    return 0;
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
      if (lParam != IDCANCEL || !pszCancelQuestion || MessageBox(hwndDlg,pszCancelQuestion,pszCancelQuestionCaption?pszCancelQuestionCaption:"Question",MB_YESNO|nCancelConfirmFlags)==IDYES)
      {
        if (lParam == IDCANCEL || lParam == 3 || ValidateFields()) {
          if (lParam == 3) g_is_back++;
          if (lParam == IDCANCEL) g_is_cancel++;
          g_done++;
          PostMessage(hwndDlg,WM_CLOSE,0,0);
        }
      }
    break;
  }
	return 0;
}




extern "C" void __declspec(dllexport) dialog(HWND hwndParent, int string_size,
                                      char *variables, stack_t **stacktop)
{
  hMainWindow=hwndParent;
  EXDLL_INIT();

  int nIdx;
  UINT nAddMsg;

  if (!hMainWindow)
  {
    popstring(NULL);
    pushstring("error finding mainwnd");
    return; // cannot be used in silent mode unfortunately.
  }
  HWND childwnd=FindWindowEx(hMainWindow,NULL,"#32770",NULL); // find window to replace
  if (!childwnd) childwnd=GetDlgItem(hMainWindow,1018);
  if (!childwnd)
  {
    popstring(NULL);
    pushstring("error finding childwnd");
    return;
  }

  if (!stacktop || !*stacktop || !(pszFilename = (*stacktop)->text) || !pszFilename[0] || !ReadSettings())
  {
    popstring(NULL);
    pushstring("error finding config");
    return;
  }
  int cw_vis=IsWindowVisible(childwnd);
  if (cw_vis) ShowWindow(childwnd,SW_HIDE);

  int was_cancel_enabled=EnableWindow(GetDlgItem(hMainWindow,IDCANCEL),1);
  int was_ok_enabled=EnableWindow(GetDlgItem(hMainWindow,IDOK),1);
  static char old_cancel[256];
  GetDlgItemText(hMainWindow,IDCANCEL,old_cancel,sizeof(old_cancel));
  if (pszCancelButtonText) SetDlgItemText(hMainWindow,IDCANCEL,pszCancelButtonText);
  static char old_ok[256];
  GetDlgItemText(hMainWindow,IDOK,old_ok,sizeof(old_ok));
  if (pszNextButtonText) SetDlgItemText(hMainWindow,IDOK,pszNextButtonText);
  static char old_back[256];
  GetDlgItemText(hMainWindow,3,old_back,sizeof(old_back));
  if (pszBackButtonText) SetDlgItemText(hMainWindow,3,pszBackButtonText);


  int old_back_enabled=!EnableWindow(GetDlgItem(hMainWindow,3),bBackEnabled);
  int old_back_visible=IsWindowVisible(GetDlgItem(hMainWindow,3));
  ShowWindow(GetDlgItem(hMainWindow,3),bBackEnabled?SW_SHOWNA:SW_HIDE);


  int old_cancel_enabled=!EnableWindow(GetDlgItem(hMainWindow,IDCANCEL),bCancelEnabled);  // by ORTIM: 13-August-2002
  int old_cancel_visible=IsWindowVisible(GetDlgItem(hMainWindow,IDCANCEL));               // by ORTIM: 13-August-2002
  EnableWindow(GetDlgItem(hMainWindow,IDCANCEL),bCancelEnabled?SW_SHOWNA:SW_HIDE);		  // by ORTIM: 13-August-2002
  ShowWindow(GetDlgItem(hMainWindow,IDCANCEL),bCancelShow?SW_SHOWNA:SW_HIDE);             // by ORTIM: 13-August-2002

  lpWndProcOld = (void *) SetWindowLong(hMainWindow,GWL_WNDPROC,(long)ParentWndProc);

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
    return;
  }

  // by ORTIM: 14-August-2002
  DWORD dwBaseUnits = GetDialogBaseUnits();

  for (nIdx = 0; nIdx < nNumFields; nIdx++) {
    static struct {
      char* pszClass;
      DWORD dwStyle;
      DWORD dwExStyle;
    } ClassTable[] = {
      { "STATIC",       // FIELD_LABEL
        WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS /*| WS_TABSTOP*/,
        WS_EX_TRANSPARENT },
      { "STATIC",       // FIELD_ICON
        WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS /*| WS_TABSTOP*/ | SS_ICON,
        0 },
      { "STATIC",       // FIELD_BITMAP
        WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS /*| WS_TABSTOP*/ | SS_BITMAP,
        0 },
      { "BUTTON",       // FIELD_BROWSEBUTTON
        WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP,
        0 },
      { "BUTTON",       // FIELD_CHECKBOX
        WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX,
        0 },
      { "BUTTON",       // FIELD_RADIOBUTTON
        WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON,
        0 },
      { "EDIT",         // FIELD_TEXT
        WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | WS_BORDER | ES_AUTOHSCROLL,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE },
      { "EDIT",         // FIELD_FILEREQUEST
        WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | WS_BORDER | ES_AUTOHSCROLL,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE },
      { "EDIT",         // FIELD_DIRREQUEST
        WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | WS_BORDER | ES_AUTOHSCROLL,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE },
      { "COMBOBOX",     // FIELD_COMBOBOX
        WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | CBS_AUTOHSCROLL | CBS_HASSTRINGS,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE },
      { "LISTBOX",      // FIELD_LISTBOX
        WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT,
        WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE }
    };

    if (pFields[nIdx].nType < 1 || pFields[nIdx].nType > (sizeof(ClassTable) / sizeof(ClassTable[0])))
      continue;

    DWORD dwStyle = ClassTable[pFields[nIdx].nType - 1].dwStyle;
    DWORD dwExStyle = ClassTable[pFields[nIdx].nType - 1].dwExStyle;

    // by ORTIM: 14-August-2002
    //  transform the pixel sizes of the widget with dialog units
    //  used example code from MS SDK

    // Changed by Dave Laundon 9th September 2002
    //  Scale to pixels /before/ adjusting for negative positions
    //  NB - scaling /could/ turn a -1 into 0, so use the original rect for the -ve tests
    //  NB - original rect used later on too

    RECT rect;
    rect.left = (pFields[nIdx].rect.left * LOWORD(dwBaseUnits)) / 8;
    rect.right = (pFields[nIdx].rect.right * LOWORD(dwBaseUnits)) / 8;
    rect.top = (pFields[nIdx].rect.top * HIWORD(dwBaseUnits)) / 16;
    rect.bottom = (pFields[nIdx].rect.bottom * HIWORD(dwBaseUnits)) / 16;
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

    pFields[nIdx].hwnd = CreateWindowEx(
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

    if (pFields[nIdx].hwnd) {
      // Changed by Amir Szekely 22nd July 2002
      // Sets the font of IO window to be the same as the main window
      SendMessage(pFields[nIdx].hwnd, WM_SETFONT, (WPARAM)hFont, TRUE);
      // make sure we created the window, then set additional attributes
      if (pFields[nIdx].nMaxLength > 0) {
        switch (pFields[nIdx].nType) {
          case FIELD_TEXT:
          case FIELD_DIRREQUEST:
          case FIELD_FILEREQUEST:
            SendMessage(pFields[nIdx].hwnd, EM_LIMITTEXT, (WPARAM)pFields[nIdx].nMaxLength, (LPARAM)0);
            break;
        }
      }
      if ((pFields[nIdx].nType == FIELD_CHECKBOX) || (pFields[nIdx].nType == FIELD_RADIOBUTTON)) {
        if (pFields[nIdx].pszState[0] == '1')
        {
          SendMessage(pFields[nIdx].hwnd, BM_SETCHECK, (WPARAM)BST_CHECKED, 0);
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
              SendMessage(pFields[nIdx].hwnd, nAddMsg, 0, (LPARAM)pszStart);
            }
            // jump to the next item, skip any redundant | characters
            do { pszEnd++; } while (*pszEnd == '|');
            pszStart = pszEnd;
          }
          pszEnd++;
        }
        if (pFields[nIdx].pszState) {
          int nItem = SendMessage(pFields[nIdx].hwnd, CB_FINDSTRINGEXACT, -1, (LPARAM)pFields[nIdx].pszState);
          if (nItem != CB_ERR) {
            SendMessage(pFields[nIdx].hwnd, CB_SETCURSEL, nItem, 0);
          }
        }
      } else if (pFields[nIdx].nType == FIELD_BITMAP || pFields[nIdx].nType == FIELD_ICON) {
        WPARAM nImageType = pFields[nIdx].nType == FIELD_BITMAP ? IMAGE_BITMAP : IMAGE_ICON;
        SendMessage(
          pFields[nIdx].hwnd,
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
            (pFields[nIdx].rect.right - pFields[nIdx].rect.left > 0)
              ? (pFields[nIdx].rect.right - pFields[nIdx].rect.left)
              : (rect.right - rect.left),
            (pFields[nIdx].rect.bottom - pFields[nIdx].rect.top > 0)
              ? (pFields[nIdx].rect.bottom - pFields[nIdx].rect.top)
              : (rect.bottom - rect.top),
            LR_LOADFROMFILE
          ):(LPARAM)LoadIcon(GetModuleHandle(0), MAKEINTRESOURCE(103))
        );
      }
    }
  }

  static char old_title[1024];
  if (pszTitle)
  {
    GetWindowText(hMainWindow,old_title,sizeof(old_title));
    SetWindowText(hMainWindow,pszTitle);
  }

  ShowWindow(hConfigWindow, SW_SHOWNA);
	SetFocus(GetDlgItem(hMainWindow,IDOK));

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
  if (was_cancel_enabled) EnableWindow(GetDlgItem(hMainWindow,IDCANCEL),0);
  if (was_ok_enabled) EnableWindow(GetDlgItem(hMainWindow,IDOK),0);
  SetDlgItemText(hMainWindow,IDCANCEL,old_cancel);
  SetDlgItemText(hMainWindow,IDOK,old_ok);
  SetDlgItemText(hMainWindow,3,old_back);

  EnableWindow(GetDlgItem(hMainWindow,3),old_back_enabled);

  EnableWindow(GetDlgItem(hMainWindow,IDCANCEL),old_cancel_enabled);                  // by ORTIM: 13-August-2002
  ShowWindow(GetDlgItem(hMainWindow,IDCANCEL),old_cancel_visible?SW_SHOWNA:SW_HIDE);  // by ORTIM: 13-August-2002

  ShowWindow(GetDlgItem(hMainWindow,3),old_back_visible?SW_SHOWNA:SW_HIDE);
  if (pszTitle) SetWindowText(hMainWindow,old_title);

  if (cw_vis) ShowWindow(childwnd,SW_SHOWNA);

  FREE(pszTitle);
  FREE(pszCancelQuestion);
  FREE(pszCancelQuestionCaption);
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



extern "C" BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  m_hInstance=(HINSTANCE) hInst;
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
