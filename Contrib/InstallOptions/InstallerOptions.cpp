/*********************************************************************************
 * 
 *  InstallerOptions by Michael Bishop:
 *  InstallerOptions/DLL Version 1.2 beta
 *
 *  highly modified by justin frankel to go in as dll, subclass, be sexy, and whatnot.
 *
 *  key changes
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

typedef struct _stack_t {
  struct _stack_t *next;
  char text[1]; // this should be the length of string_size
} stack_t;

int popstring(char *str); // 0 on success, 1 on empty stack
void pushstring(char *str);

enum
{
INST_0,         // $0
INST_1,         // $1
INST_2,         // $2
INST_3,         // $3
INST_4,         // $4
INST_5,         // $5
INST_6,         // $6
INST_7,         // $7
INST_8,         // $8
INST_9,         // $9
INST_R0,        // $R0
INST_R1,        // $R1
INST_R2,        // $R2
INST_R3,        // $R3
INST_R4,        // $R4
INST_R5,        // $R5
INST_R6,        // $R6
INST_R7,        // $R7
INST_R8,        // $R8
INST_R9,        // $R9
INST_CMDLINE,   // $CMDLINE
INST_INSTDIR,   // $INSTDIR
INST_OUTDIR,    // $OUTDIR
INST_EXEDIR,    // $EXEDIR
__INST_LAST
};

char *getuservariable(int varnum);


int g_stringsize;
stack_t **g_stacktop;
char *g_variables;

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

#define FIELD_BROWSEBUTTON (1)
#define FIELD_LABEL        (2)
#define FIELD_TEXT         (3)
#define FIELD_COMBOBOX     (4)
#define FIELD_FILEREQUEST  (5)
#define FIELD_DIRREQUEST   (6)
#define FIELD_CHECKBOX     (7)
#define FIELD_RADIOBUTTON  (8)
#define FIELD_LISTBOX      (9)
#define FIELD_ICON        (10)
#define FIELD_BITMAP      (11)

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
  int   nBitsToSet;
};

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

char *pszTitle      = NULL;
char *pszCancelQuestion = NULL;
char *pszCancelQuestionCaption = NULL;
char *pszCancelButtonText = NULL;
char *pszNextButtonText = NULL;
char *pszBackButtonText = NULL;
char *pszOldTitle   = NULL;
unsigned int nCancelQuestionIcon = 0;
BOOL bBackEnabled=FALSE;

BOOL bCancelEnabled=TRUE;  // by ORTIM: 13-August-2002
int  bCancelShow=1;        // by ORTIM: 13-August-2002

FieldType *pFields   = NULL;
int nNumFields       = 0;
int g_done;

// will contain a count of the number of controls on the main NSIS window.
unsigned int nNSISControlCount = 0;

struct WindowEntry {
  HWND hwnd;  
  long nOldStyle;
};


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
    if (pFields[nIdx].nType > FIELD_LABEL) {
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

bool SaveSettings(LPSTR pszFilename) {
  char szField[25];
  int nIdx;
  HWND hwnd;
  int nBufLen     = MAX_BUFFER_LENGTH;
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

        pFields[nIdx].rect.right = pNewField->rect.right - 3 - nWidth;

        pNewField->rect.right-=nWidth;
        pNewField->rect.left-=nWidth;



        nNumFields++;
        break;
    }
  }
}

bool ReadSettings(LPSTR pszFilename) {
  int nResult;
  char *pszResult;
  char pszField[25];
  int nSize;
  int nIdx;

  nSize = 1000;
  pszResult = (char*)MALLOC(nSize); // buffer to read from the file
  if (!pszResult) return false;

  nResult = GetPrivateProfileString("Settings", "Title", "", pszResult, nSize, pszFilename);
  pszTitle = (nResult > 0) ? strdup(pszResult) : NULL;

  nResult = GetPrivateProfileString("Settings", "CancelConfirm", "", pszResult, nSize, pszFilename);
  pszCancelQuestion = (nResult > 0) ? strdup(pszResult) : NULL;
  nResult = GetPrivateProfileString("Settings", "CancelConfirmCaption", "", pszResult, nSize, pszFilename);
  pszCancelQuestionCaption = (nResult > 0) ? strdup(pszResult) : NULL;
  nResult = GetPrivateProfileString("Settings", "CancelConfirmIcon", "", pszResult, nSize, pszFilename);
  if (!lstrcmpi(pszResult, "MB_ICONEXCLAMATION"))
    nCancelQuestionIcon = MB_ICONEXCLAMATION;
  else if (!lstrcmpi(pszResult, "MB_ICONWARNING"))
    nCancelQuestionIcon = MB_ICONWARNING;
  else if (!lstrcmpi(pszResult, "MB_ICONINFORMATION"))
    nCancelQuestionIcon = MB_ICONINFORMATION;
  else if (!lstrcmpi(pszResult, "MB_ICONASTERISK"))
    nCancelQuestionIcon = MB_ICONASTERISK;
  else if (!lstrcmpi(pszResult, "MB_ICONQUESTION"))
    nCancelQuestionIcon = MB_ICONQUESTION;
  else if (!lstrcmpi(pszResult, "MB_ICONSTOP"))
    nCancelQuestionIcon = MB_ICONSTOP;
  else if (!lstrcmpi(pszResult, "MB_ICONERROR"))
    nCancelQuestionIcon = MB_ICONERROR;
  else if (!lstrcmpi(pszResult, "MB_ICONHAND"))
    nCancelQuestionIcon = MB_ICONHAND;

  nResult = GetPrivateProfileString("Settings", "CancelButtonText", "", pszResult, nSize, pszFilename);
  pszCancelButtonText = (nResult > 0) ? strdup(pszResult) : NULL;
  nResult = GetPrivateProfileString("Settings", "NextButtonText", "", pszResult, nSize, pszFilename);
  pszNextButtonText = (nResult > 0) ? strdup(pszResult) : NULL;
  nResult = GetPrivateProfileString("Settings", "BackButtonText", "", pszResult, nSize, pszFilename);
  pszBackButtonText = (nResult > 0) ? strdup(pszResult) : NULL;
  
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
    wsprintf(pszField, "field %d", nIdx + 1);

    *pszResult = '\0';
    nResult = GetPrivateProfileString(pszField, "TYPE", "", pszResult, nSize, pszFilename);
    if (!stricmp(pszResult, "LABEL")) {
      pFields[nIdx].nType = FIELD_LABEL;
    } else if (!stricmp(pszResult, "TEXT")) {
      pFields[nIdx].nType = FIELD_TEXT;
    } else if (!stricmp(pszResult, "PASSWORD")) {
      pFields[nIdx].nType = FIELD_TEXT;
      pFields[nIdx].nFlags |= FLAG_PASSWORD;
    } else if (!stricmp(pszResult, "LISTBOX")) {
      pFields[nIdx].nType = FIELD_LISTBOX;
    } else if (!stricmp(pszResult, "COMBOBOX")) {
      pFields[nIdx].nType = FIELD_COMBOBOX;
    } else if (!stricmp(pszResult, "DROPLIST")) {
      pFields[nIdx].nType = FIELD_COMBOBOX;
      pFields[nIdx].nFlags |= FLAG_DROPLIST;
    } else if (!stricmp(pszResult, "FILEREQUEST")) {
      pFields[nIdx].nType = FIELD_FILEREQUEST;
    } else if (!stricmp(pszResult, "DIRREQUEST")) {
      pFields[nIdx].nType = FIELD_DIRREQUEST;
    } else if (!stricmp(pszResult, "CHECKBOX")) {
      pFields[nIdx].nType = FIELD_CHECKBOX;
    } else if (!stricmp(pszResult, "RADIOBUTTON")) {
      pFields[nIdx].nType = FIELD_RADIOBUTTON;
    } else if (!stricmp(pszResult, "ICON")) {
      pFields[nIdx].nType = FIELD_ICON;
    } else if (!stricmp(pszResult, "BITMAP")) {
      pFields[nIdx].nType = FIELD_BITMAP;
    } else {
      continue;
    }

    nResult = GetPrivateProfileString(pszField, "TEXT", "", pszResult, nSize, pszFilename);
    if (nResult) {
      pFields[nIdx].pszText = STRDUP(pszResult);
    }
    nResult = GetPrivateProfileString(pszField, "STATE", "", pszResult, nSize, pszFilename);
    pFields[nIdx].pszState = STRDUP(pszResult);

    nResult = GetPrivateProfileString(pszField, "ROOT", "", pszResult, nSize, pszFilename);
    if (nResult) {
      pFields[nIdx].pszRoot = STRDUP(pszResult);
    }

    nResult = GetPrivateProfileString(pszField, "ListItems", "", pszResult, nSize, pszFilename);
    if (nResult) {
      // add an extra | character to the end to simplify the loop where we add the items.
      pFields[nIdx].pszListItems = (char*)MALLOC(nResult + 2);
      wsprintf(pFields[nIdx].pszListItems, "%s|", pszResult);
    }    
    pFields[nIdx].nMaxLength = GetPrivateProfileInt(pszField, "MaxLen", 0, pszFilename);
    pFields[nIdx].nMinLength = GetPrivateProfileInt(pszField, "MinLen", 0, pszFilename);

    nResult = GetPrivateProfileString(pszField, "ValidateText", "", pszResult, nSize, pszFilename);
    if (nResult) {
      pFields[nIdx].pszValidateText = STRDUP(pszResult);
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

    nResult = GetPrivateProfileString(pszField, "Filter", "All Files|*.*", pszResult, nSize, pszFilename);
    if (nResult) {
      // add an extra | character to the end to simplify the loop where we add the items.
      pFields[nIdx].pszFilter = (char*)MALLOC(nResult + 2);
      strcpy(pFields[nIdx].pszFilter, pszResult);
      char *pszPos = pFields[nIdx].pszFilter;
      while (*pszPos) {
        if (*pszPos == '|') *pszPos = '\0';
        pszPos++;
      } 
    }    

    pFields[nIdx].rect.left = GetPrivateProfileInt(pszField, "LEFT", 0, pszFilename);
    pFields[nIdx].rect.right = GetPrivateProfileInt(pszField, "RIGHT", 0, pszFilename);
    pFields[nIdx].rect.top = GetPrivateProfileInt(pszField, "TOP", 0, pszFilename);
    pFields[nIdx].rect.bottom = GetPrivateProfileInt(pszField, "BOTTOM", 0, pszFilename);
  
    TableEntry FlagTable[] = {
      { "FILE_MUST_EXIST", OFN_FILEMUSTEXIST   },
      { "PATH_MUST_EXIST", OFN_PATHMUSTEXIST   },
      { "WARN_IF_EXIST",   OFN_OVERWRITEPROMPT },
      { "PROMPT_CREATE",   OFN_CREATEPROMPT    },
      
      { "RIGHT"        ,   FLAG_RIGHT          },

      { "PASSWORD"     ,   FLAG_PASSWORD       },
      { "DROPLIST"     ,   FLAG_DROPLIST       },

      { "MULTISELECT"  ,   FLAG_MULTISELECT    },
      { "FILE_EXPLORER",     OFN_EXPLORER      },
      { "FILE_HIDEREADONLY", OFN_HIDEREADONLY  },

/*
      { "NO_ALPHA",        0                   },
      { "NO_NUMBERS",      0                   },
      { "NO_SYMBOLS",      0                   },
      { "BOLD",            FLAG_BOLD           },
*/
      { NULL,              0                   }
    };

    nResult = GetPrivateProfileString(pszField, "flags", "", pszResult, nSize, pszFilename);
    if (nResult > 0) {
      // append the | to make parsing a bit easier
      if (lstrlen(pszResult)<nSize-1) lstrcat(pszResult, "|");
      // parse the flags text
      char *pszStart, *pszEnd;
      pszStart = pszEnd = pszResult;
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
              int nFlagIdx = 0;
              while (FlagTable[nFlagIdx].pszName != NULL) {
                if (!stricmp(FlagTable[nFlagIdx].pszName, pszStart)) {
                  pFields[nIdx].nFlags |= FlagTable[nFlagIdx].nBitsToSet;
                  break;
                }
                nFlagIdx++;
              }
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

  FREE(pszResult);

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
      if (lParam != IDCANCEL || !pszCancelQuestion || MessageBox(hwndDlg,pszCancelQuestion,pszCancelQuestionCaption?pszCancelQuestionCaption:"Question",MB_YESNO|nCancelQuestionIcon)==IDYES)
      {
        if (lParam == IDCANCEL || lParam == 3 || ValidateFields()) {
          if (lParam == 3) g_is_back++;
          if (lParam == IDCANCEL) g_is_cancel++;
          g_done++;
          PostMessage(hwndDlg,WM_CLOSE,0,0);
        }
      }
    break;
    case WM_CLOSE: break;
  }
	return 0;
}




extern "C" void __declspec(dllexport) dialog(HWND hwndParent, int string_size, 
                                      char *variables, stack_t **stacktop)
{
  hMainWindow=hwndParent;
  g_stringsize=string_size;
  g_stacktop=stacktop;
  g_variables=variables;

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

  if (!*stacktop || !(*stacktop)->text[0] || !ReadSettings((*stacktop)->text)) 
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

	lpWndProcOld = (void *) GetWindowLong(hMainWindow,GWL_WNDPROC);
	SetWindowLong(hMainWindow,GWL_WNDPROC,(long)ParentWndProc);

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
    char szFieldClass[20];

    if (pFields[nIdx].rect.left<0) pFields[nIdx].rect.left+=dialog_r.right;
    if (pFields[nIdx].rect.right<0) pFields[nIdx].rect.right+=dialog_r.right;
    if (pFields[nIdx].rect.top<0) pFields[nIdx].rect.top+=dialog_r.bottom;
    if (pFields[nIdx].rect.bottom<0) pFields[nIdx].rect.bottom+=dialog_r.bottom;

    DWORD dwExStyle = 0;
    DWORD dwStyle=0;
    char *title=pFields[nIdx].pszText;
    switch (pFields[nIdx].nType) {
      case FIELD_LABEL:
        dwStyle = WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP;
        dwExStyle = WS_EX_TRANSPARENT;
        strcpy(szFieldClass, "STATIC");
        break;
      case FIELD_FILEREQUEST:
      case FIELD_DIRREQUEST:
        pFields[nIdx].rect.right-=25;
      case FIELD_TEXT:        
        if (pFields[nIdx].nFlags & FLAG_PASSWORD) {
          dwStyle = WS_VISIBLE | WS_CHILD | WS_BORDER | WS_CLIPSIBLINGS | WS_TABSTOP | ES_AUTOHSCROLL | ES_PASSWORD;
        } else {
          dwStyle = WS_VISIBLE | WS_CHILD | WS_BORDER | WS_CLIPSIBLINGS | WS_TABSTOP | ES_AUTOHSCROLL;
        }
        dwExStyle = WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE;
        strcpy(szFieldClass, "EDIT");
        title=pFields[nIdx].pszState;
        break;
      case FIELD_COMBOBOX:        
        if (pFields[nIdx].nFlags & FLAG_DROPLIST) {
          dwStyle = CBS_DROPDOWNLIST | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | CBS_AUTOHSCROLL | CBS_HASSTRINGS;
        } else {
          dwStyle = CBS_DROPDOWN | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | WS_VSCROLL | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | CBS_AUTOHSCROLL | CBS_HASSTRINGS;
        }
        dwExStyle = WS_EX_WINDOWEDGE | WS_EX_CLIENTEDGE;
        title=pFields[nIdx].pszState;
        strcpy(szFieldClass, "COMBOBOX");
        break;
      case FIELD_BROWSEBUTTON:
        dwStyle = WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP;
        strcpy(szFieldClass, "BUTTON");
        break;
      case FIELD_LISTBOX:
        if (pFields[nIdx].nFlags & FLAG_MULTISELECT) {
          dwStyle = WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT | LBS_EXTENDEDSEL;
        } else {
          dwStyle = WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | LBS_DISABLENOSCROLL | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT;
        }
        dwExStyle = WS_EX_CLIENTEDGE | WS_EX_WINDOWEDGE;
        strcpy(szFieldClass, "LISTBOX");
        break;
      case FIELD_CHECKBOX:
        if (pFields[nIdx].nFlags & FLAG_RIGHT) {
          dwStyle = WS_VISIBLE | WS_TABSTOP | WS_CHILD | WS_CLIPSIBLINGS | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX | BS_RIGHTBUTTON;
        } else {
          dwStyle = WS_VISIBLE | WS_TABSTOP | WS_CHILD | WS_CLIPSIBLINGS | BS_TEXT | BS_VCENTER | BS_AUTOCHECKBOX;
        }
        strcpy(szFieldClass, "BUTTON");
        break;
      case FIELD_RADIOBUTTON:
        if (pFields[nIdx].nFlags & FLAG_RIGHT) {
          dwStyle = WS_VISIBLE | WS_TABSTOP | WS_CHILD | WS_CLIPSIBLINGS | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON | BS_RIGHTBUTTON;
        } else {
          dwStyle = WS_VISIBLE | WS_TABSTOP | WS_CHILD | WS_CLIPSIBLINGS | BS_TEXT | BS_VCENTER | BS_AUTORADIOBUTTON;
        }
        strcpy(szFieldClass, "BUTTON");
        break;
      case FIELD_ICON:
        dwStyle = WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | SS_ICON;
        strcpy(szFieldClass, "STATIC");
        break;
      case FIELD_BITMAP:
        dwStyle = WS_GROUP | WS_VISIBLE | WS_CHILD | WS_CLIPSIBLINGS | WS_TABSTOP | SS_BITMAP;
        strcpy(szFieldClass, "STATIC");
        break;
      default:
        continue;
    }

    // by ORTIM: 14-August-2002
    // transform the pixel sizes of the widget with dialog units
    // used example code from MS SDK

    pFields[nIdx].hwnd = CreateWindowEx(
      dwExStyle,
      szFieldClass,
      title,
      dwStyle,
      ((pFields[nIdx].rect.left) * LOWORD(dwBaseUnits)) / 8,
      ((pFields[nIdx].rect.top) * HIWORD(dwBaseUnits)) / 16,
      ((pFields[nIdx].rect.right-pFields[nIdx].rect.left) * LOWORD(dwBaseUnits)) / 8,
      ((pFields[nIdx].rect.bottom-pFields[nIdx].rect.top) * HIWORD(dwBaseUnits)) / 16,
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
            pFields[nIdx].rect.right-pFields[nIdx].rect.left,
            pFields[nIdx].rect.bottom-pFields[nIdx].rect.top,
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
  if (!g_is_cancel) SaveSettings((*stacktop)->text);
  popstring(NULL);

  pushstring(g_is_cancel?"cancel":g_is_back?"back":"success");

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
}



extern "C" BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  m_hInstance=(HINSTANCE) hInst;
	return TRUE;
}


// utility functions (not required but often useful)
int popstring(char *str)
{
  stack_t *th;
  if (!g_stacktop || !*g_stacktop) return 1;
  th=(*g_stacktop);
  if (str) lstrcpy(str,th->text);
  *g_stacktop = th->next;
  GlobalFree((HGLOBAL)th);
  return 0;
}

void pushstring(char *str)
{
  stack_t *th;
  if (!g_stacktop) return;
  th=(stack_t*)GlobalAlloc(GPTR,sizeof(stack_t)+g_stringsize);
  lstrcpyn(th->text,str,g_stringsize);
  th->next=*g_stacktop;
  *g_stacktop=th;
}

char *getuservariable(int varnum)
{
  if (varnum < 0 || varnum >= __INST_LAST) return NULL;
  return g_variables+varnum*g_stringsize;
}


