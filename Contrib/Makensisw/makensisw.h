/*
  Copyright (c) 2002 Robert Rainwater
  Contributors: Justin Frankel, Fritz Elfert, Amir Szekely, and Sunil Kamath

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
#ifndef MAKENSIS_H
#define MAKENSIS_H

#include <commctrl.h>
#include "utils.h"
#define _RICHEDIT_VER 0x0200
#include <richedit.h>
#undef _RICHEDIT_VER

// Defines
#define NSIS_URL     "http://nsis.sourceforge.net/"
#define NSIS_FOR     "http://forums.winamp.com/forumdisplay.php?forumid=65"
#define NSIS_UPDATE  "\\Bin\\NSISUpdate.exe"
#define USAGE        "Usage:\r\n\r\n - File | Load Script...\r\n - Drag the .nsi file into this window\r\n - Right click the .nsi file and choose \"Compile NSI\""
#define COPYRIGHT    "Copyright © 2002 Robert Rainwater"
#define CONTRIB      "Fritz Elfert, Justin Frankel, Amir Szekely, Sunil Kamath"
#define DOCPATH      "http://nsis.sourceforge.net/Docs/"
#define LOCALDOCS    "\\NSIS.chm"
#define NSISERROR    "Unable to intialize MakeNSIS.  Please verify that makensis.exe is in the same directory as makensisw.exe."
#define DLGERROR     "Unable to intialize MakeNSISW."
#define DEFINESERROR "Symbol cannot contain whitespace characters"
#define NSISUPDATEPROMPT "Running NSIS Update will close MakeNSISW.\nContinue?"
#define REGSEC       HKEY_LOCAL_MACHINE
#define REGKEY       "Software\\NSIS"
#define REGLOC       "MakeNSISWPlacement"
#define REGCOMPRESSOR "MakeNSISWCompressor"
#define REGDEFSUBKEY "Defines"
#define REGDEFCOUNT  "MakeNSISWDefinesCount"
#define REGMRUSUBKEY "MRU"
#define EXENAME      "makensis.exe"
#define MAX_STRING   256
#define TIMEOUT      100
#define MINWIDTH     350
#define MINHEIGHT    180
#define FILE_MENU_INDEX 0
#define EDIT_MENU_INDEX 1
#define TOOLS_MENU_INDEX 2
#define COMPRESSOR_MENU_INDEX 4
#define BZIP2_COMPRESSOR_NAME "bzip2"
#define ZLIB_COMPRESSOR_NAME "zlib"
#define COMPRESSOR_MESSAGE "\n\nThe %s compressor (%d bytes) created a smaller file than the %s compressor (%d bytes)."
#define ZLIB_COMPRESSOR_MESSAGE "\nThe bzip2 compressed version was replaced with zlib compressed version."
#define EXE_HEADER_COMPRESSOR_STAT "EXE header size:"
#define TOTAL_SIZE_COMPRESSOR_STAT "Total size:"

#define WM_MAKENSIS_PROCESSCOMPLETE (WM_USER+1001)

enum {
  MAKENSIS_NOTIFY_SCRIPT,
  MAKENSIS_NOTIFY_WARNING,
  MAKENSIS_NOTIFY_ERROR,
  MAKENSIS_NOTIFY_OUTPUT
};

typedef enum {
  COMPRESSOR_DEFAULT,
  COMPRESSOR_ZLIB,
  COMPRESSOR_BZIP2,
  COMPRESSOR_BEST,
} NCOMPRESSOR;

// Extern Variables
extern const char* NSISW_VERSION;

int WINAPI     WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, char *cmdParam, int cmdShow);
static BOOL    CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
DWORD WINAPI   MakeNSISProc(LPVOID p);
BOOL CALLBACK  DialogResize(HWND hWnd, LPARAM /* unused*/);
BOOL CALLBACK  AboutNSISProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK  AboutProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK  DefinesProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
void           CompileNSISScript();
char*          BuildDefines();
void           SetCompressor(NCOMPRESSOR);
void           RestoreDefines();
void           SaveDefines();
void           RestoreMRUList();
void           SaveMRUList();

typedef struct NSISScriptData {
  bool script_alloced;
  char *script;
  char *output_exe;
  char *input_script;
  char *branding;
  char *brandingv;
  char **defines;
  int retcode;
  DWORD logLength;
  DWORD warnings;
  BOOL appended;
  HINSTANCE hInstance;
  HWND hwnd;
  HMENU menu;
  HMENU fileSubmenu;
  HMENU editSubmenu;
  HMENU toolsSubmenu;
  HANDLE thread;
  HWND focused_hwnd;
  CHARRANGE textrange;
  NCOMPRESSOR compressor;
  char *compressor_name;
  char compressor_stats[512];
  // Added by Darren Owen (DrO) on 1/10/2003
  int recompile_test;
} NSCRIPTDATA;

typedef struct ResizeData {
  RECT resizeRect;
  RECT griprect;
  int dx;
  int dy;
} NRESIZEDATA;

typedef struct FindReplaceDialog {
  FINDREPLACE fr;
  UINT uFindReplaceMsg;
  HWND hwndFind;
} NFINDREPLACE;

typedef struct ToolTipStruct {
  HWND tip;
  HWND tip_p;
  HHOOK hook;
} NTOOLTIP;

#endif