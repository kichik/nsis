/*
  Copyright (c) 2002 Robert Rainwater
  Contributors: Justin Frankel, Fritz Elfert, Amir Szekely, Sunil Kamath, Joost Verburg

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

#define _WIN32_IE 0x0400
#include <windows.h>
#include <commctrl.h>
#include "utils.h"
#define _RICHEDIT_VER 0x0200
#include <richedit.h>
#undef _RICHEDIT_VER

// Defines
#define NSIS_URL     "http://nsis.sourceforge.net/"
#define NSIS_FOR     "http://forums.winamp.com/forumdisplay.php?forumid=65"
#define NSIS_UPDATE  "http://nsis.sourceforge.net/update.php?version="
#define NSIS_DL_URL  "http://nsis.sourceforge.net/download/"
#define USAGE        "Usage:\r\n\r\n - File | Load Script...\r\n - Drag the .nsi file into this window\r\n - Right click the .nsi file and choose \"Compile NSIS Script\""
#define COPYRIGHT    "Copyright © 2002 Robert Rainwater"
#define CONTRIB      "Fritz Elfert, Justin Frankel, Amir Szekely, Sunil Kamath, Joost Verburg"
#define DOCPATH      "http://nsis.sourceforge.net/Docs/"
#define LOCALDOCS    "\\NSIS.chm"
#define NSISERROR    "Unable to intialize MakeNSIS.  Please verify that makensis.exe is in the same directory as makensisw.exe."
#define DLGERROR     "Unable to intialize MakeNSISW."
#define SYMBOLSERROR "Symbol cannot contain whitespace characters"
#define MULTIDROPERROR "Dropping more than one script at a time is not supported"
#define NSISUPDATEPROMPT "Running NSIS Update will close MakeNSISW.\nContinue?"
#define REGSEC       HKEY_CURRENT_USER
#define REGSECDEF    HKEY_LOCAL_MACHINE
#define REGKEY       "Software\\NSIS"
#define REGLOC       "MakeNSISWPlacement"
#define REGCOMPRESSOR "MakeNSISWCompressor"
#define REGSYMSUBKEY "Symbols"
#define REGMRUSUBKEY "MRU"
#define EXENAME      "makensis.exe"
#define MAX_STRING   256
#define TIMEOUT      100
#define MINWIDTH     350
#define MINHEIGHT    180
#define COMPRESSOR_MESSAGE "\n\nThe %s compressor created the smallest installer (%d bytes)."
#define RESTORED_COMPRESSOR_MESSAGE "\n\nThe %s compressor created the smallest installer (%d bytes)."
#define EXE_HEADER_COMPRESSOR_STAT "EXE header size:"
#define TOTAL_SIZE_COMPRESSOR_STAT "Total size:"
#define SYMBOL_SET_NAME_MAXLEN 40
#define LOAD_SYMBOL_SET_DLG_NAME "Load Symbol Definitions Set"
#define SAVE_SYMBOL_SET_DLG_NAME "Save Symbol Definitions Set"
#define LOAD_BUTTON_TEXT "Load"
#define SAVE_BUTTON_TEXT "Save"
#define LOAD_SYMBOL_SET_MESSAGE "Please select a name for the Symbol Definitions Set to load."
#define SAVE_SYMBOL_SET_MESSAGE "Please enter or select a name for the Symbol Definitions Set to save."

#define WM_MAKENSIS_PROCESSCOMPLETE (WM_USER+1001)
#define WM_MAKENSIS_LOADSYMBOLSET (WM_USER+1002)
#define WM_MAKENSIS_SAVESYMBOLSET (WM_USER+1003)

enum {
  MAKENSIS_NOTIFY_SCRIPT,
  MAKENSIS_NOTIFY_WARNING,
  MAKENSIS_NOTIFY_ERROR,
  MAKENSIS_NOTIFY_OUTPUT
};

typedef enum {
  COMPRESSOR_SCRIPT,
  COMPRESSOR_ZLIB,
  COMPRESSOR_ZLIB_SOLID,
  COMPRESSOR_BZIP2,
  COMPRESSOR_BZIP2_SOLID,
  COMPRESSOR_LZMA,
  COMPRESSOR_LZMA_SOLID,
  COMPRESSOR_BEST,
} NCOMPRESSOR;

#ifdef MAKENSISW_CPP
char *compressor_names[] = {"",
                            "zlib",
                            "/SOLID zlib",
                            "bzip2",
                            "/SOLID bzip2",
                            "lzma",
                            "/SOLID lzma",
                            "Best"};
char *compressor_display_names[] = {"Defined in Script/Compiler Default",
                            "ZLIB",
                            "ZLIB (solid)",
                            "BZIP2",
                            "BZIP2 (solid)",
                            "LZMA",
                            "LZMA (solid)",
                            "Best Compressor"};
WORD compressor_commands[] = {IDM_COMPRESSOR_SCRIPT,
                              IDM_ZLIB,
                              IDM_ZLIB_SOLID,
                              IDM_BZIP2,
                              IDM_BZIP2_SOLID,
                              IDM_LZMA,
                              IDM_LZMA_SOLID,
                              IDM_BEST};
#endif

#ifdef TOOLBAR_CPP
int compressor_bitmaps[] = {IDB_COMPRESSOR_SCRIPT,
                            IDB_COMPRESSOR_ZLIB,
                            IDB_COMPRESSOR_ZLIB,
                            IDB_COMPRESSOR_BZIP2,
                            IDB_COMPRESSOR_BZIP2,
                            IDB_COMPRESSOR_LZMA,
                            IDB_COMPRESSOR_LZMA,
                            IDB_COMPRESSOR_BEST};
int compressor_strings[] = {IDS_SCRIPT,
                            IDS_ZLIB,
                            IDS_ZLIB_SOLID,
                            IDS_BZIP2,
                            IDS_BZIP2_SOLID,
                            IDS_LZMA,
                            IDS_LZMA_SOLID,
                            IDS_BEST};
#endif

// Extern Variables

extern const char* NSISW_VERSION;

int WINAPI     WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, char *cmdParam, int cmdShow);
static BOOL    CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
DWORD WINAPI   MakeNSISProc(LPVOID p);
BOOL CALLBACK  DialogResize(HWND hWnd, LPARAM /* unused*/);
BOOL CALLBACK  AboutNSISProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK  AboutProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK  SettingsProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK  SymbolSetProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK  CompressorProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
void           CompileNSISScript();
char*          BuildSymbols();
void           SetCompressor(NCOMPRESSOR);
void           RestoreSymbols();
void           SaveSymbols();
void           DeleteSymbolSet(char *);
char**         LoadSymbolSet(char *);
void           SaveSymbolSet(char *, char **);
void           RestoreMRUList();
void           SaveMRUList();

typedef struct NSISScriptData {
  bool script_alloced;
  char *script;
  char *output_exe;
  char *input_script;
  char *branding;
  char *brandingv;
  char **symbols;
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
  HANDLE sigint_event;
  HWND focused_hwnd;
  CHARRANGE textrange;
  NCOMPRESSOR default_compressor;
  NCOMPRESSOR compressor;
  char *compressor_name;
  char compressor_stats[512];
  char *best_compressor_name;
  // Added by Darren Owen (DrO) on 1/10/2003
  int recompile_test;
} NSCRIPTDATA;

extern NSCRIPTDATA g_sdata;

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