/* 
  Copyright (c) 2002 Robert Rainwater
  Contributors: Justin Frankel, Fritz Elfert, and Amir Szekely

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
#include <RichEdit.h>
#undef _RICHEDIT_VER

// Defines
#define NSIS_DEV    "http://nsis.sourceforge.net/"
#define NSIS_URL	"http://www.nullsoft.com/free/nsis/"
#define USAGE		"Usage:\r\n\r\n - File | Load Script...\r\n - Drag the .nsi file into this window\r\n - Right click the .nsi file and choose \"Compile NSI\""
#define COPYRIGHT	"Copyright © 2002 Robert Rainwater"
#define CONTRIB     "Fritz Elfert, Justin Frankel, Amir Szekely"
#define DOCPATH		"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/*checkout*/nsis/NSIS/docs/index.html?rev=HEAD"
#define LOCALDOCS	"\\docs\\index.html"
#define NSISERROR	"Unable to intialize MakeNSIS.  Please verify that makensis.exe is in the same directory as makensisw.exe."
#define DLGERROR	"Unable to intialize MakeNSISW."
#define REGSEC		HKEY_LOCAL_MACHINE
#define REGKEY		"Software\\NSIS"
#define REGLOC		"MakeNSISWPlacement"
#define EXENAME		"makensis.exe"
#define MAX_STRING	256
#define TIMEOUT		100
#define MINWIDTH	350
#define MINHEIGHT	180
#define REGSEC		HKEY_LOCAL_MACHINE 
#define REGKEY		"Software\\NSIS"
#define REGLOC		"MakeNSISWPlacement"

#define WM_MAKENSIS_PROCESSCOMPLETE (WM_USER+1001)

// Extern Variables
extern const char*	NSISW_VERSION;
extern char*		g_script;
extern HWND		  	g_hwnd;
extern HANDLE	  	g_hThread;
extern char *		g_output_exe;
extern char	*		g_input_script;
extern int			g_retcode;

int WINAPI		WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, char *cmdParam, int cmdShow);
static BOOL		CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam); 
DWORD WINAPI	MakeNSISProc(LPVOID p);
BOOL CALLBACK	DialogResize(HWND hWnd, LPARAM /* unused*/);
BOOL CALLBACK	AboutNSISProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK	AboutProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
void			CompileNSISScript();

#endif