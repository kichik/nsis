/* 
  Copyright (c) 2002 Robert Rainwater
  Portions Copyright (c) 2002 Justin Frankel and Fritz Elfert

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

#define _RICHEDIT_VER 0x0200
#include <RichEdit.h>
#undef _RICHEDIT_VER

// Defines
#define NSIS_URL	"http://sourceforge.net/projects/nsis2k/"
#define USAGE		"Usage:\r\n   makensisw full_path_of_makensis.exe [options] [script.nsi | - [...]]\r\n"
#define COPYRIGHT	"Copyright (c) 2002 Robert Rainwater"
#define CONTRIBUTOR	"Portions Copyright (c) 2002 Justin Frankel and Fritz Elfert"
#define DOCPATH		"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/*checkout*/nsis2k/NSIS/makensis.htm?rev=HEAD"
#define REGSEC		HKEY_LOCAL_MACHINE
#define REGKEY		"Software\\NSIS"
#define REGLOC		"MakeNSISWPlacement"
#define MAX_STRING	256
#define TIMEOUT		800
#define MINWIDTH	350
#define MINHEIGHT	180

#define WM_MAKENSIS_PROCESSCOMPLETE (WM_USER+1001)

// Extern Variables
extern const	char *NSISW_VERSION;
extern char		*g_script;
extern HWND		g_hwnd;
extern HANDLE	g_hThread;
extern char		g_output_exe[1024];
extern char		g_input_script[1024];

// makensisw
int WINAPI		WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, char *cmdParam, int cmdShow);
static BOOL		CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam); 
DWORD WINAPI	MakeNSISProc(LPVOID p);
BOOL CALLBACK	DialogResize(HWND hWnd, LPARAM /* unused*/);
BOOL CALLBACK	AboutProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
void			CompileNSISScript();

// utils
void			SetTitle(HWND hwnd,char *substr);
void			SetBranding(HWND hwnd);
void			CopyToClipboard(HWND hwnd);
void			ClearLog(HWND hwnd);
void			LogMessage(HWND hwnd,const char *str);
void			ErrorMessage(HWND hwnd,const char *str);
void			DisableItems(HWND hwnd);
void			EnableItems(HWND hwnd);
void			RestoreWindowPos(HWND hwnd);
void			SaveWindowPos(HWND hwnd);
#endif