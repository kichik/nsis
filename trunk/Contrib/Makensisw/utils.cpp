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
#include <windows.h>
#include "resource.h"
#include "makensisw.h"
#include "noclib.h"

void SetTitle(HWND hwnd,char *substr) {
	char title[64];
	if (substr==NULL) wsprintf(title,"MakeNSISW");
	else wsprintf(title,"MakeNSISW - %s",substr);
	SetWindowText(hwnd,title);
}

void SetBranding(HWND hwnd) {
	SetDlgItemText(hwnd, IDC_VERSION, NSISW_VERSION);
}

void CopyToClipboard(HWND hwnd) {
	int len=SendDlgItemMessage(hwnd,IDC_LOGWIN,WM_GETTEXTLENGTH,0,0);
	HGLOBAL mem = GlobalAlloc(GHND,len);
	char *existing_text = (char *)GlobalLock(mem);
	if (!hwnd||!OpenClipboard(hwnd)||!existing_text) return;
	EmptyClipboard();
	existing_text[0]=0;
	GetDlgItemText(hwnd, IDC_LOGWIN, existing_text, len);
	GlobalUnlock(mem);
	SetClipboardData(CF_TEXT,existing_text);
	CloseClipboard();
}


void ClearLog(HWND hwnd) {
	SetDlgItemText(hwnd, IDC_LOGWIN, "");
}

char *g_output_exe;
char *g_input_script;
extern BOOL g_warnings;

void LogMessage(HWND hwnd,const char *str) {
	SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_SETSEL, -1, 0);
	SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_REPLACESEL, 0, (WPARAM)str);
	SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_SCROLLCARET, /*SB_BOTTOM*/0, 0);
}

void ErrorMessage(HWND hwnd,const char *str) {
	if (!str) return;
	char buf[1028];
	wsprintf(buf,"Error - %s\r\n",str);
	LogMessage(hwnd,buf);
}

void DisableItems(HWND hwnd) {
	EnableWindow(GetDlgItem(hwnd,IDC_CLOSE),0);
	EnableWindow(GetDlgItem(hwnd,IDC_TEST),0);
	HMENU m = GetMenu(hwnd);
	EnableMenuItem(m,IDM_SAVE,MF_GRAYED);
	EnableMenuItem(m,IDM_TEST,MF_GRAYED);
	EnableMenuItem(m,IDM_EXIT,MF_GRAYED);
	EnableMenuItem(m,IDM_LOADSCRIPT,MF_GRAYED);
	EnableMenuItem(m,IDM_RECOMPILE,MF_GRAYED);
	EnableMenuItem(m,IDM_COPY,MF_GRAYED);
	EnableMenuItem(m,IDM_COPYSELECTED,MF_GRAYED);
	EnableMenuItem(m,IDM_EDITSCRIPT,MF_GRAYED);
}

void EnableItems(HWND hwnd) {
	#define MSG(a) SendDlgItemMessage(hwnd,IDC_LOGWIN,a,0,0)
	#define MSG1(a,b) SendDlgItemMessage(hwnd,IDC_LOGWIN,a,b,0)
	#define MSG2(a,b,c) SendDlgItemMessage(hwnd,IDC_LOGWIN,a,b,c)

	if (g_input_script) {
		GlobalFree(g_input_script);
		g_input_script = 0;
	}
	if (g_output_exe) {
		GlobalFree(g_output_exe);
		g_output_exe = 0;
	}

	TEXTRANGE tr;
	FINDTEXT ft;

	// find input script
	ft.chrg.cpMin = 0;
	ft.chrg.cpMax = MSG(WM_GETTEXTLENGTH);
	ft.lpstrText = "Processing script file: \"";
	ft.chrg.cpMin = tr.chrg.cpMin = MSG2(EM_FINDTEXT, 0, (LPARAM)&ft) + lstrlen("Processing script file: \"");
	ft.lpstrText = "\"";
	tr.chrg.cpMax = MSG2(EM_FINDTEXT, 0, (LPARAM)&ft);
	tr.lpstrText = g_input_script = (char *)GlobalAlloc(GPTR, tr.chrg.cpMax-tr.chrg.cpMin+1);
	MSG2(EM_GETTEXTRANGE, 0, (WPARAM)&tr);

	// find output exe
	ft.chrg.cpMin = 0;
	ft.chrg.cpMax = MSG(WM_GETTEXTLENGTH);
	ft.lpstrText = "Output: \"";
	ft.chrg.cpMin = tr.chrg.cpMin = MSG2(EM_FINDTEXT, 0, (LPARAM)&ft) + lstrlen("Output: \"");
	ft.lpstrText = "\"";
	tr.chrg.cpMax = MSG2(EM_FINDTEXT, 0, (LPARAM)&ft);
	tr.lpstrText = g_output_exe = (char *)GlobalAlloc(GPTR, tr.chrg.cpMax-tr.chrg.cpMin+1);
	MSG2(EM_GETTEXTRANGE, 0, (WPARAM)&tr);

	g_warnings = FALSE;

	ft.lpstrText = "warning:";
	if (MSG2(EM_FINDTEXT, 0, (LPARAM)&ft) != -1) g_warnings++;
	ft.lpstrText = "warnings:";
	if (MSG2(EM_FINDTEXT, 0, (LPARAM)&ft) != -1) g_warnings++;

	HMENU m = GetMenu(hwnd);
	if (g_output_exe && !g_retcode) {
			EnableWindow(GetDlgItem(hwnd,IDC_TEST),1);
			EnableMenuItem(m,IDM_TEST,MF_ENABLED);
	}
	EnableWindow(GetDlgItem(hwnd,IDC_CLOSE),1);
	EnableMenuItem(m,IDM_SAVE,MF_ENABLED);
	EnableMenuItem(m,IDM_EXIT,MF_ENABLED);
	EnableMenuItem(m,IDM_LOADSCRIPT,MF_ENABLED);
	EnableMenuItem(m,IDM_RECOMPILE,MF_ENABLED);
	EnableMenuItem(m,IDM_COPY,MF_ENABLED);
	EnableMenuItem(m,IDM_COPYSELECTED,MF_ENABLED);
	EnableMenuItem(m,IDM_EDITSCRIPT,MF_ENABLED);
}

static BOOL g_appended = FALSE;
void CompileNSISScript() {
	static char *s;
	DragAcceptFiles(g_hwnd,FALSE);
	ClearLog(g_hwnd);
	SetTitle(g_hwnd,NULL);
	SetBranding(g_hwnd);
	if (lstrlen(g_script)==0) {
		HMENU m = GetMenu(g_hwnd);
		LogMessage(g_hwnd,USAGE);
		EnableMenuItem(m,IDM_RECOMPILE,MF_GRAYED);
		EnableMenuItem(m,IDM_EDITSCRIPT,MF_GRAYED);
		EnableMenuItem(m,IDM_TEST,MF_GRAYED);
		EnableWindow(GetDlgItem(g_hwnd,IDC_TEST),0);
		DragAcceptFiles(g_hwnd,TRUE);
		return;
	}
	if (!g_appended) {
    if (s) GlobalFree(s);
    s = (char *)GlobalAlloc(GPTR, lstrlen(g_script)+lstrlen(EXENAME)+2);
		wsprintf(s,"%s %s",EXENAME,g_script);
    g_script = s;
		g_appended = TRUE;
	}
	// Disable buttons during compile
	DisableItems(g_hwnd);
	DWORD id;
	g_hThread=CreateThread(NULL,0,MakeNSISProc,0,0,&id);
}

void RestoreWindowPos(HWND hwnd) {
	HKEY hKey;
	WINDOWPLACEMENT p;
	if (RegOpenKeyEx(REGSEC,REGKEY,0,KEY_READ,&hKey) == ERROR_SUCCESS) {
		DWORD l = sizeof(p);
		DWORD t;
		if ((RegQueryValueEx(hKey,REGLOC,NULL,&t,(unsigned char*)&p,&l)==ERROR_SUCCESS)&&(t == REG_BINARY)&&(l==sizeof(p))) {
			p.length = sizeof(p);
			SetWindowPlacement(hwnd, &p);
		}
		RegCloseKey(hKey);
	}
}

void SaveWindowPos(HWND hwnd) {
	HKEY hKey;
	WINDOWPLACEMENT p;
	p.length = sizeof(p);
	GetWindowPlacement(hwnd, &p);
	if (RegCreateKey(REGSEC,REGKEY,&hKey) == ERROR_SUCCESS) {
		RegSetValueEx(hKey,REGLOC,0,REG_BINARY,(unsigned char*)&p,sizeof(p));
		RegCloseKey(hKey);
	}
}

extern HANDLE g_hThread;
extern int g_retcode;

void ResetObjects() {
	g_appended = FALSE;
	g_warnings = FALSE;
	g_retcode = -1;
	g_hThread = NULL;
}