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
	char *existing_text=(char*)GlobalAlloc(GPTR,len);
	if (!hwnd||!OpenClipboard(hwnd)||!existing_text) return;
	EmptyClipboard();
	existing_text[0]=0;
	GetDlgItemText(hwnd, IDC_LOGWIN, existing_text, len);
	SetClipboardData(CF_TEXT,existing_text);
	CloseClipboard();
}


void ClearLog(HWND hwnd) {
	SetDlgItemText(hwnd, IDC_LOGWIN, "");
}

char g_output_exe[1024];
char g_input_script[1024];
extern BOOL g_warnings;

void LogMessage(HWND hwnd,const char *str) {
	SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_REPLACESEL, 0, (WPARAM)str);
	SendDlgItemMessage(hwnd, IDC_LOGWIN, WM_VSCROLL, SB_BOTTOM, 0);
}

void ErrorMessage(HWND hwnd,const char *str) {
	if (!str) return;
	char buf[1028];
	wsprintf(buf,"Error - %s\r\n",str);
	LogMessage(hwnd,buf);
}

void DisableItems(HWND hwnd) {
	g_output_exe[0]=0;
	g_input_script[0]=0;
	EnableWindow(GetDlgItem(hwnd,IDC_CLOSE),0);
	EnableWindow(GetDlgItem(hwnd,IDC_TEST),0);
	HMENU m = GetMenu(hwnd);
	EnableMenuItem(m,IDM_SAVE,MF_GRAYED);
	EnableMenuItem(m,IDM_TEST,MF_GRAYED);
	EnableMenuItem(m,IDM_EXIT,MF_GRAYED);
	EnableMenuItem(m,IDM_RECOMPILE,MF_GRAYED);
	EnableMenuItem(m,IDM_COPY,MF_GRAYED);
	EnableMenuItem(m,IDM_COPYSELECTED,MF_GRAYED);
	EnableMenuItem(m,IDM_EDITSCRIPT,MF_GRAYED);
}

void EnableItems(HWND hwnd) {
	int len=SendDlgItemMessage(hwnd,IDC_LOGWIN,WM_GETTEXTLENGTH,0,0);
	if (len>0) {
		HGLOBAL memory;
		char *existing_text;
		memory = GlobalAlloc(GMEM_MOVEABLE,len+1);
        existing_text = (char *)GlobalLock(memory);
		if (!existing_text) return;
		existing_text[0]=0;
		GetDlgItemText(hwnd, IDC_LOGWIN, existing_text, len);
		char *p=existing_text;
		char *p2;
		char *p3;
		if ((p2=my_strstr(p,"\r\nOutput: \""))) {
			while (*p2 != '\"') p2++;
			p2++;
			if ((p3=my_strstr(p2,"\"\r\n")) && p3 < my_strstr(p2,"\r\n")) {
				*p3=0;
				lstrcpy(g_output_exe,p2);
			}
		}
		p=my_strstr(existing_text,"\r\nProcessing script file: \"");
		if (p) {
			while (*p++ != '"');
			char *p2=my_strstr(p,"\r\n");
			lstrcpyn(g_input_script,p,p2-p);
		}
		if (my_strstr(existing_text, " warning:") || my_strstr(existing_text, " warnings:")) {
			g_warnings = TRUE;
		}
		GlobalUnlock(memory);
	}
	HMENU m = GetMenu(hwnd);
	if (g_output_exe[0]) {
			EnableWindow(GetDlgItem(hwnd,IDC_TEST),1);
			EnableMenuItem(m,IDM_TEST,MF_ENABLED);
	}
	EnableWindow(GetDlgItem(hwnd,IDC_CLOSE),1);
	EnableMenuItem(m,IDM_SAVE,MF_ENABLED);
	EnableMenuItem(m,IDM_EXIT,MF_ENABLED);
	EnableMenuItem(m,IDM_RECOMPILE,MF_ENABLED);
	EnableMenuItem(m,IDM_COPY,MF_ENABLED);
	EnableMenuItem(m,IDM_COPYSELECTED,MF_ENABLED);
	EnableMenuItem(m,IDM_EDITSCRIPT,MF_ENABLED);
}

static BOOL g_appended = FALSE;
void CompileNSISScript() {
	char s[MAX_PATH];
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
		return;
	}
	if (!g_appended) {
		wsprintf(s,"%s %s",EXENAME,g_script);
		lstrcpy(g_script,s);
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
