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
#include <windows.h>
#include "resource.h"
#include "makensisw.h"
#include "noclib.h"

NTOOLTIP g_tip;
LRESULT CALLBACK TipHookProc(int nCode, WPARAM wParam, LPARAM lParam);

extern NSCRIPTDATA g_sdata;

void SetTitle(HWND hwnd,char *substr) {
	char title[64];
	if (substr==NULL) wsprintf(title,"MakeNSISW");
	else wsprintf(title,"MakeNSISW - %s",substr);
	SetWindowText(hwnd,title);
}

void SetBranding(HWND hwnd) {
	SetDlgItemText(hwnd, IDC_VERSION, g_sdata.branding);
}

void CopyToClipboard(HWND hwnd) {
	if (!hwnd||!OpenClipboard(hwnd)) return;
	int len=SendDlgItemMessage(hwnd,IDC_LOGWIN,WM_GETTEXTLENGTH,0,0);
	HGLOBAL mem = GlobalAlloc(GMEM_MOVEABLE,len+1);
	if (!mem) { CloseClipboard(); return; }
	char *existing_text = (char *)GlobalLock(mem);
	if (!existing_text) { CloseClipboard(); return; }
	EmptyClipboard();
	existing_text[0]=0;
	GetDlgItemText(hwnd, IDC_LOGWIN, existing_text, len+1);
	GlobalUnlock(mem);
	SetClipboardData(CF_TEXT,mem);
	CloseClipboard();
}

void ClearLog(HWND hwnd) {
	SetDlgItemText(hwnd, IDC_LOGWIN, "");
}

void LogMessage(HWND hwnd,const char *str) {
	SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_SETSEL, g_sdata.logLength, g_sdata.logLength);
	g_sdata.logLength += lstrlen(str);
	SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_REPLACESEL, 0, (WPARAM)str);
	SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_SCROLLCARET, 0, 0);
}

void ErrorMessage(HWND hwnd,const char *str) {
	if (!str) return;
	char buf[1028];
	wsprintf(buf,"[Error] %s\r\n",str);
	LogMessage(hwnd,buf);
}

void DisableItems(HWND hwnd) {
	EnableWindow(GetDlgItem(hwnd,IDC_CLOSE),0);
	EnableWindow(GetDlgItem(hwnd,IDC_TEST),0);
	EnableMenuItem(g_sdata.menu,IDM_SAVE,MF_GRAYED);
	EnableMenuItem(g_sdata.menu,IDM_TEST,MF_GRAYED);
	EnableMenuItem(g_sdata.menu,IDM_EXIT,MF_GRAYED);
	EnableMenuItem(g_sdata.menu,IDM_LOADSCRIPT,MF_GRAYED);
	EnableMenuItem(g_sdata.menu,IDM_RECOMPILE,MF_GRAYED);
	EnableMenuItem(g_sdata.menu,IDM_COPY,MF_GRAYED);
	EnableMenuItem(g_sdata.menu,IDM_COPYSELECTED,MF_GRAYED);
	EnableMenuItem(g_sdata.menu,IDM_EDITSCRIPT,MF_GRAYED);
	EnableMenuItem(g_sdata.menu,IDM_CLEARLOG,MF_GRAYED);
	EnableMenuItem(g_sdata.menu,IDM_BROWSESCR,MF_GRAYED);
}

void EnableItems(HWND hwnd) {
	#define MSG(a) SendDlgItemMessage(hwnd,IDC_LOGWIN,a,0,0)
	#define MSG1(a,b) SendDlgItemMessage(hwnd,IDC_LOGWIN,a,b,0)
	#define MSG2(a,b,c) SendDlgItemMessage(hwnd,IDC_LOGWIN,a,b,c)
	if (g_sdata.input_script) {
		GlobalFree(g_sdata.input_script);
		g_sdata.input_script = 0;
	}
	if (g_sdata.output_exe) {
		GlobalFree(g_sdata.output_exe);
		g_sdata.output_exe = 0;
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
    if (tr.chrg.cpMin == lstrlen("Processing script file: \"") - 1) tr.chrg.cpMax = tr.chrg.cpMin = 0;
	tr.lpstrText = g_sdata.input_script = (char *)GlobalAlloc(GPTR, tr.chrg.cpMax-tr.chrg.cpMin+1);
	MSG2(EM_GETTEXTRANGE, 0, (WPARAM)&tr);

	// find output exe
	ft.chrg.cpMin = 0;
	ft.chrg.cpMax = MSG(WM_GETTEXTLENGTH);
	ft.lpstrText = "Output: \"";
	ft.chrg.cpMin = tr.chrg.cpMin = MSG2(EM_FINDTEXT, 0, (LPARAM)&ft) + lstrlen("Output: \"");
	ft.lpstrText = "\"";
	tr.chrg.cpMax = MSG2(EM_FINDTEXT, 0, (LPARAM)&ft);
    if (tr.chrg.cpMin == lstrlen("Output: \"") - 1) tr.chrg.cpMax = tr.chrg.cpMin = 0;
	tr.lpstrText = g_sdata.output_exe = (char *)GlobalAlloc(GPTR, tr.chrg.cpMax-tr.chrg.cpMin+1);
	MSG2(EM_GETTEXTRANGE, 0, (WPARAM)&tr);

	g_sdata.warnings = FALSE;

	ft.lpstrText = "warning:";
	if (MSG2(EM_FINDTEXT, 0, (LPARAM)&ft) != -1) g_sdata.warnings++;
	ft.lpstrText = "warnings:";
	if (MSG2(EM_FINDTEXT, 0, (LPARAM)&ft) != -1) g_sdata.warnings++;

	if (g_sdata.output_exe && !g_sdata.retcode) {
			EnableWindow(GetDlgItem(hwnd,IDC_TEST),1);
			EnableMenuItem(g_sdata.menu,IDM_TEST,MF_ENABLED);
	}
	EnableWindow(GetDlgItem(hwnd,IDC_CLOSE),1);
	EnableMenuItem(g_sdata.menu,IDM_SAVE,MF_ENABLED);
	EnableMenuItem(g_sdata.menu,IDM_EXIT,MF_ENABLED);
	EnableMenuItem(g_sdata.menu,IDM_LOADSCRIPT,MF_ENABLED);
	EnableMenuItem(g_sdata.menu,IDM_RECOMPILE,MF_ENABLED);
	EnableMenuItem(g_sdata.menu,IDM_COPY,MF_ENABLED);
	EnableMenuItem(g_sdata.menu,IDM_COPYSELECTED,MF_ENABLED);
	EnableMenuItem(g_sdata.menu,IDM_EDITSCRIPT,MF_ENABLED);
	EnableMenuItem(g_sdata.menu,IDM_CLEARLOG,MF_ENABLED);
    EnableMenuItem(g_sdata.menu,IDM_BROWSESCR,MF_ENABLED);
}

void CompileNSISScript() {
	static char *s;
	DragAcceptFiles(g_sdata.hwnd,FALSE);
	ClearLog(g_sdata.hwnd);
	SetTitle(g_sdata.hwnd,NULL);
	if (lstrlen(g_sdata.script)==0) {
		LogMessage(g_sdata.hwnd,USAGE);
		EnableMenuItem(g_sdata.menu,IDM_RECOMPILE,MF_GRAYED);
		EnableMenuItem(g_sdata.menu,IDM_EDITSCRIPT,MF_GRAYED);
		EnableMenuItem(g_sdata.menu,IDM_TEST,MF_GRAYED);
		EnableMenuItem(g_sdata.menu,IDM_BROWSESCR,MF_GRAYED);
		EnableWindow(GetDlgItem(g_sdata.hwnd,IDC_TEST),0);
		DragAcceptFiles(g_sdata.hwnd,TRUE);
		return;
	}
	if (!g_sdata.appended) {
		if (s) GlobalFree(s);
		s = (char *)GlobalAlloc(GPTR, lstrlen(g_sdata.script)+sizeof(EXENAME)+2);
		wsprintf(s,"%s %s",EXENAME,g_sdata.script);
    if (g_sdata.script_alloced) GlobalFree(g_sdata.script);
    g_sdata.script_alloced = true;
		g_sdata.script = s;
		g_sdata.appended = TRUE;
	}
    g_sdata.logLength = 0;
	// Disable buttons during compile
	DisableItems(g_sdata.hwnd);
	DWORD id;
	g_sdata.thread=CreateThread(NULL,0,MakeNSISProc,0,0,&id);
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

void ResetObjects() {
	g_sdata.appended = FALSE;
	g_sdata.warnings = FALSE;
	g_sdata.retcode = -1;
	g_sdata.thread = NULL;
}

int InitBranding() {
	char *s;
	s = (char *)GlobalAlloc(GPTR,lstrlen(EXENAME)+10);
	wsprintf(s,"%s /version",EXENAME);
	{
		STARTUPINFO si={sizeof(si),};
		SECURITY_ATTRIBUTES sa={sizeof(sa),};
		SECURITY_DESCRIPTOR sd={0,};
		PROCESS_INFORMATION pi={0,};
		HANDLE newstdout=0,read_stdout=0; 

		OSVERSIONINFO osv={sizeof(osv)};
		GetVersionEx(&osv);
		if (osv.dwPlatformId == VER_PLATFORM_WIN32_NT) {
			InitializeSecurityDescriptor(&sd,SECURITY_DESCRIPTOR_REVISION);
			SetSecurityDescriptorDacl(&sd,true,NULL,false);
			sa.lpSecurityDescriptor = &sd;
		}
		else sa.lpSecurityDescriptor = NULL;
		sa.bInheritHandle = true;
		if (!CreatePipe(&read_stdout,&newstdout,&sa,0)) {
			return 0;
		}
		GetStartupInfo(&si);
		si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
		si.wShowWindow = SW_HIDE;
		si.hStdOutput = newstdout;
		si.hStdError = newstdout;
		if (!CreateProcess(NULL,s,NULL,NULL,TRUE,CREATE_NEW_CONSOLE,NULL,NULL,&si,&pi)) {
			CloseHandle(newstdout);
			CloseHandle(read_stdout);
			return 0;
		}
		char szBuf[1024];
		DWORD dwRead = 1;
		DWORD dwExit = !STILL_ACTIVE;
		if (WaitForSingleObject(pi.hProcess,10000)!=WAIT_OBJECT_0) {
			return 0;
		}
		ReadFile(read_stdout, szBuf, sizeof(szBuf)-1, &dwRead, NULL);
		szBuf[dwRead] = 0;
		if (lstrlen(szBuf)==0) return 0;
		g_sdata.branding = (char *)GlobalAlloc(GPTR,lstrlen(szBuf)+6);
		wsprintf(g_sdata.branding,"NSIS %s",szBuf);
		g_sdata.brandingv = (char *)GlobalAlloc(GPTR,lstrlen(szBuf)+1);
		lstrcpy(g_sdata.brandingv,szBuf);
		GlobalFree(s);
	}
	return 1;
}

void InitTooltips(HWND h) {
	if (h == NULL)	return;
    my_memset(&g_tip,0,sizeof(NTOOLTIP));
	g_tip.tip_p = h;
	INITCOMMONCONTROLSEX icx;
	icx.dwSize	= sizeof(icx);
	icx.dwICC	= ICC_BAR_CLASSES;
	InitCommonControlsEx(&icx);
	DWORD dwStyle = WS_POPUP | WS_BORDER | TTS_ALWAYSTIP;
	DWORD dwExStyle = WS_EX_TOOLWINDOW | WS_EX_TOPMOST;
	g_tip.tip = CreateWindowEx(dwExStyle,TOOLTIPS_CLASS,NULL,dwStyle,0,0,0,0,h,NULL,GetModuleHandle(NULL),NULL);
	if (!g_tip.tip) return;
	g_tip.hook = SetWindowsHookEx(WH_GETMESSAGE,TipHookProc,NULL, GetCurrentThreadId());
	AddTip(GetDlgItem(h,IDC_CLOSE),TEXT("Close MakeNSISW"));
	AddTip(GetDlgItem(h,IDC_TEST),TEXT("Test the installer generated by MakeNSISW"));
}

void DestroyTooltips() {
	UnhookWindowsHookEx(g_tip.hook);
}

void AddTip(HWND hWnd,LPSTR lpszToolTip) {
	TOOLINFO ti;
	ti.cbSize = sizeof(TOOLINFO);
	ti.uFlags = TTF_IDISHWND;
	ti.hwnd   = g_tip.tip_p;
	ti.uId = (UINT) hWnd;
	ti.lpszText = lpszToolTip;
	SendMessage(g_tip.tip, TTM_ADDTOOL, 0, (LPARAM) (LPTOOLINFO) &ti); 
}

LRESULT CALLBACK TipHookProc(int nCode, WPARAM wParam, LPARAM lParam) {
	if (nCode < 0) return CallNextHookEx(g_tip.hook, nCode, wParam, lParam); 
	switch (((MSG*)lParam)->message) { 
		case WM_MOUSEMOVE:
			if (IsChild(g_tip.tip_p,((MSG*)lParam)->hwnd)) 
					SendMessage(g_tip.tip, TTM_RELAYEVENT, 0,lParam); 
			break; 
		default: 
			break; 
    } 
    return CallNextHookEx(g_tip.hook, nCode, wParam, lParam); 
}

void ShowDocs() {
	char pathf[MAX_PATH],*path;
	GetModuleFileName(NULL,pathf,sizeof(pathf));
	path=my_strrchr(pathf,'\\');
	if(path!=NULL) *path=0;
	lstrcat(pathf,LOCALDOCS);
	if ((int)ShellExecute(g_sdata.hwnd,"open",pathf,NULL,NULL,SW_SHOWNORMAL)<=32) 
		ShellExecute(g_sdata.hwnd,"open",DOCPATH,NULL,NULL,SW_SHOWNORMAL);
}

int getProxyInfo(char *out) {
    DWORD v=0;
	HKEY hKey;
    if (RegOpenKeyEx(HKEY_CURRENT_USER,"Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings",0,KEY_READ,&hKey) == ERROR_SUCCESS) {
		DWORD l = 4;
		DWORD t;
        if (RegQueryValueEx(hKey,"ProxyEnable",NULL,&t,(unsigned char *)&v,&l) == ERROR_SUCCESS && t == REG_DWORD) {
            l=8192;
            if (RegQueryValueEx(hKey,"ProxyServer",NULL,&t,(unsigned char *)out,&l ) != ERROR_SUCCESS || t != REG_SZ) { 
                v=0; 
                *out=0; 
            }
        }
        else v=0;
        out[8192-1]=0;
        RegCloseKey(hKey);
    }
    return v;
}