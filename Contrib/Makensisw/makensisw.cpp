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
#include <stdio.h>
#include "makensisw.h"
#include "resource.h"
#include "noclib.h"

char *g_script;
int	g_retcode;

static RECT resizeRect;
static int dx;
static int dy;

HINSTANCE g_hInstance;
HWND g_hwnd;
HANDLE g_hThread;
BOOL g_warnings;

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, char *cmdParam, int cmdShow) {
	HACCEL haccel; 
	g_hInstance=GetModuleHandle(0);
	g_script=GetCommandLine(); // set commandline global string
	if (*g_script++=='"') while (*g_script++!='"');
	else while (*g_script++!=' ');
	while (*g_script==' ') g_script++;
	g_retcode = -1; // return code is always false unless set to true by GetExitCodeProcess
	g_warnings = FALSE;
	HWND hDialog = CreateDialog(g_hInstance,MAKEINTRESOURCE(DLG_MAIN),0,DialogProc);
	if (!hDialog) {
		char buf [MAX_STRING];
		wsprintf(buf, "Error creating dialog box.\n\nError: %x", GetLastError ());
		MessageBox(0, buf, "Error", MB_ICONEXCLAMATION | MB_OK);
		return 1;
	}
	haccel = LoadAccelerators(g_hInstance, MAKEINTRESOURCE(IDK_ACCEL)); 
	MSG	msg;
	int status;
	while ((status=GetMessage(&msg,0,0,0))!=0) {
		if (status==-1) return -1;
		if (!TranslateAccelerator(hDialog,haccel,&msg)) {
			if (!IsDialogMessage(hDialog,&msg)) {
				TranslateMessage(&msg);
				DispatchMessage(&msg);
			}
		}
	}
	ExitProcess(msg.wParam);
	return msg.wParam;
}

BOOL CALLBACK DialogProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam) {
	static HINSTANCE hRichEditDLL = 0;
	if (!hRichEditDLL) hRichEditDLL= LoadLibrary("RichEd32.dll");
	switch (msg) {
		case WM_INITDIALOG:
		{
			g_hwnd=hwndDlg;
			HICON hIcon = LoadIcon(g_hInstance,MAKEINTRESOURCE(IDI_ICON));
			SetClassLong(hwndDlg,GCL_HICON,(long)hIcon); 
			HFONT hFont = CreateFont(14,0,0,0,FW_NORMAL,0,0,0,DEFAULT_CHARSET,OUT_CHARACTER_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,FIXED_PITCH|FF_DONTCARE,"Courier New");
			SendDlgItemMessage(hwndDlg,IDC_LOGWIN,WM_SETFONT,(WPARAM)hFont,0);
			SendDlgItemMessage(hwndDlg,IDC_LOGWIN,EM_SETBKGNDCOLOR,0,GetSysColor(COLOR_BTNFACE));
			RestoreWindowPos(g_hwnd);
			CompileNSISScript();
			return TRUE;
		}
		case WM_DESTROY:
		{
			SaveWindowPos(g_hwnd);
			PostQuitMessage(0);
			return TRUE;
		}
		case WM_CLOSE:
		{
			if (!g_hThread) {
				DestroyWindow(hwndDlg);
				FreeLibrary(hRichEditDLL);
			}
			return TRUE;
		}
		case WM_GETMINMAXINFO:
		{
			((MINMAXINFO*)lParam)->ptMinTrackSize.x=MINWIDTH; 
			((MINMAXINFO*)lParam)->ptMinTrackSize.y=MINHEIGHT;
		}
		case WM_ENTERSIZEMOVE:
		{
			
			GetClientRect(g_hwnd, &resizeRect);
			return TRUE;
		}
		case WM_SIZE:
		{
			if ((wParam == SIZE_MAXHIDE)||(wParam == SIZE_MAXSHOW)) return TRUE;
		}
		case WM_SIZING:
		{
			RECT rSize;
			if (hwndDlg == g_hwnd) {
				GetClientRect(g_hwnd, &rSize);
				if (((rSize.right==0)&&(rSize.bottom==0))||((resizeRect.right==0)&&(resizeRect.bottom==0)))
					return TRUE;
				dx = rSize.right - resizeRect.right;
				dy = rSize.bottom - resizeRect.bottom;
				EnumChildWindows(g_hwnd, DialogResize, (LPARAM)0);
				resizeRect = rSize;
			}
			 return TRUE;
		 }
		case WM_MAKENSIS_PROCESSCOMPLETE:
		{
			if (g_hThread) {
				CloseHandle(g_hThread);
				g_hThread=0;
			}
			if (g_retcode==0) {
				MessageBeep(MB_ICONASTERISK);
				if (g_warnings) SetTitle(g_hwnd,"Finished with Warnings");
				else SetTitle(g_hwnd,"Finished Sucessfully");
			}
			else SetTitle(g_hwnd,"Compile Error: See Log for Details");
			EnableItems(g_hwnd);
			return TRUE;
		}
		case WM_COMMAND:
		{
			switch (LOWORD(wParam)) {
				case IDM_ABOUT:
				{
					DialogBox(g_hInstance,MAKEINTRESOURCE(DLG_ABOUT),g_hwnd,(DLGPROC)AboutProc);
					return TRUE;
				}
				case IDM_NSISHOME:
				{
					ShellExecute(g_hwnd,"open",NSIS_URL,NULL,NULL,SW_SHOWNORMAL);
					return TRUE;
				}
				case IDM_DOCS:
				{
					char pathf[MAX_PATH],*path;
					GetModuleFileName(NULL,pathf,sizeof(pathf));
					path=my_strrchr(pathf,'\\');
					if(path!=NULL) *path=0;
					lstrcat(pathf,LOCALDOCS);
					if ((int)ShellExecute(g_hwnd,"open",pathf,NULL,NULL,SW_SHOWNORMAL)<=32) 
						ShellExecute(g_hwnd,"open",DOCPATH,NULL,NULL,SW_SHOWNORMAL);
					return TRUE;
				}
				case IDM_RECOMPILE:
				{
					CompileNSISScript();
					return TRUE;
				}
				case IDM_TEST:
				case IDC_TEST:
				{
					if (g_output_exe[0]) {
						ShellExecute(g_hwnd,"open",g_output_exe,NULL,NULL,SW_SHOWNORMAL);
					}
					return TRUE;
				}
				case IDM_EDITSCRIPT:
				{
					if (g_input_script[0]) {
						if ((int)ShellExecute(g_hwnd,"open",g_input_script,NULL,NULL,SW_SHOWNORMAL)<=32) {
							char path[MAX_PATH];
							if (GetWindowsDirectory(path,sizeof(path))) {
								lstrcat(path,"\\notepad.exe");
								ShellExecute(g_hwnd,"open",path,g_input_script,NULL,SW_SHOWNORMAL);
							}
						}
					}
					return TRUE;
				}
				case IDC_CLOSE:
				case IDM_EXIT:
				{
					if (!g_hThread) {
						DestroyWindow(hwndDlg);
					}
					return TRUE;
				}
				case IDM_COPY:
				{
					CopyToClipboard(g_hwnd);
					return TRUE;
				}
				case IDM_COPYSELECTED:
				{
					SendMessage(GetDlgItem(g_hwnd,IDC_LOGWIN), WM_COPY, 0, 0);
					return TRUE;
				}
				case IDM_SAVE:
				{
					OPENFILENAME l={sizeof(l),};
					char buf[MAX_STRING];
					l.hwndOwner = hwndDlg;
					l.lpstrFilter = "Log Files (*.log)\0Text Files (*.txt)\0*.txt\0All Files (*.*)\0*.*\0";
					l.lpstrFile = buf;
					l.nMaxFile = 1023;
					l.lpstrTitle = "Save Output";
					l.lpstrDefExt = "log";
					l.lpstrInitialDir = NULL;
					l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER|OFN_PATHMUSTEXIST;
					lstrcpy(buf,"output.log");
					if (GetSaveFileName(&l)) {
						HANDLE hFile = CreateFile(buf,GENERIC_WRITE,0,0,CREATE_ALWAYS,0,0);
						if (hFile) {
							int len=SendDlgItemMessage(g_hwnd,IDC_LOGWIN,WM_GETTEXTLENGTH,0,0);
							char *existing_text=(char*)GlobalAlloc(GPTR,len);
							existing_text[0]=0;
							GetDlgItemText(g_hwnd, IDC_LOGWIN, existing_text, len);
							DWORD dwWritten = 0;
							WriteFile(hFile,existing_text,len,&dwWritten,0);
							CloseHandle(hFile);
							GlobalFree(existing_text);
						}
					} 
					return TRUE;
				}
			}
		}
		}
	return 0;
}

DWORD WINAPI MakeNSISProc(LPVOID p) {
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
		ErrorMessage(g_hwnd,"There was an error creating the pipe.");
		PostMessage(g_hwnd,WM_MAKENSIS_PROCESSCOMPLETE,0,0);
		return 1;
	}
	GetStartupInfo(&si);
	si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
	si.wShowWindow = SW_HIDE;
	si.hStdOutput = newstdout;
	si.hStdError = newstdout;
	if (!CreateProcess(NULL,g_script,NULL,NULL,TRUE,CREATE_NEW_CONSOLE,NULL,NULL,&si,&pi)) {
		char buf[MAX_STRING];
		wsprintf(buf,"Could not execute:\r\n %s.",g_script);
		ErrorMessage(g_hwnd,buf);
		CloseHandle(newstdout);
		CloseHandle(read_stdout);
		PostMessage(g_hwnd,WM_MAKENSIS_PROCESSCOMPLETE,0,0);
		return 1;
	}

	char szBuf[1024];
	DWORD dwRead = 1;
	DWORD dwExit = !STILL_ACTIVE;

	while (dwExit == STILL_ACTIVE || dwRead) {
		PeekNamedPipe(read_stdout, 0, 0, 0, &dwRead, NULL);
		if (dwRead) {
			ReadFile(read_stdout, szBuf, sizeof(szBuf)-1, &dwRead, NULL);
			szBuf[dwRead] = 0;
			LogMessage(g_hwnd, szBuf);
		}
		else Sleep(TIMEOUT);
		GetExitCodeProcess(pi.hProcess, &dwExit);
		// Make sure we have no data before killing getting out of the loop
		if (dwExit != STILL_ACTIVE) {
			PeekNamedPipe(read_stdout, 0, 0, 0, &dwRead, NULL);
		}
	}

	g_retcode = dwExit;
	CloseHandle(pi.hThread);
	CloseHandle(pi.hProcess);
	CloseHandle(newstdout);
	CloseHandle(read_stdout);
	PostMessage(g_hwnd,WM_MAKENSIS_PROCESSCOMPLETE,0,0);
	return 0;
}

BOOL CALLBACK DialogResize(HWND hWnd, LPARAM /* unused */) 
{
	RECT r;
	GetWindowRect(hWnd, &r);
	ScreenToClient(g_hwnd, (LPPOINT)&r);
	ScreenToClient(g_hwnd, ((LPPOINT)&r)+1);
	switch (GetDlgCtrlID(hWnd)) {
		case IDC_LOGWIN:
			SetWindowPos(hWnd, 0, r.left, r.top,r.right - r.left + dx, r.bottom - r.top + dy, SWP_NOZORDER|SWP_NOMOVE);
			break;
		case IDC_TEST:
		case IDC_CLOSE:
			SetWindowPos(hWnd, 0, r.left + dx, r.top + dy, 0, 0, SWP_NOZORDER|SWP_NOSIZE);
			break;
		default:
			SetWindowPos(hWnd, 0, r.left, r.top + dy, r.right - r.left + dx, r.bottom - r.top, SWP_NOZORDER);
			break;
	}
	RedrawWindow(hWnd,NULL,NULL,RDW_INVALIDATE);
	return TRUE;
}

BOOL CALLBACK AboutProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam) {
	switch(msg) {
		case WM_INITDIALOG:
		{
			HFONT bfont = CreateFont(14,0,0,0,FW_BOLD,FALSE,FALSE,FALSE,DEFAULT_CHARSET,
							OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, 
							FIXED_PITCH|FF_DONTCARE, "MS Shell Dlg");
			HFONT rfont = CreateFont(12,0,0,0,FW_NORMAL,FALSE,FALSE,FALSE,DEFAULT_CHARSET,
							OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, 
							FIXED_PITCH|FF_DONTCARE, "MS Shell Dlg");
			if (bfont) SendDlgItemMessage(hwndDlg, IDC_ABOUTVERSION, WM_SETFONT, (WPARAM)bfont, FALSE);
			if (rfont) {
				SendDlgItemMessage(hwndDlg, IDC_ABOUTCOPY, WM_SETFONT, (WPARAM)rfont, FALSE);
				SendDlgItemMessage(hwndDlg, IDC_ABOUTPORTIONS, WM_SETFONT, (WPARAM)rfont, FALSE);
			}
			//char buf[MAX_STRING];
			SetDlgItemText(hwndDlg,IDC_ABOUTVERSION,NSISW_VERSION);
			SetDlgItemText(hwndDlg,IDC_ABOUTCOPY,COPYRIGHT);
			SetDlgItemText(hwndDlg,IDC_ABOUTPORTIONS,CONTRIBUTOR);
		}
		case WM_COMMAND:
		{
			switch (LOWORD(wParam)) {
				case IDOK: {
					EndDialog(hwndDlg, TRUE);
				}
			}
		}
	}
	return FALSE;
}
