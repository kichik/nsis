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
#include <stdio.h>
#include "makensisw.h"
#include "resource.h"
#include "noclib.h"

static RECT resizeRect;
static RECT g_griprect;
static int dx;
static int dy;

char *g_script;
int	g_retcode;
HINSTANCE g_hInstance;
HWND g_hwnd;
HANDLE g_hThread;
BOOL g_warnings;
FINDREPLACE fr;
UINT uFindReplaceMsg=0;
HWND hwndFind=0;
CHARRANGE g_chrg;
HMENU g_submnu;
HMENU g_mnu;

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, char *cmdParam, int cmdShow) {
	MSG	msg;
	int status;
	HACCEL haccel; 
	g_hInstance=GetModuleHandle(0);
	g_script=GetCommandLine();
    if (*g_script=='"') { g_script++; while (*g_script && *g_script++!='"' ); }
	else while (*g_script!=' ' && *g_script) g_script++;
	while (*g_script==' ') g_script++;
	if (!InitBranding()) {
		MessageBox(0,NSISERROR,"Error",MB_ICONEXCLAMATION|MB_OK);
		return 1;
	}
	ResetObjects();
	HWND hDialog = CreateDialog(g_hInstance,MAKEINTRESOURCE(DLG_MAIN),0,DialogProc);
	if (!hDialog) {
		MessageBox(0,DLGERROR,"Error",MB_ICONEXCLAMATION|MB_OK);
		return 1;
	}
	haccel = LoadAccelerators(g_hInstance, MAKEINTRESOURCE(IDK_ACCEL)); 
		while ((status=GetMessage(&msg,0,0,0))!=0) {
		if (status==-1) return -1;
		if (!IsDialogMessage(hwndFind, &msg)) {
			if (!TranslateAccelerator(hDialog,haccel,&msg)) {
				if (!IsDialogMessage(hDialog,&msg)) {
				TranslateMessage(&msg);
				DispatchMessage(&msg);
				}
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
            SendMessage(GetDlgItem(hwndDlg,IDC_LOGWIN),EM_SETEVENTMASK,NULL,ENM_SELCHANGE);  
			DragAcceptFiles(g_hwnd,FALSE);
			InitTooltips(g_hwnd);
            g_mnu = GetMenu(hwndDlg);
            g_submnu = GetSubMenu(g_mnu,1);
			SetBranding(g_hwnd);
			HFONT hFont = CreateFont(14,0,0,0,FW_NORMAL,0,0,0,DEFAULT_CHARSET,OUT_CHARACTER_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,FIXED_PITCH|FF_DONTCARE,"Courier New");
			SendDlgItemMessage(hwndDlg,IDC_LOGWIN,WM_SETFONT,(WPARAM)hFont,0);
			SendDlgItemMessage(hwndDlg,IDC_LOGWIN,EM_SETBKGNDCOLOR,0,GetSysColor(COLOR_BTNFACE));
			RestoreWindowPos(g_hwnd);
			CompileNSISScript();
			return TRUE;
		}
		case WM_PAINT:
		{
			PAINTSTRUCT ps;
			GetClientRect(g_hwnd, &g_griprect);
			HDC hdc = BeginPaint(g_hwnd, &ps);
			g_griprect.left = g_griprect.right - GetSystemMetrics(SM_CXVSCROLL);
			g_griprect.top = g_griprect.bottom - GetSystemMetrics(SM_CYVSCROLL);
			DrawFrameControl(hdc, &g_griprect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
			EndPaint(g_hwnd,&ps);
			return TRUE;
		}
		case WM_DESTROY:
		{
			SaveWindowPos(g_hwnd);
			DestroyTooltips();
			PostQuitMessage(0);
			return TRUE;
		}
		case WM_CLOSE:
		{
			if (!g_hThread) {
				DragAcceptFiles(g_hwnd,FALSE);
				DestroyWindow(hwndDlg);
				FreeLibrary(hRichEditDLL);
			}
			return TRUE;
		}
		case WM_CONTEXTMENU:
		{
			if ((HWND)wParam==GetDlgItem(g_hwnd,IDC_LOGWIN)) {
				TrackPopupMenu(g_submnu,NULL,(int)(short)LOWORD(lParam),(int)(short)HIWORD(lParam),0,g_hwnd,0);
			}
			return TRUE;
		}
		case WM_DROPFILES: {
			int num;
			char szTmp[MAX_PATH];
			num = DragQueryFile((HDROP)wParam,-1,NULL,0);
			if (num==1) {
				DragQueryFile((HDROP)wParam,0,szTmp,MAX_PATH);
				if (lstrlen(szTmp)>0) {
					g_script = (char *)GlobalAlloc(GPTR,sizeof(szTmp)+7);
					wsprintf(g_script,"/CD \"%s\"",szTmp);
					ResetObjects();
					CompileNSISScript();
				}
			}
			break;
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
			RECT rSize;
			if (hwndDlg == g_hwnd) {
				GetClientRect(g_hwnd, &rSize);
				if (((rSize.right==0)&&(rSize.bottom==0))||((resizeRect.right==0)&&(resizeRect.bottom==0)))	return TRUE;
				dx = rSize.right - resizeRect.right;
				dy = rSize.bottom - resizeRect.bottom;
				EnumChildWindows(g_hwnd, DialogResize, (LPARAM)0);
				resizeRect = rSize;
			}
			return TRUE;
		}
		case WM_SIZING:
		{
			InvalidateRect(g_hwnd,&g_griprect,TRUE);
			GetClientRect(g_hwnd, &g_griprect);
			g_griprect.left = g_griprect.right - GetSystemMetrics(SM_CXVSCROLL);
			g_griprect.top = g_griprect.bottom - GetSystemMetrics(SM_CYVSCROLL);
			return TRUE;
		}
		case WM_MAKENSIS_PROCESSCOMPLETE:
		{
			if (g_hThread) {
				CloseHandle(g_hThread);
				g_hThread=0;
			}
			EnableItems(g_hwnd);
			if (g_retcode==0) {
				MessageBeep(MB_ICONASTERISK);
				if (g_warnings) SetTitle(g_hwnd,"Finished with Warnings");
				else SetTitle(g_hwnd,"Finished Sucessfully");
			}
			else {
				MessageBeep(MB_ICONEXCLAMATION);
				SetTitle(g_hwnd,"Compile Error: See Log for Details");
			}
			DragAcceptFiles(g_hwnd,TRUE);
			return TRUE;
		}
        case WM_NOTIFY:
            switch (((NMHDR*)lParam)->code ) {
                case EN_SELCHANGE: 
                    SendDlgItemMessage(hwndDlg,IDC_LOGWIN, EM_EXGETSEL, 0, (LPARAM) &g_chrg);
					EnableMenuItem(g_mnu,IDM_COPYSELECTED,(g_chrg.cpMax-g_chrg.cpMin<=0?MF_GRAYED:MF_ENABLED));
                    break;  
            }
            return TRUE;
		case WM_COMMAND:
		{
			switch (LOWORD(wParam)) {
                case IDM_BROWSESCR: {
                    if (g_input_script) {
                        char str[MAX_PATH],*str2;
                        lstrcpy(str,g_input_script);
		                str2=strrchr(str,'\\');
		                if(str2!=NULL) *str2=0;
                        ShellExecute(g_hwnd,"open",str,NULL,NULL,SW_SHOWNORMAL);
                    }
                    return TRUE;
                }
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
				case IDM_NSISDEV:
				{
					ShellExecute(g_hwnd,"open",NSIS_DEV,NULL,NULL,SW_SHOWNORMAL);
					return TRUE;
				}
				case IDM_SELECTALL:
				{
					SendDlgItemMessage(g_hwnd, IDC_LOGWIN, EM_SETSEL, 0, -1);
					return TRUE;
				}
				case IDM_DOCS:
				{
					ShowDocs();
					return TRUE;
				}
				case IDM_LOADSCRIPT:
				{
					if (!g_hThread) {
						OPENFILENAME l={sizeof(l),};
						char buf[MAX_STRING];
						l.hwndOwner = hwndDlg;
						l.lpstrFilter = "NSIS Script (*.nsi)\0*.nsi\0All Files (*.*)\0*.*\0";
						l.lpstrFile = buf;
						l.nMaxFile = MAX_STRING-1;
						l.lpstrTitle = "Load Script";
						l.lpstrDefExt = "log";
						l.lpstrFileTitle = NULL;
						l.lpstrInitialDir = NULL;
						l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER|OFN_PATHMUSTEXIST|OFN_FILEMUSTEXIST;
						lstrcpy(buf,"");
						if (GetOpenFileName(&l)) {
							g_script = (char *)GlobalAlloc(GPTR,sizeof(buf)+7);
							wsprintf(g_script,"/CD \"%s\"",buf);
							ResetObjects();
							CompileNSISScript();
						}
					}
					return TRUE;
				}
				case IDM_CLEARLOG:
				{
					if (!g_hThread) {
						ClearLog(g_hwnd);
						LogMessage(g_hwnd,USAGE);
					}
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
					if (g_output_exe) {
						ShellExecute(g_hwnd,"open",g_output_exe,NULL,NULL,SW_SHOWNORMAL);
					}
					return TRUE;
				}
				case IDM_EDITSCRIPT:
				{
					if (g_input_script) {
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
					SendDlgItemMessage(g_hwnd,IDC_LOGWIN, WM_COPY, 0, 0);
					return TRUE;
				}
				case IDM_SAVE:
				{
					OPENFILENAME l={sizeof(l),};
					char buf[MAX_STRING];
					l.hwndOwner = hwndDlg;
					l.lpstrFilter = "Log Files (*.log)\0*.log\0Text Files (*.txt)\0*.txt\0All Files (*.*)\0*.*\0";
					l.lpstrFile = buf;
					l.nMaxFile = MAX_STRING-1;
					l.lpstrTitle = "Save Output";
					l.lpstrDefExt = "log";
					l.lpstrInitialDir = NULL;
					l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER|OFN_PATHMUSTEXIST;
					lstrcpy(buf,"output");
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
				case IDM_FIND:
				{
					if (!uFindReplaceMsg) uFindReplaceMsg = RegisterWindowMessage(FINDMSGSTRING);
					my_memset(&fr, 0, sizeof(FINDREPLACE));
					fr.lStructSize = sizeof(FINDREPLACE);
					fr.hwndOwner = hwndDlg;
					fr.Flags = FR_NOUPDOWN;
					fr.lpstrFindWhat = (char *)GlobalAlloc(GPTR, 128);
					if (!fr.lpstrFindWhat) return TRUE;
					fr.wFindWhatLen = 128;
					hwndFind = FindText(&fr);
					return TRUE;
				}
			}
		}
	}
	if (uFindReplaceMsg && msg == uFindReplaceMsg) {
		LPFINDREPLACE lpfr = (LPFINDREPLACE)lParam;
		if (lpfr->Flags & FR_FINDNEXT) {
			WPARAM flags = FR_DOWN;
			if (lpfr->Flags & FR_MATCHCASE) flags |= FR_MATCHCASE;
			if (lpfr->Flags & FR_WHOLEWORD) flags |= FR_WHOLEWORD;
			FINDTEXTEX ft;
			SendDlgItemMessage(hwndDlg, IDC_LOGWIN, EM_EXGETSEL, 0, (LPARAM)&ft.chrg);
			if (ft.chrg.cpMax == ft.chrg.cpMin) ft.chrg.cpMin = 0;
			else ft.chrg.cpMin = ft.chrg.cpMax;
			ft.chrg.cpMax = SendDlgItemMessage(hwndDlg, IDC_LOGWIN, WM_GETTEXTLENGTH, 0, 0);
			ft.lpstrText = lpfr->lpstrFindWhat;
			ft.chrg.cpMin = SendDlgItemMessage(hwndDlg, IDC_LOGWIN, EM_FINDTEXTEX, flags, (LPARAM)&ft);
			if (ft.chrg.cpMin != -1) SendDlgItemMessage(hwndDlg, IDC_LOGWIN, EM_SETSEL, ft.chrgText.cpMin, ft.chrgText.cpMax);
			else MessageBeep(MB_ICONASTERISK);
		}
		if (lpfr->Flags & FR_DIALOGTERM) hwndFind = 0;
		return TRUE;
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

extern char *g_branding;

BOOL CALLBACK AboutProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam) {
	static HBRUSH hBrush;
	switch(msg) {
		case WM_INITDIALOG:
		{
			HFONT bfont = CreateFont(13,0,0,0,FW_NORMAL,FALSE,FALSE,FALSE,DEFAULT_CHARSET,
							OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, 
							FIXED_PITCH|FF_DONTCARE, "Tahoma");
 			HFONT bfontb = CreateFont(13,0,0,0,FW_BOLD,FALSE,FALSE,FALSE,DEFAULT_CHARSET,
							OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, 
							FIXED_PITCH|FF_DONTCARE, "Tahoma");
			HFONT rfont = CreateFont(12,0,0,0,FW_NORMAL,FALSE,FALSE,FALSE,DEFAULT_CHARSET,
							OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, 
							FIXED_PITCH|FF_DONTCARE, "MS Shell Dlg");
            HFONT rfontb = CreateFont(12,0,0,0,FW_BOLD,FALSE,FALSE,FALSE,DEFAULT_CHARSET,
							OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, 
							FIXED_PITCH|FF_DONTCARE, "MS Shell Dlg");
			if (bfont&&bfontb) {
                SendDlgItemMessage(hwndDlg, IDC_ABOUTVERSION, WM_SETFONT, (WPARAM)bfontb, FALSE);
				SendDlgItemMessage(hwndDlg, IDC_ABOUTCOPY, WM_SETFONT, (WPARAM)bfont, FALSE);
				SendDlgItemMessage(hwndDlg, IDC_ABOUTPORTIONS, WM_SETFONT, (WPARAM)bfont, FALSE);
			    SendDlgItemMessage(hwndDlg, IDC_NSISVER, WM_SETFONT, (WPARAM)bfont, FALSE);
			    SendDlgItemMessage(hwndDlg, IDC_OTHERCONTRIB, WM_SETFONT, (WPARAM)bfont, FALSE);
            }
            else if (rfont&&rfontb) {
                SendDlgItemMessage(hwndDlg, IDC_ABOUTVERSION, WM_SETFONT, (WPARAM)rfontb, FALSE);
				SendDlgItemMessage(hwndDlg, IDC_ABOUTCOPY, WM_SETFONT, (WPARAM)rfont, FALSE);
				SendDlgItemMessage(hwndDlg, IDC_ABOUTPORTIONS, WM_SETFONT, (WPARAM)rfont, FALSE);
			    SendDlgItemMessage(hwndDlg, IDC_NSISVER, WM_SETFONT, (WPARAM)rfont, FALSE);
			    SendDlgItemMessage(hwndDlg, IDC_OTHERCONTRIB, WM_SETFONT, (WPARAM)rfont, FALSE);
            }
            SetDlgItemText(hwndDlg,IDC_NSISVER,g_branding);
			SetDlgItemText(hwndDlg,IDC_ABOUTVERSION,NSISW_VERSION);
			SetDlgItemText(hwndDlg,IDC_ABOUTCOPY,COPYRIGHT);
            SetDlgItemText(hwndDlg,IDC_OTHERCONTRIB,CONTRIB);
			break;
		}
		case WM_CTLCOLORDLG:
		case WM_CTLCOLORSTATIC:
		case WM_CTLCOLORLISTBOX:
		{
			if(!hBrush) hBrush=CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
			SetBkColor((HDC)wParam, GetSysColor(COLOR_BTNFACE));
			SelectObject((HDC)wParam, hBrush);
			return((LONG)hBrush);
		}
		case WM_COMMAND:
		{
			switch (LOWORD(wParam)) {
				case IDOK: 
					EndDialog(hwndDlg, TRUE);
					break;
			}
		}
	}
	return FALSE;
}
