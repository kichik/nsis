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

NSCRIPTDATA g_sdata;
NRESIZEDATA g_resize;
NFINDREPLACE g_find;

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, char *cmdParam, int cmdShow) {
	MSG	msg;
	int status;
	HACCEL haccel; 
    ZeroMemory(&g_sdata,sizeof(NSCRIPTDATA));
    ZeroMemory(&g_resize,sizeof(NRESIZEDATA));
    ZeroMemory(&g_find,sizeof(NFINDREPLACE));
	g_sdata.hInstance=GetModuleHandle(0);
	g_sdata.script=GetCommandLine();
    if (*g_sdata.script=='"') { g_sdata.script++; while (*g_sdata.script && *g_sdata.script++!='"' ); }
	else while (*g_sdata.script!=' ' && *g_sdata.script) g_sdata.script++;
	while (*g_sdata.script==' ') g_sdata.script++;
	if (!InitBranding()) {
		MessageBox(0,NSISERROR,"Error",MB_ICONEXCLAMATION|MB_OK);
		return 1;
	}
	ResetObjects();
	HWND hDialog = CreateDialog(g_sdata.hInstance,MAKEINTRESOURCE(DLG_MAIN),0,DialogProc);
	if (!hDialog) {
		MessageBox(0,DLGERROR,"Error",MB_ICONEXCLAMATION|MB_OK);
		return 1;
	}
	haccel = LoadAccelerators(g_sdata.hInstance, MAKEINTRESOURCE(IDK_ACCEL)); 
		while ((status=GetMessage(&msg,0,0,0))!=0) {
		if (status==-1) return -1;
		if (!IsDialogMessage(g_find.hwndFind, &msg)) {
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
			g_sdata.hwnd=hwndDlg;
			HICON hIcon = LoadIcon(g_sdata.hInstance,MAKEINTRESOURCE(IDI_ICON));
			SetClassLong(hwndDlg,GCL_HICON,(long)hIcon); 
            SendMessage(GetDlgItem(hwndDlg,IDC_LOGWIN),EM_SETEVENTMASK,NULL,ENM_SELCHANGE);  
			DragAcceptFiles(g_sdata.hwnd,FALSE);
			InitTooltips(g_sdata.hwnd);
            g_sdata.menu = GetMenu(g_sdata.hwnd);
            g_sdata.submenu = GetSubMenu(g_sdata.menu,1);
			SetBranding(g_sdata.hwnd);
			HFONT hFont = CreateFont(14,0,0,0,FW_NORMAL,0,0,0,DEFAULT_CHARSET,OUT_CHARACTER_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,FIXED_PITCH|FF_DONTCARE,"Courier New");
			SendDlgItemMessage(hwndDlg,IDC_LOGWIN,WM_SETFONT,(WPARAM)hFont,0);
			SendDlgItemMessage(hwndDlg,IDC_LOGWIN,EM_SETBKGNDCOLOR,0,GetSysColor(COLOR_BTNFACE));
			RestoreWindowPos(g_sdata.hwnd);
			CompileNSISScript();
			return TRUE;
		}
		case WM_PAINT:
		{
			PAINTSTRUCT ps;
			GetClientRect(g_sdata.hwnd, &g_resize.griprect);
			HDC hdc = BeginPaint(g_sdata.hwnd, &ps);
			g_resize.griprect.left = g_resize.griprect.right - GetSystemMetrics(SM_CXVSCROLL);
			g_resize.griprect.top = g_resize.griprect.bottom - GetSystemMetrics(SM_CYVSCROLL);
			DrawFrameControl(hdc, &g_resize.griprect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
			EndPaint(g_sdata.hwnd,&ps);
			return TRUE;
		}
		case WM_DESTROY:
		{
			SaveWindowPos(g_sdata.hwnd);
			DestroyTooltips();
			PostQuitMessage(0);
			return TRUE;
		}
		case WM_CLOSE:
		{
			if (!g_sdata.thread) {
				DragAcceptFiles(g_sdata.hwnd,FALSE);
				DestroyWindow(hwndDlg);
				FreeLibrary(hRichEditDLL);
			}
			return TRUE;
		}
		case WM_CONTEXTMENU:
		{
			if ((HWND)wParam==GetDlgItem(g_sdata.hwnd,IDC_LOGWIN)) {
				TrackPopupMenu(g_sdata.submenu,NULL,(int)(short)LOWORD(lParam),(int)(short)HIWORD(lParam),0,g_sdata.hwnd,0);
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
					g_sdata.script = (char *)GlobalAlloc(GPTR,sizeof(szTmp)+7);
					wsprintf(g_sdata.script,"/CD \"%s\"",szTmp);
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
			GetClientRect(g_sdata.hwnd, &g_resize.resizeRect);
			return TRUE;
		}
		case WM_SIZE:
		{
			if ((wParam == SIZE_MAXHIDE)||(wParam == SIZE_MAXSHOW)) return TRUE;
			RECT rSize;
			if (hwndDlg == g_sdata.hwnd) {
				GetClientRect(g_sdata.hwnd, &rSize);
				if (((rSize.right==0)&&(rSize.bottom==0))||((g_resize.resizeRect.right==0)&&(g_resize.resizeRect.bottom==0)))	return TRUE;
				g_resize.dx = rSize.right - g_resize.resizeRect.right;
				g_resize.dy = rSize.bottom - g_resize.resizeRect.bottom;
				EnumChildWindows(g_sdata.hwnd, DialogResize, (LPARAM)0);
				g_resize.resizeRect = rSize;
			}
			return TRUE;
		}
		case WM_SIZING:
		{
			InvalidateRect(g_sdata.hwnd,&g_resize.griprect,TRUE);
			GetClientRect(g_sdata.hwnd, &g_resize.griprect);
			g_resize.griprect.left = g_resize.griprect.right - GetSystemMetrics(SM_CXVSCROLL);
			g_resize.griprect.top = g_resize.griprect.bottom - GetSystemMetrics(SM_CYVSCROLL);
			return TRUE;
		}
		case WM_MAKENSIS_PROCESSCOMPLETE:
		{
			if (g_sdata.thread) {
				CloseHandle(g_sdata.thread);
				g_sdata.thread=0;
			}
			EnableItems(g_sdata.hwnd);
			if (g_sdata.retcode==0) {
				MessageBeep(MB_ICONASTERISK);
				if (g_sdata.warnings) SetTitle(g_sdata.hwnd,"Finished with Warnings");
				else SetTitle(g_sdata.hwnd,"Finished Sucessfully");
			}
			else {
				MessageBeep(MB_ICONEXCLAMATION);
				SetTitle(g_sdata.hwnd,"Compile Error: See Log for Details");
			}
			DragAcceptFiles(g_sdata.hwnd,TRUE);
			return TRUE;
		}
        case WM_NOTIFY:
            switch (((NMHDR*)lParam)->code ) {
                case EN_SELCHANGE: 
                    SendDlgItemMessage(hwndDlg,IDC_LOGWIN, EM_EXGETSEL, 0, (LPARAM) &g_sdata.textrange);
					EnableMenuItem(g_sdata.menu,IDM_COPYSELECTED,(g_sdata.textrange.cpMax-g_sdata.textrange.cpMin<=0?MF_GRAYED:MF_ENABLED));
                    break;  
            }
            return TRUE;
		case WM_COMMAND:
		{
			switch (LOWORD(wParam)) {
                case IDM_BROWSESCR: {
                    if (g_sdata.input_script) {
                        char str[MAX_PATH],*str2;
                        lstrcpy(str,g_sdata.input_script);
		                str2=strrchr(str,'\\');
		                if(str2!=NULL) *str2=0;
                        ShellExecute(g_sdata.hwnd,"open",str,NULL,NULL,SW_SHOWNORMAL);
                    }
                    return TRUE;
                }
				case IDM_ABOUT:
				{
					DialogBox(g_sdata.hInstance,MAKEINTRESOURCE(DLG_ABOUT),g_sdata.hwnd,(DLGPROC)AboutProc);
					return TRUE;
				}
				case IDM_NSISHOME:
				{
					ShellExecute(g_sdata.hwnd,"open",NSIS_URL,NULL,NULL,SW_SHOWNORMAL);
					return TRUE;
				}
				case IDM_NSISDEV:
				{
					ShellExecute(g_sdata.hwnd,"open",NSIS_DEV,NULL,NULL,SW_SHOWNORMAL);
					return TRUE;
				}
				case IDM_SELECTALL:
				{
					SendDlgItemMessage(g_sdata.hwnd, IDC_LOGWIN, EM_SETSEL, 0, -1);
					return TRUE;
				}
				case IDM_DOCS:
				{
					ShowDocs();
					return TRUE;
				}
				case IDM_LOADSCRIPT:
				{
					if (!g_sdata.thread) {
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
							g_sdata.script = (char *)GlobalAlloc(GPTR,sizeof(buf)+7);
							wsprintf(g_sdata.script,"/CD \"%s\"",buf);
							ResetObjects();
							CompileNSISScript();
						}
					}
					return TRUE;
				}
				case IDM_CLEARLOG:
				{
					if (!g_sdata.thread) {
						ClearLog(g_sdata.hwnd);
						LogMessage(g_sdata.hwnd,USAGE);
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
					if (g_sdata.output_exe) {
						ShellExecute(g_sdata.hwnd,"open",g_sdata.output_exe,NULL,NULL,SW_SHOWNORMAL);
					}
					return TRUE;
				}
				case IDM_EDITSCRIPT:
				{
					if (g_sdata.input_script) {
						if ((int)ShellExecute(g_sdata.hwnd,"open",g_sdata.input_script,NULL,NULL,SW_SHOWNORMAL)<=32) {
							char path[MAX_PATH];
							if (GetWindowsDirectory(path,sizeof(path))) {
								lstrcat(path,"\\notepad.exe");
								ShellExecute(g_sdata.hwnd,"open",path,g_sdata.input_script,NULL,SW_SHOWNORMAL);
							}
						}
					}
					return TRUE;
				}
				case IDC_CLOSE:
				case IDM_EXIT:
				{
					if (!g_sdata.thread) {
						DestroyWindow(g_sdata.hwnd);
					}
					return TRUE;
				}
				case IDM_COPY:
				{
					CopyToClipboard(g_sdata.hwnd);
					return TRUE;
				}
				case IDM_COPYSELECTED:
				{
					SendDlgItemMessage(g_sdata.hwnd,IDC_LOGWIN, WM_COPY, 0, 0);
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
							int len=SendDlgItemMessage(g_sdata.hwnd,IDC_LOGWIN,WM_GETTEXTLENGTH,0,0);
							char *existing_text=(char*)GlobalAlloc(GPTR,len);
							existing_text[0]=0;
							GetDlgItemText(g_sdata.hwnd, IDC_LOGWIN, existing_text, len);
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
					if (!g_find.uFindReplaceMsg) g_find.uFindReplaceMsg = RegisterWindowMessage(FINDMSGSTRING);
					my_memset(&g_find.fr, 0, sizeof(FINDREPLACE));
					g_find.fr.lStructSize = sizeof(FINDREPLACE);
					g_find.fr.hwndOwner = hwndDlg;
					g_find.fr.Flags = FR_NOUPDOWN;
					g_find.fr.lpstrFindWhat = (char *)GlobalAlloc(GPTR, 128);
					if (!g_find.fr.lpstrFindWhat) return TRUE;
					g_find.fr.wFindWhatLen = 128;
					g_find.hwndFind = FindText(&g_find.fr);
					return TRUE;
				}
			}
		}
	}
	if (g_find.uFindReplaceMsg && msg == g_find.uFindReplaceMsg) {
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
		if (lpfr->Flags & FR_DIALOGTERM) g_find.hwndFind = 0;
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
		ErrorMessage(g_sdata.hwnd,"There was an error creating the pipe.");
		PostMessage(g_sdata.hwnd,WM_MAKENSIS_PROCESSCOMPLETE,0,0);
		return 1;
	}
	GetStartupInfo(&si);
	si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
	si.wShowWindow = SW_HIDE;
	si.hStdOutput = newstdout;
	si.hStdError = newstdout;
	if (!CreateProcess(NULL,g_sdata.script,NULL,NULL,TRUE,CREATE_NEW_CONSOLE,NULL,NULL,&si,&pi)) {
		char buf[MAX_STRING];
		wsprintf(buf,"Could not execute:\r\n %s.",g_sdata.script);
		ErrorMessage(g_sdata.hwnd,buf);
		CloseHandle(newstdout);
		CloseHandle(read_stdout);
		PostMessage(g_sdata.hwnd,WM_MAKENSIS_PROCESSCOMPLETE,0,0);
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
			LogMessage(g_sdata.hwnd, szBuf);
		}
		else Sleep(TIMEOUT);
		GetExitCodeProcess(pi.hProcess, &dwExit);
		// Make sure we have no data before killing getting out of the loop
		if (dwExit != STILL_ACTIVE) {
			PeekNamedPipe(read_stdout, 0, 0, 0, &dwRead, NULL);
		}
	}

	g_sdata.retcode = dwExit;
	CloseHandle(pi.hThread);
	CloseHandle(pi.hProcess);
	CloseHandle(newstdout);
	CloseHandle(read_stdout);
	PostMessage(g_sdata.hwnd,WM_MAKENSIS_PROCESSCOMPLETE,0,0);
	return 0;
}

BOOL CALLBACK DialogResize(HWND hWnd, LPARAM /* unused */) 
{
	RECT r;
	GetWindowRect(hWnd, &r);
	ScreenToClient(g_sdata.hwnd, (LPPOINT)&r);
	ScreenToClient(g_sdata.hwnd, ((LPPOINT)&r)+1);
	switch (GetDlgCtrlID(hWnd)) {
		case IDC_LOGWIN:
			SetWindowPos(hWnd, 0, r.left, r.top,r.right - r.left + g_resize.dx, r.bottom - r.top + g_resize.dy, SWP_NOZORDER|SWP_NOMOVE);
			break;
		case IDC_TEST:
		case IDC_CLOSE:
			SetWindowPos(hWnd, 0, r.left + g_resize.dx, r.top + g_resize.dy, 0, 0, SWP_NOZORDER|SWP_NOSIZE);
			break;
		default:
			SetWindowPos(hWnd, 0, r.left, r.top + g_resize.dy, r.right - r.left + g_resize.dx, r.bottom - r.top, SWP_NOZORDER);
			break;
	}
	RedrawWindow(hWnd,NULL,NULL,RDW_INVALIDATE);
	return TRUE;
}

BOOL CALLBACK AboutProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam) {
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
            SetDlgItemText(hwndDlg,IDC_NSISVER,g_sdata.branding);
			SetDlgItemText(hwndDlg,IDC_ABOUTVERSION,NSISW_VERSION);
			SetDlgItemText(hwndDlg,IDC_ABOUTCOPY,COPYRIGHT);
            SetDlgItemText(hwndDlg,IDC_OTHERCONTRIB,CONTRIB);
			break;
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
