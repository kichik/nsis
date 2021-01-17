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

  Unicode support by Jim Park -- 08/18/2007
*/
#define MAKENSISW_CPP

#include "makensisw.h"
#include <windowsx.h>
#include <shlwapi.h>
#include <stdio.h>
#include "resource.h"
#include "toolbar.h"
#include "update.h"

namespace MakensisAPI {
#ifdef _WIN64
  const TCHAR* SigintEventNameFmt = _T("makensis win32 sigint event %Iu");
#else
  const TCHAR* SigintEventNameFmt = _T("makensis win32 sigint event %u");
#endif
  const TCHAR* SigintEventNameLegacy = _T("makensis win32 signint event");
}

NSCRIPTDATA g_sdata;
NRESIZEDATA g_resize;
NFINDREPLACE g_find;
TCHAR g_findbuf[128];
extern NTOOLBAR g_toolbar;
void* g_ModalDlgData;
BYTE g_MiniCommand = false;

NSIS_ENTRYPOINT_SIMPLEGUI
int WINAPI _tWinMain(HINSTANCE hInst,HINSTANCE hOldInst,LPTSTR CmdLineParams,int ShowCmd) {

  // We can be associated with .nsi files and when launched from the shell we inherit the current directory so 
  // we need to prevent LoadLibrary from searching the current directory because it can contain untrusted DLLs!
  FARPROC SDDA = GetSysProcAddr("KERNEL32", "SetDllDirectoryA"); // WinXP.SP1+
  if (SDDA) ((BOOL(WINAPI*)(LPCSTR))SDDA)(""); // Remove the current directory from the default DLL search order

  // Try to register the SysLink class
  DWORD iccestruct[2] = { 8, 0x8000 }; // ICC_LINK_CLASS (ComCtl32v6)
  FARPROC icce = SupportsW95() ? GetSysProcAddr("COMCTL32", "InitCommonControlsEx") : (FARPROC) InitCommonControlsEx;
  BOOL succ = ((BOOL(WINAPI*)(const void*))icce)(iccestruct);
  if (!succ && (sizeof(void*) > 4 || LOBYTE(GetVersion()) >= 5)) // Must check the version because older shell32 versions have a incompatible function at the same ordinal
  {
    FARPROC lwrc = GetSysProcAddr("SHELL32", (LPCSTR) 258); // LinkWindow_RegisterClass
    if (lwrc) ((BOOL(WINAPI*)())lwrc)();
    WNDCLASS wc;
    if (GetClassInfo(NULL, _T("Link Window"), &wc))
    {
      wc.lpszClassName = _T("SysLink");
      RegisterClass(&wc); // Superclass the old link window class if SysLink is not available
    }
  }

  memset(&g_sdata,0,sizeof(NSCRIPTDATA));
  memset(&g_resize,0,sizeof(NRESIZEDATA));
  memset(&g_find,0,sizeof(NFINDREPLACE));
  g_sdata.hInstance = hInst;
  g_sdata.symbols = NULL;
  g_sdata.sigint_event_legacy = CreateEvent(NULL, FALSE, FALSE, MakensisAPI::SigintEventNameLegacy);
  g_sdata.verbosity = (unsigned char) ReadRegSettingDW(REGVERBOSITY, 4);
  if (g_sdata.verbosity > 4) g_sdata.verbosity = 4;
  RestoreSymbols();
  LoadSysLibrary("RichEd20");

  if (!InitBranding()) {
    MessageBox(0,NSISERROR,ERRBOXTITLE,MB_ICONEXCLAMATION|MB_OK|MB_TASKMODAL);
    return 1;
  }
  ResetObjects();
  HACCEL haccel = LoadAccelerators(g_sdata.hInstance, MAKEINTRESOURCE(IDK_ACCEL));
  HWND hDialog = CreateDialog(g_sdata.hInstance,MAKEINTRESOURCE(DLG_MAIN),0,DialogProc);
  if (!hDialog && !g_MiniCommand) {
    MessageBox(0,DLGERROR,ERRBOXTITLE,MB_ICONEXCLAMATION|MB_OK|MB_TASKMODAL);
    return 1;
  }
  MSG  msg;
  int status;
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
  MemSafeFree(g_sdata.script);
  if (g_sdata.script_cmd_args) GlobalFree(g_sdata.script_cmd_args);
  if (g_sdata.sigint_event) CloseHandle(g_sdata.sigint_event);
  if (g_sdata.sigint_event_legacy) CloseHandle(g_sdata.sigint_event_legacy);
  return (int) msg.wParam;
}

void SetScript(const TCHAR *script, bool clearArgs /*= true*/)
{
  MemSafeFree(g_sdata.script);

  if (clearArgs)
  {
    if (g_sdata.script_cmd_args)
    {
      GlobalFree(g_sdata.script_cmd_args);
    }

    // Pointing to a single char.  Maybe _T('\0')
    g_sdata.script_cmd_args = GlobalAlloc(GHND, sizeof(TCHAR));
  }

  g_sdata.script = (TCHAR*) MemAlloc((lstrlen(script) + 1)*sizeof(TCHAR));
  lstrcpy(g_sdata.script, script);
}

static void AddScriptCmdArgs(const TCHAR *arg)
{
  g_sdata.script_cmd_args = GlobalReAlloc(g_sdata.script_cmd_args,
    GlobalSize(g_sdata.script_cmd_args) + (lstrlen(arg) + 2/* quotes */ + 1 /* space */)*sizeof(TCHAR),
    0);

  TCHAR *args = (TCHAR *) GlobalLock(g_sdata.script_cmd_args);

  lstrcat(args, _T(" \""));
  lstrcat(args, arg);
  lstrcat(args, _T("\""));

  GlobalUnlock(g_sdata.script_cmd_args);
}

enum { CMD_PICKCOMP = 0x0001, CMD_SPY = 0x0080, CMD_LOOKUP = 0x8000 };
static UINT ProcessCommandLine()
{
  TCHAR **argv;
  int i, j, retflags = 0;
  int argc = SetArgv((TCHAR *)GetCommandLine(), &argv);
  if (argc > 1) {
    for (i = 1; i < argc; i++)
    {
      if (!lstrcmpi(argv[i], _T("/Spy"))) retflags |= CMD_SPY;
      else if (!lstrcmpi(argv[i], _T("/Lookup"))) retflags |= CMD_LOOKUP;
      else if (!StrCmpNI(argv[i], _T("/XSetCompressor "), COUNTOF("/XSetCompressor ") - !0))
      {
        TCHAR *p = argv[i] + lstrlen(_T("/XSetCompressor ")), cchSlashFinalSpace = COUNTOF("/FINAL ") - !0;
        if (!StrCmpNI(p,_T("/FINAL "), cchSlashFinalSpace)) p += cchSlashFinalSpace;
        while (*p == _T(' ')) p++;

        for (j = (int) COMPRESSOR_SCRIPT + 1; j < (int) COMPRESSOR_BEST; j++)
        {
          if (!lstrcmpi(p, compressor_names[j]))
          {
            SetCompressor((NCOMPRESSOR) j);
          }
        }
      }
      else if (!lstrcmpi(argv[i], _T("/ChooseCompressor")))
      {
        retflags |= CMD_PICKCOMP;
      }
      else if (argv[i][0] == _T('-') || argv[i][0] == _T('/'))
      {
        AddScriptCmdArgs(argv[i]);
      }
      else
      {
        SetScript(argv[i], false);
        PushMRUFile(g_sdata.script);
        break;
      }
    }
  }
  MemSafeFree(argv);
  return retflags;
}

DWORD CALLBACK SaveFileStreamCallback(DWORD_PTR dwCookie, LPBYTE pbBuff, LONG cb, LONG *pcb)
{
  HANDLE hFile = (HANDLE) ((DWORD_PTR*)dwCookie)[0];
  DWORD cbio;
#ifdef UNICODE
  if (!((DWORD_PTR*)dwCookie)[1])
  {
    if (!WriteUTF16LEBOM(hFile)) return -1;
    ((DWORD_PTR*)dwCookie)[1] = TRUE;
  }
#endif
  BOOL wop = WriteFile(hFile, pbBuff, cb, &cbio, 0);
  return (*pcb = (LONG) cbio, !wop);
}

static void ToolBarSizeChanged(HWND hDlg)
{
  RECT r;
  HWND hEd = GetDlgItem(hDlg, IDC_LOGWIN);
  GetWindowRect(g_toolbar.hwnd, &r);
  LONG tbh = RectH(r);
  GetWindowRect(hEd, &r);
  LONG oldh = RectH(r), margin = DlgUnitToPixelY(hDlg, 7), top = tbh + margin;
  POINT pt = { r.left, r.top };
  ScreenToClient(hDlg, &pt);
  SetWindowPos(hEd, 0, pt.x, top, RectW(r), oldh + (pt.y - top), SWP_NOZORDER|SWP_NOACTIVATE); // Update IDC_LOGWIN position and size
}

static BOOL CALLBACK DialogResize(HWND hWnd, LPARAM param)
{
  RECT r, r2, &dlgrect = *(RECT*) param;
  GetWindowRect(hWnd, &r);
  ScreenToClient(g_sdata.hwnd, ((LPPOINT)&r)+0), ScreenToClient(g_sdata.hwnd, ((LPPOINT)&r)+1);
  switch (GetDlgCtrlID(hWnd))
  {
  case IDC_TOOLBAR:
    GetWindowRect(hWnd, &r2);
    SetWindowPos(hWnd, 0, 0, 0, RectW(r) + g_resize.dx, RectH(r2), SWP_NOMOVE|SWP_NOZORDER|SWP_NOACTIVATE);
    break;
  case IDC_LOGWIN:
    if (!g_resize.bottompanelsize)
    {
      r2.top = 246, r2.bottom = 22 + 190; // Dialog units from the .rc file
      MapDialogRect(GetParent(hWnd), &r2);
      g_resize.bottompanelsize = r2.top - r2.bottom;
    }
    SetWindowPos(hWnd, 0, r.left, r.top, dlgrect.right - (r.left * 2), dlgrect.bottom - (r.top + g_resize.bottompanelsize), SWP_NOZORDER|SWP_NOMOVE|SWP_NOACTIVATE);
    break;
  case IDC_TEST:
  case IDCANCEL:
    SetWindowPos(hWnd, 0, r.left + g_resize.dx, r.top + g_resize.dy, 0, 0, SWP_NOZORDER|SWP_NOSIZE|SWP_NOACTIVATE);
    break;
  default:
    SetWindowPos(hWnd, 0, r.left, r.top + g_resize.dy, RectW(r) + g_resize.dx, RectH(r), SWP_NOZORDER|SWP_NOACTIVATE);
    break;
  }
  RedrawWindow(hWnd,NULL,NULL,RDW_INVALIDATE);
  return TRUE;
}

INT_PTR CALLBACK DialogProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam) {
  switch (msg) {
    case WM_INITDIALOG:
    {
      g_sdata.hwnd=hwndDlg;
      HICON hIcon = LoadIcon(g_sdata.hInstance,MAKEINTRESOURCE(IDI_ICON));
      SetClassLongPtr(hwndDlg,GCLP_HICON,(LONG_PTR)hIcon);
      // Altered by Darren Owen (DrO) on 29/9/2003
      // Added in receiving of mouse and key events from the richedit control
      SendDlgItemMessage(hwndDlg,IDC_LOGWIN,EM_SETEVENTMASK,(WPARAM)NULL,ENM_SELCHANGE|ENM_MOUSEEVENTS|ENM_KEYEVENTS);
      g_sdata.menu = GetMenu(g_sdata.hwnd);
      g_sdata.fileSubmenu = FindSubMenu(g_sdata.menu, IDM_FILE);
      g_sdata.editSubmenu = FindSubMenu(g_sdata.menu, IDM_EDIT);
      g_sdata.toolsSubmenu = FindSubMenu(g_sdata.menu, IDM_TOOLS);
      RestoreMRUList();
      CreateToolBar();
      InitTooltips(g_sdata.hwnd);
      SetDlgItemText(g_sdata.hwnd,IDC_VERSION,g_sdata.branding);
      LPCTSTR fontname = _T("Courier New"), fontconsolas = _T("Consolas");
      BYTE fontsize = 8, fontcharset = DEFAULT_CHARSET, suppwin4 = SupportsWNT4() || SupportsW9X();
      if (FontExists(fontconsolas))
      {
        fontname = fontconsolas, ++fontsize;
      }
      else if (SupportsW2000() && GetACP() == 932) // According to older Inno, Courier New cannot display Japanese on < WinXP
      {
        LPCWSTR msgothlocalutf = L"\xff2d\xff33 \xff30\x30b4\x30b7\x30c3\x30af";
        const CHAR msgothlocal932[] = { -126, 'l', -126, 'r', ' ', -125, 'S', -125, 'V', -125, 'b', -125, 'N', '\0' };
        fontcharset = SHIFTJIS_CHARSET, ++fontsize;
        fontname = _T("MS Gothic"); // Win2000 can handle this, downlevel cannot
        if (suppwin4 && !FontExists(fontname)) fontname = sizeof(TCHAR) > 1 ? (LPCTSTR) msgothlocalutf : (LPCTSTR) msgothlocal932;
      }
      HFONT hFont = CreateFontPt(hwndDlg,fontsize,FW_NORMAL,FIXED_PITCH|FF_DONTCARE,fontcharset,fontname);
      SendDlgItemMessage(hwndDlg,IDC_LOGWIN,WM_SETFONT,(WPARAM)hFont,0);
      g_sdata.compressor = COMPRESSOR_NONE_SELECTED;
      SetScript(_T(""));
      RestoreCompressor();
      ToolBarSizeChanged(hwndDlg);

      UINT docmd = ProcessCommandLine();
      if ((docmd & (CMD_SPY|CMD_LOOKUP)))
      {
        INT_PTR r = ((docmd & CMD_LOOKUP) ? ShowLookupDialog : ShowWndSpy)(0);
        g_sdata.hwnd = NULL; // Don't save window pos
        g_MiniCommand++;
        return SendMessage(hwndDlg, WM_CLOSE, r, r);
      }

      RestoreWindowPos(g_sdata.hwnd);

      if(g_sdata.compressor == COMPRESSOR_NONE_SELECTED) {
        SetCompressor(g_sdata.default_compressor);
      }

      if(docmd & CMD_PICKCOMP) {
        if (DialogBox(g_sdata.hInstance,MAKEINTRESOURCE(DLG_COMPRESSOR),g_sdata.hwnd,API_cast<DLGPROC>(CompressorProc))) {
          EnableItems(g_sdata.hwnd);
          return TRUE;
        }
      }

      CompileNSISScript();
      return TRUE;
    }
    case WM_PAINT:
    {
      PAINTSTRUCT ps;
      GetGripperPos(hwndDlg, g_resize.griprect);
      HDC hdc = BeginPaint(hwndDlg, &ps);
      DrawGripper(hwndDlg, hdc, g_resize.griprect);
      EndPaint(hwndDlg, &ps);
      return TRUE;
    }
    case WM_DESTROY:
    {
      SaveSymbols();
      SaveMRUList();
      SaveWindowPos(g_sdata.hwnd);
      ImageList_Destroy(g_toolbar.imagelist);
      ImageList_Destroy(g_toolbar.imagelistd);
      ImageList_Destroy(g_toolbar.imagelisth);
      DestroyTooltips();
      PostQuitMessage(0);
      return TRUE;
    }
    case WM_CLOSE: tryquitapp:
    {
      if (!g_sdata.thread) {
        DestroyWindow(hwndDlg);
        PostQuitMessage((int) wParam);
      }
      return TRUE;
    }
    case WM_DROPFILES: {
      int num;
      TCHAR szTmp[MAX_PATH];
      num = DragQueryFile((HDROP)wParam,(UINT)-1,NULL,0);
      if (num==1) {
        DragQueryFile((HDROP)wParam,0,szTmp,MAX_PATH);
        if (szTmp[0]) {
          SetScript(szTmp);
          PushMRUFile(g_sdata.script);
          ResetObjects();
          CompileNSISScript();
        }
      } else {
        MessageBox(hwndDlg,MULTIDROPERROR,ERRBOXTITLE,MB_OK|MB_ICONSTOP);
      }
      DragFinish((HDROP)wParam);
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
      if (wParam == SIZE_MAXHIDE || wParam == SIZE_MAXSHOW) return TRUE;
      const LONG oldW = g_resize.resizeRect.right, oldH = g_resize.resizeRect.bottom;
      GetClientRect(hwndDlg, &g_resize.resizeRect);
      g_resize.dx = g_resize.resizeRect.right - oldW;
      g_resize.dy = g_resize.resizeRect.bottom - oldH;
      EnumChildWindows(g_sdata.hwnd, DialogResize, (LPARAM) &g_resize.resizeRect);
      return TRUE;
    }
    case WM_SIZING:
    {
      InvalidateRect(hwndDlg, &g_resize.griprect, TRUE);
      GetGripperPos(hwndDlg, g_resize.griprect);
      return TRUE;
    }
    case WM_NCHITTEST:
    {
      RECT r = g_resize.griprect;
      MapWindowPoints(hwndDlg, 0, (POINT*)&r, 2);
      POINT pt = { GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam) };
      if (PtInRect(&r, pt))
      {
        SetWindowLongPtr(hwndDlg, DWLP_MSGRESULT, HTBOTTOMRIGHT);
        return TRUE;
      }
      return FALSE;
    }
    case WM_MAKENSIS_PROCESSCOMPLETE:
    {
      if (g_sdata.thread) {
        CloseHandle(g_sdata.thread);
        g_sdata.thread=0;
      }
      if(g_sdata.compressor == COMPRESSOR_BEST) {
        if (g_sdata.retcode==0 && FileExists(g_sdata.output_exe)) {
          TCHAR temp_file_name[1024]; // BUGBUG: Hardcoded buffer size
          wsprintf(temp_file_name,_T("%s_makensisw_temp"),g_sdata.output_exe);
          if(!lstrcmpi(g_sdata.compressor_name,compressor_names[(int)COMPRESSOR_SCRIPT+1])) {
            SetCompressorStats();
            CopyFile(g_sdata.output_exe,temp_file_name,false);
            g_sdata.best_compressor_name = g_sdata.compressor_name;
            g_sdata.compressor_name = compressor_names[(int)COMPRESSOR_SCRIPT+2];
            ResetObjects();

            CompileNSISScript();
            return TRUE;
          }
          else {
            int this_compressor=0, i;
            HANDLE hPrev, hThis;
            DWORD prevSize=0, thisSize=0;

            for(i=(int)COMPRESSOR_SCRIPT+2; i<(int)COMPRESSOR_BEST; i++) {
              if(!lstrcmpi(g_sdata.compressor_name,compressor_names[i])) {
                this_compressor = i;
                break;
              }
            }

            if(FileExists(temp_file_name)) {
              hPrev = CreateFile(temp_file_name,GENERIC_READ, FILE_SHARE_READ,
                                 NULL, OPEN_EXISTING, 0, NULL);
              if(hPrev != INVALID_HANDLE_VALUE) {
                prevSize = GetFileSize(hPrev, 0);
                CloseHandle(hPrev);

                if(prevSize != INVALID_FILE_SIZE) {
                  hThis = CreateFile(g_sdata.output_exe,GENERIC_READ, FILE_SHARE_READ,
                                     NULL, OPEN_EXISTING, 0, NULL);
                  if(hThis != INVALID_HANDLE_VALUE) {
                    thisSize = GetFileSize(hThis, 0);
                    CloseHandle(hThis);

                    if(thisSize != INVALID_FILE_SIZE) {
                      if(prevSize > thisSize) {
                        CopyFile(g_sdata.output_exe,temp_file_name,false);
                        SetCompressorStats();
                        g_sdata.best_compressor_name = g_sdata.compressor_name;
                      }
                    }
                  }
                }
              }
            }

            if(this_compressor == ((int)COMPRESSOR_BEST - 1)) {
              TCHAR buf[1024];

              g_sdata.compressor_name = compressor_names[(int)COMPRESSOR_SCRIPT+1];

              if(!lstrcmpi(g_sdata.best_compressor_name,compressor_names[this_compressor])) {
                wsprintf(buf,COMPRESSOR_MESSAGE,g_sdata.best_compressor_name,thisSize);
                LogMessage(g_sdata.hwnd,buf);
              }
              else {
                CopyFile(temp_file_name,g_sdata.output_exe,false);
                wsprintf(buf,RESTORED_COMPRESSOR_MESSAGE,g_sdata.best_compressor_name,prevSize);
                LogMessage(g_sdata.hwnd,buf);
                LogMessage(g_sdata.hwnd, g_sdata.compressor_stats);
              }
              DeleteFile(temp_file_name);
              g_sdata.compressor_stats[0] = _T('\0');
            }
            else {
              g_sdata.compressor_name = compressor_names[this_compressor+1];
              ResetObjects();

              CompileNSISScript();
              return TRUE;
            }
          }
        }
      }
      EnableItems(g_sdata.hwnd);
      if (!g_sdata.retcode) {
        if (g_sdata.warnings) {
          SetTitle(g_sdata.hwnd,_T("Finished with Warnings"));
          PlayAppSoundAsync(("BuildWarning"), MB_ICONWARNING);
          SetLogColor(LC_WARNING);
        }
        else {
          SetTitle(g_sdata.hwnd,_T("Finished Sucessfully"));
          PlayAppSoundAsync(("BuildComplete"), MB_ICONASTERISK);
          SetLogColor(LC_SUCCESS);
        }
        // Added by Darren Owen (DrO) on 1/10/2003
        if(g_sdata.recompile_test)
          PostMessage(g_sdata.hwnd, WM_COMMAND, LOWORD(IDC_TEST), 0);
      }
      else {
        SetTitle(g_sdata.hwnd,_T("Compile Error: See Log for Details"));
        PlayAppSoundAsync(("BuildError"), MB_ICONEXCLAMATION);
        SetLogColor(LC_ERROR);
      }

      // Added by Darren Owen (DrO) on 1/10/2003
      // ensures the recompile and run state is reset after use
      g_sdata.recompile_test = 0;
      DragAcceptFiles(g_sdata.hwnd,TRUE);
      return TRUE;
    }
    case MakensisAPI::QUERYHOST: {
      if (MakensisAPI::QH_OUTPUTCHARSET == wParam) {
        const UINT reqcp = 1200; // We want UTF-16LE
        return DlgRet(hwndDlg, (LONG_PTR)(1+reqcp));
      }
      else if (MakensisAPI::QH_SUPPORTEDVERSION == wParam)
        return DlgRet(hwndDlg, 0x03006000);
      return FALSE;
    }
    case WM_NOTIFY:
      switch (((NMHDR*)lParam)->code ) {
        case EN_SELCHANGE:
          EnableMenuItem(g_sdata.menu, IDM_COPYSELECTED, RicheditHasSelection(GetDlgItem(hwndDlg, IDC_LOGWIN)) ? MF_ENABLED : MF_GRAYED);
          break;
        
        // Altered by Darren Owen (DrO) on 6/10/2003
        // Allows the detection of the right-click menu when running on OSes below Windows 2000
        // and will then simulate the effective WM_CONTEXTMENU message that would be received
        // note: removed the WM_CONTEXTMENU handling to prevent a duplicate menu appearing on
        // Windows 2000 and higher
        case EN_MSGFILTER:
          #define lpnmMsg ((MSGFILTER*)lParam)
          if(WM_RBUTTONUP == lpnmMsg->msg || (WM_KEYUP == lpnmMsg->msg && lpnmMsg->wParam == VK_APPS))
          {
            POINT pt;
            HWND edit = GetDlgItem(g_sdata.hwnd,IDC_LOGWIN);
            RECT r;
            GetCursorPos(&pt);

            // Added and altered by Darren Owen (DrO) on 29/9/2003
            // Will place the right-click menu in the top left corner of the window
            // if the application key is pressed and the mouse is not in the window
            // from here...
            ScreenToClient(edit, &pt);
            GetClientRect(edit, &r);
            if (!PtInRect(&r, pt)) pt.x = pt.y = 0;
            MapWindowPoints(edit, HWND_DESKTOP, &pt, 1);
            TrackPopupMenu(g_sdata.editSubmenu, TPM_LEFTALIGN | TPM_LEFTBUTTON | TPM_RIGHTBUTTON, pt.x, pt.y, 0, g_sdata.hwnd, 0);
          }
          break;
        case TBN_DROPDOWN:
        {
          LPNMTOOLBAR pToolBar = (LPNMTOOLBAR) lParam;
          if (pToolBar->hdr.hwndFrom == g_toolbar.hwnd && pToolBar->iItem == IDM_COMPRESSOR) {
            ShowCompressorToolbarDropdownMenu(*pToolBar);
            return TBDDRET_DEFAULT;
          }
          return TBDDRET_NODEFAULT;
        }
      }
      return TRUE;
    case WM_COPYDATA:
    {
      using namespace MakensisAPI;
      COPYDATASTRUCT *cds = (COPYDATASTRUCT*) lParam, cdsret;
      switch (cds->dwData) {
        case MakensisAPI::NOTIFY_SCRIPT:
          MemSafeFree(g_sdata.input_script);
          if ((g_sdata.input_script = (TCHAR*) MemAlloc(cds->cbData * sizeof(TCHAR))))
            lstrcpy(g_sdata.input_script, (TCHAR*) cds->lpData);
          EnableUICommand(IDM_BROWSESCR, !!g_sdata.input_script);
          break;
        case MakensisAPI::NOTIFY_WARNING:
          g_sdata.warnings++;
          break;
        case MakensisAPI::NOTIFY_ERROR:
          break;
        case MakensisAPI::NOTIFY_OUTPUT:
          MemSafeFree(g_sdata.output_exe);
          g_sdata.output_exe = (TCHAR*) MemAlloc(cds->cbData * sizeof(TCHAR));
          lstrcpy(g_sdata.output_exe, (TCHAR *)cds->lpData);
          break;
        case MakensisAPI::PROMPT_FILEPATH:
          if ((((PROMPT_FILEPATH_DATA*)cds->lpData)->Platform & 7) == sizeof(TCHAR))
          {
            TCHAR buf[MAX_PATH];
            lstrcpyn(buf, FSPath::FindLastComponent(((PROMPT_FILEPATH_DATA*)cds->lpData)->Path), COUNTOF(buf));
            OPENFILENAME of = { sizeof(of) };
            of.hwndOwner = hwndDlg;
            of.lpstrFilter = _T("*.exe\0*.exe\0*\0*.*\0");
            of.lpstrFile = buf, of.nMaxFile = COUNTOF(buf);
            of.Flags = OFN_EXPLORER|OFN_ENABLESIZING|OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_PATHMUSTEXIST|OFN_NOCHANGEDIR;
            if (GetSaveFileName(&of))
            {
              cdsret.dwData = cds->dwData, cdsret.cbData = (lstrlen(buf) + 1) * sizeof(TCHAR), cdsret.lpData = buf;
              SendMessage((HWND) wParam, WM_COPYDATA, (SIZE_T) hwndDlg, (SIZE_T) &cdsret);
            }
            return TRUE;
          }
          return FALSE;
      }
      return TRUE;
    }
    case WM_INITMENU:
      EnableMenuItem(g_sdata.menu, IDM_CANCEL, g_sdata.thread ? MF_ENABLED : MF_GRAYED);
      break;
    case WM_MAKENSIS_UPDATEUISTATE:
    {
      UINT i, emptylog = SendDlgItemMessage(hwndDlg, IDC_LOGWIN, WM_GETTEXTLENGTH, 0, 0) == 0;
      static const PACKEDCMDID_T nonemptylogids [] = { PACKCMDID(IDM_COPY), PACKCMDID(IDM_COPYALL), PACKCMDID(IDM_CLEARLOG), PACKCMDID(IDM_SELECTALL) };
      for (i = 0; i < COUNTOF(nonemptylogids); ++i) EnableUICommand(UNPACKCMDID(nonemptylogids[i]), !emptylog);
      EnableUICommand(IDM_BROWSESCR, !!g_sdata.input_script);
      break;
    }
    case WM_COMMAND:
    {
      switch (LOWORD(wParam)) {
        case IDM_UI_SWITCHSECTION: //devblogs.microsoft.com/oldnewthing/20191022-00/?p=103016
          SetDialogFocus(hwndDlg, g_toolbar.hwnd); // Toolbar does not have WS_TABSTOP and we have no other "UI areas" to switch to so just go there
          break;
        case IDM_BROWSESCR: {
          if (g_sdata.input_script) {
            TCHAR str[MAX_PATH],*str2;
            lstrcpy(str,g_sdata.input_script);
            str2=_tcsrchr(str,_T('\\'));
            if(str2!=NULL) *(str2+1)=0;
            ShellExecute(g_sdata.hwnd,_T("open"),str,NULL,NULL,SW_SHOWNORMAL);
          }
          return TRUE;
        }
        case IDM_DOCS: return (ShowDocs(), TRUE);
        case IDM_NSISHOME: return OpenUrlInDefaultBrowser(g_sdata.hwnd, NSIS_URL);
        case IDM_FORUM: return OpenUrlInDefaultBrowser(g_sdata.hwnd, NSIS_FORUM_URL);
        case IDM_NSISUPDATE: return (CheckForUpdate(), TRUE);
        case IDM_ABOUT: return ShowAboutDialog(hwndDlg)|TRUE;
        case IDM_SELECTALL:
        {
          SendDlgItemMessage(g_sdata.hwnd, IDC_LOGWIN, EM_SETSEL, 0, -1);
          return TRUE;
        }
        case IDM_LOADSCRIPT:
        {
          if (!g_sdata.thread) {
            OPENFILENAME l={sizeof(l),};
            TCHAR buf[MAX_PATH];
            l.hwndOwner = hwndDlg;
            l.lpstrFilter = _T("NSIS Script (*.nsi)\0*.nsi\0All Files (*.*)\0*.*\0");
            l.lpstrFile = buf;
            l.nMaxFile = MAX_STRING-1;
            l.lpstrTitle = _T("Load Script");
            l.lpstrDefExt = _T("log");
            l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER|OFN_PATHMUSTEXIST|OFN_FILEMUSTEXIST;
            buf[0] = _T('\0');
            if (GetOpenFileName(&l)) {
              SetScript(buf);
              PushMRUFile(g_sdata.script);
              ResetObjects();
              CompileNSISScript();
            }
          }
          return TRUE;
        }
        case IDM_MRU_FILE:
        case IDM_MRU_FILE+1:
        case IDM_MRU_FILE+2:
        case IDM_MRU_FILE+3:
        case IDM_MRU_FILE+4:
          LoadMRUFile(LOWORD(wParam)-IDM_MRU_FILE);
          return TRUE;
        case IDM_CLEAR_MRU_LIST:
          ClearMRUList();
          return TRUE;
        case IDM_COMPRESSOR:
        {
          SetCompressor((NCOMPRESSOR)(g_sdata.compressor+1));
          return TRUE;
        }
        case IDM_CLEARLOG:
        {
          if (!g_sdata.thread) {
            ClearLog(g_sdata.hwnd);
          }
          return TRUE;
        }
        case IDM_RECOMPILE:
        {
          CompileNSISScript();
          return TRUE;
        }
        // Added by Darren Owen (DrO) on 1/10/2003
        case IDM_RECOMPILE_TEST:
        {
          g_sdata.recompile_test = 1;
          CompileNSISScript();
          return TRUE;
        }
        case IDM_SETTINGS:
        {
          DialogBox(g_sdata.hInstance,MAKEINTRESOURCE(DLG_SETTINGS),g_sdata.hwnd,API_cast<DLGPROC>(SettingsProc));
          return TRUE;
        }
        case IDM_WNDSPY: return ShowWndSpy(hwndDlg);
        case IDM_LOOKUP: return ShowLookupDialog(hwndDlg);
        case IDM_GUIDGEN:
        {
          GUID guid;
          TCHAR buf[41 * (1 + (sizeof(TCHAR) < 2))];
          FARPROC func = GetKeyState(VK_CONTROL) < 0 ? GetSysProcAddr("RPCRT4", "UuidCreateSequential") : NULL;
          ((HRESULT(WINAPI*)(GUID*))(func ? func : GetSysProcAddr("RPCRT4", "UuidCreate")))(&guid);
          ((int(WINAPI*)(GUID*, TCHAR*, int))(GetSysProcAddr("OLE32", "StringFromGUID2")))(&guid, buf, 39);
          for (UINT i = 0; sizeof(TCHAR) < 2; ++i) if (!(buf[i] = (CHAR) ((WCHAR*)buf)[i])) break; // WCHAR to TCHAR if ANSI
          LogMessage(g_sdata.hwnd, (buf[38] = '\r', buf[39] = '\n', buf[40] = '\0', buf));
          break;
        }
        case IDM_TEST:
        case IDC_TEST:
        {
          if (g_sdata.output_exe) {
            ShellExecute(g_sdata.hwnd,_T("open"),g_sdata.output_exe,NULL,NULL,SW_SHOWNORMAL);
          }
          return TRUE;
        }
        case IDM_EDITSCRIPT:
        {
          if (g_sdata.input_script) {
            LPCTSTR verb = _T("open"); // BUGBUG: Should not force the open verb?
            HINSTANCE hi = ShellExecute(g_sdata.hwnd,verb,g_sdata.input_script,NULL,NULL,SW_SHOWNORMAL);
            if ((UINT_PTR)hi <= 32) {
              TCHAR path[MAX_PATH];
              if (GetWindowsDirectory(path,sizeof(path))) {
                lstrcat(path,_T("\\notepad.exe"));
                ShellExecute(g_sdata.hwnd,verb,path,g_sdata.input_script,NULL,SW_SHOWNORMAL);
              }
            }
          }
          return TRUE;
        }
        case IDCANCEL:
        case IDM_EXIT:
          wParam = 0;
          goto tryquitapp;
        case IDM_CANCEL:
        {
          SetEvent(g_sdata.sigint_event);
          SetEvent(g_sdata.sigint_event_legacy);
          return TRUE;
        }
        case IDM_COPY:
          if (RicheditHasSelection(GetDlgItem(hwndDlg, IDC_LOGWIN))) goto logwndcopysel;
          // fall through
        case IDM_COPYALL:
          CopyToClipboard(g_sdata.hwnd);
          return TRUE;
        case IDM_COPYSELECTED: logwndcopysel:
          SendDlgItemMessage(g_sdata.hwnd, IDC_LOGWIN, WM_COPY, 0, 0);
          return TRUE;
        case IDM_SAVE:
        {
          OPENFILENAME l={sizeof(l),};
          TCHAR buf[MAX_STRING];
          l.hwndOwner = hwndDlg;
          l.lpstrFilter = _T("Log Files (*.log)\0*.log\0Text Files (*.txt)\0*.txt\0All Files (*.*)\0*.*\0");
          l.lpstrFile = buf;
          l.nMaxFile = MAX_STRING-1;
          l.lpstrTitle = _T("Save Output");
          l.lpstrDefExt = _T("log");
          l.lpstrInitialDir = NULL;
          l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER|OFN_PATHMUSTEXIST;
          lstrcpy(buf,_T("output"));
          if (GetSaveFileName(&l)) {
            HANDLE hFile = CreateFile(buf, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, 0, 0);
            if (INVALID_HANDLE_VALUE != hFile) { // BUGBUG:TODO: Error message for bad hFile or failed EM_STREAMOUT?
              WPARAM opts = sizeof(TCHAR) > 1 ? (SF_TEXT|SF_UNICODE) : (SF_TEXT);
              DWORD_PTR cookie[2] = { (DWORD_PTR)hFile, FALSE };
              EDITSTREAM es = { (DWORD_PTR)&cookie, 0, SaveFileStreamCallback };
              SendMessage(GetDlgItem(g_sdata.hwnd, IDC_LOGWIN), EM_STREAMOUT, opts, (LPARAM)&es);
              CloseHandle(hFile);
            }
          }
          return TRUE;
        }
        case IDM_FIND:
        {
          bool reuseWindow = true;
          if (!g_find.uFindReplaceMsg) g_find.uFindReplaceMsg = RegisterWindowMessage(FINDMSGSTRING);
          memset(&g_find.fr, 0, sizeof(FINDREPLACE));
          g_find.fr.lStructSize = sizeof(FINDREPLACE);
          g_find.fr.hwndOwner = hwndDlg;
          g_find.fr.Flags = FR_NOUPDOWN;
          g_find.fr.lpstrFindWhat = g_findbuf;
          g_find.fr.wFindWhatLen = COUNTOF(g_findbuf);
          if (!reuseWindow || !SetForegroundWindow(g_find.hwndFind))
            g_find.hwndFind = FindText(&g_find.fr);
          return TRUE;
        }
        default:
          {
            int i;
            DWORD command = LOWORD(wParam);
            for(i=(int)COMPRESSOR_SCRIPT; i<=(int)COMPRESSOR_BEST; i++) {
              if(command == compressor_commands[i]) {
                SetCompressor((NCOMPRESSOR)i);
                return TRUE;
              }
            }
          }
      }
    }
  }
  if (msg == g_find.uFindReplaceMsg && msg) {
    LPFINDREPLACE lpfr = (LPFINDREPLACE)lParam;
    if (lpfr->Flags & FR_FINDNEXT) {
      WPARAM flags = FR_DOWN;
      if (lpfr->Flags & FR_MATCHCASE) flags |= FR_MATCHCASE;
      if (lpfr->Flags & FR_WHOLEWORD) flags |= FR_WHOLEWORD;
      FINDTEXTEX ft;
      SendDlgItemMessage(hwndDlg, IDC_LOGWIN, EM_EXGETSEL, 0, (LPARAM)&ft.chrg);
      ft.chrg.cpMin = (ft.chrg.cpMax == ft.chrg.cpMin) ? 0 : ft.chrg.cpMax;
      ft.chrg.cpMax = (LONG) SendDlgItemMessage(hwndDlg, IDC_LOGWIN, WM_GETTEXTLENGTH, 0, 0);
      ft.lpstrText = lpfr->lpstrFindWhat;
      ft.chrg.cpMin = (LONG) SendDlgItemMessage(hwndDlg, IDC_LOGWIN, EM_FINDTEXTEX, flags, (LPARAM)&ft);
      if (ft.chrg.cpMin != -1)
        SendDlgItemMessage(hwndDlg, IDC_LOGWIN, EM_SETSEL, ft.chrgText.cpMin, ft.chrgText.cpMax);
      else
        MessageBeep(MB_ICONASTERISK);
    }
    if (lpfr->Flags & FR_DIALOGTERM) g_find.hwndFind = 0;
    return TRUE;
  }
  return 0;
}

DWORD WINAPI MakeNSISProc(LPVOID TreadParam) {
  TCHAR eventnamebuf[100];
  wsprintf(eventnamebuf, MakensisAPI::SigintEventNameFmt, g_sdata.hwnd);
  if (g_sdata.sigint_event) CloseHandle(g_sdata.sigint_event);
  g_sdata.sigint_event = CreateEvent(NULL, FALSE, FALSE, eventnamebuf);
  if (!g_sdata.sigint_event) {
    ErrorMessage(g_sdata.hwnd, _T("There was an error creating the abort event."));
    PostMessage(g_sdata.hwnd, WM_MAKENSIS_PROCESSCOMPLETE, 0, 0);
    return 1;
  }

  STARTUPINFO si;
  HANDLE newstdout,read_stdout;
  
  if (!InitSpawn(si, read_stdout, newstdout)) {
    ErrorMessage(g_sdata.hwnd, _T("There was an error creating the pipe."));
    PostMessage(g_sdata.hwnd, WM_MAKENSIS_PROCESSCOMPLETE, 0, 0);
    return 1;
  }
  PROCESS_INFORMATION pi;
  if (!CreateProcess(0, g_sdata.compile_command, 0, 0, TRUE, CREATE_NEW_CONSOLE, 0, 0, &si, &pi)) {
    TCHAR buf[MAX_STRING]; // BUGBUG: TODO: Too small?
    wsprintf(buf,_T("Could not execute:\r\n %s."), g_sdata.compile_command);
    ErrorMessage(g_sdata.hwnd, buf);
    FreeSpawn(0, read_stdout, newstdout);
    PostMessage(g_sdata.hwnd, WM_MAKENSIS_PROCESSCOMPLETE, 0, 0);
    return 1;
  }
  CloseHandle(newstdout); // Close this handle (duplicated in subprocess) now so we get ERROR_BROKEN_PIPE

  char iob[(1024 & ~1) + sizeof(WCHAR)];
  WCHAR *p = (WCHAR*) iob, wcl = 0;
  DWORD cbiob = sizeof(iob) - sizeof(WCHAR), cb = 0, cbofs = 0, cch, cbio;
  for(;;)
  {
    BOOL rok = ReadFile(read_stdout, iob+cbofs, cbiob-cbofs, &cbio, NULL);
    cb += cbio, cch = cb / sizeof(WCHAR);
    if (!cch)
    {
      if (!rok) break; // TODO: If cb is non-zero we should report a incomplete read error?
      cbofs += cbio; // We only have 1 byte, need to read more to get a complete WCHAR
      continue;
    }
    char oddbyte = (char)(cb % 2), incompsurr;
    cbofs = 0;
    if ((incompsurr = IS_HIGH_SURROGATE(p[cch-1])))
      wcl = p[--cch], cbofs = sizeof(WCHAR); // Store leading surrogate part and complete it later
    if (oddbyte)
      oddbyte = iob[cb-1], ++cbofs;
logappendfinal:
    p[cch] = L'\0';
    LogMessage(g_sdata.hwnd, p);
    p[0] = wcl, iob[cbofs - !!oddbyte] = oddbyte, cb = 0;
    if (!rok) // No more data can be read
    {
      if (cbofs) // Unable to complete the surrogate pair or odd byte
      {
        p[0] = 0xfffd, cch = 1, cbofs = 0; 
        goto logappendfinal;
      }
      break;
    }
  }
  FreeSpawn(&pi, read_stdout, 0);
  g_sdata.retcode = pi.dwProcessId;
  PostMessage(g_sdata.hwnd, WM_MAKENSIS_PROCESSCOMPLETE, 0, 0);
  return 0;
}

static INT_PTR CALLBACK AboutProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam) {
  ABOUTDLGDATA &dd = *(ABOUTDLGDATA*) g_ModalDlgData;
  switch(msg) {
    case WM_DRAWITEM:
      if (wParam == IDC_ABOUTHEADER)
      {
        DRAWITEMSTRUCT &dis = *(DRAWITEMSTRUCT*) lParam;
        RECT r, r2;
        const TCHAR txt[] = TEXT("MakeNSISW");
        INT dt = DT_NOCLIP|DT_NOPREFIX|DT_SINGLELINE|DT_VCENTER, cch = COUNTOF(txt) - 1, line = DpiScaleY(dis.hwndItem, 2), shadow = 1;
        GetClientRect(dis.hwndItem, &r);
        if (!dd.hHeaderFont)
          dd.hHeaderFont = CreateFont(0, CFF_RAWSIZE, r.bottom / 2, FW_BOLD, DEFAULT_PITCH|FF_DONTCARE, ANSI_CHARSET, _T("Trebuchet MS")); // IE4.01SP2+
        HGDIOBJ hOrgFont = SelectObject(dis.hDC, dd.hHeaderFont);
        DrawHorzGradient(dis.hDC, r.left, r.top, r.right, r.bottom - line, RGB(22, 77, 160), RGB(29, 100, 207));
        DrawHorzGradient(dis.hDC, r.left, r.bottom - line, r.right, r.bottom, RGB(255, 142, 42), RGB(190, 90, 2));
        SetBkMode(dis.hDC, TRANSPARENT);
        if (!dd.FinalHeaderPos)
        {
          r2 = r;
          DrawText(dis.hDC, txt, cch, &r2, dt|DT_CALCRECT);
          dd.FinalHeaderPos = r.right - (r2.right - r2.left);
          dd.FinalHeaderPos -= DlgUnitToPixelX(hwndDlg, 7); // Margin
        }
        r.left += dd.AnimPos;
        SetTextColor(dis.hDC, RGB(0, 0, 0)), OffsetRect(&r, +shadow, +shadow);
        DrawText(dis.hDC, txt, cch, &r, dt);
        SetTextColor(dis.hDC, RGB(255, 255, 255)), OffsetRect(&r, -shadow, -shadow);
        DrawText(dis.hDC, txt, cch, &r, dt);
        SelectObject(dis.hDC, hOrgFont);
        return TRUE;
      }
      break;
    case WM_TIMER:
      if (wParam == ABOUTDLGDATA::TID_HEADER)
      {
        INT finalpos = dd.FinalHeaderPos, dir;
        if (dd.AnimSpeed >= 4)
        {
          dd.AnimPos += (dir = (dd.AnimPos >= finalpos)) ? -(INT)dd.AnimSpeed : +(INT)dd.AnimSpeed;
          if (dd.AnimDir != dir) dd.AnimDir = dir, dd.AnimSpeed /= 2;
        }
        else
        {
          dd.AnimPos = finalpos;
          SetTimer(hwndDlg, ABOUTDLGDATA::TID_HEADER, INFINITE, NULL);
        }
        InvalidateRect(GetDlgItem(hwndDlg, IDC_ABOUTHEADER), NULL, false);
      }
      break;
    case WM_DESTROY:
      DeleteObject(dd.hHeaderFont);
      DeleteObject(dd.hFont);
      DeleteObject(dd.hBoldFont);
      break;
    case WM_INITDIALOG:
    {
      CenterOnParent(hwndDlg);
      HFONT fontnorm = CreateFontPt(hwndDlg, 8, FW_NORMAL, FIXED_PITCH|FF_DONTCARE, ANSI_CHARSET, _T("Tahoma")),
            fontbold = CreateFontPt(hwndDlg, 8, FW_BOLD, FIXED_PITCH|FF_DONTCARE, ANSI_CHARSET, _T("Tahoma"));
      if (!fontbold && (SupportsWNT4() || SupportsW95())) { // Tahoma shipped with 98+ and 2000+
        fontnorm = CreateFontPt(hwndDlg, 8, FW_NORMAL, FIXED_PITCH|FF_DONTCARE, ANSI_CHARSET, _T("MS Shell Dlg"));
        fontbold = CreateFontPt(hwndDlg, 8, FW_BOLD, FIXED_PITCH|FF_DONTCARE, ANSI_CHARSET, _T("MS Shell Dlg"));
      }
      dd.hFont = fontnorm, dd.hBoldFont = fontbold;
      SendDlgItemMessage(hwndDlg, IDC_ABOUTVERSION, WM_SETFONT, (WPARAM)fontbold, FALSE);
      static const BYTE fontnormctlids[] = { IDC_ABOUTCOPY, IDC_ABOUTPORTIONS, IDC_ABOUTDONATE, IDC_OTHERCONTRIB, IDC_NSISVER };
      for (UINT i = 0; i < COUNTOF(fontnormctlids); ++i) SendDlgItemMessage(hwndDlg, fontnormctlids[i], WM_SETFONT, (WPARAM)fontnorm, FALSE);
      SendMessage(hwndDlg, WM_APP, 0, 0); // Set IDC_ABOUTVERSION
      SetDlgItemText(hwndDlg, IDC_ABOUTCOPY, COPYRIGHT);
      SetDlgItemText(hwndDlg, IDC_OTHERCONTRIB, CONTRIB);
      SetDlgItemText(hwndDlg, IDC_ABOUTDONATE, DONATE);
      SetDlgItemText(hwndDlg, IDC_NSISVER, g_sdata.branding);
      SetTimer(hwndDlg, ABOUTDLGDATA::TID_HEADER, 50, NULL);
      break;
    }
    case WM_NOTIFY:
      switch (((NMHDR*)lParam)->code)
      {
        case NM_CLICK:
        // fall through
        case NM_RETURN:
          if (((NMHDR*)lParam)->idFrom == IDC_ABOUTDONATE)
          {
            static const BYTE x = 128, encurl[] = DONATEURL;
            char url[COUNTOF(encurl)];
            for (UINT i = 0;; ++i) if (!(url[i] = (char) (encurl[i] & ~x))) break; // "Decrypt" URL
            OpenUrlInDefaultBrowser(hwndDlg, url);
          }
          break;
      }
      break;
    case WM_COMMAND:
      if (wParam == MAKELONG(IDC_ABOUTVERSION, STN_DBLCLK)) goto showversion;
      if (IDOK != LOWORD(wParam)) break;
      // fall through
    case WM_CLOSE:
      return EndDialog(hwndDlg, TRUE);
    case WM_APP: showversion:
      {
        TCHAR buf[200], showver = wParam != 0;
        wsprintf(buf, _T("MakeNSISW %s%s(NSIS Compiler Interface)"), showver ? NSISW_VERSION : _T(""), showver ? _T(" ") : _T(""));
        SetDlgItemText(hwndDlg, IDC_ABOUTVERSION, buf);
      }
      break;
  }
  return FALSE;
}

INT_PTR ShowAboutDialog(HWND hwndOwner)
{
  ABOUTDLGDATA dd;
  g_ModalDlgData = &dd;
  dd.hHeaderFont = NULL, dd.FinalHeaderPos = 0;
  dd.AnimSpeed = 55, dd.AnimPos = 0, dd.AnimDir = 0;
  return DialogBox(g_sdata.hInstance, MAKEINTRESOURCE(DLG_ABOUT), hwndOwner, API_cast<DLGPROC>(AboutProc));
}

static void EnableSymbolSetButtons(HWND hwndDlg)
{
  LRESULT n = SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETCOUNT, 0, 0);
  EnableWindow(GetDlgItem(hwndDlg, IDC_CLEAR), n > 0);
  EnableWindow(GetDlgItem(hwndDlg, IDC_SAVE), n > 0);
}

static void EnableSymbolEditButtons(HWND hwndDlg)
{
  LRESULT n = SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETSELCOUNT, 0, 0);
  EnableWindow(GetDlgItem(hwndDlg, IDC_LEFT), n == 1);
  EnableWindow(GetDlgItem(hwndDlg, IDC_DEL), n != 0);
}

static void SetSymbols(HWND hwndDlg, TCHAR **symbols)
{
    SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_RESETCONTENT , 0, 0);
    if (symbols) {
      for (SIZE_T i = 0; symbols[i]; ++i)
        SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_ADDSTRING, 0, (LPARAM)symbols[i]);
    }
    EnableSymbolSetButtons(hwndDlg);
    EnableWindow(GetDlgItem(hwndDlg, IDC_RIGHT), FALSE);
    EnableWindow(GetDlgItem(hwndDlg, IDC_LEFT), FALSE);
    EnableWindow(GetDlgItem(hwndDlg, IDC_DEL), FALSE);
}

static TCHAR **GetSymbols(HWND hwndDlg)
{
  LRESULT n = SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETCOUNT, 0, 0);
  TCHAR **symbols = NULL;
  if(n > 0) {
    symbols = (TCHAR **) GlobalAlloc(GPTR, (n+1)*sizeof(TCHAR *));
    for (LRESULT i = 0; i < n; i++) {
      LRESULT len = SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETTEXTLEN, (WPARAM)i, 0);
      symbols[i] = (TCHAR*) MemAllocZI((len+1)*sizeof(TCHAR));
      if (!symbols[i]) {
        FreeSymbolSet(symbols);
        return NULL;
      }
      SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETTEXT, (WPARAM)i, (LPARAM)symbols[i]);
    }
    symbols[n] = NULL;
  }

  return symbols;
}

INT_PTR CALLBACK SettingsProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam) {
  switch(msg) {
    case WM_INITDIALOG:
    {
      CenterOnParent(hwndDlg);
      for(int i = (int)COMPRESSOR_SCRIPT; i <= (int)COMPRESSOR_BEST; i++)
        SendDlgItemMessage(hwndDlg, IDC_COMPRESSOR, CB_ADDSTRING, 0, (LPARAM)compressor_display_names[i]);
      SendDlgItemMessage(hwndDlg, IDC_COMPRESSOR, CB_SETCURSEL, (WPARAM)g_sdata.default_compressor, (LPARAM)0);

      SetSymbols(hwndDlg, g_sdata.symbols);
      SetFocus(GetDlgItem(hwndDlg, IDC_SYMBOL));
      break;
    }
    case WM_MAKENSIS_LOADSYMBOLSET:
    {
      TCHAR *name = (TCHAR *)wParam;
      TCHAR **symbols = LoadSymbolSet(name);
      if(symbols) {
        SetSymbols(hwndDlg, symbols);
        GlobalFree((HGLOBAL) symbols);
      }
      break;
    }
    case WM_MAKENSIS_SAVESYMBOLSET:
    {
      TCHAR *name = (TCHAR *)wParam;
      TCHAR **symbols = GetSymbols(hwndDlg);
      if(symbols) {
        SaveSymbolSet(name, symbols);
        GlobalFree((HGLOBAL) symbols);
      }
      break;
    }
    case WM_COMMAND:
    {
      switch (LOWORD(wParam)) {
        case IDOK:
        {
          ResetObjects();
          ResetSymbols();
          g_sdata.symbols = GetSymbols(hwndDlg);

          INT_PTR n = SendDlgItemMessage(hwndDlg, IDC_COMPRESSOR, CB_GETCURSEL, (WPARAM)0, (LPARAM)0);
          if (n >= (INT_PTR)COMPRESSOR_SCRIPT && n <= (INT_PTR)COMPRESSOR_BEST)
            g_sdata.default_compressor = (NCOMPRESSOR)n;
          else
            g_sdata.default_compressor = COMPRESSOR_SCRIPT;
          SaveCompressor();
          SetCompressor(g_sdata.default_compressor);
          EndDialog(hwndDlg, TRUE);
        }
        break;
        case IDCANCEL:
          EndDialog(hwndDlg, TRUE);
          break;
        case IDC_RIGHT:
        {
          LRESULT n = SendDlgItemMessage(hwndDlg, IDC_SYMBOL, WM_GETTEXTLENGTH, 0, 0);
          if(n > 0) {
            TCHAR *buf = (TCHAR*) MemAllocZI((n+1)*sizeof(TCHAR));
            SendDlgItemMessage(hwndDlg, IDC_SYMBOL, WM_GETTEXT, n+1, (LPARAM)buf);
            if(_tcsstr(buf,_T(" ")) || _tcsstr(buf,_T("\t"))) {
              MessageBox(hwndDlg,SYMBOLSERROR,ERRBOXTITLE,MB_OK|MB_ICONSTOP);
              MemFree(buf);
              break;
            }

            n = SendDlgItemMessage(hwndDlg, IDC_VALUE, WM_GETTEXTLENGTH, 0, 0);
            if(n > 0) {
              TCHAR *buf2 = (TCHAR*) MemAllocZI((n+1)*sizeof(TCHAR));
              SendDlgItemMessage(hwndDlg, IDC_VALUE, WM_GETTEXT, n+1, (LPARAM)buf2);
              TCHAR *buf3 = (TCHAR*) MemAllocZI((lstrlen(buf)+lstrlen(buf2)+2)*sizeof(TCHAR));
              wsprintf(buf3,_T("%s=%s"),buf,buf2);
              MemFree(buf);
              buf = buf3;
              MemFree(buf2);
            }
            INT_PTR idx = SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_ADDSTRING, 0, (LPARAM)buf);
            if (idx >= 0)
            {
              SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_SETSEL, FALSE, -1);
              SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_SETSEL, TRUE, idx);
            }
            EnableSymbolEditButtons(hwndDlg);
            SendDlgItemMessage(hwndDlg, IDC_SYMBOL, WM_SETTEXT, 0, 0);
            SendDlgItemMessage(hwndDlg, IDC_VALUE, WM_SETTEXT, 0, 0);
            MemFree(buf);
            EnableSymbolSetButtons(hwndDlg);
          }
        }
        break;
        case IDC_LEFT:
        {
          if (SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETSELCOUNT, 0, 0) != 1)
            break;

          int index;
          INT_PTR num = SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETSELITEMS, 1, (LPARAM)&index);
          if(num == 1) {
            INT_PTR n = SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETTEXTLEN, (WPARAM)index, 0);
            if(n > 0) {
              TCHAR *buf = (TCHAR*) MemAllocZI((n+1)*sizeof(TCHAR));
              SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETTEXT, (WPARAM)index, (LPARAM)buf);
              TCHAR *p = _tcsstr(buf,_T("="));
              if(p) {
                SendDlgItemMessage(hwndDlg, IDC_VALUE, WM_SETTEXT, 0, (LPARAM)(p+1));
                *p=0;
              }
              SendDlgItemMessage(hwndDlg, IDC_SYMBOL, WM_SETTEXT, 0, (LPARAM)buf);
              MemFree(buf);
              SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_DELETESTRING, (WPARAM)index, 0);
              EnableWindow(GetDlgItem(hwndDlg, IDC_LEFT), FALSE);
              EnableWindow(GetDlgItem(hwndDlg, IDC_DEL), FALSE);
              EnableSymbolSetButtons(hwndDlg);
            }
          }
        }
        break;
        case IDC_CLEAR:
        {
          SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_RESETCONTENT , 0, 0);
          EnableSymbolSetButtons(hwndDlg);
        }
        break;
        case IDC_LOAD:
        case IDC_SAVE:
          ShowSymbolSetDialog(hwndDlg, IDC_LOAD == LOWORD(wParam));
        break;
        case IDC_DEL:
        {
          INT_PTR n = SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETSELCOUNT, 0, 0);
          int *items = (int*) MemAllocZI(n*sizeof(int));
          if (items) {
            SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_GETSELITEMS, (WPARAM)n, (LPARAM)items);
            for(INT_PTR i=n-1;i>=0;i--)
              SendDlgItemMessage(hwndDlg, IDC_SYMBOLS, LB_DELETESTRING, (WPARAM)items[i], 0);
            MemFree(items);
          }
          EnableSymbolEditButtons(hwndDlg);
          EnableSymbolSetButtons(hwndDlg);
        }
        break;
        case IDC_SYMBOL:
          if(HIWORD(wParam) == EN_CHANGE)
          {
            LRESULT n = SendDlgItemMessage(hwndDlg, IDC_SYMBOL, WM_GETTEXTLENGTH, 0, 0);
            EnableWindow(GetDlgItem(hwndDlg, IDC_RIGHT), n > 0);
          }
          break;
        case IDC_SYMBOLS:
          if (HIWORD(wParam) == LBN_SELCHANGE)
          {
            EnableSymbolEditButtons(hwndDlg);
          }
          else if (HIWORD(wParam) == LBN_DBLCLK)
          {
            SendDlgItemMessage(hwndDlg, IDC_LEFT, BM_CLICK, 0, 0);
          }
          break;
        }
      break;
    }
  }
  return FALSE;
}

INT_PTR CALLBACK CompressorProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam) {
  switch(msg) {
    case WM_INITDIALOG:
    {
      CenterOnParent(hwndDlg);
      for(int i=(int)COMPRESSOR_SCRIPT; i <= (int)COMPRESSOR_BEST; i++) {
        SendDlgItemMessage(hwndDlg, IDC_COMPRESSOR, CB_ADDSTRING, 0, (LPARAM)compressor_display_names[i]);
      }
      SendDlgItemMessage(hwndDlg, IDC_COMPRESSOR, CB_SETCURSEL, (WPARAM)g_sdata.compressor, (LPARAM)0);

      SetFocus(GetDlgItem(hwndDlg, IDC_COMPRESSOR));
      break;
    }
    case WM_COMMAND:
    {
      switch (LOWORD(wParam)) {
        case IDOK:
        {
          INT_PTR n = SendDlgItemMessage(hwndDlg, IDC_COMPRESSOR, CB_GETCURSEL, (WPARAM)0, (LPARAM)0);
          if(n >= (INT_PTR)COMPRESSOR_SCRIPT && n <= (INT_PTR)COMPRESSOR_BEST)
            SetCompressor((NCOMPRESSOR)n);
          else
            SetCompressor(g_sdata.default_compressor);
          EndDialog(hwndDlg, 0);
          break;
        }
        case IDCANCEL:
        {
          EndDialog(hwndDlg, 1);
          LogMessage(g_sdata.hwnd,USAGE);
          break;
        }
      }
      break;
    }
  }
  return FALSE;
}

static void FixSimpleComboBoxSizeBug(HWND hCombo) // Fix Win10 CBS_SIMPLE height drawing bug when DPI != 96
{
  RECT r;
  GetWindowRect(hCombo, &r);
  SetWindowPos(hCombo, 0, 0, 0, RectW(r), RectH(r)-1, SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOMOVE);
  SetWindowPos(hCombo, 0, 0, 0, RectW(r), RectH(r), SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOMOVE|SWP_FRAMECHANGED);
}

#define SymbolSetDlgDpiChanged(hwndDlg) ( FixSimpleComboBoxSizeBug(GetDlgItem((hwndDlg), IDC_NAMES)) )

static INT_PTR CALLBACK SymbolSetProc(HWND hwndDlg, UINT msg, WPARAM wParam, LPARAM lParam) {
  SYMSETDLGDATA &dd = *(SYMSETDLGDATA*) g_ModalDlgData;
  switch(msg) {
    case WM_INITDIALOG:
    {
      HKEY hKey;
      CenterOnParent(hwndDlg);
      EnableWindow(GetDlgItem(hwndDlg, IDC_DEL), FALSE);
      if (OpenRegSettingsKey(hKey)) {
        HKEY hSubKey;

        if (RegOpenKeyEx(hKey, REGSYMSUBKEY, 0, KEY_READ, &hSubKey) == ERROR_SUCCESS) {
          TCHAR subkey[1024];
          int i=0;

          while (RegEnumKey(hSubKey, i, subkey, sizeof(subkey)) == ERROR_SUCCESS) {
            SendDlgItemMessage(hwndDlg, IDC_NAMES, CB_ADDSTRING, 0, (LPARAM)subkey);
            i++;
          }
          RegCloseKey(hSubKey);
        }
        RegCloseKey(hKey);
      }
      HWND hwndEdit = GetComboEdit(GetDlgItem(hwndDlg, IDC_NAMES));
      SendMessage(hwndEdit, EM_LIMITTEXT, (WPARAM)SYMSETNAME_MAXLEN, 0);
      if (dd.LoadingMode) {
        SetWindowText(hwndDlg, LOAD_SYMBOL_SET_DLG_NAME);
        SetWindowText(GetDlgItem(hwndDlg, IDOK), LOAD_BUTTON_TEXT);
        SendMessage(hwndEdit, EM_SETREADONLY, (WPARAM)TRUE, 0);
      }
      else {
        SetWindowText(hwndDlg, SAVE_SYMBOL_SET_DLG_NAME);
        SetWindowText(GetDlgItem(hwndDlg, IDOK), SAVE_BUTTON_TEXT);
      }
      SymbolSetDlgDpiChanged(hwndDlg);
      break;
    }
    case WM_COMMAND:
    {
      switch (LOWORD(wParam)) {
        case IDOK:
        {
          TCHAR name[SYMSETNAME_MAXLEN+1];
          HWND hwndEdit = GetComboEdit(GetDlgItem(hwndDlg, IDC_NAMES));
          SendMessage(hwndEdit, WM_GETTEXT, (WPARAM)COUNTOF(name), (LPARAM)name);
          if (!*name) {
            LPCTSTR msg = dd.LoadingMode ? LOAD_SYMBOL_SET_MESSAGE : SAVE_SYMBOL_SET_MESSAGE;
            LPCTSTR tit = dd.LoadingMode ? LOAD_SYMBOL_SET_DLG_NAME : SAVE_SYMBOL_SET_DLG_NAME;
            MessageBox(hwndDlg, msg, tit, MB_OK|MB_ICONEXCLAMATION);
          }
          else {
            HWND hwndParent = GetParent(hwndDlg);
            UINT msg = dd.LoadingMode ? WM_MAKENSIS_LOADSYMBOLSET : WM_MAKENSIS_SAVESYMBOLSET;
            SendMessage(hwndParent, msg, (WPARAM)name, (LPARAM)NULL);
            EndDialog(hwndDlg, TRUE);
          }
          break;
        }
        case IDCANCEL:
        {
          EndDialog(hwndDlg, TRUE);
          break;
        }
        case IDC_DEL:
        {
          LONG_PTR n = SendDlgItemMessage(hwndDlg, IDC_NAMES, CB_GETCURSEL, 0, 0);
          if(n != CB_ERR) {
            INT_PTR len = SendDlgItemMessage(hwndDlg, IDC_NAMES, CB_GETLBTEXTLEN, (WPARAM)n, 0);
            TCHAR *buf = (TCHAR*) MemAllocZI((len+1)*sizeof(TCHAR));
            if(SendDlgItemMessage(hwndDlg, IDC_NAMES, CB_GETLBTEXT, (WPARAM)n, (LPARAM)buf) != CB_ERR) {
              SendDlgItemMessage(hwndDlg, IDC_NAMES, CB_DELETESTRING, n, 0);
              DeleteSymbolSet(buf);
            }
            MemFree(buf);
          }
          EnableWindow(GetDlgItem(hwndDlg, IDC_DEL), FALSE);
          break;
        }
        case IDC_NAMES:
        {
          if(HIWORD(wParam) == CBN_SELCHANGE)
          {
            LONG_PTR n = SendDlgItemMessage(hwndDlg, IDC_NAMES, CB_GETCURSEL, 0, 0);
            EnableWindow(GetDlgItem(hwndDlg, IDC_DEL), CB_ERR != n);
          }
          else if(HIWORD(wParam) == CBN_DBLCLK)
          {
            LONG_PTR n = SendDlgItemMessage(hwndDlg, IDC_NAMES, CB_GETCURSEL, 0, 0);
            if (n != CB_ERR) SendDlgItemMessage(hwndDlg, IDOK, BM_CLICK, 0, 0);
          }
          break;
        }
      }
      break;
    }
  }
  return FALSE;
}

INT_PTR ShowSymbolSetDialog(HWND hwndOwner, BOOL LoadingSet)
{
  SYMSETDLGDATA dd = { g_ModalDlgData, LoadingSet };
  g_ModalDlgData = &dd;
  INT_PTR retval = DialogBox(g_sdata.hInstance, MAKEINTRESOURCE(DLG_SYMBOLSET), hwndOwner, API_cast<DLGPROC>(SymbolSetProc));
  g_ModalDlgData = dd.pOldMDD; // Restore the old pointer
  return retval;
}

void SetCompressor(NCOMPRESSOR compressor)
{
  int i;

  if(g_sdata.compressor != compressor) {
    WORD command;
    LPCTSTR compressor_name;

    if(compressor > COMPRESSOR_SCRIPT && compressor < COMPRESSOR_BEST) {
      command = compressor_commands[(int)compressor];
      compressor_name = compressor_names[(int)compressor];
    }
    else if(compressor == COMPRESSOR_BEST) {
      command = compressor_commands[(int)compressor];
      compressor_name = compressor_names[(int)COMPRESSOR_SCRIPT+1];
    }
    else {
      compressor = COMPRESSOR_SCRIPT;
      command = IDM_COMPRESSOR_SCRIPT;
      compressor_name = _T("");
    }
    g_sdata.compressor = compressor;
    g_sdata.compressor_name = compressor_name;
    UpdateToolBarCompressorButton();
    for(i=(int)COMPRESSOR_SCRIPT; i<= (int)COMPRESSOR_BEST; i++) {
      CheckMenuItem(g_sdata.menu, compressor_commands[i], MF_BYCOMMAND | MF_UNCHECKED);
    }
    CheckMenuItem(g_sdata.menu, command, MF_BYCOMMAND | MF_CHECKED);
    ResetObjects();
  }
}
