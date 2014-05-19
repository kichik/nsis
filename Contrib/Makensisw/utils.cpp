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

  Unicode support by Jim Park -- 08/20/2007

*/

#include "makensisw.h"
#include "resource.h"
#include "toolbar.h"
#include <shlwapi.h>

typedef BYTE PACKEDCMDID_T;
#define PACKCMDID(id) ( PACKEDCMDID_T((id) - IDM_CMDBASE) )
#define UNPACKCMDID(id) ( IDM_CMDBASE + (id) )

NTOOLTIP g_tip;
LRESULT CALLBACK TipHookProc(int nCode, WPARAM wParam, LPARAM lParam);

TCHAR g_mru_list[MRU_LIST_SIZE][MAX_PATH] = { _T(""), _T(""), _T(""), _T(""), _T("") };

extern NSCRIPTDATA g_sdata;
extern const TCHAR *compressor_names[];

void MemSafeFree(void*mem) { if (mem) GlobalFree(mem); }
void*MemAllocZI(SIZE_T cb) { return GlobalAlloc(GPTR, cb); }

int SetArgv(const TCHAR *cmdLine, TCHAR ***argv) {
  const TCHAR *p;
  TCHAR *arg, *argSpace;
  int size, argSpaceSize, inquote, copy, slashes;

  size = 2;
  for (p = cmdLine; *p != _T('\0'); p++) {
    if ((*p == _T(' ')) || (*p == _T('\t'))) {
      size++;
      while ((*p == _T(' ')) || (*p == _T('\t'))) {
        p++;
      }
      if (*p == _T('\0')) {
        break;
      }
    }
  }

  argSpaceSize = (size+1) * sizeof(TCHAR *) + (lstrlen(cmdLine) + 1) * sizeof(TCHAR);
  argSpace = (TCHAR *) MemAlloc(argSpaceSize);
  *argv = (TCHAR **) argSpace;
  if (!argSpace)
    return 0;

  argSpace = (TCHAR *) ((*argv)+size);
  size--;

  p = cmdLine;
  int argc;
  for (argc = 0; argc < size; argc++) {
    (*argv)[argc] = arg = argSpace;
    while ((*p == _T(' ')) || (*p == _T('\t'))) {
      p++;
    }
    if (*p == _T('\0')) {
      break;
    }

    inquote = 0;
    slashes = 0;
    while (1) {
      copy = 1;
      while (*p == _T('\\')) {
        slashes++;
        p++;
      }
      if (*p == _T('"')) {
        if ((slashes & 1) == 0) {
          copy = 0;
          if ((inquote) && (p[1] == _T('"'))) {
            p++;
            copy = 1;
          }
          else {
            inquote = !inquote;
          }
        }
        slashes >>= 1;
      }

      while (slashes) {
        *arg = _T('\\');
        arg++;
        slashes--;
      }

      if ((*p == _T('\0')) || (!inquote && ((*p == _T(' ')) || (*p == _T('\t'))))) {
        break;
      }
      if (copy != 0) {
        *arg = *p;
        arg++;
      }
      p++;
    }
    *arg = _T('\0');
    argSpace = arg + 1;
  }
  (*argv)[argc] = NULL;

  return argc;
}

void SetTitle(HWND hwnd,const TCHAR *substr) {
  TCHAR title[64];
  if (substr==NULL) wsprintf(title,_T("MakeNSISW"));
  else wsprintf(title,_T("MakeNSISW - %s"),substr); 
  SetWindowText(hwnd,title);
}

void CopyToClipboard(HWND hwnd) {
  if (!hwnd||!OpenClipboard(hwnd)) return;
  int len=SendDlgItemMessage(hwnd,IDC_LOGWIN,WM_GETTEXTLENGTH,0,0);
  HGLOBAL mem = GlobalAlloc(GMEM_MOVEABLE,(len+1)*sizeof(TCHAR));
  if (!mem) { CloseClipboard(); return; }
  TCHAR *existing_text = (TCHAR *)GlobalLock(mem);
  if (!existing_text) { CloseClipboard(); return; }
  EmptyClipboard();
  existing_text[0]=0;
  GetDlgItemText(hwnd, IDC_LOGWIN, existing_text, len+1);
  GlobalUnlock(mem);
#ifdef _UNICODE
  SetClipboardData(CF_UNICODETEXT,mem);
#else
  SetClipboardData(CF_TEXT,mem);
#endif
  CloseClipboard();
}

void ClearLog(HWND hwnd) {
  SetDlgItemText(hwnd, IDC_LOGWIN, _T(""));
}

void LogMessage(HWND hwnd,const TCHAR *str) {
  SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_SETSEL, g_sdata.logLength, g_sdata.logLength);
  g_sdata.logLength += lstrlen(str);
  SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_REPLACESEL, 0, (LPARAM)str);
  SendDlgItemMessage(hwnd, IDC_LOGWIN, EM_SCROLLCARET, 0, 0);
}

void ErrorMessage(HWND hwnd,const TCHAR *str) {
  if (!str) return;
  TCHAR buf[1028];
  wsprintf(buf,_T("[Error] %s\r\n"),str);
  LogMessage(hwnd,buf);
}

void SetDialogFocus(HWND hDlg, HWND hCtl)
{
  //blogs.msdn.com/b/oldnewthing/archive/2004/08/02/205624.aspx
  SendMessage(hDlg, WM_NEXTDLGCTL, (WPARAM)hCtl, TRUE);
}

void Items(HWND hwnd, int on) 
{
  const HWND hCloseBtn = GetDlgItem(hwnd, IDCANCEL);
  const HWND hTestBtn = GetDlgItem(hwnd, IDC_TEST);
  const HMENU hMenu = g_sdata.menu;
  const UINT mf = (!on ? MF_GRAYED : MF_ENABLED);
  const UINT nmf = (!on ? MF_ENABLED : MF_GRAYED);
  const bool compsuccess = !g_sdata.retcode && on;

  if(!on) g_sdata.focused_hwnd = GetFocus();

  if(compsuccess || !on) {
    EnableWindow(hTestBtn, on);
    EnableToolBarButton(IDM_TEST, on);
    EnableMenuItem(hMenu, IDM_TEST, mf);
  }
  EnableMenuItem(hMenu, IDM_CANCEL, nmf);
  EnableWindow(hCloseBtn, on);

  static const PACKEDCMDID_T cmds [] = {
    PACKCMDID(IDM_EXIT), PACKCMDID(IDM_LOADSCRIPT), PACKCMDID(IDM_EDITSCRIPT), 
    PACKCMDID(IDM_COPY), PACKCMDID(IDM_COPYSELECTED), PACKCMDID(IDM_SAVE), 
    PACKCMDID(IDM_CLEARLOG), PACKCMDID(IDM_BROWSESCR), 
    PACKCMDID(IDM_COMPRESSOR), PACKCMDID(IDM_COMPRESSOR_SUBMENU),
    PACKCMDID(IDM_RECOMPILE), PACKCMDID(IDM_RECOMPILE_TEST)
  };
  for (UINT i = 0; i < COUNTOF(cmds); ++i) {
    UINT id = UNPACKCMDID(cmds[i]);
    EnableMenuItem(hMenu, id, mf);
    if (IDM_COPYSELECTED != id && IDM_COMPRESSOR_SUBMENU != id)
      EnableToolBarButton(id, on);
  }

  HWND hFocus = g_sdata.focused_hwnd, hOptimal = hTestBtn;
  if (on && hCloseBtn == hFocus) hFocus = hOptimal;
  if (!IsWindowEnabled(hFocus)) hFocus = GetDlgItem(hwnd, IDC_LOGWIN);
  SetDialogFocus(hwnd, hOptimal);
  SetDialogFocus(hwnd, hFocus);
}

void SetCompressorStats()
{
  DWORD line_count, i;
  TCHAR buf[1024];
  bool found = false;

  line_count = SendDlgItemMessage(g_sdata.hwnd, IDC_LOGWIN, EM_GETLINECOUNT, 0, 0);
  for(i=0; i<line_count; i++) {
    *((LPWORD)buf) = sizeof(buf); 
    SendDlgItemMessage(g_sdata.hwnd, IDC_LOGWIN, EM_GETLINE, (WPARAM)i, (LPARAM)buf);
    if(found) {
      DWORD len = lstrlen(TOTAL_SIZE_COMPRESSOR_STAT);
      lstrcat(g_sdata.compressor_stats,buf);

      if(!StrCmpN(buf,TOTAL_SIZE_COMPRESSOR_STAT,len)) {
        break;
      }
    }
    else {
      DWORD len = lstrlen(EXE_HEADER_COMPRESSOR_STAT);
      if(!StrCmpN(buf,EXE_HEADER_COMPRESSOR_STAT,len)) {
        found = true;
        lstrcpy(g_sdata.compressor_stats,_T("\n\n"));
        lstrcat(g_sdata.compressor_stats,buf);
      }
    }
  }
}

void CompileNSISScript() {
  DragAcceptFiles(g_sdata.hwnd,FALSE);
  ClearLog(g_sdata.hwnd);
  SetTitle(g_sdata.hwnd,NULL);
  if (lstrlen(g_sdata.script)==0) {
    LogMessage(g_sdata.hwnd,USAGE);

    static const PACKEDCMDID_T cmds [] = {
      PACKCMDID(IDM_RECOMPILE),PACKCMDID(IDM_RECOMPILE_TEST),PACKCMDID(IDM_TEST), 
      PACKCMDID(IDM_BROWSESCR),PACKCMDID(IDM_EDITSCRIPT)
    };
    for (UINT i = 0; i < COUNTOF(cmds); ++i) {
      int id = UNPACKCMDID(cmds[i]);
      EnableMenuItem(g_sdata.menu,id,MF_GRAYED);
      EnableToolBarButton(id,FALSE);
    }
    EnableWindow(GetDlgItem(g_sdata.hwnd,IDC_TEST),FALSE);
    DragAcceptFiles(g_sdata.hwnd,TRUE);
    return;
  }
  if (!g_sdata.compile_command) {
    TCHAR *symbols = BuildSymbols();
    TCHAR compressor[40];

    compressor[0] = _T('\0');
    if(*g_sdata.compressor_name)
      wsprintf(compressor,_T("/X\"SetCompressor /FINAL %s\""),g_sdata.compressor_name);

    TCHAR *args = (TCHAR *) GlobalLock(g_sdata.script_cmd_args);

    size_t byteSize = sizeof(TCHAR)*(
      /* makensis.exe        */ lstrlen(EXENAME)        + /* space */ 1 +
      /* script path         */ lstrlen(g_sdata.script) + /* space */ 1 +
      /* script cmd args     */ lstrlen(args)           + /* space */ 1 +
      /* defines /Dblah=...  */ lstrlen(symbols)        + /* space */ 1 +
      /* /XSetCompressor...  */ lstrlen(compressor)     + /* space */ 1 +
      /* /V + UINT8          */ 2 + 3                   + /* space */ 1 +
      /* /NOTIFYHWND + HWND  */ COUNTOF(_T("/NOTIFYHWND -4294967295")) + /* space */ 1
      +6); /* for -- \"\" and NULL */
      
    g_sdata.compile_command = (TCHAR*) MemAlloc(byteSize);

    wsprintf(
      g_sdata.compile_command,
      _T("%s /V%u %s %s /NOTIFYHWND %d %s -- \"%s\""),
      EXENAME,
      g_sdata.verbosity,
      compressor,
      symbols,
      g_sdata.hwnd,
      args,
      g_sdata.script
    );

    GlobalUnlock(g_sdata.script_cmd_args);
    MemFree(symbols);
  }
  MemSafeFree(g_sdata.input_script);
  MemSafeFree(g_sdata.output_exe);
  g_sdata.input_script = 0;
  g_sdata.output_exe = 0;
  g_sdata.warnings = 0;
  g_sdata.logLength = 0;
  // Disable buttons during compile
  DisableItems(g_sdata.hwnd);
  DWORD tid;
  g_sdata.thread=CreateThread(NULL,0,MakeNSISProc,0,0,&tid);
}

static bool InternalOpenRegSettingsKey(HKEY root, HKEY &key, bool create) {
  if (create) {
    if (RegCreateKey(root, REGKEY, &key) == ERROR_SUCCESS)
      return true;
  } else {
    if (RegOpenKeyEx(root, REGKEY, 0, KEY_READ, &key) == ERROR_SUCCESS)
      return true;
  }
  return false;
}

bool OpenRegSettingsKey(HKEY &hKey, bool create) {
  if (InternalOpenRegSettingsKey(REGSEC, hKey, create))
    return true;
  if (InternalOpenRegSettingsKey(REGSECDEF, hKey, create))
    return true;
  return false;
}

DWORD ReadRegSettingDW(LPCTSTR name, const DWORD defval) {
  DWORD val = defval, siz = sizeof(val), typ;
  HKEY hKey;
  if (OpenRegSettingsKey(hKey)) {
    if (RegQueryValueEx(hKey,name,NULL,&typ,(LPBYTE)&val,&siz) || REG_DWORD != typ || sizeof(val) != siz)
      val = defval;
    RegCloseKey(hKey);
  }
  return val;
}

void RestoreWindowPos(HWND hwnd) {
  HKEY hKey;
  WINDOWPLACEMENT p;
  if (OpenRegSettingsKey(hKey)) {
    DWORD l = sizeof(p);
    DWORD t;
    if ((RegQueryValueEx(hKey,REGLOC,NULL,&t,(LPBYTE)&p,&l)==ERROR_SUCCESS)&&(t == REG_BINARY)&&(l==sizeof(p))) {
      int width, height;
      int windowWidth, windowHeight;

      width = GetSystemMetrics(SM_CXFULLSCREEN);
      height = GetSystemMetrics(SM_CYFULLSCREEN);
      height += GetSystemMetrics(SM_CYCAPTION);
      windowWidth = p.rcNormalPosition.right-p.rcNormalPosition.left;
      if(windowWidth > width) {
        p.rcNormalPosition.left = 0;
        p.rcNormalPosition.right = width;
      }
      else if(p.rcNormalPosition.right > width) {
        p.rcNormalPosition.left = width - windowWidth;
        p.rcNormalPosition.right = width;
      }
      else if(p.rcNormalPosition.left < 0) {
        p.rcNormalPosition.left = 0;
        p.rcNormalPosition.right = windowWidth;
      }

      windowHeight = p.rcNormalPosition.bottom-p.rcNormalPosition.top;
      if(windowHeight > height) {
        p.rcNormalPosition.top = 0;
        p.rcNormalPosition.bottom = height;
      }
      else if(p.rcNormalPosition.bottom > height) {
        p.rcNormalPosition.top = height - windowHeight;
        p.rcNormalPosition.bottom = height;
      }
      else if(p.rcNormalPosition.top < 0) {
        p.rcNormalPosition.top = 0;
        p.rcNormalPosition.bottom = windowHeight;
      }

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
  if (OpenRegSettingsKey(hKey, true)) {
    RegSetValueEx(hKey,REGLOC,0,REG_BINARY,(LPBYTE)&p,sizeof(p));
    RegCloseKey(hKey);
  }
}

void RestoreSymbols()
{
  g_sdata.symbols = LoadSymbolSet(NULL);
}

void SaveSymbols()
{
  SaveSymbolSet(NULL, g_sdata.symbols);
}

void DeleteSymbolSet(TCHAR *name)
{
  if(name) {
    HKEY hKey;
    if (OpenRegSettingsKey(hKey)) {
      TCHAR subkey[1024];
      wsprintf(subkey,_T("%s\\%s"),REGSYMSUBKEY,name);
      RegDeleteKey(hKey,subkey);
      RegCloseKey(hKey);
    }
  }
}

TCHAR** LoadSymbolSet(TCHAR *name)
{
  HKEY hKey;
  HKEY hSubKey;
  TCHAR **symbols = NULL;
  if (OpenRegSettingsKey(hKey)) {
    TCHAR subkey[1024];
    if(name) {
      wsprintf(subkey,_T("%s\\%s"),REGSYMSUBKEY,name);
    }
    else {
      lstrcpy(subkey,REGSYMSUBKEY);
    }
    if (RegCreateKey(hKey,subkey,&hSubKey) == ERROR_SUCCESS) {
      TCHAR buf[8];
      DWORD l;
      DWORD t;
      DWORD bufSize;
      DWORD i = 0;
      HGLOBAL hMem = NULL;

      while(TRUE) {
        l = 0;
        bufSize = sizeof(buf);
        if ((RegEnumValue(hSubKey,i, buf, &bufSize,NULL,&t,NULL,&l)==ERROR_SUCCESS)&&(t == REG_SZ)) {
          if(symbols) {
            GlobalUnlock(hMem);
            hMem = GlobalReAlloc(hMem, (i+2)*sizeof(TCHAR *), GMEM_MOVEABLE|GMEM_ZEROINIT);
            symbols = (TCHAR **)GlobalLock(hMem);
          }
          else {
            hMem = GlobalAlloc(GMEM_MOVEABLE|GMEM_ZEROINIT, (i+2)*sizeof(TCHAR *));
            symbols = (TCHAR **)GlobalLock(hMem);
          }
          if(symbols) {
            l++;
            DWORD bytes = sizeof(TCHAR) * l;
            symbols[i] = (TCHAR*) MemAllocZI(bytes);
            if (symbols[i]) {
              RegQueryValueEx(hSubKey,buf,NULL,&t,(unsigned char*)symbols[i],&bytes);
            }
            else {
              break;
            }
          }
          else {
            break;
          }
          i++;
          symbols[i] = NULL;
        }
        else {
          break;
        }
      }
      RegCloseKey(hSubKey);
    }
    RegCloseKey(hKey);
  }

  return symbols;
}

void SaveSymbolSet(TCHAR *name, TCHAR **symbols)
{
  HKEY hKey;
  HKEY hSubKey;
  int n = 0;
  if (OpenRegSettingsKey(hKey, true)) {
    TCHAR subkey[1024];
    if(name) {
      wsprintf(subkey,_T("%s\\%s"),REGSYMSUBKEY,name);
    }
    else {
      lstrcpy(subkey,REGSYMSUBKEY);
    }

    if (RegOpenKey(hKey,subkey,&hSubKey) == ERROR_SUCCESS) {
      TCHAR buf[8];
      DWORD l;
      while(TRUE) {
        l = sizeof(buf);
        if (RegEnumValue(hSubKey,0, buf, &l,NULL,NULL,NULL,NULL)==ERROR_SUCCESS) {
          RegDeleteValue(hSubKey,buf);
        }
        else {
          break;
        }
      }
      RegCloseKey(hSubKey);
    }
    if(symbols) {
      if (RegCreateKey(hKey,subkey,&hSubKey) == ERROR_SUCCESS) {
        TCHAR buf[8];
        n = 0;
        while(symbols[n]) {
          wsprintf(buf,_T("%d"),n);
          RegSetValueEx(hSubKey,buf,0,REG_SZ,(CONST BYTE *)symbols[n],(lstrlen(symbols[n])+1)*sizeof(TCHAR));
          n++;
        }
        RegCloseKey(hSubKey);
      }
    }
    RegCloseKey(hKey);
  }
}

void ResetObjects() {
  MemSafeFree(g_sdata.compile_command);
  g_sdata.compile_command = NULL;
  g_sdata.warnings = FALSE;
  g_sdata.retcode = -1;
  g_sdata.thread = NULL;
}

void ResetSymbols() {
  if(g_sdata.symbols) {
    HGLOBAL hMem;
    int i = 0;
    while(g_sdata.symbols[i]) {
      MemFree(g_sdata.symbols[i]);
      i++;
    }
    hMem = GlobalHandle(g_sdata.symbols);
    GlobalUnlock(hMem);
    GlobalFree(hMem);
    g_sdata.symbols = NULL;
  }
}

void FreeSpawn(PROCESS_INFORMATION *pPI, HANDLE hRd, HANDLE hWr) {
  if (pPI) {
    GetExitCodeProcess(pPI->hProcess, &pPI->dwProcessId);
    CloseHandle(pPI->hProcess);
    CloseHandle(pPI->hThread);
  }
  CloseHandle(hRd);
  CloseHandle(hWr);
}
BOOL InitSpawn(STARTUPINFO &si, HANDLE &hRd, HANDLE &hWr) {
  OSVERSIONINFO osv = {sizeof(osv)};
  GetVersionEx(&osv);
  const bool winnt = VER_PLATFORM_WIN32_NT == osv.dwPlatformId;

  memset(&si, 0, sizeof(STARTUPINFO));
  si.cb = sizeof(STARTUPINFO);
  GetStartupInfo(&si);
  si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_HIDE;

  SECURITY_ATTRIBUTES sa={sizeof(sa)};
  SECURITY_DESCRIPTOR sd;
  if (winnt) {
    InitializeSecurityDescriptor(&sd, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(&sd, true, NULL, false);
    sa.lpSecurityDescriptor = &sd;
  }
  sa.bInheritHandle = true;
  BOOL okp = CreatePipe(&hRd, &hWr, &sa, 0);
  si.hStdOutput = hWr, si.hStdError = hWr;
  si.hStdInput = INVALID_HANDLE_VALUE;
  return okp;
}

int InitBranding() {
  const TCHAR *opt = _T(" /version");
  UINT cch = lstrlen(EXENAME) + lstrlen(opt) + 1;
  TCHAR *cmd = (TCHAR*) MemAlloc(cch*sizeof(TCHAR));
  if (!cmd) return 0;
  lstrcpy(cmd, EXENAME);
  lstrcat(cmd, opt);
  STARTUPINFO si;
  HANDLE newstdout, read_stdout;
  char szBuf[1024], retval = 0;
  if (InitSpawn(si, read_stdout, newstdout)) {
    PROCESS_INFORMATION pi, *ppi = 0;
    if (CreateProcess(0, cmd, 0, 0, TRUE, CREATE_NEW_CONSOLE, 0, 0, &si, &pi)) {
      DWORD dwRead = 0;
      if (WAIT_OBJECT_0 == WaitForSingleObject(pi.hProcess, 10000)) {
        ReadFile(read_stdout, szBuf, sizeof(szBuf)-1, &dwRead, NULL);
        retval = 1;
      }
      szBuf[dwRead] = 0, ppi = &pi;
      int len = lstrlenA(szBuf);
      if (!len) retval = 0;
      g_sdata.branding = (TCHAR*) MemAlloc((len+6)*sizeof(TCHAR)); // LEAKED
      wsprintf(g_sdata.branding, _T("NSIS %hs"), szBuf);
      g_sdata.brandingv = (char*) MemAlloc(len+1); // LEAKED
      lstrcpyA(g_sdata.brandingv, szBuf);
    }
    FreeSpawn(ppi, read_stdout, newstdout);
  }
  MemFree(cmd);
  return retval;
}

void InitTooltips(HWND h) {
  if (h == NULL)  return;
  memset(&g_tip,0,sizeof(NTOOLTIP));
  g_tip.tip_p = h;
  INITCOMMONCONTROLSEX icx;
  icx.dwSize = sizeof(icx);
  icx.dwICC  = ICC_BAR_CLASSES;
  InitCommonControlsEx(&icx);
  DWORD dwStyle = WS_POPUP | WS_BORDER | TTS_ALWAYSTIP;
  DWORD dwExStyle = WS_EX_TOOLWINDOW | WS_EX_TOPMOST;
  g_tip.tip = CreateWindowEx(dwExStyle,TOOLTIPS_CLASS,NULL,dwStyle,0,0,0,0,h,NULL,GetModuleHandle(NULL),NULL);
  if (!g_tip.tip) return;
  g_tip.hook = SetWindowsHookEx(WH_GETMESSAGE,TipHookProc,NULL, GetCurrentThreadId());
  AddTip(GetDlgItem(h,IDCANCEL),_T("Close MakeNSISW"));
  AddTip(GetDlgItem(h,IDC_TEST),_T("Test the installer generated by MakeNSISW"));
  AddToolBarTooltips();
}

void DestroyTooltips() {
  UnhookWindowsHookEx(g_tip.hook);
}

void AddTip(HWND hWnd,LPCTSTR lpszToolTip) {
  TOOLINFO ti;
  ti.cbSize = sizeof(TOOLINFO);
  ti.uFlags = TTF_IDISHWND;
  ti.hwnd   = g_tip.tip_p;
  ti.uId = (UINT) hWnd;
  ti.lpszText = (LPTSTR) lpszToolTip;
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
  TCHAR pathf[MAX_PATH],*path;
  GetModuleFileName(NULL,pathf,sizeof(pathf));
  path=_tcsrchr(pathf,_T('\\'));
  if(path!=NULL) *path=0;
  lstrcat(pathf,LOCALDOCS);
  if ((int)ShellExecute(g_sdata.hwnd,_T("open"),pathf,NULL,NULL,SW_SHOWNORMAL)<=32) 
  ShellExecuteA(g_sdata.hwnd,"open",DOCPATH,NULL,NULL,SW_SHOWNORMAL);
}

TCHAR* BuildSymbols()
{
  TCHAR *buf = NULL;

  if(g_sdata.symbols) {
    int i=0;
    while(g_sdata.symbols[i]) {
      if(buf) {
        TCHAR *buf3 = (TCHAR*) MemAlloc((lstrlen(buf)+lstrlen(g_sdata.symbols[i])+6)*sizeof(TCHAR));
        wsprintf(buf3,_T("%s \"/D%s\""),buf,g_sdata.symbols[i]);
        MemFree(buf);
        buf = buf3;
      }
      else {
        buf = (TCHAR*) MemAlloc((lstrlen(g_sdata.symbols[i])+5)*sizeof(TCHAR));
        wsprintf(buf,_T("\"/D%s\""),g_sdata.symbols[i]);
      }
      i++;
    }
  }
  else {
    buf = (TCHAR*) MemAlloc(sizeof(TCHAR));
    buf[0] = _T('\0');
  }

  return buf;
}

static inline bool IsValidFile(const TCHAR *fname)
{
  return FileExists(fname);
}

BOOL PopMRUFile(TCHAR* fname)
{
  int i;

  for(i=0; i<MRU_LIST_SIZE; i++) {
    if(!lstrcmpi(g_mru_list[i], fname)) {
      break;
    }
  }

  if(i < MRU_LIST_SIZE) {
    int j;
    for(j = i; j < MRU_LIST_SIZE-1; j++) {
      lstrcpy(g_mru_list[j],g_mru_list[j+1]);
    }
    g_mru_list[MRU_LIST_SIZE-1][0]=_T('\0');
    return TRUE;
  }
  else {
    return FALSE;
  }
}

void PushMRUFile(TCHAR* fname)
{
  int i;
  DWORD   rv;
  TCHAR full_file_name[MAX_PATH+1];

  if(!fname || fname[0] == _T('\0') || fname[0] == _T('/') || fname[0] == _T('-')) {
    return;
  }

  memset(full_file_name,0,sizeof(full_file_name));
  rv = GetFullPathName(fname,COUNTOF(full_file_name),full_file_name,NULL);
  if (rv == 0) {
    return;
  }

  if(IsValidFile(full_file_name)) {
    PopMRUFile(full_file_name);
    for(i = MRU_LIST_SIZE - 2; i >= 0; i--) {
      lstrcpy(g_mru_list[i+1], g_mru_list[i]);
    }
    lstrcpy(g_mru_list[0],full_file_name);
    BuildMRUMenus();
  }
}

void BuildMRUMenus()
{
  HMENU hMenu = g_sdata.fileSubmenu;
  int i, n;
  MENUITEMINFO mii;
  TCHAR buf[MRU_DISPLAY_LENGTH + 5/*number*/ + 1/*null*/];
  TCHAR buf2[MRU_DISPLAY_LENGTH - 6];
  TCHAR buf3[MRU_DISPLAY_LENGTH + 1];
  mii.cbSize = sizeof(mii);

  for(i = 0; i < MRU_LIST_SIZE; i++) {
    DeleteMenu(hMenu, IDM_MRU_FILE+i, MF_BYCOMMAND);
  }

  n = GetMenuItemCount(hMenu);

  // Remove MRU separator
  int seppos = n - 1;
  mii.fMask = MIIM_TYPE;
  if (GetMenuItemInfo(hMenu, seppos, TRUE, &mii)) {
    if (MFT_SEPARATOR & mii.fType) {
      DeleteMenu(hMenu, seppos, MF_BYPOSITION);
      n--;
    }
  }
  
  for(i = 0; i < MRU_LIST_SIZE; i++) {
    if(g_mru_list[i][0]) {
      if (seppos) {
        // We have MRU items so add the separator
        mii.fMask = MIIM_TYPE;
        mii.fType = MFT_SEPARATOR;
        InsertMenuItem(hMenu, n++, TRUE, &mii);
        seppos = 0;
      }
      memset(buf,0,sizeof(buf));
      mii.fMask = MIIM_ID | MIIM_TYPE | MIIM_STATE;
      mii.wID = IDM_MRU_FILE+i;
      mii.fType = MFT_STRING;
      wsprintf(buf, _T("&%d "), i + 1);
      if(lstrlen(g_mru_list[i]) > MRU_DISPLAY_LENGTH) {
        TCHAR *p = _tcsrchr(g_mru_list[i],_T('\\'));
        if(p) {
          p++;
          if(lstrlen(p) > MRU_DISPLAY_LENGTH - 7) {
            *buf2 = 0;
            lstrcpyn(buf2,p,MRU_DISPLAY_LENGTH - 9);
            lstrcat(buf2,_T("..."));

            lstrcpyn(buf3,g_mru_list[i],4);
            lstrcat(buf,buf3);
            lstrcat(buf,_T("...\\"));
            lstrcat(buf,buf2);
          }
          else {
            lstrcpyn(buf3,g_mru_list[i],(MRU_DISPLAY_LENGTH - lstrlen(p) - 3));
            lstrcat(buf,buf3);
            lstrcat(buf,_T("...\\"));
            lstrcat(buf,p);
          }
        }
        else {
          lstrcpyn(buf3,g_mru_list[i],(MRU_DISPLAY_LENGTH-2));
          lstrcat(buf,buf3);
          lstrcat(buf,_T("..."));
        }
      }
      else {
        lstrcat(buf, g_mru_list[i]);
      }

      mii.dwTypeData = buf;
      mii.cch = lstrlen(buf)+1;
      mii.fState = MFS_ENABLED;
      InsertMenuItem(hMenu, n++, TRUE, &mii);

    }
    else {
      break;
    }
  }

  hMenu = g_sdata.toolsSubmenu;
  mii.fMask = MIIM_STATE;
  mii.fState = g_mru_list[0][0] ? MFS_ENABLED : MFS_GRAYED;

  SetMenuItemInfo(hMenu, IDM_CLEAR_MRU_LIST,FALSE,&mii);
}

void LoadMRUFile(int position)
{
  if (!g_sdata.thread && position >=0 && position < MRU_LIST_SIZE && g_mru_list[position][0]) {
    SetScript(g_mru_list[position]);
    if(IsValidFile(g_mru_list[position])) {
      PushMRUFile(g_mru_list[position]);
    }
    else {
      PopMRUFile(g_mru_list[position]);
      BuildMRUMenus();
    }
    ResetObjects();
    CompileNSISScript();
  }
}

void RestoreMRUList()
{
  HKEY hKey;
  HKEY hSubKey;
  int n = 0;
  int i;
  if (OpenRegSettingsKey(hKey)) {
    if (RegCreateKey(hKey,REGMRUSUBKEY,&hSubKey) == ERROR_SUCCESS) {
      TCHAR buf[8];
      DWORD l, ec;
      for(int i=0; i<MRU_LIST_SIZE; i++) {
        wsprintf(buf,_T("%d"),i);
        l = sizeof(g_mru_list[n]);
        ec = RegQueryValueEx(hSubKey,buf,NULL,NULL,(LPBYTE)g_mru_list[n],&l);
        if(!ec && g_mru_list[n][0] != _T('\0')) {
          n++;
        }
      }
      RegCloseKey(hSubKey);
    }
    RegCloseKey(hKey);
  }
  for(i = n; i < MRU_LIST_SIZE; i++) {
    g_mru_list[i][0] = _T('\0');
  }

  BuildMRUMenus();
}

void SaveMRUList()
{
  HKEY hKey;
  HKEY hSubKey;
  int i = 0;
  if (OpenRegSettingsKey(hKey, true)) {
    if (RegCreateKey(hKey,REGMRUSUBKEY,&hSubKey) == ERROR_SUCCESS) {
      TCHAR buf[8];
      for(i = 0; i < MRU_LIST_SIZE; i++) {
        wsprintf(buf,_T("%d"),i);
        if (*g_mru_list[i]) {
          // cbData must include the size of the terminating null character.
          RegSetValueEx(hSubKey,buf,0,REG_SZ,(const BYTE*)g_mru_list[i],(lstrlen(g_mru_list[i])+1)*sizeof(TCHAR));
        }
        else {
          RegDeleteValue(hSubKey,buf);
        }
      }
      RegCloseKey(hSubKey);
    }
    RegCloseKey(hKey);
  }
}

void ClearMRUList()
{
  int i;
  for(i=0; i<MRU_LIST_SIZE; i++) {
    g_mru_list[i][0] = _T('\0');
  }

  BuildMRUMenus();
}

void RestoreCompressor()
{
  HKEY hKey;
  NCOMPRESSOR v = COMPRESSOR_SCRIPT;
  if (OpenRegSettingsKey(hKey)) {
    TCHAR compressor_name[32];
    DWORD l = sizeof(compressor_name);
    DWORD t;

    if (RegQueryValueEx(hKey,REGCOMPRESSOR,NULL,&t,(LPBYTE)compressor_name,&l)==ERROR_SUCCESS) {
      int i;
      for(i=(int)COMPRESSOR_SCRIPT; i<= (int)COMPRESSOR_BEST; i++) {
        if(!lstrcmpi(compressor_names[i],compressor_name)) {
          v = (NCOMPRESSOR)i;
          break;
        }
      }
    }
    RegCloseKey(hKey);
  }
  g_sdata.default_compressor=v;
}

void SaveCompressor()
{
  HKEY hKey;
  int n = (int)COMPRESSOR_SCRIPT;
  NCOMPRESSOR v = g_sdata.default_compressor;

  if(v >= COMPRESSOR_SCRIPT && v <= COMPRESSOR_BEST) {
    n = (int)v;
  }

  if (OpenRegSettingsKey(hKey, true)) {
    // compressor_names, even if Unicode is saved as BYTE* data.
    RegSetValueEx(hKey,REGCOMPRESSOR,0,REG_SZ,(const BYTE*)compressor_names[n],
                  lstrlen(compressor_names[n])*sizeof(TCHAR));
    RegCloseKey(hKey);
  }
}

bool FileExists(const TCHAR *fname)
{
  WIN32_FIND_DATA wfd;
  HANDLE h = FindFirstFile(fname,&wfd);
  if(INVALID_HANDLE_VALUE != h) {
    FindClose(h);
    return true;
  }
  return false;
}

HMENU FindSubMenu(HMENU hMenu, UINT uId)
{
  MENUITEMINFO mii;
  mii.cbSize = sizeof(MENUITEMINFO);
  mii.fMask = MIIM_SUBMENU;
  return GetMenuItemInfo(hMenu, uId, FALSE, &mii) ? mii.hSubMenu : 0;
}

HFONT CreateFont(int Height, int Weight, DWORD PitchAndFamily, LPCTSTR Face)
{
  return CreateFont(Height, 0, 0, 0, Weight, FALSE, FALSE, FALSE, DEFAULT_CHARSET,
                    OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                    PitchAndFamily, Face);
}
