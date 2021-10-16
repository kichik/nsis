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

#ifndef MONITOR_DEFAULTTONEAREST
#define MONITOR_DEFAULTTONEAREST 2
WINUSERAPI HMONITOR WINAPI MonitorFromWindow(HWND hwnd, DWORD dwFlags);
#endif
#ifndef GRADIENT_FILL_RECT_H
#define GRADIENT_FILL_RECT_H 0
#if !defined(_WIN32_WINNT) || _WIN32_WINNT-0 < 0x0410
WINGDIAPI BOOL WINAPI GradientFill(HDC,TRIVERTEX*,ULONG,PVOID,ULONG,ULONG);
#endif
#endif


NTOOLTIP g_tip;
LRESULT CALLBACK TipHookProc(int nCode, WPARAM wParam, LPARAM lParam);

TCHAR g_mru_list[MRU_LIST_SIZE][MAX_PATH] = { _T(""), _T(""), _T(""), _T(""), _T("") };

extern NSCRIPTDATA g_sdata;
extern const TCHAR *compressor_names[];

void MemSafeFree(void*mem) { if (mem) GlobalFree(mem); }
void*MemAllocZI(SIZE_T cb) { return GlobalAlloc(GPTR, cb); }

HMODULE LoadSysLibrary(LPCSTR Mod)
{
  TCHAR buf[MAX_PATH+20], *path;
  UINT dirmax = MAX_PATH, cch;
  if ((cch = GetSystemDirectory(buf, dirmax)) >= dirmax) cch = 0;
  wsprintf(buf + cch, _T("\\%hs.dll"), Mod); // Note: We always append ".dll"
  path = buf + !cch; // Full path or just the filename
  return LoadLibrary(path);
}

FARPROC GetSysProcAddr(LPCSTR Mod, LPCSTR FuncName)
{
  return GetProcAddress(LoadSysLibrary(Mod), FuncName);
}

static bool WriteFile(HANDLE hFile, const void*pData, DWORD cb)
{
  DWORD cbio;
  return WriteFile(hFile, pData, cb, &cbio, 0) && cb == cbio;
}
bool WriteUTF16LEBOM(HANDLE hFile)
{
  static const unsigned char u16lb[] = {0xFF,0xFE};
  return WriteFile(hFile, u16lb, sizeof(u16lb));
}

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

typedef struct { LPCSTR SoundName; int MBFallback; } PLAYAPPSOUNDDATA;
static DWORD CALLBACK PlayAppSoundProc(LPVOID ThreadParam) {
  PLAYAPPSOUNDDATA *p = (PLAYAPPSOUNDDATA*) ThreadParam;
  BOOL succ = PlaySoundA(p->SoundName, NULL, (SND_APPLICATION|SND_ALIAS|SND_NODEFAULT) & ~SND_ASYNC); // Cannot use SND_ASYNC because we need to detect if the sound played
  if (!succ && p->MBFallback >= 0) succ = MessageBeep(p->MBFallback);
  MemFree(p);
  return succ;
}

void PlayAppSoundAsync(LPCSTR SoundName, int MBFallback) {
  DWORD tid;
  PLAYAPPSOUNDDATA *p = (PLAYAPPSOUNDDATA*) MemAlloc(sizeof(PLAYAPPSOUNDDATA));
  if (p) {
    p->SoundName = SoundName, p->MBFallback = MBFallback; // Note: The string must be valid until the sound has started because we don't copy it
    HANDLE hThread = CreateThread(NULL, 0, PlayAppSoundProc, p, 0, &tid);
    if (hThread) CloseHandle(hThread); else PlayAppSoundProc(p);
  }
}

void CopyToClipboard(HWND hwnd) {
  if (!hwnd || !OpenClipboard(hwnd)) return;
  LRESULT len = SendDlgItemMessage(hwnd, IDC_LOGWIN, WM_GETTEXTLENGTH, 0, 0);
  HGLOBAL mem = GlobalAlloc(GMEM_MOVEABLE, (++len)*sizeof(TCHAR));
  if (!mem) { CloseClipboard(); return; }
  TCHAR *txt = (TCHAR *)GlobalLock(mem);
  if (!txt) { CloseClipboard(); return; }
  EmptyClipboard();
  txt[0] = 0;
  SendDlgItemMessage(hwnd, IDC_LOGWIN, WM_GETTEXT, (WPARAM)(len), (LPARAM)txt);
  GlobalUnlock(mem);
#ifdef _UNICODE
  SetClipboardData(CF_UNICODETEXT, mem);
#else
  SetClipboardData(CF_TEXT, mem);
#endif
  CloseClipboard();
}

void SetLogColor(enum LOGCOLOR lc)
{
  enum { em_seteditstyle = (WM_USER + 204), ses_extendbackcolor = 4 };
  HWND hEd = GetDlgItem(g_sdata.hwnd, IDC_LOGWIN);
  bool sysclr = lc >= LC_SYSCOLOR || !ReadRegSettingDW(REGCOLORIZE, true);
  static const COLORREF clrs[] = { RGB(0, 50, 0), RGB(210, 255, 210), RGB(50, 30, 0), RGB(255, 220, 190), RGB(50, 0, 0), RGB(255, 210, 210) };
  CHARFORMAT cf;
  cf.cbSize = sizeof(cf), cf.dwMask = CFM_COLOR;
  cf.dwEffects = sysclr ? CFE_AUTOCOLOR : 0;
  cf.crTextColor = sysclr ? RGB(0, 0, 0) : clrs[(lc * 2) + 0];
  SendMessage(hEd, em_seteditstyle, sysclr ? 0 : ses_extendbackcolor, ses_extendbackcolor);
  SendMessage(hEd, EM_SETCHARFORMAT, 0, (LPARAM) &cf);
  SendMessage(hEd, EM_SETBKGNDCOLOR, sysclr, sysclr ? sysclr /*Irrelevant*/ : clrs[(lc * 2) + 1]);
}

void ClearLog(HWND hwnd) {
  SetDlgItemText(hwnd, IDC_LOGWIN, _T(""));
  SetLogColor(LC_SYSCOLOR);
  SendMessage(g_sdata.hwnd, WM_MAKENSIS_UPDATEUISTATE, 0, 0);
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
  wsprintf(buf, _T("[Error] %s\r\n"), str);
  LogMessage(hwnd,buf);
}

static void CenterOnParent(HWND hwnd, HWND hParent)
{
  RECT r;
  GetWindowRect(hwnd, &r);
  UINT w = (r.right - r.left), h = (r.bottom - r.top), swp = SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE;
  if (GetWindowRect(hParent, &r))
    SetWindowPos(hwnd, 0, r.left + ((r.right - r.left)/2) - (w/2), r.top + ((r.bottom - r.top)/2) - (h/2), 0, 0, swp);
}
void CenterOnParent(HWND hwnd)
{
  CenterOnParent(hwnd, GetWindow(hwnd, GW_OWNER));
}

void SetDialogFocus(HWND hDlg, HWND hCtl)
{
  //blogs.msdn.com/b/oldnewthing/archive/2004/08/02/205624.aspx
  SendMessage(hDlg, WM_NEXTDLGCTL, (WPARAM)hCtl, TRUE);
}

HWND GetComboEdit(HWND hCB)
{
  /* CB_GETCOMBOBOXINFO crashes on 64-bit NT 5.x (KB947841).
  We are left with GetComboBoxInfo(), FindWindowEx()*2 and 
  ChildWindowFromPoint(h,{1,1}) (docs.microsoft.com/en-us/windows/desktop/Controls/subclass-a-combo-box#). */
  if (!SupportsWNT4() && !SupportsW95())
  {
    COMBOBOXINFO cbi;
    cbi.cbSize = FIELD_OFFSET(COMBOBOXINFO, hwndList) + sizeof(HWND);
    BOOL succ = GetComboBoxInfo(hCB, &cbi);
    return succ ? cbi.hwndItem : (HWND)(INT_PTR) succ;
  }
  HWND hList = FindWindowEx(hCB, 0, 0, 0);
  return FindWindowEx(hCB, hList, 0, 0);
}

void EnableDisableItems(HWND hwnd, int on) 
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
    PACKCMDID(IDM_SAVE), PACKCMDID(IDM_CLEARLOG), PACKCMDID(IDM_GUIDGEN),
    PACKCMDID(IDM_COMPRESSOR), PACKCMDID(IDM_COMPRESSOR_SUBMENU),
    PACKCMDID(IDM_RECOMPILE), PACKCMDID(IDM_RECOMPILE_TEST)
  };
  for (UINT i = 0; i < COUNTOF(cmds); ++i) {
    UINT id = UNPACKCMDID(cmds[i]);
    EnableMenuItem(hMenu, id, mf);
    if (IDM_COPYSELECTED != id && IDM_COMPRESSOR_SUBMENU != id)
      EnableToolBarButton(id, on);
  }

  SendMessage(g_sdata.hwnd, WM_MAKENSIS_UPDATEUISTATE, 0 ,0);
  EnableMenuItem(hMenu, IDM_FILE, mf); // Disable the whole File menu because of the MRU list
  DrawMenuBar(g_sdata.hwnd);

  HWND hFocus = g_sdata.focused_hwnd, hOptimal = hTestBtn;
  if (on && hCloseBtn == hFocus) hFocus = hOptimal;
  if (!IsWindowEnabled(hFocus)) hFocus = GetDlgItem(hwnd, IDC_LOGWIN);
  SetDialogFocus(hwnd, hOptimal);
  SetDialogFocus(hwnd, hFocus);
}

void SetCompressorStats()
{
  DWORD_PTR line_count, i;
  TCHAR buf[1024];
  bool found = false;

  line_count = SendDlgItemMessage(g_sdata.hwnd, IDC_LOGWIN, EM_GETLINECOUNT, 0, 0);
  for(i=0; i<line_count; i++) {
    *((LPWORD)buf) = COUNTOF(buf); 
    LRESULT cchLine = SendDlgItemMessage(g_sdata.hwnd, IDC_LOGWIN, EM_GETLINE, (WPARAM)i, (LPARAM)buf);
    buf[cchLine] = _T('\0');
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

static void SetUIState_NoScript()
{
  static const PACKEDCMDID_T cmds [] = {
    PACKCMDID(IDM_RECOMPILE),PACKCMDID(IDM_RECOMPILE_TEST),PACKCMDID(IDM_TEST), 
    PACKCMDID(IDM_BROWSESCR),PACKCMDID(IDM_EDITSCRIPT)
  };
  for (UINT i = 0; i < COUNTOF(cmds); ++i)
    EnableUICommand(UNPACKCMDID(cmds[i]), FALSE);
  EnableWindow(GetDlgItem(g_sdata.hwnd, IDC_TEST), FALSE);
}

void CompileNSISScript() {
  DragAcceptFiles(g_sdata.hwnd,FALSE);
  ClearLog(g_sdata.hwnd);
  SetTitle(g_sdata.hwnd,NULL);
  PostMessage(g_sdata.hwnd, WM_MAKENSIS_UPDATEUISTATE, 0, 0);
  if (!g_sdata.script[0]) {
    LogMessage(g_sdata.hwnd,USAGE);
    SetUIState_NoScript();
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

static DWORD RegWriteString(HKEY hKey, LPCTSTR Name, LPCTSTR Data)
{
  const DWORD cb = (lstrlen(Data) + 1) * sizeof(*Data);
  return RegSetValueEx(hKey, Name, 0, REG_SZ, (LPBYTE)Data, cb);
}

static DWORD RegReadString(HKEY hKey, LPCTSTR Name, LPTSTR Buf, DWORD cbBufSize) {
  DWORD ec, rt, cb = cbBufSize, cbCh = sizeof(*Buf);
  ec = RegQueryValueEx(hKey, Name, NULL, &rt, (BYTE*) Buf, &cb);
  if (cbBufSize) {
#if 0
    if (rt == REG_DWORD) cb = cbCh * wsprintf(Buf, _T("%d"), *((INT32*)Buf));
#endif
    if (cb+cbCh < cbBufSize) Buf[cb / cbCh] = _T('\0'); // Add a \0 after the data if there is room
    Buf[(cbBufSize / cbCh) - 1] = _T('\0'); // Always \0 terminate, truncating data if necessary
  }
  return ec;
}

static DWORD RegOpenKeyForReading(HKEY hRoot, LPCTSTR SubKey, HKEY*pKey) {
  return RegOpenKeyEx(hRoot, SubKey, 0, KEY_READ, pKey);
}

static bool InternalOpenRegSettingsKey(HKEY root, HKEY &key, bool create) {
  if (create) {
    if (RegCreateKey(root, REGKEY, &key) == ERROR_SUCCESS)
      return true;
  } else {
    if (RegOpenKeyForReading(root, REGKEY, &key) == ERROR_SUCCESS)
      return true;
  }
  return false;
}

bool OpenRegSettingsKey(HKEY &hKey, bool create) {
  return InternalOpenRegSettingsKey(REGSEC, hKey, create)
      || InternalOpenRegSettingsKey(REGSECDEF, hKey, create);
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
  if (CreateRegSettingsKey(hKey)) {
    RegSetValueEx(hKey, REGLOC, 0, REG_BINARY, (LPBYTE)&p, sizeof(p));
    RegCloseKey(hKey);
  }
}

void RestoreSymbols() {
  g_sdata.symbols = LoadSymbolSet(NULL);
}

void SaveSymbols() {
  SaveSymbolSet(NULL, g_sdata.symbols);
}

#define SYMSET_SUBKEY_MAXLEN (100 + SYMSETNAME_MAXLEN) // REGSYMSUBKEY + [\name]
static int CreateSymbolSetSubkeyPath(const TCHAR *name, TCHAR *buffer) {
  return wsprintf(buffer, name ? _T("%s\\%s") : _T("%s"), REGSYMSUBKEY, name);
}

void FreeSymbolSet(TCHAR **symbols) {
  if (symbols) {
    for (SIZE_T i = 0; symbols[i]; ++i)
      MemSafeFree(symbols[i]);
    GlobalFree((HGLOBAL) symbols);
  }
}

void DeleteSymbolSet(const TCHAR *name) {
  HKEY hKey;
  if (name && OpenRegSettingsKey(hKey)) {
    TCHAR subkey[SYMSET_SUBKEY_MAXLEN+1];
    CreateSymbolSetSubkeyPath(name, subkey);
    RegDeleteKey(hKey, subkey);
    RegCloseKey(hKey);
  }
}

TCHAR** LoadSymbolSet(const TCHAR *name) {
  HKEY hCfgKey, hSymKey;
  TCHAR **symbols = NULL;
  if (OpenRegSettingsKey(hCfgKey)) {
    TCHAR subkey[SYMSET_SUBKEY_MAXLEN+1];
    CreateSymbolSetSubkeyPath(name, subkey);
    if (RegOpenKeyForReading(hCfgKey, subkey, &hSymKey) == ERROR_SUCCESS) {
      TCHAR bufName[8];
      for (DWORD i = 0, rt, cbBuf, cbData;;) {
        cbBuf = sizeof(bufName);
        if (RegEnumValue(hSymKey, i, bufName, &cbBuf, NULL, &rt, NULL, &cbData) == ERROR_SUCCESS && rt == REG_SZ) {
          if(symbols) {
            HGLOBAL newmem = GlobalReAlloc(symbols, (i+2)*sizeof(TCHAR*), GMEM_MOVEABLE|GMEM_ZEROINIT);
            if (!newmem) FreeSymbolSet(symbols);
            symbols = (TCHAR**) newmem;
          }
          else {
            symbols = (TCHAR**) GlobalAlloc(GPTR, (i+2)*sizeof(TCHAR*));
          }
          if (!symbols) break; // Out of memory, abort!
          symbols[i] = (TCHAR*) MemAllocZI(cbData += sizeof(TCHAR));
          if (!symbols[i] || RegReadString(hSymKey, bufName, symbols[i], cbData)) {
            FreeSymbolSet(symbols);
            break;
          }
          symbols[++i] = NULL; // The symbols array is terminated by a NULL pointer
        }
        else
          break;
      }
      RegCloseKey(hSymKey);
    }
    RegCloseKey(hCfgKey);
  }
  return symbols;
}

void SaveSymbolSet(const TCHAR *name, TCHAR **symbols) {
  HKEY hCfgKey, hSymKey;
  if (CreateRegSettingsKey(hCfgKey)) {
    TCHAR subkey[SYMSET_SUBKEY_MAXLEN+1], bufName[8];
    CreateSymbolSetSubkeyPath(name, subkey);
    if (RegOpenKey(hCfgKey, subkey, &hSymKey) == ERROR_SUCCESS) {
      // Cannot use DeleteSymbolSet because name might be NULL and named sets are stored inside the base symbols key
      for (DWORD cb;;) {
        cb = sizeof(bufName);
        if (RegEnumValue(hSymKey,0, bufName, &cb,NULL,NULL,NULL,NULL)!=ERROR_SUCCESS) break;
        RegDeleteValue(hSymKey, bufName);
      }
      RegCloseKey(hSymKey);
    }
    if(symbols) {
      if (RegCreateKey(hCfgKey, subkey, &hSymKey) == ERROR_SUCCESS) {
        for (SIZE_T i = 0; symbols[i]; ++i) {
          wsprintf(bufName, _T("%d"), (INT) i);
          RegWriteString(hSymKey, bufName, symbols[i]);
        }
        RegCloseKey(hSymKey);
      }
    }
    RegCloseKey(hCfgKey);
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
  FreeSymbolSet(g_sdata.symbols);
  g_sdata.symbols = NULL;
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
  OSVERSIONINFO osv;
  GetVersionEx((osv.dwOSVersionInfoSize = sizeof(osv), &osv));
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
      while(len && ((szBuf[len - 1] == '\n')|(szBuf[len - 1] == '\r'))) szBuf[--len] = '\0';
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
  ti.uId = (UINT_PTR) hWnd;
  ti.lpszText = const_cast<LPTSTR>(lpszToolTip);
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
  if ((int)(INT_PTR) ShellExecute(g_sdata.hwnd,_T("open"),pathf,NULL,NULL,SW_SHOWNORMAL) <= 32) 
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
  TCHAR full_file_name[MAX_PATH+1];

  if(!fname || fname[0] == _T('\0') || fname[0] == _T('/') || fname[0] == _T('-')) {
    return;
  }

  DWORD rv = GetFullPathName(fname,COUNTOF(full_file_name),full_file_name,NULL);
  if (rv == 0 || rv >= COUNTOF(full_file_name)) {
    return;
  }

  if(IsValidFile(full_file_name)) {
    int i;
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
  mii.fMask = MIIM_TYPE, mii.cch = 0;
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
  HKEY hCfgKey, hMRUKey;
  UINT n = 0, i;
  if (OpenRegSettingsKey(hCfgKey)) {
    if (RegOpenKeyForReading(hCfgKey, REGMRUSUBKEY, &hMRUKey) == ERROR_SUCCESS) {
      for(int i=0; i<MRU_LIST_SIZE; i++) {
        TCHAR bufName[8];
        wsprintf(bufName, _T("%d"), i);
        DWORD ec = RegReadString(hMRUKey, bufName, g_mru_list[n], sizeof(g_mru_list[n]));
        if(!ec && g_mru_list[n][0] != _T('\0')) {
          n++;
        }
      }
      RegCloseKey(hMRUKey);
    }
    RegCloseKey(hCfgKey);
  }
  for(i = n; i < MRU_LIST_SIZE; i++) {
    g_mru_list[i][0] = _T('\0');
  }

  BuildMRUMenus();
}

void SaveMRUList()
{
  UINT c = 0, i;
  for (i = 0; i < MRU_LIST_SIZE; ++i)
    if (*g_mru_list[i])
      ++c;
  HKEY hCfgKey, hMRUKey;
  if (CreateRegSettingsKey(hCfgKey)) {
    if ((c ? RegCreateKey : RegOpenKey)(hCfgKey, REGMRUSUBKEY, &hMRUKey) == ERROR_SUCCESS) {
      for (i = 0; i < MRU_LIST_SIZE; ++i) {
        TCHAR bufName[8];
        wsprintf(bufName, _T("%d"), i);
        if (*g_mru_list[i])
          RegWriteString(hMRUKey, bufName, g_mru_list[i]);
        else
          RegDeleteValue(hMRUKey, bufName);
      }
      RegCloseKey(hMRUKey);
    }
    RegCloseKey(hCfgKey);
  }
}

void ClearMRUList()
{
  for(UINT i=0; i < MRU_LIST_SIZE; ++i) {
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
    if (RegReadString(hKey, REGCOMPRESSOR, compressor_name, sizeof(compressor_name))==ERROR_SUCCESS) {
      for(UINT i= (UINT)COMPRESSOR_SCRIPT; i <= (UINT)COMPRESSOR_BEST; i++) {
        if(!lstrcmpi(compressor_names[i], compressor_name)) {
          v = (NCOMPRESSOR)i;
          break;
        }
      }
    }
    RegCloseKey(hKey);
  }
  g_sdata.default_compressor = v;
}

void SaveCompressor()
{
  HKEY hKey;
  int n = (int)COMPRESSOR_SCRIPT;
  NCOMPRESSOR v = g_sdata.default_compressor;

  if(v >= COMPRESSOR_SCRIPT && v <= COMPRESSOR_BEST) {
    n = (int)v;
  }

  if (CreateRegSettingsKey(hKey)) {
    if (compressor_names[n][0])
      RegWriteString(hKey, REGCOMPRESSOR, compressor_names[n]);
    else
      RegDeleteValue(hKey, REGCOMPRESSOR);
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

bool OpenUrlInDefaultBrowser(HWND hwnd, LPCSTR Url)
{
  return (int)(INT_PTR) ShellExecuteA(hwnd, NULL, Url, NULL, NULL, SW_SHOWNORMAL) > 32;
}

HMENU FindSubMenu(HMENU hMenu, UINT uId)
{
  MENUITEMINFO mii;
  mii.cbSize = sizeof(MENUITEMINFO);
  mii.fMask = MIIM_SUBMENU;
  return GetMenuItemInfo(hMenu, uId, FALSE, &mii) ? mii.hSubMenu : 0;
}

static UINT DpiGetClassicSystemDpiY() { HDC hDC = GetDC(NULL); UINT dpi = GetDeviceCaps(hDC, LOGPIXELSY); ReleaseDC(NULL, hDC); return dpi; }
static HRESULT WINAPI DpiFallbackGetDpiForMonitor(HMONITOR hMon, int MDT, UINT*pX, UINT*pY) { return (*pX = *pY = DpiGetClassicSystemDpiY(), S_OK); }
static UINT WINAPI DpiFallbackGetDpiForWindow(HWND hWnd) { return 0; }
static HMONITOR WINAPI DpiFallbackMonitorFromWindow(HWND hWnd, DWORD Flags) { return NULL; }

static UINT DpiNativeGetForMonitor(HMONITOR hMon)
{
  static HRESULT(WINAPI*f)(HMONITOR, int, UINT*, UINT*);
  if (!f && !((FARPROC&)f = GetSysProcAddr("SHCORE", "GetDpiForMonitor"))) f = DpiFallbackGetDpiForMonitor;
  UINT x, y, mdt_effective_dpi = 0;
  return SUCCEEDED(f(hMon, mdt_effective_dpi, &x, &y)) ? y : 0; 
}
UINT DpiGetForMonitor(HWND hWnd)
{
  HMONITOR(WINAPI*monitorfromwindow)(HWND, DWORD);
  if (SupportsWNT4() || SupportsW95())
  {
    static HMONITOR(WINAPI*g)(HWND, DWORD);
    if (!g && !((FARPROC&)g = GetSysProcAddr("USER32", "MonitorFromWindow"))) g = DpiFallbackMonitorFromWindow;
    monitorfromwindow = g;
  }
  else
  {
    monitorfromwindow = MonitorFromWindow;
  }
  HMONITOR hMon = monitorfromwindow(hWnd, MONITOR_DEFAULTTONEAREST);
  return hMon ? DpiNativeGetForMonitor(hMon) : (UINT)(UINT_PTR) hMon;
}

UINT DpiGetForWindow(HWND hWnd)
{
  UINT dpi;
  if (DpiAwarePerMonitor() || DpiAwarePerMonitor2())
  {
    static UINT(WINAPI*f)(HWND);
    if (!f && !((FARPROC&)f = GetSysProcAddr("USER32", "GetDpiForWindow"))) f = DpiFallbackGetDpiForWindow;
    if ((dpi = f(hWnd))) return dpi;
  }
  if (DpiAwarePerMonitor() && (dpi = DpiGetForMonitor(hWnd))) return dpi;
  return DpiGetClassicSystemDpiY();
}

int DpiScaleY(HWND hWnd, int Val)
{
  return MulDiv(Val, DpiGetForWindow(hWnd), 96);
}

HFONT CreateFontHelper(INT_PTR Data, int Height, DWORD p1, LPCTSTR Face)
{
  WORD w = LOBYTE(p1)<<2, flags = HIBYTE(p1), cs = HIWORD(LOBYTE(p1)), paf = HIWORD(HIBYTE(p1));
  if (flags & CFF_DPIPT)
  {
    UINT dpi = (flags & CFF_DPIFROMHWND) ? DpiGetForWindow((HWND) Data) : (UINT) Data;
    Height = -MulDiv(Height, dpi, 72);
  }
  return CreateFont(Height, 0, 0, 0, w, FALSE, FALSE, FALSE, cs, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, paf, Face);
}

BOOL FillRectColor(HDC hDC, const RECT &Rect, COLORREF Color)
{
  COLORREF orgclr = SetBkColor(hDC, Color);
  ExtTextOut(hDC, 0, 0, ETO_OPAQUE, &Rect, _T(""), 0, NULL);
  return TRUE|SetBkColor(hDC, orgclr);
}

static BOOL DrawHorzGradient(HDC hDC, const RECT&rect, COLOR16 r1, COLOR16 g1, COLOR16 b1, COLOR16 r2, COLOR16 g2, COLOR16 b2)
{
  TRIVERTEX v[2] = { {rect.left, rect.top, r1, g1, b1, 0xffff}, {rect.right, rect.bottom, r2, g2, b2, 0xffff} };
  GRADIENT_RECT r = { 0, 1 };
  BOOL(WINAPI*gf)(HDC,TRIVERTEX*,ULONG,VOID*,ULONG,ULONG);
  if (SupportsWNT4() || SupportsW95())
  {
    if (!((FARPROC&)gf = GetSysProcAddr("MSIMG32", "GradientFill")))
    {
      return FillRectColor(hDC, rect, RGB((((UINT)r1+r2)/2)>>8, (((UINT)g1+g2)/2)>>8, (((UINT)b1+b2)/2)>>8)); // TODO: Actually try to draw a gradient
    }
  }
  else
    gf = GradientFill;
  return gf(hDC, v, 2, &r, 1, GRADIENT_FILL_RECT_H);
}

BOOL DrawHorzGradient(HDC hDC, LONG l, LONG t, LONG r, LONG b, COLORREF c1, COLORREF c2)
{
  RECT rect = { l, t, r, b };
  return DrawHorzGradient(hDC, rect, (WORD)GetRValue(c1)<<8, (WORD)GetGValue(c1)<<8, (WORD)GetBValue(c1)<<8, (WORD)GetRValue(c2)<<8, (WORD)GetGValue(c2)<<8, (WORD)GetBValue(c2)<<8);
}

long DlgUnitToPixelX(HWND hDlg, long x) { RECT r = { x, 0, 0, 0 }; MapDialogRect(hDlg, &r); return r.left; }
long DlgUnitToPixelY(HWND hDlg, long y) { RECT r = { 0, y, 0, 0 }; MapDialogRect(hDlg, &r); return r.top; }

#ifndef SP_GRIPPER
#ifndef HTHEME
#define HTHEME HTHEME_OLDSDK
struct OLDSDK_TYPE_HTHEME {int unused;}; typedef struct OLDSDK_TYPE_HTHEME* HTHEME;
#endif
#define SP_GRIPPER 3
#endif
struct VisualStyles {
  VisualStyles() : m_OpenThemeData(NULL) {}
  static HTHEME WINAPI Compat_OpenThemeData(HWND hWnd, LPCWSTR Class) { return NULL; }
  HTHEME OpenThemeData(HWND hWnd, LPCWSTR Class) { return (InitUXTheme(), m_OpenThemeData(hWnd, Class)); }
  void InitUXTheme()
  {
    if (m_OpenThemeData) return ;
    HMODULE hUXT = LoadSysLibrary("UXTHEME");
    if (!((FARPROC&) m_OpenThemeData = GetProcAddress(hUXT, "OpenThemeData"))) m_OpenThemeData = Compat_OpenThemeData;
    (FARPROC&) CloseThemeData = GetProcAddress(hUXT, "CloseThemeData");
    (FARPROC&) DrawThemeBackground = GetProcAddress(hUXT, "DrawThemeBackground");
  }

  HTHEME(WINAPI*m_OpenThemeData)(HWND,LPCWSTR);
  HRESULT(WINAPI*CloseThemeData)(HTHEME);
  HRESULT(WINAPI*DrawThemeBackground)(HTHEME,HDC,int,int,LPCRECT,LPCRECT);
} VS;

void DrawGripper(HWND hWnd, HDC hDC, const RECT&r)
{
  HTHEME hTheme = VS.OpenThemeData(hWnd, L"STATUS");
  if (hTheme)
  {
    VS.DrawThemeBackground(hTheme, hDC, SP_GRIPPER, 0, &r, NULL);
    VS.CloseThemeData(hTheme);
  }
  else
  {
    DrawFrameControl(hDC, const_cast<LPRECT>(&r), DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
  }
}

bool RicheditHasSelection(HWND hRE)
{
  CHARRANGE tr;
  SendMessage(hRE, EM_EXGETSEL, 0, (LPARAM) &tr);
  return tr.cpMax - tr.cpMin <= 0 ? FALSE : TRUE;
}

void EnableUICommand(UINT Id, INT_PTR Enabled)
{
  EnableToolBarButton(Id, !!Enabled);
  EnableMenuItem(g_sdata.menu, Id, Enabled ? MF_ENABLED : MF_GRAYED);
}
