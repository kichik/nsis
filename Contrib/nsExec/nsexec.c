/*
Copyright (c) 2002 Robert Rainwater <rrainwater@yahoo.com>

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

Unicode support by Jim Park -- 08/24/2007

*/
#include <windows.h>
#include <commctrl.h>
#include <winnt.h>
#include <nsis/pluginapi.h> // nsis plugin

#if defined(_MSC_VER) && !defined(GetVersion)
#if _MSC_VER >= 1500
FORCEINLINE DWORD NoDepr_GetVersion() { __pragma(warning(push))__pragma(warning(disable:4996)) DWORD r = GetVersion(); __pragma(warning(pop)) return r; }
#define GetVersion NoDepr_GetVersion
#endif //~ _MSC_VER >= 1500
#endif //~ _MSC_VER

#ifndef true
#define true TRUE
#endif
#ifndef false
#define false FALSE
#endif

#define LOOPTIMEOUT  100
HWND g_hwndParent;
HWND g_hwndList;

void ExecScript(BOOL log);
void LogMessage(const TCHAR *pStr, BOOL bOEM);
TCHAR *my_strstr(TCHAR *a, TCHAR *b);
unsigned int my_atoi(TCHAR *s);
int WINAPI AsExeWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow);

void __declspec(dllexport) Exec(HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop) {
  g_hwndParent=hwndParent;
  EXDLL_INIT();
  {
    ExecScript(0);
  }
}

void __declspec(dllexport) ExecToLog(HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop) {
  g_hwndParent=hwndParent;
  EXDLL_INIT();
  {
    ExecScript(1);
  }
}

void __declspec(dllexport) ExecToStack(HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop) {
  g_hwndParent=hwndParent;
  EXDLL_INIT();
  {
    ExecScript(2);
  }
}

HINSTANCE g_hInst;
BOOL WINAPI DllMain(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
  g_hInst = hInst;
  return TRUE;
}

#define TAB_REPLACE _T("        ")
#define TAB_REPLACE_SIZE (sizeof(TAB_REPLACE)-1)

BOOL IsWOW64() {
  typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);
  BOOL wow64;
  LPFN_ISWOW64PROCESS fnIsWow64Process;

  fnIsWow64Process = (LPFN_ISWOW64PROCESS) GetProcAddress(
    GetModuleHandle(_T("kernel32")), "IsWow64Process");

  if (fnIsWow64Process != NULL) {
    if (fnIsWow64Process(GetCurrentProcess(), &wow64)) {
      return wow64;
    }
  }

  return FALSE;
}

/**
 * Convert the ansiStr if storing ANSI strings, otherwise, assume that the
 * string is wide and don't convert, but straight copy.
 * @param ansiStr [in]  the suspected ANSI string.
 * @param wideBuf [out] the buffer to write to.
 * @param cnt     [in]  the size of widebuf in wchar_t's.
 * @return true, if ASCII, false if suspected as wide.
 */
BOOL WideConvertIfASCII(const char* ansiStr, int strLen, WCHAR* wideBuf, int cnt)
{
   BOOL rval = FALSE;
   wideBuf[0] = 0;
   if (lstrlenA(ansiStr) == strLen)
   {
      MultiByteToWideChar(CP_ACP, 0, ansiStr, -1, wideBuf, cnt);
      rval = TRUE;
   }
   else
   {
      // Going to assume that it's a wide char array.
      lstrcpyW(wideBuf, (const WCHAR*) ansiStr);
   }

   return rval;
}

void ExecScript(int log) {
  TCHAR szRet[128] = _T("");
  TCHAR meDLLPath[MAX_PATH];    
  TCHAR *executor;
  TCHAR *g_exec;
  TCHAR *pExec;
  unsigned int g_to;
  BOOL bOEM;

  if (!IsWOW64()) {
    TCHAR* p;
    int nComSpecSize;

    nComSpecSize = GetModuleFileName(g_hInst, meDLLPath, MAX_PATH) + 2; // 2 chars for quotes
    g_exec = (TCHAR *)GlobalAlloc(GPTR, sizeof(TCHAR)*(g_stringsize+nComSpecSize+2)); // 1 for space, 1 for null
    p = meDLLPath + nComSpecSize - 2; // point p at null char of meDLLPath
    *g_exec = _T('"');
    executor = g_exec + 1;

    // Look for the last '\' in path.
    do
    {
      if (*p == _T('\\'))
        break;
      p = CharPrev(meDLLPath, p);
    }
    while (p > meDLLPath);
    if (p == meDLLPath)
    {
      // bad path
      pushstring(_T("error"));
      GlobalFree(g_exec);
      return;
    }

    *p = 0;
    GetTempFileName(meDLLPath, _T("ns"), 0, executor);  // executor = new temp file name in module path.
    *p = _T('\\');
    if (CopyFile(meDLLPath, executor, FALSE))  // copy current DLL to temp file in module path.
    {
      HANDLE hFile, hMapping;
      LPBYTE pMapView;
      PIMAGE_NT_HEADERS pNTHeaders;
      hFile = CreateFile(executor, GENERIC_READ|GENERIC_WRITE, 0, NULL, OPEN_EXISTING,0, 0);
      hMapping = CreateFileMapping(hFile, NULL, PAGE_READWRITE, 0, 0, NULL);
      pMapView = MapViewOfFile(hMapping, FILE_MAP_WRITE, 0, 0, 0);
      if (pMapView)
      {
        pNTHeaders = (PIMAGE_NT_HEADERS)(pMapView + ((PIMAGE_DOS_HEADER)pMapView)->e_lfanew);
        // Turning the copied DLL into a stripped down executable.
        pNTHeaders->FileHeader.Characteristics = IMAGE_FILE_32BIT_MACHINE | IMAGE_FILE_LOCAL_SYMS_STRIPPED | 
          IMAGE_FILE_LINE_NUMS_STRIPPED | IMAGE_FILE_EXECUTABLE_IMAGE;
        // Windows character-mode user interface (CUI) subsystem.
        pNTHeaders->OptionalHeader.Subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI;
        // g_hInst is assumed to be the very base of the DLL in memory.
        // WinMain will have the address of the WinMain function in memory.
        // Getting the difference gets you the relative location of the
        // WinMain function.
        pNTHeaders->OptionalHeader.AddressOfEntryPoint = (DWORD) ((DWORD_PTR)AsExeWinMain - (DWORD_PTR)g_hInst);
        UnmapViewOfFile(pMapView);
      }
      CloseHandle(hMapping);
      CloseHandle(hFile);
    }

    lstrcat(g_exec, _T("\""));

    // add space
    pExec = g_exec + lstrlen(g_exec);
    *pExec = _T(' ');
    pExec++;
  } else {
    executor = NULL;
    g_exec = (TCHAR *)GlobalAlloc(GPTR, sizeof(TCHAR)*(g_stringsize+1)); // 1 for NULL
    pExec = g_exec;
  }

  g_to = 0;      // default is no timeout
  bOEM = FALSE;  // default is no OEM->ANSI conversion

  g_hwndList = NULL;
  
  // g_hwndParent = the caller, usually NSIS installer.
  if (g_hwndParent) // The window class name for dialog boxes is "#32770"
    g_hwndList = FindWindowEx(FindWindowEx(g_hwndParent,NULL,_T("#32770"),NULL),NULL,_T("SysListView32"),NULL);

  // g_exec is the complete command to run: It has the copy of this DLL turned
  // into an executable right now.

params:
  // Get the command I need to run from the NSIS stack.
  popstring(pExec);
  if (my_strstr(pExec, _T("/TIMEOUT=")) == pExec) {
    TCHAR *szTimeout = pExec + 9;
    g_to = my_atoi(szTimeout);
    *pExec = 0;
    goto params;
  }
  if (!lstrcmpi(pExec, _T("/OEM"))) {
    bOEM = TRUE;
    *pExec = 0;
    goto params;
  }

  if (!pExec[0]) 
  {
    pushstring(_T("error"));
    if (pExec-2 >= g_exec)
      *(pExec-2) = _T('\0'); // skip space and quote
    if (executor) DeleteFile(executor);
    GlobalFree(g_exec);
    return;
  }

  // Got all the params off the stack.
  
  {
    STARTUPINFO si={sizeof(si),};
    SECURITY_ATTRIBUTES sa={sizeof(sa),};
    SECURITY_DESCRIPTOR sd={0,};
    PROCESS_INFORMATION pi={0,};
    const BOOL isNT = sizeof(void*) > 4 || (GetVersion() < 0x80000000);
    HANDLE newstdout=0,read_stdout=0;
    HANDLE newstdin=0,read_stdin=0;
    DWORD dwRead = 1;
    DWORD dwExit = 0;
    DWORD dwWait = WAIT_TIMEOUT;
    DWORD dwLastOutput;
    static TCHAR szBuf[1024];
#ifdef _UNICODE
    static char ansiBuf[1024];
#endif
    HGLOBAL hUnusedBuf = NULL;
    TCHAR *szUnusedBuf = 0;

    if (log) {
      hUnusedBuf = GlobalAlloc(GHND, log & 2 ? (g_stringsize*sizeof(TCHAR)) : sizeof(szBuf)*4); // Note: will not grow if (log & 2)
      if (!hUnusedBuf) {
        lstrcpy(szRet, _T("error"));
        goto done;
      }
      szUnusedBuf = (TCHAR *)GlobalLock(hUnusedBuf);
    }

    sa.bInheritHandle = true;
    sa.lpSecurityDescriptor = NULL;
    if (isNT) {
      InitializeSecurityDescriptor(&sd,SECURITY_DESCRIPTOR_REVISION);
      SetSecurityDescriptorDacl(&sd,true,NULL,false);
      sa.lpSecurityDescriptor = &sd;
    }

    if (!CreatePipe(&read_stdout,&newstdout,&sa,0)) {
      lstrcpy(szRet, _T("error"));
      goto done;
    }
    if (!CreatePipe(&read_stdin,&newstdin,&sa,0)) {
      lstrcpy(szRet, _T("error"));
      goto done;
    }

    GetStartupInfo(&si);
    si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
    si.wShowWindow = SW_HIDE;
    si.hStdInput = newstdin;
    si.hStdOutput = newstdout;
    si.hStdError = newstdout;
    if (!CreateProcess(NULL,g_exec,NULL,NULL,TRUE,CREATE_NEW_CONSOLE,NULL,NULL,&si,&pi)) {
      lstrcpy(szRet, _T("error"));
      goto done;
    }

    dwLastOutput = GetTickCount();

    // Now I'm talking with an executable copy of myself.
    while (dwWait != WAIT_OBJECT_0 || dwRead) {
      PeekNamedPipe(read_stdout, 0, 0, 0, &dwRead, NULL);
      if (dwRead) {
        dwLastOutput = GetTickCount();
#ifdef _UNICODE
        ReadFile(read_stdout, ansiBuf, sizeof(ansiBuf)-1, &dwRead, NULL);
        ansiBuf[dwRead] = 0;
        WideConvertIfASCII(ansiBuf, dwRead, szBuf, sizeof(szBuf)/sizeof(szBuf[0]));
#else
        ReadFile(read_stdout, szBuf, sizeof(szBuf)-1, &dwRead, NULL);
        szBuf[dwRead] = '\0';
#endif
        if (log) {
          if (log & 2) {
            lstrcpyn(szUnusedBuf + lstrlen(szUnusedBuf), szBuf, g_stringsize - lstrlen(szUnusedBuf));
          }
          else {
            TCHAR *p, *p2;
            SIZE_T iReqLen = lstrlen(szBuf) + lstrlen(szUnusedBuf) + 1;
            if (GlobalSize(hUnusedBuf) < iReqLen*sizeof(TCHAR)) {
              GlobalUnlock(hUnusedBuf);
              hUnusedBuf = GlobalReAlloc(hUnusedBuf, iReqLen*sizeof(TCHAR)+sizeof(szBuf), GHND);
              if (!hUnusedBuf) {
                lstrcpy(szRet, _T("error"));
                break;
              }
              szUnusedBuf = (TCHAR *)GlobalLock(hUnusedBuf);
            }
            p = szUnusedBuf; // get the old left overs
            lstrcat(p, szBuf);
            while ((p = my_strstr(p, _T("\t")))) {
              if ((int)(p - szUnusedBuf) > (int)(GlobalSize(hUnusedBuf)/sizeof(TCHAR) - TAB_REPLACE_SIZE - 1))
              {
                *p++ = _T(' ');
              }
              else
              {
                int len = lstrlen(p);
                TCHAR *c_out=(TCHAR*)p+TAB_REPLACE_SIZE+len;
                TCHAR *c_in=(TCHAR *)p+len;
                while (len-- > 0) {
                  *c_out--=*c_in--;
                }

                lstrcpy(p, TAB_REPLACE);
                p += TAB_REPLACE_SIZE;
                *p = _T(' ');
              }
            }
            
            p = szUnusedBuf; // get the old left overs
            for (p2 = p; *p2;) {
              if (*p2 == _T('\r')) {
                *p2++ = 0;
                continue;
              }
              if (*p2 == _T('\n')) {
                *p2 = 0;
                while (!*p && p != p2) p++;
                LogMessage(p, bOEM);
                p = ++p2;
                continue;
              }
              p2 = CharNext(p2);
            }
            
            // If data was taken out from the unused buffer, move p contents to the start of szUnusedBuf
            if (p != szUnusedBuf) {
              TCHAR *p2 = szUnusedBuf;
              while (*p) *p2++ = *p++;
              *p2 = 0;
            }
          }
        }
      }
      else {
        if (g_to && GetTickCount() > dwLastOutput+g_to) {
          TerminateProcess(pi.hProcess, -1);
          lstrcpy(szRet, _T("timeout"));
        }
        else Sleep(LOOPTIMEOUT);
      }

      dwWait = WaitForSingleObject(pi.hProcess, 0);
      GetExitCodeProcess(pi.hProcess, &dwExit);
      PeekNamedPipe(read_stdout, 0, 0, 0, &dwRead, NULL);
    }
done:
    if (log & 2) pushstring(szUnusedBuf);
    if (log & 1 && *szUnusedBuf) LogMessage(szUnusedBuf, bOEM);
    if ( dwExit == STATUS_ILLEGAL_INSTRUCTION )
      lstrcpy(szRet, _T("error"));
    if (!szRet[0]) wsprintf(szRet,_T("%d"),dwExit);
    pushstring(szRet);
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
    CloseHandle(newstdout);
    CloseHandle(read_stdout);
    CloseHandle(newstdin);
    CloseHandle(read_stdin);
    if (pExec-2 >= g_exec)
      *(pExec-2) = _T('\0'); // skip space and quote
    if (executor) DeleteFile(executor);
    GlobalFree(g_exec);
    if (log) {
      GlobalUnlock(hUnusedBuf);
      GlobalFree(hUnusedBuf);
    }
  }
}

// Tim Kosse's LogMessage
void LogMessage(const TCHAR *pStr, BOOL bOEM) {
  LVITEM item={0};
  int nItemCount;
  if (!g_hwndList) return;
  //if (!lstrlen(pStr)) return;
#ifndef _UNICODE
  if (bOEM == TRUE) OemToCharBuff(pStr, (char*)pStr, lstrlen(pStr));
#endif
  nItemCount=(int) SendMessage(g_hwndList, LVM_GETITEMCOUNT, 0, 0);
  item.mask=LVIF_TEXT;
  item.pszText=(TCHAR *)pStr;
  item.cchTextMax=0;
  item.iItem=nItemCount;
  ListView_InsertItem(g_hwndList, &item);
  ListView_EnsureVisible(g_hwndList, item.iItem, 0);
}

TCHAR *my_strstr(TCHAR *a, TCHAR *b)
{
  int l = lstrlen(b);
  while (lstrlen(a) >= l)
  {
    TCHAR c = a[l];
    a[l] = 0;
    if (!lstrcmpi(a, b))
    {
      a[l] = c;
      return a;
    }
    a[l] = c;
    a = CharNext(a);
  }
  return NULL;
}

unsigned int my_atoi(TCHAR *s) {
  unsigned int v=0;
  if (*s == _T('0') && (s[1] == _T('x') || s[1] == _T('X'))) {
    s+=2;
    for (;;) {
      int c=*s++;
      if (c >= _T('0') && c <= _T('9')) c-=_T('0');
      else if (c >= _T('a') && c <= _T('f')) c-=_T('a')-10;
      else if (c >= _T('A') && c <= _T('F')) c-=_T('A')-10;
      else break;
      v<<=4;
      v+=c;
    }
  }
  else if (*s == _T('0') && s[1] <= _T('7') && s[1] >= _T('0')) {
    s++;
    for (;;) {
      int c=*s++;
      if (c >= _T('0') && c <= _T('7')) c-=_T('0');
      else break;
      v<<=3;
      v+=c;
    }
  }
  else {
    for (;;) {
      int c=*s++ - _T('0');
      if (c < 0 || c > 9) break;
      v*=10;
      v+=c;
    }
  }
  return (int)v;
}

int WINAPI AsExeWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow)
{
  DWORD               Ret;
  STARTUPINFO         si   = {0};
  PROCESS_INFORMATION pi   = {0};
  TCHAR command_line[1024]; //BUGBUG
  TCHAR seekchar=_T(' ');
  TCHAR *cmdline;
  
  si.cb = sizeof(si);
  // Make child process use this app's standard files. Not needed because the handles
  // we created when executing this process were inheritable.
  //si.dwFlags    = STARTF_USESTDHANDLES;
  //si.hStdInput  = GetStdHandle (STD_INPUT_HANDLE);
  //si.hStdOutput = GetStdHandle (STD_OUTPUT_HANDLE);
  //si.hStdError  = GetStdHandle (STD_ERROR_HANDLE);
  lstrcpyn(command_line, GetCommandLine(), 1024);
  
  cmdline = command_line;
  if (*cmdline == _T('\"')) seekchar = *cmdline++;

  while (*cmdline && *cmdline != seekchar) cmdline=CharNext(cmdline);
  cmdline=CharNext(cmdline);
  // skip any spaces before the arguments
  while (*cmdline && *cmdline == _T(' ')) cmdline++;

  Ret = CreateProcess (NULL, cmdline,
    NULL, NULL,
    TRUE, 0,
    NULL, NULL,
    &si, &pi
    );

  if (Ret)
  {
    WaitForSingleObject(pi.hProcess, INFINITE);
    GetExitCodeProcess(pi.hProcess, &Ret);
    CloseHandle (pi.hProcess);
    CloseHandle (pi.hThread);
    ExitProcess(Ret);
  }
  else
  {
    ExitProcess(STATUS_ILLEGAL_INSTRUCTION);
  }

  return 0; // dummy
}
