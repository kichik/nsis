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

*/
#include <windows.h>
#include <commctrl.h>
#include <winnt.h>
#include "../ExDLL/exdll.h"

#ifndef true
#define true TRUE
#endif
#ifndef false
#define false FALSE
#endif
#define LOOPTIMEOUT  100

HWND          g_hwndParent;
HWND          g_hwndList;

void ExecScript(BOOL log);
void LogMessage(const char *pStr, BOOL bOEM);
char *my_strstr(char *a, char *b);
unsigned int my_atoi(char *s);

void __declspec(dllexport) Exec(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) {
  g_hwndParent=hwndParent;
  EXDLL_INIT();
  {
    ExecScript(0);
  }
}

void __declspec(dllexport) ExecToLog(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) {
  g_hwndParent=hwndParent;
  EXDLL_INIT();
  {
    ExecScript(1);
  }
}

void __declspec(dllexport) ExecToStack(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) {
  g_hwndParent=hwndParent;
  EXDLL_INIT();
  {
    ExecScript(2);
  }
}

HINSTANCE g_hInst;
BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
  g_hInst = hInst;
  return TRUE;
}

#define TAB_REPLACE "        "
#define TAB_REPLACE_SIZE (sizeof(TAB_REPLACE)-1)

// Turn a pair of chars into a word
// Turn four chars into a dword
#ifdef __BIG_ENDIAN__ // Not very likely, but, still...
#define CHAR2_TO_WORD(a,b) (((WORD)(b))|((a)<<8))
#define CHAR4_TO_DWORD(a,b,c,d)	(((DWORD)CHAR2_TO_WORD(c,d))|(CHAR2_TO_WORD(a,b)<<16))
#else
#define CHAR2_TO_WORD(a,b) (((WORD)(a))|((b)<<8))
#define CHAR4_TO_DWORD(a,b,c,d)	(((DWORD)CHAR2_TO_WORD(a,b))|(CHAR2_TO_WORD(c,d)<<16))
#endif

void ExecScript(int log) {
  char szRet[128] = "";
  char *pExec;
  int nComSpecSize;
  char meDLLPath[MAX_PATH];    
  char *p;
  char *executor;
  char *g_exec;
  unsigned int g_to;
  BOOL bOEM;

  nComSpecSize = GetModuleFileName(g_hInst, meDLLPath, MAX_PATH) + 2; // 2 chars for quotes
  p = meDLLPath + nComSpecSize - 2; // point p at null char of meDLLPath
  g_exec = (char *)GlobalAlloc(GPTR, sizeof(char)*g_stringsize+nComSpecSize+2); // 1 for space, 1 for null
  *g_exec = '"';
  executor = g_exec + 1;

  do
  {
    if (*p == '\\')
      break;
    p = CharPrev(meDLLPath, p);
  }
  while (p > meDLLPath);
  if (p == meDLLPath)
  {
    // bad path
    pushstring("error");
    GlobalFree(g_exec);
    return;
  }

  *p = 0;
  GetTempFileName(meDLLPath, "ns", 0, executor);
  *p = '\\';
  if (CopyFile(meDLLPath, executor, FALSE))
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
      pNTHeaders->FileHeader.Characteristics = IMAGE_FILE_32BIT_MACHINE | IMAGE_FILE_LOCAL_SYMS_STRIPPED | 
        IMAGE_FILE_LINE_NUMS_STRIPPED | IMAGE_FILE_EXECUTABLE_IMAGE;
      pNTHeaders->OptionalHeader.Subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI;
      pNTHeaders->OptionalHeader.AddressOfEntryPoint = (DWORD)WinMain - (DWORD)g_hInst;  
      UnmapViewOfFile(pMapView);
    }
    CloseHandle(hMapping);
    CloseHandle(hFile);
  }

  lstrcat(g_exec, "\"");

  g_to = 0;      // default is no timeout
  bOEM = FALSE;  // default is no OEM->ANSI conversion

  g_hwndList = NULL;
  if (g_hwndParent)
    g_hwndList = FindWindowEx(FindWindowEx(g_hwndParent,NULL,"#32770",NULL),NULL,"SysListView32",NULL);

  // add space
  pExec = g_exec + lstrlen(g_exec);
  *pExec = ' ';
  pExec++;

params:
  popstring(pExec);
  if (my_strstr(pExec, "/TIMEOUT=") == pExec) {
    char *szTimeout = pExec + 9;
    g_to = my_atoi(szTimeout);
    *pExec = 0;
    goto params;
  }
  if (!lstrcmpi(pExec, "/OEM")) {
    bOEM = TRUE;
    *pExec = 0;
    goto params;
  }

  if (!pExec[0]) 
  {
    pushstring("error");
    *(pExec-2) = '\0'; // skip space and quote
    DeleteFile(executor);
    GlobalFree(g_exec);
    return;
  }
  
  {
    STARTUPINFO si={sizeof(si),};
    SECURITY_ATTRIBUTES sa={sizeof(sa),};
    SECURITY_DESCRIPTOR sd={0,};
    PROCESS_INFORMATION pi={0,};
    OSVERSIONINFO osv={sizeof(osv)};
    HANDLE newstdout=0,read_stdout=0;
    HANDLE newstdin=0,read_stdin=0;
    DWORD dwRead = 1;
    DWORD dwExit = !STILL_ACTIVE;
    DWORD dwLastOutput;
    static char szBuf[1024];
    HGLOBAL hUnusedBuf;
    char *szUnusedBuf = 0;

    if (log) {
      hUnusedBuf = GlobalAlloc(GHND, log & 2 ? g_stringsize : sizeof(szBuf)*4);
      if (!hUnusedBuf) {
        lstrcpy(szRet, "error");
        goto done;
      }
      szUnusedBuf = (char *)GlobalLock(hUnusedBuf);
    }

    GetVersionEx(&osv);
    if (osv.dwPlatformId == VER_PLATFORM_WIN32_NT) {
      InitializeSecurityDescriptor(&sd,SECURITY_DESCRIPTOR_REVISION);
      SetSecurityDescriptorDacl(&sd,true,NULL,false);
      sa.lpSecurityDescriptor = &sd;
    }
    else 
      sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = true;
    if (!CreatePipe(&read_stdout,&newstdout,&sa,0)) {
      lstrcpy(szRet, "error");
      goto done;
    }
    if (!CreatePipe(&read_stdin,&newstdin,&sa,0)) {
      lstrcpy(szRet, "error");
      goto done;
    }

    GetStartupInfo(&si);
    si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
    si.wShowWindow = SW_HIDE;
    si.hStdInput = newstdin;
    si.hStdOutput = newstdout;
    si.hStdError = newstdout;
    if (!CreateProcess(NULL,g_exec,NULL,NULL,TRUE,CREATE_NEW_CONSOLE,NULL,NULL,&si,&pi)) {
      lstrcpy(szRet, "error");
      goto done;
    }

    dwLastOutput = GetTickCount();

    while (dwExit == STILL_ACTIVE || dwRead) {
      PeekNamedPipe(read_stdout, 0, 0, 0, &dwRead, NULL);
      if (dwRead) {
        dwLastOutput = GetTickCount();
        ReadFile(read_stdout, szBuf, sizeof(szBuf)-1, &dwRead, NULL);
        szBuf[dwRead] = 0;
        if (log) {
          char *p, *p2;
          SIZE_T iReqLen = lstrlen(szBuf) + lstrlen(szUnusedBuf);
          if (GlobalSize(hUnusedBuf) < iReqLen && (iReqLen < g_stringsize || !(log & 2))) {
            GlobalUnlock(hUnusedBuf);
            hUnusedBuf = GlobalReAlloc(hUnusedBuf, iReqLen+sizeof(szBuf), GHND);
            if (!hUnusedBuf) {
              lstrcpy(szRet, "error");
              break;
            }
            szUnusedBuf = (char *)GlobalLock(hUnusedBuf);
          }
          p = szUnusedBuf; // get the old left overs
          if (iReqLen < g_stringsize || !(log & 2)) lstrcat(p, szBuf);
          else {
            lstrcpyn(p + lstrlen(p), szBuf, g_stringsize - lstrlen(p));
          }

          if (!(log & 2)) {
            while (p = my_strstr(p, "\t")) {
              if ((int)(p - szUnusedBuf) > (int)(GlobalSize(hUnusedBuf) - TAB_REPLACE_SIZE - 1))
              {
                *p++ = ' ';
              }
              else
              {
                int len = lstrlen(p);
                char *c_out=(char*)p+TAB_REPLACE_SIZE+len;
                char *c_in=(char *)p+len;
                while (len-- > 0) {
                  *c_out--=*c_in--;
                }

                lstrcpy(p, TAB_REPLACE);
                p += TAB_REPLACE_SIZE;
                *p = ' ';
              }
            }
            
            p = szUnusedBuf; // get the old left overs
            for (p2 = p; *p2;) {
              if (*p2 == '\r') {
                *p2++ = 0;
                continue;
              }
              if (*p2 == '\n') {
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
              char *p2 = szUnusedBuf;
              while (*p) *p2++ = *p++;
              *p2 = 0;
            }
          }
        }
      }
      else {
        if (g_to && GetTickCount() > dwLastOutput+g_to) {
          TerminateProcess(pi.hProcess, -1);
          lstrcpy(szRet, "timeout");
        }
        else Sleep(LOOPTIMEOUT);
      }
      GetExitCodeProcess(pi.hProcess, &dwExit);
      if (dwExit != STILL_ACTIVE) {
        PeekNamedPipe(read_stdout, 0, 0, 0, &dwRead, NULL);
      }
    }
done:
    if (log & 2) pushstring(szUnusedBuf);
    if (log & 1 && *szUnusedBuf) LogMessage(szUnusedBuf, bOEM);
    if ( dwExit == STATUS_ILLEGAL_INSTRUCTION )
      lstrcpy(szRet, "error");
    if (!szRet[0]) wsprintf(szRet,"%d",dwExit);
    pushstring(szRet);
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
    CloseHandle(newstdout);
    CloseHandle(read_stdout);
    CloseHandle(newstdin);
    CloseHandle(read_stdin);
    *(pExec-2) = '\0'; // skip space and quote
    DeleteFile(executor);
    GlobalFree(g_exec);
    if (log) {
      GlobalUnlock(hUnusedBuf);
      GlobalFree(hUnusedBuf);
    }
  }
}

// Tim Kosse's LogMessage
void LogMessage(const char *pStr, BOOL bOEM) {
  LVITEM item={0};
  int nItemCount;
  if (!g_hwndList) return;
  //if (!lstrlen(pStr)) return;
  if (bOEM == TRUE) OemToCharBuff(pStr, (char *)pStr, lstrlen(pStr));
  nItemCount=SendMessage(g_hwndList, LVM_GETITEMCOUNT, 0, 0);
  item.mask=LVIF_TEXT;
  item.pszText=(char *)pStr;
  item.cchTextMax=0;
  item.iItem=nItemCount;
  ListView_InsertItem(g_hwndList, &item);
  ListView_EnsureVisible(g_hwndList, item.iItem, 0);
}

char *my_strstr(char *a, char *b)
{
  int l = lstrlen(b);
  while (lstrlen(a) >= l)
  {
    char c = a[l];
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

unsigned int my_atoi(char *s) {
  unsigned int v=0;
  if (*s == '0' && (s[1] == 'x' || s[1] == 'X')) {
    s+=2;
    for (;;) {
      int c=*s++;
      if (c >= '0' && c <= '9') c-='0';
      else if (c >= 'a' && c <= 'f') c-='a'-10;
      else if (c >= 'A' && c <= 'F') c-='A'-10;
      else break;
      v<<=4;
      v+=c;
    }
  }
  else if (*s == '0' && s[1] <= '7' && s[1] >= '0') {
    s++;
    for (;;) {
      int c=*s++;
      if (c >= '0' && c <= '7') c-='0';
      else break;
      v<<=3;
      v+=c;
    }
  }
  else {
    for (;;) {
      int c=*s++ - '0';
      if (c < 0 || c > 9) break;
      v*=10;
      v+=c;
    }
  }
  return (int)v;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
  DWORD               Ret;
  STARTUPINFO         si   = {0};
  PROCESS_INFORMATION pi   = {0};
  char command_line[1024];
  char seekchar=' ';
  char *cmdline;
  
  si.cb = sizeof(si);
  // Make child process use this app's standard files. Not needed because the handles
  // we created when executing this process were inheritable.
  //si.dwFlags    = STARTF_USESTDHANDLES;
  //si.hStdInput  = GetStdHandle (STD_INPUT_HANDLE);
  //si.hStdOutput = GetStdHandle (STD_OUTPUT_HANDLE);
  //si.hStdError  = GetStdHandle (STD_ERROR_HANDLE);
  lstrcpyn(command_line, GetCommandLine(), 1024);
  
  cmdline = command_line;
  if (*cmdline == '\"') seekchar = *cmdline++;

  while (*cmdline && *cmdline != seekchar) cmdline=CharNext(cmdline);
  cmdline=CharNext(cmdline);
  // skip any spaces before the arguments
  while (*cmdline && *cmdline == ' ') cmdline++;

  Ret = CreateProcess (NULL, cmdline,
    NULL, NULL,
    TRUE, 0,
    NULL, NULL,
    &si, &pi
    );

  if (Ret)
  {
    do
    {
      GetExitCodeProcess(pi.hProcess, &Ret);
      Sleep(LOOPTIMEOUT);
    } while ( Ret == STILL_ACTIVE );
    CloseHandle (pi.hProcess);
    CloseHandle (pi.hThread);
    return Ret;
  }
  else
    return STATUS_ILLEGAL_INSTRUCTION;
}
