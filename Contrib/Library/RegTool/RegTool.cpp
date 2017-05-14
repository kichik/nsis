// RegTool v3
// Unicode support by Jim Park & Olivier Marcoux

#include "../../../Source/Platform.h"
#include <windows.h>

#define STR_SIZE 1024


/*
All A/W functions need this ugly hack so we can call them in template functions.
Everything is implemented in template functions because the x86 version needs to 
call the W functions on NT and the A functions on 9x.
The macros assume that "T" is a [W]CHAR type.
*/
#define MKSTR(str) ( sizeof(T) > 1 ? (const T*) L##str : (const T*) str )
#ifdef UNICODE
#define CALL(func) hack::func##W<T>
#else
#define CALL(func) hack::func##A<T>
#endif
#define CALL_wsprintf (sizeof(T) > 1 ? (int(*)(T*,const T*,...)) wsprintfW : (int(*)(T*,const T*,...)) wsprintfA)
namespace hack {
// Allow cast from [const] T* to [W]CHAR* but nothing else (losing constness is acceptable).
inline WCHAR* WP(const WCHAR*p) { return (WCHAR*) p; }
inline WCHAR* WP(const CHAR*p) { return (WCHAR*) p; }
inline CHAR* NP(const WCHAR*p) { return (CHAR*) p; }
inline CHAR* NP(const CHAR*p) { return (CHAR*) p; }
// These function names rely on the C preprocessor & PSDK macros to append A/W to their name so they can be found by the CALL macro.
template<class T> T* GetCommandLine() { return sizeof(T) > 1 ? (T*) ::GetCommandLineW() : (T*) ::GetCommandLineA(); }
template<class T> T* CharNext(const T*p1) { return sizeof(T) > 1 ? (T*) ::CharNextW(WP(p1)) : (T*) ::CharNextA(NP(p1)); }
template<class T> T* lstrcpy(T*p1,const T*p2) { return sizeof(T) > 1 ? (T*) ::lstrcpyW(WP(p1),WP(p2)) : (T*) ::lstrcpyA(NP(p1),NP(p2)); }
template<class T> T* lstrcat(T*p1,const T*p2) { return sizeof(T) > 1 ? (T*) ::lstrcatW(WP(p1),WP(p2)) : (T*) ::lstrcatA(NP(p1),NP(p2)); }
template<class T> LONG RegEnumKey(HKEY p1, DWORD p2, T*p3,DWORD p4) { return sizeof(T) > 1 ? ::RegEnumKeyW(p1,p2,WP(p3),p4) : ::RegEnumKeyA(p1,p2,NP(p3),p4); }
template<class T> LONG RegOpenKeyEx(HKEY p1,const T*p2,DWORD p3,REGSAM p4,PHKEY p5) { return sizeof(T) > 1 ? ::RegOpenKeyExW(p1,WP(p2),p3,p4,p5) : ::RegOpenKeyExA(p1,NP(p2),p3,p4,p5); }
template<class T> LONG RegQueryValueEx(HKEY p1,const T*p2,LPDWORD p3,LPDWORD p4,LPBYTE p5,LPDWORD p6) { return sizeof(T) > 1 ? ::RegQueryValueExW(p1,WP(p2),p3,p4,p5,p6) : ::RegQueryValueExA(p1,NP(p2),p3,p4,p5,p6); }
template<class T> LONG RegDeleteKey(HKEY p1,const T*p2) { return sizeof(T) > 1 ? ::RegDeleteKeyW(p1,WP(p2)) : ::RegDeleteKeyA(p1,NP(p2)); }
template<class T> DWORD GetModuleFileName(HMODULE p1,T*p2,DWORD p3) { return sizeof(T) > 1 ? ::GetModuleFileNameW(p1,WP(p2),p3) : ::GetModuleFileNameA(p1,NP(p2),p3); }
template<class T> HMODULE LoadLibraryEx(const T*p1,void*p2,DWORD p3) { return sizeof(T) > 1 ? ::LoadLibraryExW(WP(p1),p2,p3) : ::LoadLibraryExA(NP(p1),p2,p3); }
template<class T> UINT GetWindowsDirectory(const T*p1,UINT p2) { return sizeof(T) > 1 ? ::GetWindowsDirectoryW(WP(p1),p2) : ::GetWindowsDirectoryA(NP(p1),p2); }
template<class T> UINT GetSystemDirectory(const T*p1,UINT p2) { return sizeof(T) > 1 ? ::GetSystemDirectoryW(WP(p1),p2) : ::GetSystemDirectoryA(NP(p1),p2); }
template<class T> DWORD GetShortPathName(const T*p1,const T*p2,DWORD p3) { return sizeof(T) > 1 ? ::GetShortPathNameW(WP(p1),WP(p2),p3) : ::GetShortPathNameA(NP(p1),NP(p2),p3); }
template<class T> DWORD GetFileAttributes(const T*p1) { return sizeof(T) > 1 ? ::GetFileAttributesW(WP(p1)) : ::GetFileAttributesA(NP(p1)); }
template<class T> BOOL MoveFileEx(const T*p1,const T*p2,DWORD p3) { return sizeof(T) > 1 ? ::MoveFileExW(WP(p1),WP(p2),p3) : ::MoveFileExA(NP(p1),NP(p2),p3); }
template<class T> HANDLE CreateFile(const T*p1,DWORD p2,DWORD p3,LPSECURITY_ATTRIBUTES p4,DWORD p5,DWORD p6,HANDLE p7) { return sizeof(T) > 1 ? ::CreateFileW(WP(p1),p2,p3,p4,p5,p6,p7) : ::CreateFileA(NP(p1),p2,p3,p4,p5,p6,p7); }
template<class T> BOOL CreateProcess(const T*p1,const T*p2,LPSECURITY_ATTRIBUTES p3,LPSECURITY_ATTRIBUTES p4,BOOL p5,DWORD p6,LPVOID p7,const T*p8,STARTUPINFO*p9,LPPROCESS_INFORMATION p10) { return sizeof(T) > 1 ? ::CreateProcessW(WP(p1),WP(p2),p3,p4,p5,p6,p7,WP(p8),(STARTUPINFOW*)p9,p10) : ::CreateProcessA(NP(p1),NP(p2),p3,p4,p5,p6,p7,NP(p8),(STARTUPINFOA*)p9,p10); }
}


static bool IsWinNT()
{
#if defined(_WIN64) || (defined(_M_ARM) || defined(__arm__))
  return true;
#else
  LPCWSTR str = L"count"; // Using this string because it's already used in other parts of the code
  return CharNextW(str) != NULL;
#endif
}

void SafeWow64EnableWow64FsRedirection(BYTE EnableFsRedirection)
{
#ifndef _WIN64
  HMODULE hK32 = LoadLibraryA("KERNEL32");
  FARPROC proc = GetProcAddress(hK32, "Wow64EnableWow64FsRedirection");
  if (proc)
  {
    typedef BYTE WINNTBOOLEAN;
    typedef WINNTBOOLEAN (WINAPI*W64EW64FSR)(WINNTBOOLEAN);
    W64EW64FSR Wow64EnableWow64FsRedirectionFunc = (W64EW64FSR) proc;
    Wow64EnableWow64FsRedirectionFunc(EnableFsRedirection);
  }
#endif
}

char *mystrstriA(char *a, const char *b)
{
  int l = lstrlenA(b);
  while (lstrlenA(a) >= l)
  {
    char c = a[l];
    a[l] = 0;
    if (!lstrcmpiA(a, b))
    {
      a[l] = c;
      return a;
    }
    a[l] = c;
    a = CharNextA(a);
  }
  return NULL;
}

void mini_memcpy(void *out, const void *in, int len)
{
  char *c_out=(char*)out;
  char *c_in=(char *)in;
  while (len-- > 0)
  {
    *c_out++=*c_in++;
  }
}

template<class T> static HANDLE myOpenFile(const T *fn, DWORD da, DWORD cd)
{
  DWORD attr = CALL(GetFileAttributes)(fn), share = FILE_SHARE_READ;
  return CALL(CreateFile)(fn, da, share, NULL, cd, attr == INVALID_FILE_ATTRIBUTES ? 0 : attr, NULL);
}

#ifndef _WIN64
/** Modifies the wininit.ini file to rename / delete a file.
 *
 * @param prevName The previous / current name of the file.
 * @param newName The new name to move the file to.  If NULL, the current file
 * will be deleted.
 */
template<class T> void RenameViaWininit(const T* prevName, const T* newName) // Note: Not thread safe!
{
  static char szRenameLine[1024];
  static T wininit[1024];
  static T tmpbuf[1024];

  int cchRenameLine;
  LPCSTR szRenameSec = "[Rename]\r\n"; // rename section marker
  HANDLE hfile;
  DWORD dwFileSize;
  DWORD dwBytes, dwRenameLinePos;
  char *pszWinInit;   // Contains the file contents of wininit.ini

  int spn;   // length of the short path name in TCHARs.

  CALL(lstrcpy)(tmpbuf, MKSTR("NUL"));

  if (newName) {
    // create the file if it's not already there to prevent GetShortPathName from failing
    CloseHandle(myOpenFile(newName,0,CREATE_NEW));
    spn = CALL(GetShortPathName)(newName,tmpbuf,COUNTOF(tmpbuf));
    if (!spn || spn > 1024)
      return;
  }
  // wininit is used as a temporary here
  spn = CALL(GetShortPathName)(prevName,wininit,COUNTOF(wininit));
  if (!spn || spn > 1024)
    return;
#ifdef _UNICODE
  cchRenameLine = wsprintfA(szRenameLine, "%S=%S\r\n", tmpbuf, wininit);
#else
  cchRenameLine = wsprintfA(szRenameLine, "%s=%s\r\n", tmpbuf, wininit);
#endif
  // Get the path to the wininit.ini file.
  CALL(GetWindowsDirectory)(wininit, COUNTOF(wininit)-16);
  CALL(lstrcat)(wininit, MKSTR("\\wininit.ini"));

  hfile = myOpenFile(wininit, GENERIC_READ | GENERIC_WRITE, OPEN_ALWAYS);

  if (hfile != INVALID_HANDLE_VALUE)
  {
    // We are now working on the Windows wininit file
    dwFileSize = GetFileSize(hfile, NULL);
    pszWinInit = (char*) GlobalAlloc(GPTR, dwFileSize + cchRenameLine + 10);

    if (pszWinInit != NULL)
    {
      if (ReadFile(hfile, pszWinInit, dwFileSize, &dwBytes, NULL) && dwFileSize == dwBytes)
      {
        // Look for the rename section in the current file.
        LPSTR pszRenameSecInFile = mystrstriA(pszWinInit, szRenameSec);
        if (pszRenameSecInFile == NULL)
        {
          // No rename section.  So we add it to the end of file.
          lstrcpyA(pszWinInit+dwFileSize, szRenameSec);
          dwFileSize += 10;
          dwRenameLinePos = dwFileSize;
        }
        else
        {
          // There is a rename section, but is there another section after it?
          char *pszFirstRenameLine = pszRenameSecInFile+10;
          char *pszNextSec = mystrstriA(pszFirstRenameLine,"\n[");
          if (pszNextSec)
          {
            char *p = pszWinInit + dwFileSize;
            char *pEnd = pszWinInit + dwFileSize + cchRenameLine;

            while (p > pszNextSec)
            {
              *pEnd-- = *p--;
            }

            dwRenameLinePos = BUGBUG64TRUNCATE(DWORD, pszNextSec - pszWinInit) + 1; // +1 for the \n
          }
          // rename section is last, stick item at end of file
          else dwRenameLinePos = dwFileSize;
        }

        mini_memcpy(&pszWinInit[dwRenameLinePos], szRenameLine, cchRenameLine);
        dwFileSize += cchRenameLine;

        SetFilePointer(hfile, 0, NULL, FILE_BEGIN);
        WriteFile(hfile, pszWinInit, dwFileSize, &dwBytes, NULL);

        GlobalFree(pszWinInit);
      }
    }
    
    CloseHandle(hfile);
  }
}
#endif

template<class T> BOOL DeleteFileOnReboot(const T *pszFile)
{
  BOOL fOk = CALL(MoveFileEx)(pszFile, NULL, MOVEFILE_DELAY_UNTIL_REBOOT);
#ifndef _WIN64
  if (!fOk && sizeof(T) == 1)
  {
    RenameViaWininit(pszFile, (const T*)NULL);
    fOk = TRUE; // BUGBUG: We just pretend everything is OK, nobody checks our return value anyway
  }
#endif
  return fOk;
}

template<class T> void RegFile(T cmd, const T *file, BOOL x64)
{
  T *self; // These are allocated on the heap to avoid _chkstk
  T *cmdline;
  int ready = 0;

  if (!*file || (cmd != _T('D') && cmd != _T('T') && cmd != _T('E')))
    return;

  self = (T*) GlobalAlloc(GPTR, sizeof(T)*STR_SIZE);
  cmdline = (T*) GlobalAlloc(GPTR, sizeof(T)*STR_SIZE);

  if (cmd == ('E'))
  {
    CALL_wsprintf(cmdline, MKSTR("\"%s\" /regserver"), file);
    ready++;
  }
  else if (!x64)
  {
    if (CALL(GetModuleFileName)(GetModuleHandle(NULL), self, STR_SIZE))
    {
      CALL_wsprintf(cmdline, MKSTR("\"%s\" /%c%s"), self, cmd, file);
      ready++;
    }
  }
  else
  {
    if (CALL(GetSystemDirectory)(self, STR_SIZE))
    {
      CALL_wsprintf(cmdline, MKSTR("\"%s\\regsvr32.exe\" /s \"%s\""), self, file);
      ready++;

      SafeWow64EnableWow64FsRedirection(FALSE);
    }
  }

  if (ready)
  {
    PROCESS_INFORMATION pi;
    BYTE sibuf[sizeof(T) > 1 ? sizeof(STARTUPINFOW) : sizeof(STARTUPINFOA)] = {0,};
    STARTUPINFO &si = (STARTUPINFO&) sibuf;
    si.cb = sizeof(sibuf);

    if (CALL(CreateProcess)(NULL, cmdline, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi))
    {
      WaitForSingleObject(pi.hProcess, INFINITE);
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    }
    
    if (x64)
    {
      SafeWow64EnableWow64FsRedirection(TRUE);
    }
  }

  GlobalFree(self);
  GlobalFree(cmdline);
}

template<class T> void RegDll(const T *file)
{
  HMODULE mod = CALL(LoadLibraryEx)(file, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
  if (mod)
  {
    FARPROC regfunc = GetProcAddress(mod, "DllRegisterServer");
    if (regfunc)
      regfunc();
    FreeLibrary(mod);
  }
}

template<class T> void RegTypeLib(T *file)
{
  WCHAR wbuf[sizeof(T) > 1 ? 1 : STR_SIZE]; // Buffer only used by ANSI implementation!
  WCHAR *wfile = wbuf; // Not const because of RegisterTypeLib in old PSDK
  if (sizeof(T) > 1)
    wfile = (WCHAR*) file;
  else
  {
    if (MultiByteToWideChar(CP_ACP, 0, (const CHAR*) file, -1, wbuf, STR_SIZE) == 0)
      return;
  }

  ITypeLib* tlib;
  if (SUCCEEDED(LoadTypeLib(wfile, &tlib)))
  {
    RegisterTypeLib(tlib, wfile, NULL);
    tlib->Release();
  }
}

template<class T> int RegTool()
{
  T *cmdline;
  T seekchar = (' ');

  cmdline = CALL(GetCommandLine)();
  if (*cmdline == ('\"'))
    seekchar = *cmdline++;

  while (*cmdline && *cmdline != seekchar)
    cmdline = CALL(CharNext)(cmdline);
  cmdline = CALL(CharNext)(cmdline);
  while (*cmdline == (' '))
    cmdline++;

  if (*cmdline++ != ('/'))
  {
    return 1;
  }

  if (*cmdline == ('S'))
  {
    HKEY rootkey;
    T *keyname, *file; // These are allocated on the heap to avoid _chkstk
    keyname = (T*) GlobalAlloc(GPTR, STR_SIZE*sizeof(T));
    file    = (T*) GlobalAlloc(GPTR, STR_SIZE*sizeof(T));

    if (SUCCEEDED(RegOpenKeyExA(HKEY_LOCAL_MACHINE, "Software\\NSIS.Library.RegTool.v3", 0, KEY_READ, &rootkey)))
    {
      DWORD keyidx = 0;
      while (CALL(RegEnumKey)(rootkey, keyidx, keyname, STR_SIZE) == ERROR_SUCCESS)
      {
        HKEY key;

        if (SUCCEEDED(CALL(RegOpenKeyEx)(rootkey, keyname, 0, KEY_READ, &key)))
        {
          DWORD t, count, l = sizeof(DWORD);

          if (SUCCEEDED(CALL(RegQueryValueEx)(key, MKSTR("count"), NULL, &t, (LPBYTE) &count, &l)) && t == REG_DWORD)
          {
            DWORD j;
            T valname[128], mode[3];

            for (j = 0; ++j <= count;)
            {
              CALL_wsprintf(valname, MKSTR("%u.mode"), j);
              l = sizeof(mode);
              if (FAILED(CALL(RegQueryValueEx)(key, valname, NULL, &t, (LPBYTE) mode, &l)) || t != REG_SZ)
                continue;

              CALL_wsprintf(valname, MKSTR("%u.file"), j);
              l = STR_SIZE*sizeof(T);
              if (FAILED(CALL(RegQueryValueEx)(key, valname, NULL, &t, (LPBYTE) file, &l)) || t != REG_SZ)
                continue;

              // JP: Note, if this mode[1] is used as anything but a boolean later on,
              // we'll need to consider the next line carefully.
              RegFile(mode[0], file, mode[1] == 'X');
            }
          }

          RegCloseKey(key);
          
          CALL(RegDeleteKey)(rootkey, keyname);
        }
        keyidx++; // Must increment this so we don't loop forever if a non-admin accidentally executes RegTool /S
      }

      RegCloseKey(rootkey);
      RegDeleteKeyA(HKEY_LOCAL_MACHINE, "Software\\NSIS.Library.RegTool.v3");
    }

    {
      if (CALL(GetModuleFileName)(GetModuleHandle(NULL), file, STR_SIZE))
      {
        DeleteFileOnReboot(file);
      }
    }
    GlobalFree(keyname);
    GlobalFree(file);
  }
  else
  {
    DWORD orgerrmode = SetErrorMode(SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS);
    OleInitialize(NULL);

    if (*cmdline == ('D'))
    {
      RegDll(cmdline + 1);
    }
    else if (*cmdline == ('T'))
    {
      RegTypeLib(cmdline + 1);
    }

    OleUninitialize();
    SetErrorMode(orgerrmode);
  }

  return 0;
}

NSIS_ENTRYPOINT_GUINOCRT
EXTERN_C void NSISWinMainNOCRT()
{
  int ec = IsWinNT() ? RegTool<WCHAR>() : RegTool<CHAR>();
  ExitProcess(ec);
}
