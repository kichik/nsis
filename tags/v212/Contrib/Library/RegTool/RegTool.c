#include <windows.h>

#define STR_SIZE 1024

void RunSelf(char cmd, char *file);
void RegDll(char *file);
void RegTypeLib(char *file);
void DeleteFileOnReboot(char *pszFile);

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
  char *cmdline;
  char seekchar = ' ';

  cmdline = GetCommandLine();
  if (*cmdline == '\"')
    seekchar = *cmdline++;

  while (*cmdline && *cmdline != seekchar)
    cmdline = CharNext(cmdline);
  cmdline = CharNext(cmdline);
  while (*cmdline == ' ')
    cmdline++;

  if (*cmdline++ != '/')
  {
    ExitProcess(1);
    return 0;
  }

  if (*cmdline == 'S')
  {
    HKEY rootkey;

    if (SUCCEEDED(RegOpenKeyEx(HKEY_LOCAL_MACHINE, "Software\\NSIS.Library.RegTool.v2", 0, KEY_READ, &rootkey)))
    {
      char keyname[STR_SIZE];

      while (RegEnumKey(rootkey, 0, keyname, STR_SIZE) == ERROR_SUCCESS)
      {
        HKEY key;

        if (SUCCEEDED(RegOpenKeyEx(rootkey, keyname, 0, KEY_READ, &key)))
        {
          DWORD t, count, l = sizeof(DWORD);

          if (SUCCEEDED(RegQueryValueEx(key, "count", NULL, &t, (LPBYTE) &count, &l)) && t == REG_DWORD)
          {
            DWORD j;
            char valname[128], mode[2], file[STR_SIZE];

            for (j = 1; j <= count; j++)
            {
              wsprintf(valname, "%u.mode", j);
              l = sizeof(mode);
              if (FAILED(RegQueryValueEx(key, valname, NULL, &t, mode, &l)) || t != REG_SZ)
                continue;

              wsprintf(valname, "%u.file", j);
              l = STR_SIZE;
              if (FAILED(RegQueryValueEx(key, valname, NULL, &t, file, &l)) || t != REG_SZ)
                continue;

              RunSelf(mode[0], file);
            }
          }

          RegCloseKey(key);
          RegDeleteKey(rootkey, keyname);
        }
      }

      RegCloseKey(rootkey);
      RegDeleteKey(HKEY_LOCAL_MACHINE, "Software\\NSIS.Library.RegTool.v2");
    }

    {
      char file[STR_SIZE];
      if (GetModuleFileName(GetModuleHandle(NULL), file, STR_SIZE))
      {
        DeleteFileOnReboot(file);
      }
    }
  }
  else
  {
    SetErrorMode(SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS);
    OleInitialize(NULL);

    if (*cmdline == 'D')
    {
      RegDll(cmdline + 1);
    }
    else if (*cmdline == 'T')
    {
      RegTypeLib(cmdline + 1);
    }

    OleUninitialize();
    SetErrorMode(0);
  }

  ExitProcess(0);
  return 0;
}

void RunSelf(char cmd, char *file)
{
  char self[STR_SIZE];
  char cmdline[STR_SIZE];

  if (!*file || (cmd != 'D' && cmd != 'T'))
    return;

  if (GetModuleFileName(GetModuleHandle(NULL), self, STR_SIZE))
  {
    PROCESS_INFORMATION pi;
    STARTUPINFO si = { sizeof(STARTUPINFO) };

    wsprintf(cmdline, "\"%s\" /%c%s", self, cmd, file);

    if (CreateProcess(NULL, cmdline, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi))
    {
      CloseHandle(pi.hThread);

      WaitForSingleObject(pi.hProcess, INFINITE);

      CloseHandle(pi.hProcess);
    }
  }
}

void RegDll(char *file)
{
  HMODULE mod = LoadLibraryEx(file, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
  if (mod)
  {
    FARPROC regfunc = GetProcAddress(mod, "DllRegisterServer");
    if (regfunc)
      regfunc();
    FreeLibrary(mod);
  }
}

void RegTypeLib(char *file)
{
  WCHAR wfile[STR_SIZE];

  if (MultiByteToWideChar(CP_ACP, 0, file, -1, wfile, STR_SIZE) != 0)
  {
    ITypeLib* tlib;
    if (SUCCEEDED(LoadTypeLib(wfile, &tlib))) {
      RegisterTypeLib(tlib, wfile, NULL);
      tlib->lpVtbl->Release(tlib);
    }
  }
}

char *mystrstri(char *a, char *b)
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

void mini_memcpy(void *out, const void *in, int len)
{
  char *c_out=(char*)out;
  char *c_in=(char *)in;
  while (len-- > 0)
  {
    *c_out++=*c_in++;
  }
}

void DeleteFileOnReboot(char *pszFile)
{
  BOOL fOk = 0;
  HMODULE hLib=GetModuleHandle("KERNEL32.dll");
  if (hLib)
  {
    typedef BOOL (WINAPI *mfea_t)(LPCSTR lpExistingFileName,LPCSTR lpNewFileName,DWORD dwFlags);
    mfea_t mfea;
    mfea=(mfea_t) GetProcAddress(hLib,"MoveFileExA");
    if (mfea)
    {
      fOk=mfea(pszFile, NULL, MOVEFILE_DELAY_UNTIL_REBOOT);
    }
  }

  if (!fOk)
  {
    static char szRenameLine[1024];
    static char wininit[1024];
    int cchRenameLine;
    char *szRenameSec = "[Rename]\r\n";
    HANDLE hfile, hfilemap;
    DWORD dwFileSize, dwRenameLinePos;

    int spn;

    // wininit is used as a temporary here
    spn = GetShortPathName(pszFile,wininit,1024);
    if (!spn || spn > 1024)
      return;
    cchRenameLine = wsprintf(szRenameLine,"NUL=%s\r\n",wininit);

    GetWindowsDirectory(wininit, 1024-16);
    lstrcat(wininit, "\\wininit.ini");
    hfile = CreateFile(wininit,
        GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_ALWAYS,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL);

    if (hfile != INVALID_HANDLE_VALUE)
    {
      dwFileSize = GetFileSize(hfile, NULL);
      hfilemap = CreateFileMapping(hfile, NULL, PAGE_READWRITE, 0, dwFileSize + cchRenameLine + 10, NULL);

      if (hfilemap != NULL)
      {
        LPSTR pszWinInit = (LPSTR) MapViewOfFile(hfilemap, FILE_MAP_WRITE, 0, 0, 0);

        if (pszWinInit != NULL)
        {
          LPSTR pszRenameSecInFile = mystrstri(pszWinInit, szRenameSec);
          if (pszRenameSecInFile == NULL)
          {
            lstrcpy(pszWinInit+dwFileSize, szRenameSec);
            dwFileSize += 10;
            dwRenameLinePos = dwFileSize;
          }
          else
          {
            char *pszFirstRenameLine = pszRenameSecInFile+10;
            char *pszNextSec = mystrstri(pszFirstRenameLine,"\n[");
            if (pszNextSec)
            {
              char *p = ++pszNextSec;
              while (p < pszWinInit + dwFileSize) {
                p[cchRenameLine] = *p;
                p++;
              }

              dwRenameLinePos = pszNextSec - pszWinInit;
            }
            // rename section is last, stick item at end of file
            else dwRenameLinePos = dwFileSize;
          }

          mini_memcpy(&pszWinInit[dwRenameLinePos], szRenameLine, cchRenameLine);
          dwFileSize += cchRenameLine;

          UnmapViewOfFile(pszWinInit);
        }
        CloseHandle(hfilemap);
      }
      SetFilePointer(hfile, dwFileSize, NULL, FILE_BEGIN);
      SetEndOfFile(hfile);
      CloseHandle(hfile);
    }
  }
}
