#include <windows.h>
#include <shellapi.h>
#include "util.h"
#include "state.h"
#include "config.h"
#include "lang.h"
#include "exec.h"

#include "fileform.h"
#include "ui.h"

#ifdef NSIS_CONFIG_LOG
#ifndef NSIS_CONFIG_LOG_ODS
char g_log_file[1024];
#endif
#endif

// *** DO NOT DECLARE MORE VARIABLES INSIDE THIS PRAGMAS ***
// This will produce a special section called ".ndata" (stands for nsis data)
// this way makensis during build time, can search for this section by name
// and change the virtual size of this section
// which result in extra memory for extra variables without code to do allocation :)
// nsis then removes the "DISCARDABLE" style from section (for safe)
#pragma bss_seg( VARS_SECTION_NAME )
NSIS_STRING g_usrvars[1];
#pragma bss_seg()
#define SECTION_VARS_RWD "/section:" ## VARS_SECTION_NAME ## ",rwd"
#pragma comment(linker, SECTION_VARS_RWD)

#ifndef INVALID_FILE_ATTRIBUTES
#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)
#endif

int NSISCALL my_PIDL2Path(char *out, LPITEMIDLIST idl)
{
  int Res;
  IMalloc *m;
  SHGetMalloc(&m);
  Res = SHGetPathFromIDList(idl, out);
  if (m)
  {
    m->lpVtbl->Free(m, idl);
    m->lpVtbl->Release(m);
  }
  return Res;
}

HANDLE NSISCALL myCreateProcess(char *cmd, char *dir)
{
  DWORD d;
  PROCESS_INFORMATION ProcInfo;
  static STARTUPINFO StartUp;
  StartUp.cb=sizeof(StartUp);
  d=GetFileAttributes(dir);
  if (d == INVALID_FILE_ATTRIBUTES || !(d&FILE_ATTRIBUTE_DIRECTORY))
    dir=0;
  if (!CreateProcess(NULL, cmd, NULL, NULL, FALSE, 0, NULL, dir, &StartUp, &ProcInfo))
    return NULL;
  CloseHandle(ProcInfo.hThread);
  return ProcInfo.hProcess;
}

/*BOOL NSISCALL my_SetWindowText(HWND hWnd, const char *val)
{
  return SendMessage(hWnd,WM_SETTEXT,0,(LPARAM)val);
}*/

BOOL NSISCALL my_SetDialogItemText(HWND dlg, UINT idx, const char *val)
{
  return SetDlgItemText(dlg,idx,val);
  //return my_SetWindowText(GetDlgItem(dlg,idx),val);
}

int NSISCALL my_GetDialogItemText(UINT idx, char *val)
{
  extern HWND m_curwnd;
  return GetDlgItemText(m_curwnd, idx, val, NSIS_MAX_STRLEN);
}

int NSISCALL my_MessageBox(const char *text, UINT type) {
  int _type = type & 0x000FFFFF;
  // default for silent installers
  if (g_exec_flags.silent && type >> 20)
    return type >> 20;
  // no silent or no default, just show
  if (g_exec_flags.rtl)
    _type ^= MB_RIGHT | MB_RTLREADING;
  return MessageBox(g_hwnd, text, g_caption, _type);
}

void * NSISCALL my_GlobalAlloc(DWORD dwBytes) {
  return (void *)GlobalAlloc(GPTR, dwBytes);
}

#ifdef NSIS_SUPPORT_RMDIR
void NSISCALL doRMDir(char *buf, int flags) // 1 - recurse, 2 - rebootok
{
  if (is_valid_instpath(buf))
  {
    if (flags&1) {
      SHFILEOPSTRUCT op;

      op.hwnd=g_hwnd;
      op.wFunc=FO_DELETE;
      buf[mystrlen(buf)+1]=0;
      op.pFrom=buf;
      op.pTo=0;

      op.fFlags=FOF_NOERRORUI|FOF_SILENT|FOF_NOCONFIRMATION;

      SHFileOperation(&op);
    }
#ifdef NSIS_SUPPORT_MOVEONREBOOT
    else if (!RemoveDirectory(buf) && flags&2) {
      log_printf2("Remove folder on reboot: %s",buf);
      MoveFileOnReboot(buf,0);
    }
#else
    else RemoveDirectory(buf);
#endif
  }
  log_printf2("RMDir: RemoveDirectory(\"%s\")",buf);
}
#endif//NSIS_SUPPORT_RMDIR

char *NSISCALL addtrailingslash(char *str)
{
  if (lastchar(str)!='\\') lstrcat(str,"\\");
  return str;
}

/*char NSISCALL lastchar(const char *str)
{
  return *CharPrev(str,str+mystrlen(str));
}*/

void NSISCALL trimslashtoend(char *buf)
{
  char *p = buf + mystrlen(buf);
  do
  {
    if (*p == '\\')
      break;
    p = CharPrev(buf, p);
  } while (p > buf);

  *p = 0;
}

int NSISCALL validpathspec(char *ubuf)
{
  char dl = ubuf[0] | 0x20; // convert alleged drive letter to lower case
  return ((*(WORD*)ubuf==CHAR2_TO_WORD('\\','\\')) || (dl >= 'a' && dl <= 'z' && *CharNext(ubuf)==':'));
}

char * NSISCALL skip_root(char *path)
{
  char *p = CharNext(path);
  char *p2 = CharNext(p);

  if (*path && *(WORD*)p == CHAR2_TO_WORD(':', '\\'))
  {
    return CharNext(p2);
  }
  else if (*(WORD*)path == CHAR2_TO_WORD('\\','\\'))
  {
    // skip host and share name
    int x = 2;
    while (x--)
    {
      while (*p2 != '\\')
      {
        if (!*p2)
          return NULL;
        p2 = CharNext(p2);
      }
      p2 = CharNext(p2);
    }

    return p2;
  }
  else
    return NULL;
}

int NSISCALL is_valid_instpath(char *s)
{
  static char tmp[NSIS_MAX_STRLEN];
  char *root;
  
  mystrcpy(tmp, s);

  root = skip_root(tmp);

  if (!root)
    return 0;

  if ((g_flags & CH_FLAGS_NO_ROOT_DIR) && (!*root || *root == '\\'))
    return 0;

  while (mystrlen(tmp) > root - tmp)
  {
    WIN32_FIND_DATA *fd = file_exists(tmp);
    // if the directory bit not set then it's a file, which is not a valid inst dir...
    // GetFileAttributes is not used because it doesn't work with certain files (error 32)
    // as for concers of the user using * or ?, that's invalid anyway...
    if (fd && !(fd->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
      return 0;
    trimslashtoend(tmp);
  }

  // if the root drive exists
  if (GetFileAttributes(tmp) == (DWORD)-1)
    return 0;

  return 1;
}

char * NSISCALL mystrstr(char *a, char *b)
{
  int len_of_a = mystrlen(a) - mystrlen(b);
  while (*a && len_of_a >= 0)
  {
    char *t=a,*u=b;
    while (*t && *t == *u)
    {
      t++;
      u++;
    }
    if (!*u) return a;
    a++;
    len_of_a--;
  }
  return NULL;
}


void * NSISCALL mini_memcpy(void *out, const void *in, int len)
{
  char *c_out=(char*)out;
  char *c_in=(char *)in;
  while (len-- > 0)
  {
    *c_out++=*c_in++;
  }
  return out;
}


HANDLE NSISCALL myOpenFile(const char *fn, DWORD da, DWORD cd)
{
  int attr = GetFileAttributes(fn);
  return CreateFile(
    fn,
    da,
    FILE_SHARE_READ,
    NULL,
    cd,
    attr == INVALID_FILE_ATTRIBUTES ? 0 : attr,
    NULL
  );
}

char * NSISCALL my_GetTempFileName(char *buf, const char *dir)
{
  int n = 100;
  while (n--)
  {
    char prefix[4] = "nsa";
    prefix[2] += (char)(GetTickCount() % 26);
    if (GetTempFileName(dir, prefix, 0, buf))
      return buf;
  }
  *buf = 0;
  return 0;
}

#ifdef NSIS_SUPPORT_MOVEONREBOOT
void NSISCALL MoveFileOnReboot(LPCTSTR pszExisting, LPCTSTR pszNew)
{
  BOOL fOk = 0;
  HMODULE hLib=GetModuleHandle("kernel32.dll");
  if (hLib)
  {
    typedef BOOL (WINAPI *mfea_t)(LPCSTR lpExistingFileName,LPCSTR lpNewFileName,DWORD dwFlags);
    mfea_t mfea;
    mfea=(mfea_t) GetProcAddress(hLib,"MoveFileExA");
    if (mfea)
    {
      fOk=mfea(pszExisting, pszNew, MOVEFILE_DELAY_UNTIL_REBOOT|MOVEFILE_REPLACE_EXISTING);
    }
  }

  if (!fOk)
  {
    static char szRenameLine[1024];
    static char wininit[1024];
    static char tmpbuf[1024];
    int cchRenameLine;
    char *szRenameSec = "[Rename]\r\n";
    HANDLE hfile, hfilemap;
    DWORD dwFileSize, dwRenameLinePos;

    int spn;

    *((int *)tmpbuf) = *((int *)"NUL");

    if (pszNew) {
      // create the file if it's not already there to prevent GetShortPathName from failing
      CloseHandle(myOpenFile(pszNew,0,CREATE_NEW));
      spn = GetShortPathName(pszNew,tmpbuf,1024);
      if (!spn || spn > 1024)
        return;
    }
    // wininit is used as a temporary here
    spn = GetShortPathName(pszExisting,wininit,1024);
    if (!spn || spn > 1024)
      return;
    cchRenameLine = wsprintf(szRenameLine,"%s=%s\r\n",tmpbuf,wininit);

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
          LPSTR pszRenameSecInFile = mystrstr(pszWinInit, szRenameSec);
          if (pszRenameSecInFile == NULL)
          {
            mystrcpy(pszWinInit+dwFileSize, szRenameSec);
            dwFileSize += 10;
            dwRenameLinePos = dwFileSize;
          }
          else
          {
            char *pszFirstRenameLine = pszRenameSecInFile+10;
            char *pszNextSec = mystrstr(pszFirstRenameLine,"\n[");
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

          //fOk++;
        }
        CloseHandle(hfilemap);
      }
      SetFilePointer(hfile, dwFileSize, NULL, FILE_BEGIN);
      SetEndOfFile(hfile);
      CloseHandle(hfile);
    }
  }
  //return fOk;

#ifdef NSIS_SUPPORT_REBOOT
  g_exec_flags.exec_reboot++;
#endif
}
#endif

void NSISCALL myRegGetStr(HKEY root, const char *sub, const char *name, char *out)
{
  HKEY hKey;
  *out=0;
  if (RegOpenKeyEx(root,sub,0,KEY_READ,&hKey) == ERROR_SUCCESS)
  {
    DWORD l = NSIS_MAX_STRLEN;
    DWORD t;
    if (RegQueryValueEx(hKey,name,NULL,&t,out,&l ) != ERROR_SUCCESS || (t != REG_SZ && t != REG_EXPAND_SZ)) *out=0;
    out[NSIS_MAX_STRLEN-1]=0;
    RegCloseKey(hKey);
  }
}

void NSISCALL myitoa(char *s, int d)
{
  wsprintf(s,"%d",d);
}

int NSISCALL myatoi(char *s)
{
  unsigned int v=0;
  int sign=1; // sign of positive
  char m=10; // base of 0
  char t='9'; // cap top of numbers at 9

  if (*s == '-')
  {
    s++;  //skip over -
    sign=-1; // sign flip
  }

  if (*s == '0')
  {
    s++; // skip over 0
    if (s[0] >= '0' && s[0] <= '7')
    {
      m=8; // base of 8
      t='7'; // cap top at 7
    }
    if ((s[0] & ~0x20) == 'X')
    {
      m=16; // base of 16
      s++; // advance over 'x'
    }
  }

  for (;;)
  {
    int c=*s++;
    if (c >= '0' && c <= t) c-='0';
    else if (m==16 && (c & ~0x20) >= 'A' && (c & ~0x20) <= 'F') c = (c & 7) + 9;
    else break;
    v*=m;
    v+=c;
  }
  return ((int)v)*sign;
}

// Straight copies of selected shell functions.  Calling local functions
// requires less code than DLL functions.  For the savings to outweigh the cost
// of a new function there should be about a couple of dozen or so calls.
char * NSISCALL mystrcpy(char *out, const char *in)
{
  return lstrcpy(out, in);
}

int NSISCALL mystrlen(const char *in)
{
  return lstrlen(in);
}

char ps_tmpbuf[NSIS_MAX_STRLEN*2];

#define SYSREGKEY "Software\\Microsoft\\Windows\\CurrentVersion"

// Based on Dave Laundon's simplified process_string
char * NSISCALL GetNSISString(char *outbuf, int strtab)
{
  char *in = (char*)GetNSISStringNP(GetNSISTab(strtab));
  char *out = ps_tmpbuf;
  if ((unsigned int) (outbuf - ps_tmpbuf) < sizeof(ps_tmpbuf))
  {
    out = outbuf;
    outbuf = 0;
  }
  while (*in && out - ps_tmpbuf < NSIS_MAX_STRLEN)
  {
    unsigned char nVarIdx = (unsigned char)*in++;
    int nData;
    int fldrs[4];
    if (nVarIdx > NS_SKIP_CODE)
    {
      nData = ((in[1] & 0x7F) << 7) | (in[0] & 0x7F);
      fldrs[0] = in[0]; // current user
      fldrs[1] = in[0] | CSIDL_FLAG_CREATE;
      fldrs[2] = in[1]; // all users
      fldrs[3] = in[1] | CSIDL_FLAG_CREATE;
      in += 2;
    }
    if (nVarIdx == NS_SKIP_CODE)
    {
      *out++ = *in++;
    }
    else if (nVarIdx == NS_SHELL_CODE)
    {
      // NOTE 1: the code CSIDL_PRINTERS, is used for QUICKLAUNCH
      // NOTE 2: the code CSIDL_BITBUCKET is used for COMMONFILES
      // NOTE 3: the code CSIDL_CONTROLS is used for PROGRAMFILES
      LPITEMIDLIST idl;
      char *append = 0;

      int x = 0;

      *out = 0;

      if (fldrs[2] == CSIDL_PRINTERS) // QUICKLAUNCH
      {
        append = "\\Microsoft\\Internet Explorer\\Quick Launch";
        x = 2;
      }
      if (fldrs[0] == CSIDL_PROGRAM_FILES_COMMON)
      {
        myRegGetStr(HKEY_LOCAL_MACHINE, SYSREGKEY, "CommonFilesDir", out);
      }
      if (fldrs[0] == CSIDL_PROGRAM_FILES)
      {
        myRegGetStr(HKEY_LOCAL_MACHINE, SYSREGKEY, "ProgramFilesDir", out);
        if (!*out)
          mystrcpy(out, "C:\\Program Files");
      }
      if (fldrs[0] == CSIDL_SYSTEM)
      {
        GetSystemDirectory(out, NSIS_MAX_STRLEN);
      }
      if (fldrs[0] == CSIDL_WINDOWS)
      {
        GetWindowsDirectory(out, NSIS_MAX_STRLEN);
      }

      if (!*out)
      {
        x = 4;
        if (!g_exec_flags.all_user_var)
          x = 2;
      }

      while (x--)
      {
        if (!SHGetSpecialFolderLocation(g_hwnd, fldrs[x], &idl))
        {
          if (my_PIDL2Path(out, idl))
          {
            break;
          }
        }
        else
          *out=0;
      }

      if (*out && append)
      {
        lstrcat(out, append);
      }

      validate_filename(out);
      out += mystrlen(out);
    }
    else if (nVarIdx == NS_VAR_CODE)
    {
      if (nData == 27) // HWNDPARENT
        myitoa(out, (unsigned int) g_hwnd);
      else
        mystrcpy(out, g_usrvars[nData]);
      // validate the directory name
      if ((unsigned int)(nData - 21) < 6) {
        // validate paths for $INSTDIR, $OUTDIR, $EXEDIR, $LANGUAGE, $TEMP and $PLUGINSDIR
        // $LANGUAGE is just a number anyway...
        validate_filename(out);
      }
      out += mystrlen(out);
    } // == VAR_CODES_START
    else if (nVarIdx == NS_LANG_CODE)
    {
      GetNSISString(out, -nData-1);
      out += mystrlen(out);
    }
    else // Normal char
    {
      *out++ = nVarIdx;
    }
  } // while
  *out = 0;
  if (outbuf)
    return lstrcpyn(outbuf, ps_tmpbuf, NSIS_MAX_STRLEN);
  return ps_tmpbuf;
}

char * NSISCALL validate_filename(char *in) {
  char *nono = "*?|<>/\":";
  short cur_char = 0;
  char *out;
  char *out_save;
  while (*in == ' ') in=CharNext(in);
  if (in[0] == '\\' && in[1] == '\\' && in[2] == '?' && in[3] == '\\') {
    // at least four bytes
    in += 4;
  }
  if (*in) {
    // at least two bytes
    if (validpathspec(in)) in += 2;
  }
  out = out_save = in;
  while (*(char*)&cur_char = *in) {
    if (cur_char > 31 && !mystrstr(nono, (char*)&cur_char)) {
      mini_memcpy(out, in, CharNext(in) - in);
      out = CharNext(out);
    }
    in = CharNext(in);
  }
  do {
    *out = 0;
    --out;
  } while (out_save <= out && (*out == ' ' || *out == '\\'));
  return out_save;
}

#ifdef NSIS_CONFIG_LOG
int log_dolog;
char log_text[NSIS_MAX_STRLEN*4];

#ifndef NSIS_CONFIG_LOG_ODS
void NSISCALL log_write(int close)
{
  static HANDLE fp=INVALID_HANDLE_VALUE;
  if (close)
  {
    if (fp!=INVALID_HANDLE_VALUE)
    {
      CloseHandle(fp);
    }
    fp=INVALID_HANDLE_VALUE;
    return;
  }
  if (log_dolog)
  {
    if (g_log_file[0] && fp==INVALID_HANDLE_VALUE)
    {
      fp = myOpenFile(g_log_file,GENERIC_WRITE,OPEN_ALWAYS);
      if (fp!=INVALID_HANDLE_VALUE)
        SetFilePointer(fp,0,NULL,FILE_END);
    }
    if (fp!=INVALID_HANDLE_VALUE)
    {
      DWORD d;
      lstrcat(log_text,"\r\n");
      WriteFile(fp,log_text,mystrlen(log_text),&d,NULL);
    }
  }
}
#endif//!NSIS_CONFIG_LOG_ODS

void log_printf(char *format, ...)
{
  va_list val;
  va_start(val,format);
  wvsprintf(log_text,format,val);
  va_end(val);
#ifdef NSIS_CONFIG_LOG_ODS
  if (log_dolog)
    OutputDebugString(log_text);
#else
  log_write(0);
#endif
}
#endif//NSIS_CONFIG_LOG

WIN32_FIND_DATA * NSISCALL file_exists(char *buf)
{
  HANDLE h;
  static WIN32_FIND_DATA fd;
  // Avoid a "There is no disk in the drive" error box on empty removable drives
  SetErrorMode(SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS);
  h = FindFirstFile(buf,&fd);
  SetErrorMode(0);
  if (h != INVALID_HANDLE_VALUE)
  {
    FindClose(h);
    return &fd;
  }
  return NULL;
}
