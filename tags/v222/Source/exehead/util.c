/*
 * util.c
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2006 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "../Platform.h"
#include <shellapi.h>
#include "util.h"
#include "state.h"
#include "config.h"
#include "lang.h"
#include "fileform.h"
#include "exec.h"
#include "ui.h"

#ifdef NSIS_CONFIG_LOG
#if !defined(NSIS_CONFIG_LOG_ODS) && !defined(NSIS_CONFIG_LOG_STDOUT)
char g_log_file[1024];
#endif
#endif

// *** DO NOT DECLARE MORE VARIABLES INSIDE THIS PRAGMAS ***
// This will produce a special section called ".ndata" (stands for nsis data)
// this way makensis during build time, can search for this section by name
// and change the virtual size of this section
// which result in extra memory for extra variables without code to do allocation :)
// nsis then removes the "DISCARDABLE" style from section (for safe)
#ifdef _MSC_VER
#  pragma bss_seg(NSIS_VARS_SECTION)
NSIS_STRING g_usrvars[1];
#  pragma bss_seg()
#  pragma comment(linker, "/section:" NSIS_VARS_SECTION ",rwd")
#else
#  ifdef __GNUC__
NSIS_STRING g_usrvars[1] __attribute__((section (NSIS_VARS_SECTION)));
#  else
#    error Unknown compiler. You must implement the seperate PE section yourself.
#  endif
#endif

void NSISCALL FreePIDL(LPITEMIDLIST idl)
{
  IMalloc *m;
  SHGetMalloc(&m);
  if (m)
  {
    m->lpVtbl->Free(m, idl);
    m->lpVtbl->Release(m);
  }
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
//  return my_SetWindowText(GetDlgItem(dlg, idx), val);
}

int NSISCALL my_GetDialogItemText(UINT idx, char *val)
{
  extern HWND m_curwnd;
  return GetDlgItemText(m_curwnd, idx, val, NSIS_MAX_STRLEN);
//  return my_GetWindowText(GetDlgItem(m_curwnd, idx), val, NSIS_MAX_STRLEN);
}

int NSISCALL my_MessageBox(const char *text, UINT type) {
  int _type = type & 0x001FFFFF;
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  // default for silent installers
  if (g_exec_flags.silent && type >> 21)
    return type >> 21;
#endif
  // no silent or no default, just show
  if (g_exec_flags.rtl)
    _type ^= MB_RIGHT | MB_RTLREADING;
  return MessageBox(g_hwnd, text, g_caption, _type);
}

void NSISCALL myDelete(char *buf, int flags)
{
  static char lbuf[NSIS_MAX_STRLEN];

  HANDLE h;
  WIN32_FIND_DATA fd;
  char *fn;
  int valid_dir=is_valid_instpath(buf);

  if ((flags & DEL_SIMPLE))
  {
    g_exec_flags.exec_error += !DeleteFile(buf);
    return;
  }

#ifdef NSIS_SUPPORT_RMDIR
  if (!(flags & DEL_DIR) || (valid_dir && (flags & DEL_RECURSE)))
#endif//NSIS_SUPPORT_RMDIR
  {
    mystrcpy(lbuf,buf);
#ifdef NSIS_SUPPORT_RMDIR
    if (flags & DEL_DIR)
      mystrcat(lbuf,"\\*.*");
    else
#endif//NSIS_SUPPORT_RMDIR
      trimslashtoend(buf);

    mystrcat(buf,"\\");

    fn=buf+mystrlen(buf);

    h = FindFirstFile(lbuf,&fd);
    if (h != INVALID_HANDLE_VALUE)
    {
      do
      {
        char *fdfn = fd.cFileName;
        if (*findchar(fdfn, '?') && *fd.cAlternateFileName)
          // name contains unicode, use short name
          fdfn = fd.cAlternateFileName;

#ifdef NSIS_SUPPORT_RMDIR
        if (fdfn[0] == '.' && !fdfn[1]) continue;
        if (fdfn[0] == '.' && fdfn[1] == '.' && !fdfn[2]) continue;
#endif//NSIS_SUPPORT_RMDIR
        {
          mystrcpy(fn,fdfn);
          if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
          {
#ifdef NSIS_SUPPORT_RMDIR
            if ((flags & (DEL_DIR | DEL_RECURSE)) == (DEL_DIR | DEL_RECURSE))
            {
              myDelete(buf,flags);
            }
#endif//NSIS_SUPPORT_RMDIR
          }
          else
          {
            log_printf2("Delete: DeleteFile(\"%s\")",buf);
            remove_ro_attr(buf);
            if (!DeleteFile(buf))
            {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
              if (flags & DEL_REBOOT)
              {
                log_printf2("Delete: DeleteFile on Reboot(\"%s\")",buf);
                update_status_text(LANG_DELETEONREBOOT,buf);
                MoveFileOnReboot(buf,NULL);
              }
              else
#endif//NSIS_SUPPORT_MOVEONREBOOT
              {
                log_printf2("Delete: DeleteFile failed(\"%s\")",buf);
                g_exec_flags.exec_error++;
              }
            }
            else
              update_status_text(LANG_DELETEFILE,buf);
          }
        }
      } while (FindNextFile(h,&fd));
      FindClose(h);
    }

#ifdef NSIS_SUPPORT_RMDIR
    if (flags & DEL_DIR)
      fn[-1]=0;
#endif//NSIS_SUPPORT_RMDIR
  }

#ifdef NSIS_SUPPORT_RMDIR
  if ((flags & DEL_DIR))
  {
    if (!valid_dir)
    {
      log_printf2("RMDir: RemoveDirectory invalid input(\"%s\")",buf);
      g_exec_flags.exec_error++;
    }
    else if (file_exists(buf))
    {
      addtrailingslash(buf);
      log_printf2("RMDir: RemoveDirectory(\"%s\")",buf);
      remove_ro_attr(buf);
      if (!RemoveDirectory(buf))
      {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
        if (flags & DEL_REBOOT)
        {
          log_printf2("RMDir: RemoveDirectory on Reboot(\"%s\")",buf);
          update_status_text(LANG_DELETEONREBOOT,buf);
          MoveFileOnReboot(buf,NULL);
        }
        else
#endif//NSIS_SUPPORT_MOVEONREBOOT
        {
          log_printf2("RMDir: RemoveDirectory failed(\"%s\")",buf);
          g_exec_flags.exec_error++;
        }
      }
      else
      {
        update_status_text(LANG_REMOVEDIR,buf);
      }
    }
  }
#endif//NSIS_SUPPORT_RMDIR
}

char *NSISCALL addtrailingslash(char *str)
{
  if (lastchar(str)!='\\') mystrcat(str,"\\");
  return str;
}

/*char NSISCALL lastchar(const char *str)
{
  return *CharPrev(str,str+mystrlen(str));
}*/

char * NSISCALL findchar(char *str, char c)
{
  while (*str && *str != c)
  {
    str = CharNext(str);
  }
  return str;
}

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
  return ((*(WORD*)ubuf==CHAR2_TO_WORD('\\','\\')) || (dl >= 'a' && dl <= 'z' && ubuf[1]==':'));
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
      p2 = findchar(p2, '\\');
      if (!*p2)
        return NULL;
      p2++; // skip backslash
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

  // must be called after skip_root or AllowRootDirInstall won't work.
  // validate_filename removes trailing blackslashes and so converts
  // "C:\" to "C:" which is not a valid directory. skip_root returns
  // NULL for "C:" so the above test returns 0.
  // validate_filename is called so directories such as "C:\ " will
  // not pass as a valid non-root directory.
  validate_filename(root);

  if ((g_flags & CH_FLAGS_NO_ROOT_DIR) && (!*root || *root == '\\'))
    return 0;

  while (mystrlen(tmp) > root - tmp)
  {
    WIN32_FIND_DATA *fd = file_exists(tmp);
    // if the directory bit not set then it's a file, which is not a valid inst dir...
    // GetFileAttributes is not used because it doesn't work with certain files (error 32)
    // as for concerns of the user using * or ?, that's invalid anyway...
    if (fd && !(fd->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
      return 0;
    trimslashtoend(tmp);
  }

  // if the root drive exists
  addtrailingslash(tmp); // don't check the current directory, check the root directory
  if (GetFileAttributes(tmp) == INVALID_FILE_ATTRIBUTES)
    return 0;

  return 1;
}

char * NSISCALL mystrstri(char *a, char *b)
{
  int l = mystrlen(b);
  while (mystrlen(a) >= l)
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

void NSISCALL mini_memcpy(void *out, const void *in, int len)
{
  char *c_out=(char*)out;
  char *c_in=(char *)in;
  while (len-- > 0)
  {
    *c_out++=*c_in++;
  }
}

void NSISCALL remove_ro_attr(char *file)
{
  int attr = GetFileAttributes(file);
  if (attr != INVALID_FILE_ATTRIBUTES)
    SetFileAttributes(file,attr&(~FILE_ATTRIBUTE_READONLY));
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
    char prefix[4];
    *(LPDWORD)prefix = CHAR4_TO_DWORD('n', 's', 'a', 0);
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
  typedef BOOL (WINAPI *mfea_t)(LPCSTR lpExistingFileName,LPCSTR lpNewFileName,DWORD dwFlags);
  mfea_t mfea;
  mfea=(mfea_t) myGetProcAddress("KERNEL32.dll","MoveFileExA");
  if (mfea)
  {
    fOk=mfea(pszExisting, pszNew, MOVEFILE_DELAY_UNTIL_REBOOT|MOVEFILE_REPLACE_EXISTING);
  }

  if (!fOk)
  {
    static char szRenameLine[1024];
    static char wininit[1024];
    static char tmpbuf[1024];
    int cchRenameLine;
    char *szRenameSec = "[Rename]\r\n";
    HANDLE hfile;
    DWORD dwFileSize;
    DWORD dwBytes;
    DWORD dwRenameLinePos;
    char *pszWinInit;

    int spn;

    *(DWORD*)tmpbuf = CHAR4_TO_DWORD('N', 'U', 'L', 0);

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
    mystrcat(wininit, "\\wininit.ini");
    hfile = myOpenFile(wininit, GENERIC_READ | GENERIC_WRITE, OPEN_ALWAYS);

    if (hfile != INVALID_HANDLE_VALUE)
    {
      dwFileSize = GetFileSize(hfile, NULL);
      pszWinInit = GlobalAlloc(GPTR, dwFileSize + cchRenameLine + 10);

      if (pszWinInit != NULL)
      {
        if (ReadFile(hfile, pszWinInit, dwFileSize, &dwBytes, NULL) && dwFileSize == dwBytes)
        {
          LPSTR pszRenameSecInFile = mystrstri(pszWinInit, szRenameSec);
          if (pszRenameSecInFile == NULL)
          {
            mystrcpy(pszWinInit+dwFileSize, szRenameSec);
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

          SetFilePointer(hfile, 0, NULL, FILE_BEGIN);
          WriteFile(hfile, pszWinInit, dwFileSize, &dwBytes, NULL);

          GlobalFree(pszWinInit);
        }
      }
      
      CloseHandle(hfile);
    }
  }

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
  static const char c[] = "%d";
  wsprintf(s,c,d);
}

int NSISCALL myatoi(char *s)
{
  unsigned int v=0;
  int sign=1; // sign of positive
  char m=10; // base of 10
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
  return lstrcpyn(out, in, NSIS_MAX_STRLEN);
}

int NSISCALL mystrlen(const char *in)
{
  return lstrlen(in);
}

char * NSISCALL mystrcat(char *out, const char *concat)
{
  return lstrcat(out, concat);
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
    if (nVarIdx > NS_CODES_START)
    {
      nData = ((in[1] & 0x7F) << 7) | (in[0] & 0x7F);
      fldrs[0] = in[0]; // current user
      fldrs[1] = in[0] | CSIDL_FLAG_CREATE;
      fldrs[2] = in[1]; // all users
      fldrs[3] = in[1] | CSIDL_FLAG_CREATE;
      in += 2;

      if (nVarIdx == NS_SHELL_CODE)
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
        else if (fldrs[0] == CSIDL_PROGRAM_FILES_COMMON)
        {
          myRegGetStr(HKEY_LOCAL_MACHINE, SYSREGKEY, "CommonFilesDir", out);
        }
        else if (fldrs[0] == CSIDL_PROGRAM_FILES)
        {
          myRegGetStr(HKEY_LOCAL_MACHINE, SYSREGKEY, "ProgramFilesDir", out);
          if (!*out)
            mystrcpy(out, "C:\\Program Files");
        }
        else if (fldrs[0] == CSIDL_SYSTEM)
        {
          GetSystemDirectory(out, NSIS_MAX_STRLEN);
        }
        else if (fldrs[0] == CSIDL_WINDOWS)
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
            BOOL res = SHGetPathFromIDList(idl, out);
            FreePIDL(idl);
            if (res)
            {
              break;
            }
          }
          else
            *out=0;
        }

        if (*out && append)
        {
          mystrcat(out, append);
        }

        validate_filename(out);
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
      } // == VAR_CODES_START
      else if (nVarIdx == NS_LANG_CODE)
      {
        GetNSISString(out, -nData-1);
      }
      out += mystrlen(out);
    }
    else if (nVarIdx == NS_SKIP_CODE)
    {
      *out++ = *in++;
    }
    else // Normal char
    {
      *out++ = nVarIdx;
    }
  } // while
  *out = 0;
  if (outbuf)
    return mystrcpy(outbuf, ps_tmpbuf);
  return ps_tmpbuf;
}

void NSISCALL validate_filename(char *in) {
  char *nono = "*?|<>/\":";
  char *out;
  char *out_save;

  // ignoring spaces is wrong, " C:\blah" is invalid
  //while (*in == ' ') in = CharNext(in);

  if (in[0] == '\\' && in[1] == '\\' && in[2] == '?' && in[3] == '\\')
  {
    // at least four bytes
    in += 4;
  }
  if (*in)
  {
    // at least two bytes
    if (validpathspec(in)) in += 2;
  }
  out = out_save = in;
  while (*in)
  {
    if ((unsigned char)*in > 31 && !*findchar(nono, *in))
    {
      mini_memcpy(out, in, CharNext(in) - in);
      out = CharNext(out);
    }
    in = CharNext(in);
  }
  *out = 0;
  do
  {
    out = CharPrev(out_save, out);
    if (*out == ' ' || *out == '\\')
      *out = 0;
    else
      break;
  } while (out_save < out);
}

#ifdef NSIS_CONFIG_LOG
int log_dolog;
char log_text[2048]; // 1024 for each wsprintf

#if !defined(NSIS_CONFIG_LOG_ODS) && !defined(NSIS_CONFIG_LOG_STDOUT)
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
      mystrcat(log_text,"\r\n");
      WriteFile(fp,log_text,mystrlen(log_text),&d,NULL);
    }
  }
}
#endif//!NSIS_CONFIG_LOG_ODS && !NSIS_CONFIG_LOG_STDOUT

const char * _RegKeyHandleToName(HKEY hKey)
{
  if (hKey == HKEY_CLASSES_ROOT)
    return "HKEY_CLASSES_ROOT";
  else if (hKey == HKEY_CURRENT_USER)
    return "HKEY_CURRENT_USER";
  else if (hKey == HKEY_LOCAL_MACHINE)
    return "HKEY_LOCAL_MACHINE";
  else if (hKey == HKEY_USERS)
    return "HKEY_USERS";
  else if (hKey == HKEY_PERFORMANCE_DATA)
    return "HKEY_PERFORMANCE_DATA";
  else if (hKey == HKEY_CURRENT_CONFIG)
    return "HKEY_CURRENT_CONFIG";
  else if (hKey == HKEY_DYN_DATA)
    return "HKEY_DYN_DATA";
  else
    return "invalid registry key";
}

void _LogData2Hex(char *buf, size_t buflen, unsigned char *data, size_t datalen)
{
  char *p = buf;

  size_t i;

  int dots = 0;
  size_t bufbytes = buflen / 3; // 2 hex digits, one space/null

  if (datalen > bufbytes)
  {
    bufbytes--;
    dots = 1;
  }
  else
    bufbytes = datalen;

  for (i = 0; i < bufbytes; i++)
  {
    wsprintf(p, "%02x%c", data[i], (i == bufbytes - 1) ? '\0' : ' ');
    p += 3;
  }

  if (dots)
    mystrcat(buf, "...");
}

#ifdef NSIS_CONFIG_LOG_TIMESTAMP
void log_timestamp(char *buf)
{
  SYSTEMTIME st;
  GetLocalTime(&st);
  wsprintf(buf,"[%04hu/%02hu/%02hu %02hu:%02hu:%02hu] ", st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond);
}
#else
#  define log_timestamp(x)
#endif//NSIS_CONFIG_LOG_TIMESTAMP

void log_printf(char *format, ...)
{
  va_list val;
  va_start(val,format);

  log_text[0] = '\0';
  log_timestamp(log_text);
  wvsprintf(log_text+mystrlen(log_text),format,val);

  va_end(val);
#ifdef NSIS_CONFIG_LOG_ODS
  if (log_dolog)
    OutputDebugString(log_text);
#endif
#ifdef NSIS_CONFIG_LOG_STDOUT
  if (log_dolog && GetStdHandle(STD_OUTPUT_HANDLE) != INVALID_HANDLE_VALUE)
  {
    DWORD dwBytes;
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), log_text, lstrlen(log_text), &dwBytes, NULL);
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), "\n", 1, &dwBytes, NULL);
  }
#endif
#if !defined(NSIS_CONFIG_LOG_ODS) && !defined(NSIS_CONFIG_LOG_STDOUT)
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

void * NSISCALL myGetProcAddress(char *dll, char *func)
{
  HMODULE hModule = GetModuleHandle(dll);
  if (!hModule)
    hModule = LoadLibrary(dll);
  if (!hModule)
    return NULL;

  return GetProcAddress(hModule, func);
}

void NSISCALL MessageLoop(UINT uCheckedMsg)
{
  MSG msg;
  while (PeekMessage(&msg, NULL, uCheckedMsg, uCheckedMsg, PM_REMOVE))
    DispatchMessage(&msg);
}
