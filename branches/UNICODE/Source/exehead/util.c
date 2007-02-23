/*
 * util.c
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
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
TCHAR g_log_file[1024];
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

HANDLE NSISCALL myCreateProcess(TCHAR *cmd, TCHAR *dir)
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

BOOL NSISCALL my_SetDialogItemText(HWND dlg, UINT idx, const TCHAR *val)
{
  return SetDlgItemText(dlg,idx,val);
//  return my_SetWindowText(GetDlgItem(dlg, idx), val);
}

int NSISCALL my_GetDialogItemText(UINT idx, TCHAR *val)
{
  extern HWND m_curwnd;
  return GetDlgItemText(m_curwnd, idx, val, NSIS_MAX_STRLEN);
//  return my_GetWindowText(GetDlgItem(m_curwnd, idx), val, NSIS_MAX_STRLEN);
}

int NSISCALL my_MessageBox(const TCHAR *text, UINT type) {
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

void NSISCALL myDelete(TCHAR *buf, int flags)
{
  static TCHAR lbuf[NSIS_MAX_STRLEN];

  HANDLE h;
  WIN32_FIND_DATA fd;
  TCHAR *fn;
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
      mystrcat(lbuf, TEXT("\\*.*"));
    else
#endif//NSIS_SUPPORT_RMDIR
      trimslashtoend(buf);

    mystrcat(buf, TEXT("\\"));

    fn=buf+mystrlen(buf);

    h = FindFirstFile(lbuf,&fd);
    if (h != INVALID_HANDLE_VALUE)
    {
      do
      {
        TCHAR *fdfn = fd.cFileName;
        if (*findchar(fdfn, TEXT('?')) && *fd.cAlternateFileName)
          // name contains unicode, use short name
          fdfn = fd.cAlternateFileName;

#ifdef NSIS_SUPPORT_RMDIR
        if (fdfn[0] == TEXT('.') && !fdfn[1]) continue;
        if (fdfn[0] == TEXT('.') && fdfn[1] == TEXT('.') && !fdfn[2]) continue;
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
            log_printf2(TEXT("Delete: DeleteFile(\"%s\")"),buf);
            remove_ro_attr(buf);
            if (!DeleteFile(buf))
            {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
              if (flags & DEL_REBOOT)
              {
                log_printf2(TEXT("Delete: DeleteFile on Reboot(\"%s\")"),buf);
                update_status_text(LANG_DELETEONREBOOT,buf);
                MoveFileOnReboot(buf,NULL);
              }
              else
#endif//NSIS_SUPPORT_MOVEONREBOOT
              {
                log_printf2(TEXT("Delete: DeleteFile failed(\"%s\")"),buf);
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
      log_printf2(TEXT("RMDir: RemoveDirectory invalid input(\"%s\")"),buf);
      g_exec_flags.exec_error++;
    }
    else if (file_exists(buf))
    {
      addtrailingslash(buf);
      log_printf2(TEXT("RMDir: RemoveDirectory(\"%s\")"),buf);
      remove_ro_attr(buf);
      if (!RemoveDirectory(buf))
      {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
        if (flags & DEL_REBOOT)
        {
          log_printf2(TEXT("RMDir: RemoveDirectory on Reboot(\"%s\")"),buf);
          update_status_text(LANG_DELETEONREBOOT,buf);
          MoveFileOnReboot(buf,NULL);
        }
        else
#endif//NSIS_SUPPORT_MOVEONREBOOT
        {
          log_printf2(TEXT("RMDir: RemoveDirectory failed(\"%s\")"),buf);
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

TCHAR *NSISCALL addtrailingslash(TCHAR *str)
{
  if (lastchar(str)!=TEXT('\\')) mystrcat(str,TEXT("\\"));
  return str;
}

/*char NSISCALL lastchar(const char *str)
{
  return *CharPrev(str,str+mystrlen(str));
}*/

TCHAR * NSISCALL findchar(TCHAR *str, TCHAR c)
{
  while (*str && *str != c)
  {
    str = CharNext(str);
  }
  return str;
}

void NSISCALL trimslashtoend(TCHAR *buf)
{
  TCHAR *p = buf + mystrlen(buf);
  do
  {
    if (*p == TEXT('\\'))
      break;
    p = CharPrev(buf, p);
  } while (p > buf);

  *p = 0;
}

int NSISCALL validpathspec(TCHAR *ubuf)
{
  TCHAR dl = ubuf[0] | 0x20; // convert alleged drive letter to lower case
  return ((*(DTCHAR*)ubuf==CHAR2_TO_WORD(TEXT('\\'),TEXT('\\'))) || (dl >= TEXT('a') && dl <= TEXT('z') && ubuf[1]==TEXT(':')));
}

TCHAR * NSISCALL skip_root(TCHAR *path)
{
  TCHAR *p = CharNext(path);
  TCHAR *p2 = CharNext(p);

  if (*path && *(DTCHAR*)p == CHAR2_TO_WORD(TEXT(':'), TEXT('\\')))
  {
    return CharNext(p2);
  }
  else if (*(DTCHAR*)path == CHAR2_TO_WORD(TEXT('\\'),TEXT('\\')))
  {
    // skip host and share name
    int x = 2;
    while (x--)
    {
      p2 = findchar(p2, TEXT('\\'));
      if (!*p2)
        return NULL;
      p2++; // skip backslash
    }

    return p2;
  }
  else
    return NULL;
}

int NSISCALL is_valid_instpath(TCHAR *s)
{
  static TCHAR tmp[NSIS_MAX_STRLEN];
  TCHAR *root;

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

TCHAR * NSISCALL mystrstri(TCHAR *a, TCHAR *b)
{
  int l = mystrlen(b);
  while (mystrlen(a) >= l)
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

void NSISCALL mini_memcpy(void *out, const void *in, size_t len)
{
  char *c_out=(char*)out;
  char *c_in=(char *)in;
  while (len-- != 0)
  {
    *c_out++=*c_in++;
  }
}

void NSISCALL remove_ro_attr(TCHAR *file)
{
  int attr = GetFileAttributes(file);
  if (attr != INVALID_FILE_ATTRIBUTES)
    SetFileAttributes(file,attr&(~FILE_ATTRIBUTE_READONLY));
}

HANDLE NSISCALL myOpenFile(const TCHAR *fn, DWORD da, DWORD cd)
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

TCHAR * NSISCALL my_GetTempFileName(TCHAR *buf, const TCHAR *dir)
{
  int n = 100;
  while (n--)
  {
    TCHAR prefix[4];
    *(QTCHAR *)prefix = CHAR4_TO_DWORD(TEXT('n'), TEXT('s'), TEXT('a'), 0); 
    prefix[2] += (TCHAR)(GetTickCount() % 26);
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
  typedef BOOL (WINAPI *mfea_t)(LPCTSTR lpExistingFileName,LPCTSTR lpNewFileName,DWORD dwFlags);
  mfea_t mfea;
#ifdef UNICODE
	mfea=(mfea_t) myGetProcAddress(TEXT("KERNEL32.dll"),"MoveFileExW");
#else
	mfea=(mfea_t) myGetProcAddress(TEXT("KERNEL32.dll"),"MoveFileExA");	
#endif
  if (mfea)
  {
    fOk=mfea(pszExisting, pszNew, MOVEFILE_DELAY_UNTIL_REBOOT|MOVEFILE_REPLACE_EXISTING);
  }

  if (!fOk)
  {
    static TCHAR szRenameLine[1024];
    static TCHAR wininit[1024];
    static TCHAR tmpbuf[1024];
    int cchRenameLine;
    TCHAR *szRenameSec = TEXT("[Rename]\r\n");
    HANDLE hfile;
    DWORD dwFileSize;
    DWORD dwBytes;
    DWORD dwRenameLinePos;
    TCHAR *pszWinInit;

    int spn;

    *(QTCHAR*)tmpbuf = CHAR4_TO_DWORD(TEXT('N'), TEXT('U'), TEXT('L'), 0);

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
    cchRenameLine = wsprintf(szRenameLine,TEXT("%s=%s\r\n"),tmpbuf,wininit);

    GetWindowsDirectory(wininit, 1024-16);
    mystrcat(wininit, TEXT("\\wininit.ini"));
    hfile = myOpenFile(wininit, GENERIC_READ | GENERIC_WRITE, OPEN_ALWAYS);

    if (hfile != INVALID_HANDLE_VALUE)
    {
      dwFileSize = GetFileSize(hfile, NULL);
      pszWinInit = GlobalAlloc(GPTR, dwFileSize + cchRenameLine + 10);

      if (pszWinInit != NULL)
      {
        if (ReadFile(hfile, pszWinInit, dwFileSize, &dwBytes, NULL) && dwFileSize == dwBytes)
        {
          LPTSTR pszRenameSecInFile = mystrstri(pszWinInit, szRenameSec);
          if (pszRenameSecInFile == NULL)
          {
            mystrcpy(pszWinInit+dwFileSize, szRenameSec);
            dwFileSize += 10;
            dwRenameLinePos = dwFileSize;
          }
          else
          {
            TCHAR *pszFirstRenameLine = pszRenameSecInFile+10;
            TCHAR *pszNextSec = mystrstri(pszFirstRenameLine,TEXT("\n["));
            if (pszNextSec)
            {
              TCHAR *p = ++pszNextSec;
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

void NSISCALL myRegGetStr(HKEY root, const TCHAR *sub, const TCHAR *name, TCHAR *out)
{
  HKEY hKey;
  *out=0;
  if (RegOpenKeyEx(root,sub,0,KEY_READ,&hKey) == ERROR_SUCCESS)
  {
    DWORD l = NSIS_MAX_STRLEN;
    DWORD t;
    if (RegQueryValueEx(hKey,name,NULL,&t,(LPBYTE)out,&l ) != ERROR_SUCCESS || (t != REG_SZ && t != REG_EXPAND_SZ)) *out=0;
    out[NSIS_MAX_STRLEN-1]=0;
    RegCloseKey(hKey);
  }
}

void NSISCALL myitoa(TCHAR *s, int d)
{
  static const TCHAR c[] = TEXT("%d");
  wsprintf(s,c,d);
}

int NSISCALL myatoi(TCHAR *s)
{
  unsigned int v=0;
  int sign=1; // sign of positive
  TCHAR m=10; // base of 10
  TCHAR t=TEXT('9'); // cap top of numbers at 9

  if (*s == TEXT('-'))
  {
    s++;  //skip over -
    sign=-1; // sign flip
  }

  if (*s == TEXT('0'))
  {
    s++; // skip over 0
    if (s[0] >= TEXT('0') && s[0] <= TEXT('7'))
    {
      m=8; // base of 8
      t=TEXT('7'); // cap top at 7
    }
    if ((s[0] & ~0x20) == TEXT('X'))
    {
      m=16; // base of 16
      s++; // advance over 'x'
    }
  }

  for (;;)
  {
    int c=*s++;
    if (c >= TEXT('0') && c <= t) c-=TEXT('0');
    else if (m==16 && (c & ~0x20) >= TEXT('A') && (c & ~0x20) <= TEXT('F')) c = (c & 7) + 9;
    else break;
    v*=m;
    v+=c;
  }
  return ((int)v)*sign;
}

// Straight copies of selected shell functions.  Calling local functions
// requires less code than DLL functions.  For the savings to outweigh the cost
// of a new function there should be about a couple of dozen or so calls.
TCHAR * NSISCALL mystrcpy(TCHAR *out, const TCHAR *in)
{
  return lstrcpyn(out, in, NSIS_MAX_STRLEN);
}

int NSISCALL mystrlen(const TCHAR *in)
{
  return lstrlen(in);
}

TCHAR * NSISCALL mystrcat(TCHAR *out, const TCHAR *concat)
{
  return lstrcat(out, concat);
}

TCHAR ps_tmpbuf[NSIS_MAX_STRLEN*2];

#define SYSREGKEY TEXT("Software\\Microsoft\\Windows\\CurrentVersion")

// Based on Dave Laundon's simplified process_string
TCHAR * NSISCALL GetNSISString(TCHAR *outbuf, int strtab)
{
  TCHAR *in = (TCHAR*)GetNSISStringNP(GetNSISTab(strtab));
  TCHAR *out = ps_tmpbuf;
  if ((unsigned int) (outbuf - ps_tmpbuf) < (sizeof(ps_tmpbuf)/sizeof(*ps_tmpbuf)))
  {
    out = outbuf;
    outbuf = 0;
  }
  while (*in && out - ps_tmpbuf < NSIS_MAX_STRLEN)
  {
    unsigned char nVarIdx = (unsigned char)*in++; // TODO: <benski>
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
        LPITEMIDLIST idl;
        TCHAR *append = 0;

        int x = 2;

        // all users' version is CSIDL_APPDATA only for $QUICKLAUNCH
        // for normal $APPDATA, it'd be CSIDL_APPDATA_COMMON
        if (fldrs[2] == CSIDL_APPDATA)
        {
          append = TEXT("\\Microsoft\\Internet Explorer\\Quick Launch");
        }

        if (g_exec_flags.all_user_var)
        {
          x = 4;
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
          *out=0;
        }

        // resort to old registry methods, only when CSIDL failed
        if (!*out)
        {
          if (fldrs[0] == CSIDL_PROGRAM_FILES_COMMON)
          {
            myRegGetStr(HKEY_LOCAL_MACHINE, SYSREGKEY, TEXT("CommonFilesDir"), out);
          }
          else if (fldrs[0] == CSIDL_PROGRAM_FILES)
          {
            myRegGetStr(HKEY_LOCAL_MACHINE, SYSREGKEY, TEXT("ProgramFilesDir"), out);
            if (!*out)
              mystrcpy(out, TEXT("C:\\Program Files"));
          }
          else if (fldrs[0] == CSIDL_SYSTEM)
          {
            GetSystemDirectory(out, NSIS_MAX_STRLEN);
          }
          else if (fldrs[0] == CSIDL_WINDOWS)
          {
            GetWindowsDirectory(out, NSIS_MAX_STRLEN);
          }
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
          myitoa(out, (unsigned int) g_hwnd); // TODO: <benski> not 64bit compat
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

void NSISCALL validate_filename(TCHAR *in) {
  TCHAR *nono = TEXT("*?|<>/\":");
  TCHAR *out;
  TCHAR *out_save;

  // ignoring spaces is wrong, " C:\blah" is invalid
  //while (*in == ' ') in = CharNext(in);

  if (in[0] == TEXT('\\') && in[1] == TEXT('\\') && in[2] == TEXT('?') && in[3] == TEXT('\\'))
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
TCHAR log_text[2048]; // 1024 for each wsprintf

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
      mystrcat(log_text,TEXT("\r\n"));
      WriteFile(fp,log_text,mystrlen(log_text)*sizeof(TCHAR),&d,NULL);
    }
  }
}
#endif//!NSIS_CONFIG_LOG_ODS && !NSIS_CONFIG_LOG_STDOUT

const TCHAR * _RegKeyHandleToName(HKEY hKey)
{
  if (hKey == HKEY_CLASSES_ROOT)
    return TEXT("HKEY_CLASSES_ROOT");
  else if (hKey == HKEY_CURRENT_USER)
    return TEXT("HKEY_CURRENT_USER");
  else if (hKey == HKEY_LOCAL_MACHINE)
    return TEXT("HKEY_LOCAL_MACHINE");
  else if (hKey == HKEY_USERS)
    return TEXT("HKEY_USERS");
  else if (hKey == HKEY_PERFORMANCE_DATA)
    return TEXT("HKEY_PERFORMANCE_DATA");
  else if (hKey == HKEY_CURRENT_CONFIG)
    return TEXT("HKEY_CURRENT_CONFIG");
  else if (hKey == HKEY_DYN_DATA)
    return TEXT("HKEY_DYN_DATA");
  else
    return TEXT("invalid registry key");
}

void _LogData2Hex(TCHAR *buf, size_t buflen, unsigned char *data, size_t datalen)
{
  TCHAR *p = buf;

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
    wsprintf(p, TEXT("%02x%c"), data[i], (i == bufbytes - 1) ? TEXT('\0') : TEXT(' '));
    p += 3;
  }

  if (dots)
    mystrcat(buf, TEXT("..."));
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

void log_printf(TCHAR *format, ...)
{
  va_list val;
  va_start(val,format);

  log_text[0] = TEXT('\0');
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
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), log_text, lstrlen(log_text)*sizeof(TCHAR), &dwBytes, NULL);
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), TEXT("\n"), 1*sizeof(TCHAR), &dwBytes, NULL);
  }
#endif
#if !defined(NSIS_CONFIG_LOG_ODS) && !defined(NSIS_CONFIG_LOG_STDOUT)
  log_write(0);
#endif
}
#endif//NSIS_CONFIG_LOG

WIN32_FIND_DATA * NSISCALL file_exists(TCHAR *buf)
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

void * NSISCALL myGetProcAddress(TCHAR *dll, char *func)
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
