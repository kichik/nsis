#include <windows.h>
#include <shlobj.h>
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

#ifdef NSIS_SUPPORT_NAMED_USERVARS
  // *** DO NOT DECLARE MORE VARIABLES INSIDE THIS PRAGMAS ***
  // This will produce a special section called ".ndata" (stands for nsis data)
  // this way makensis during build time, can search for this section by name
  // and change the virtual size of this section
  // which result in extra memory for extra variables without code to do allocation :)
  // nsis then removes the "DISCARDABLE" style from section (for safe)
  #pragma bss_seg( VARS_SECTION_NAME )
  NSIS_STRING g_usrvars[TOTAL_COMPATIBLE_STATIC_VARS_COUNT];
  #pragma bss_seg()
  #define SECTION_VARS_RWD "/section:" ## VARS_SECTION_NAME ## ",rwd"
  #pragma comment(linker, SECTION_VARS_RWD)
#else
  char temp_directory[NSIS_MAX_STRLEN];
  char g_usrvars[USER_VARS_COUNT][NSIS_MAX_STRLEN];
  char *state_command_line=g_usrvars[20];
  char *state_install_directory=g_usrvars[21];
  char *state_output_directory=g_usrvars[22];
  char *state_exe_directory=g_usrvars[23];
  char *state_language=g_usrvars[24];
  #ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    char *state_plugins_dir=g_usrvars[25];
  #endif
  char *state_plugins_dir=g_usrvars[36];
#endif

#ifndef INVALID_FILE_ATTRIBUTES
#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)
#endif

HANDLE NSISCALL myCreateProcess(char *cmd, char *dir)
{
  DWORD d;
  static PROCESS_INFORMATION ProcInfo;
  STARTUPINFO StartUp = {sizeof(StartUp), };
  d=GetFileAttributes(dir);
  if (d == INVALID_FILE_ATTRIBUTES || !(d&FILE_ATTRIBUTE_DIRECTORY)) dir=0;
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

/*int NSISCALL my_GetWindowText(HWND hWnd, char *val, int size)
{
  return SendMessage(hWnd,WM_GETTEXT,size,(LPARAM)val);
}

int NSISCALL my_GetDialogItemText(HWND dlg, UINT idx, char *val, int size)
{
  return my_GetWindowText(GetDlgItem(dlg,idx),val,size);
}*/

int NSISCALL my_MessageBox(const char *text, UINT type) {
  // default for silent installers
  if (g_exec_flags.silent && type >> 20)
    return type >> 20;
  // no silent or no default, just show
  if (!g_exec_flags.rtl)
    return MessageBox(g_hwnd, text, g_caption, type & 0x000FFFFF);
  else
    return MessageBox(g_hwnd, text, g_caption, (type & 0x000FFFFF) ^ (MB_RIGHT | MB_RTLREADING));
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

      op.hwnd=0;
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

// Based on Dave Laundon's simplified process_string
char * NSISCALL GetNSISString(char *outbuf, int strtab)
{
  char *in = (char*)GetNSISStringNP(GetNSISTab(strtab));
  char *out;
  if (outbuf >= ps_tmpbuf && outbuf < ps_tmpbuf+sizeof(ps_tmpbuf))
  {
    out = outbuf;
    outbuf = 0;
  }
  else
    out = ps_tmpbuf;
  while (*in && out - ps_tmpbuf < NSIS_MAX_STRLEN)
  {
    int nVarIdx = (unsigned char)*in++;
#ifndef NSIS_SUPPORT_NAMED_USERVARS
    if (nVarIdx < VAR_CODES_START)
    {
      *out++ = nVarIdx;
    }
    else if (nVarIdx == 255)
    {
      *out++ = *in++;
    }
#ifdef NSIS_SUPPORT_LANG_IN_STRINGS
    else if (nVarIdx == LANG_CODES_START)
    {
      nVarIdx = *(short*)in; in+=sizeof(short);
      GetNSISString(out, nVarIdx);
      out+=mystrlen(out);
    }
#endif
    else
    {
      DWORD f;
      static char smwcvesf[]="Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders";
      switch (nVarIdx) // The order of this list must match that in ..\strlist.cpp (err, build.cpp -J)
      {
        case VAR_CODES_START + 0: // HWNDPARENT
          myitoa(out, (unsigned int)g_hwnd);
          break;
        case VAR_CODES_START + 1:  // 0
        case VAR_CODES_START + 2:  // 1
        case VAR_CODES_START + 3:  // 2
        case VAR_CODES_START + 4:  // 3
        case VAR_CODES_START + 5:  // 4
        case VAR_CODES_START + 6:  // 5
        case VAR_CODES_START + 7:  // 6
        case VAR_CODES_START + 8:  // 7
        case VAR_CODES_START + 9:  // 8
        case VAR_CODES_START + 10: // 9
        case VAR_CODES_START + 11: // R0
        case VAR_CODES_START + 12: // R1
        case VAR_CODES_START + 13: // R2
        case VAR_CODES_START + 14: // R3
        case VAR_CODES_START + 15: // R4
        case VAR_CODES_START + 16: // R5
        case VAR_CODES_START + 17: // R6
        case VAR_CODES_START + 18: // R7
        case VAR_CODES_START + 19: // R8
        case VAR_CODES_START + 20: // R9
        case VAR_CODES_START + 21: // CMDLINE
        case VAR_CODES_START + 22: // INSTDIR
        case VAR_CODES_START + 23: // OUTDIR
        case VAR_CODES_START + 24: // EXEDIR
        case VAR_CODES_START + 25: // LANGUAGE
        case VAR_CODES_START + 26: // TEMP
        case VAR_CODES_START + 27: // _CLICK
        case VAR_CODES_START + 28: // PLUGINSDIR
          mystrcpy(out, g_usrvars[nVarIdx - (VAR_CODES_START + 1)]);
          break;

        case VAR_CODES_START + 29: // PROGRAMFILES
          smwcvesf[41]=0;
          myRegGetStr(HKEY_LOCAL_MACHINE, smwcvesf, "ProgramFilesDir", out);
          if (!*out)
            mystrcpy(out, "C:\\Program Files");
          break;

        case VAR_CODES_START + 30: // SMPROGRAMS
        case VAR_CODES_START + 31: // SMSTARTUP
        case VAR_CODES_START + 32: // DESKTOP
        case VAR_CODES_START + 33: // STARTMENU
        case VAR_CODES_START + 34: // QUICKLAUNCH
          {
            static const char *tab[]={
              "Programs",
              "Startup",
              "Desktop",
              "Start Menu",
              "AppData"
            };
            static char name[20]="Common ";
            const char *name_=tab[nVarIdx-(VAR_CODES_START+30)];
            mystrcpy(name+7,name_);
            f=g_exec_flags.all_user_var & (nVarIdx != VAR_CODES_START + 34);

            again:

              smwcvesf[41]='\\';
              myRegGetStr(f?HKEY_LOCAL_MACHINE:HKEY_CURRENT_USER,
                smwcvesf,
                f?name:name_,out);
              if (!out[0])
              {
                if (f)
                {
                  f=0; goto again;
                }
                mystrcpy(out,temp_directory);
              }

            if (nVarIdx == VAR_CODES_START + 34) {
              lstrcat(out, "\\Microsoft\\Internet Explorer\\Quick Launch");
              f = GetFileAttributes(out);
              if (f != (DWORD)-1 && (f & FILE_ATTRIBUTE_DIRECTORY))
                break;
            }
            else break;
          }

        case VAR_CODES_START + 35: // TEMP
          mystrcpy(out,temp_directory);
          break;

        case VAR_CODES_START + 36: // WINDIR
          GetWindowsDirectory(out, NSIS_MAX_STRLEN);
          break;

        case VAR_CODES_START + 37: // SYSDIR
          GetSystemDirectory(out, NSIS_MAX_STRLEN);
          break;

        #if VAR_CODES_START + 37 >= 255
          #error "Too many variables!  Extend VAR_CODES_START!"
        #endif
      } // switch
      // validate the directory name
      if (nVarIdx > 21+VAR_CODES_START && nVarIdx != VAR_CODES_START+27) {
         // only if not $0 to $R9, $CMDLINE, $HWNDPARENT, or $_CLICK
        // ($LANGUAGE can't have trailing backslash anyway...)
        validate_filename(out);
      }
      out+=mystrlen(out);
    } // >= VAR_CODES_START
#else

    if (nVarIdx == 255)
    {
      *out++ = *in++;
    }
    else if (nVarIdx == VAR_CODES_START)
    {
      DWORD f;
      static char smwcvesf[]="Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders";
      nVarIdx = (*(WORD*)in & 0x0FFF)-1; in+=sizeof(WORD);
      switch (nVarIdx) // The order of this list must match that in ..\strlist.cpp (err, build.cpp -J)
      {
        case 28: // PROGRAMFILES
          smwcvesf[41]=0;
          myRegGetStr(HKEY_LOCAL_MACHINE, smwcvesf, "ProgramFilesDir", out);
          if (!*out)
            mystrcpy(out, "C:\\Program Files");
          break;

        case 29: // SMPROGRAMS
        case 30: // SMSTARTUP
        case 31: // DESKTOP
        case 32: // STARTMENU
        case 33: // QUICKLAUNCH
          {
            static const char *tab[]={
              "Programs",
              "Startup",
              "Desktop",
              "Start Menu",
              "AppData"
            };
            static char name[20]="Common ";
            const char *name_=tab[nVarIdx-29];
            mystrcpy(name+7,name_);
            f=g_exec_flags.all_user_var & (nVarIdx != 33);

            again:

              smwcvesf[41]='\\';
              myRegGetStr(f?HKEY_LOCAL_MACHINE:HKEY_CURRENT_USER,
                smwcvesf,
                f?name:name_,out);
              if (!out[0])
              {
                if (f)
                {
                  f=0; goto again;
                }
                mystrcpy(out,state_temp_dir);
              }

            if (nVarIdx == 33) {
              lstrcat(out, "\\Microsoft\\Internet Explorer\\Quick Launch");
              f = GetFileAttributes(out);
              if (f != (DWORD)-1 && (f & FILE_ATTRIBUTE_DIRECTORY))
                break;
            }
            else break;
          }

        case 34: // WINDIR
          GetWindowsDirectory(out, NSIS_MAX_STRLEN);
          break;

        case 35: // SYSDIR
          GetSystemDirectory(out, NSIS_MAX_STRLEN);
          break;

        case 36: // HWNDPARENT
          myitoa(out, (unsigned int)g_hwnd);
          break;

        default:
          mystrcpy(out, g_usrvars[nVarIdx]);
      } // switch
      // validate the directory name
      if (nVarIdx > 20 && nVarIdx < TOTAL_COMPATIBLE_STATIC_VARS_COUNT && nVarIdx != 26) {
        // only if not $0 to $R9, $CMDLINE or $_CLICK and not great than $HWNDPARENT
        // ($LANGUAGE can't have trailing backslash anyway...)
        validate_filename(out);
      }
      out+=mystrlen(out);
    } // == VAR_CODES_START
#ifdef NSIS_SUPPORT_LANG_IN_STRINGS
    else if (nVarIdx == LANG_CODES_START)
    {
      nVarIdx = *(short*)in; in+=sizeof(short);
      GetNSISString(out, nVarIdx);
      out+=mystrlen(out);
    }
#endif
    else // Normal char
    {
      *out++ = nVarIdx;
    }
#endif
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
