#include <windows.h>
#include <shlobj.h>
#include "util.h"
#include "state.h"
#include "config.h"
#include "lang.h"

#include "fileform.h"
#include "ui.h"

#ifdef NSIS_CONFIG_LOG
char g_log_file[1024];
#endif

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
extern char plugins_temp_dir[NSIS_MAX_STRLEN];
#endif

char g_usrvars[25][NSIS_MAX_STRLEN];
char *state_command_line=g_usrvars[20];
char *state_install_directory=g_usrvars[21];
char *state_output_directory=g_usrvars[22];
char *state_exe_directory=g_usrvars[23];
char *state_language=g_usrvars[24];

HANDLE g_hInstance;

HANDLE NSISCALL myCreateProcess(char *cmd, char *dir)
{
  PROCESS_INFORMATION ProcInfo={0,};
  STARTUPINFO StartUp={0,};
  StartUp.cb=sizeof(StartUp);
  if (!CreateProcess(NULL, cmd, NULL, NULL, FALSE, 0, NULL, dir, &StartUp, &ProcInfo))
    return NULL;
  if (NULL != ProcInfo.hThread)  CloseHandle( ProcInfo.hThread );
  return ProcInfo.hProcess;
}

BOOL NSISCALL my_SetDialogItemText(HWND dlg, UINT idx, const char *val)
{
  return SetDlgItemText(dlg,idx,val);
}

int NSISCALL my_MessageBox(const char *text, UINT type) {
  return MessageBox(g_hwnd, text, g_caption, type);
}

void * NSISCALL my_GlobalAlloc(DWORD dwBytes) {
  return (void *)GlobalAlloc(GPTR, dwBytes);
}

#ifdef NSIS_SUPPORT_RMDIR
void NSISCALL doRMDir(char *buf, int recurse)
{
  if (recurse && is_valid_instpath(buf))
  {
    int i=mystrlen(buf);
    HANDLE h;
    WIN32_FIND_DATA fd;
    lstrcat(buf,"\\*.*");
    h = FindFirstFile(buf,&fd);
    if (h != INVALID_HANDLE_VALUE)
    {
      do
      {
        if (fd.cFileName[0] != '.' ||
            (fd.cFileName[1] != '.' && fd.cFileName[1]))
        {
          mystrcpy(buf+i+1,fd.cFileName);
          if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) doRMDir(buf,recurse);
          else
          {
            update_status_text_from_lang(LANG_DELETEFILE,buf);
            if (fd.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
              SetFileAttributes(buf,fd.dwFileAttributes^FILE_ATTRIBUTE_READONLY);
            DeleteFile(buf);
          }
        }
      } while (FindNextFile(h,&fd));
      FindClose(h);
    }
    buf[i]=0; // fix buffer
  }
  log_printf2("RMDir: RemoveDirectory(\"%s\")",buf);
  update_status_text_from_lang(LANG_REMOVEDIR,buf);
  RemoveDirectory(buf);
}
#endif//NSIS_SUPPORT_RMDIR

char *NSISCALL addtrailingslash(char *str)
{
  if (lastchar(str)!='\\') lstrcat(str,"\\");
  return str;
}

char NSISCALL lastchar(const char *str)
{
  return *CharPrev(str,str+mystrlen(str));
}

void NSISCALL trimslashtoend(char *buf)
{
  char *p=scanendslash(buf);
  if (p<buf) p=buf;
  *p=0;
}


char * NSISCALL scanendslash(const char *str)
{
  char *s=CharPrev(str,str+mystrlen(str));
	if (!*str) return (char*)str-1;
	for (;;)
	{
    char *t;
  	if ('\\' == *s) return s;
		t=CharPrev(str,s);
		if (t==s) return (char*)str-1;
		s=t;
	}
}

int NSISCALL validpathspec(char *ubuf)
{
  return ((*(WORD*)ubuf==CHAR2_TO_WORD('\\','\\')) || (ubuf[0] && *CharNext(ubuf)==':'));
}

int NSISCALL is_valid_instpath(char *s)
{
  int ivp=0;
  // if 8 is set, req is 0, which means rootdirs are not allowed.
  int req=!(g_inst_cmnheader->misc_flags&8);
  if (*(WORD*)s == CHAR2_TO_WORD('\\','\\')) // \\ path
  {
    if (lastchar(s)!='\\') ivp++;
    while (*s)
    {
      if (*s == '\\') ivp++;
      s=CharNext(s);
    }
    ivp/=5-req;
  }
  else
  {
    if (*s)
    {
      s=CharNext(s);
      if (*(WORD*)s == CHAR2_TO_WORD(':','\\'))
      {
        s+=2;
        if (req || (*s && *s != '\\')) ivp++;
      }
    }
  }
  return ivp;
}

static char * NSISCALL findinmem(char *a, char *b, int len_of_a)
{
  if (len_of_a<0) len_of_a=mystrlen(a);
  len_of_a -= mystrlen(b);
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
  return CreateFile(fn,da,FILE_SHARE_READ,NULL,cd,0,NULL);
}

#ifdef NSIS_SUPPORT_MOVEONREBOOT
BOOL NSISCALL MoveFileOnReboot(LPCTSTR pszExisting, LPCTSTR pszNew)
{
  BOOL fOk = 0;
  HMODULE hLib=LoadLibrary("kernel32.dll");
  if (hLib)
  {
    typedef BOOL (WINAPI *mfea_t)(LPCSTR lpExistingFileName,LPCSTR lpNewFileName,DWORD dwFlags);
    mfea_t mfea;
    mfea=(mfea_t) GetProcAddress(hLib,"MoveFileExA");
    if (mfea)
    {
      fOk=mfea(pszExisting, pszNew, MOVEFILE_DELAY_UNTIL_REBOOT|MOVEFILE_REPLACE_EXISTING);
    }
    FreeLibrary(hLib);
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
    static const char nulint[4]="NUL";

    if (pszNew) GetShortPathName(pszNew,tmpbuf,1024);
    else *((int *)tmpbuf) = *((int *)nulint);
    // wininit is used as a temporary here
    GetShortPathName(pszExisting,wininit,1024);
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
          LPSTR pszRenameSecInFile = findinmem(pszWinInit, szRenameSec,-1);
          if (pszRenameSecInFile == NULL)
          {
            mystrcpy(pszWinInit+dwFileSize, szRenameSec);
            dwFileSize += 10;
            dwRenameLinePos = dwFileSize;
          }
          else
          {
            char *pszFirstRenameLine = pszRenameSecInFile+10;
            char *pszNextSec = findinmem(pszFirstRenameLine,"\n[",-1);
            if (pszNextSec)
            {
              int l=dwFileSize - (pszNextSec - pszWinInit);
              void* data=(void*)my_GlobalAlloc(l);
              mini_memcpy(data, pszNextSec, l);
              mini_memcpy(pszNextSec + cchRenameLine, data, l);
              GlobalFree((HGLOBAL)data);

              dwRenameLinePos = pszNextSec - pszWinInit;
            }
            // rename section is last, stick item at end of file
            else dwRenameLinePos = dwFileSize;
          }

          mini_memcpy(&pszWinInit[dwRenameLinePos], szRenameLine,cchRenameLine);
          dwFileSize += cchRenameLine;

          UnmapViewOfFile(pszWinInit);

          fOk++;
        }
        CloseHandle(hfilemap);
      }
      SetFilePointer(hfile, dwFileSize, NULL, FILE_BEGIN);
      SetEndOfFile(hfile);
      CloseHandle(hfile);
    }
  }
  return fOk;
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
    if (RegQueryValueEx(hKey,name,NULL,&t,out,&l ) != ERROR_SUCCESS || t != REG_SZ) *out=0;
    out[NSIS_MAX_STRLEN-1]=0;
    RegCloseKey(hKey);
  }
}


static char smwcvesf[]="Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders";
char g_all_user_var_flag;

static void NSISCALL queryShellFolders(const char *name_, char *out)
{
  static char name[20] = "Common ";
  mystrcpy(name + 7, name_);
  {
    char f=g_all_user_var_flag;

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
      GetTempPath(NSIS_MAX_STRLEN,out);
    }
  }
}

char ps_tmpbuf[NSIS_MAX_STRLEN*2];


char * NSISCALL process_string_fromtab(char *out, int offs)
{
  return lstrcpyn(out,process_string(ps_tmpbuf,GetStringFromStringTab(offs)),NSIS_MAX_STRLEN);
}

void NSISCALL myitoa(char *s, int d) { wsprintf(s,"%d",d); }

int NSISCALL myatoi(char *s)
{
  unsigned int v=0;
  int sign=0; // sign of positive
  char m=10; // base of 0
  char t='9'; // cap top of numbers at 9

  if (*s == '-') 
  { 
    s++;  //skip over -
    sign++; // sign flip
  }

  if (*s == '0')
  {
    s++; // skip over 0
    if (s[0] >= '0' && s[0] <= '7')
    {
      m=8; // base of 8
      t='7'; // cap top at 7
    }
    if (s[0] == 'x' || s[0] == 'X')
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
  if (sign) return -(int) v;
  return (int)v;
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

// Dave Laundon's simplified process_string
char * NSISCALL process_string(char *out, const char *in)
{
  char *outsave = out;
  while (*in && out - outsave < NSIS_MAX_STRLEN)
  {
    int nVarIdx = (unsigned char)*in++;
    if (nVarIdx < VAR_CODES_START)
    {
      *out++ = nVarIdx;
    }
    else if (nVarIdx == 255)
    {
      *out++ = *in++;
    }
    else
    {
      DWORD f;
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
          mystrcpy(out, g_usrvars[nVarIdx - (VAR_CODES_START + 1)]);
          break;

        case VAR_CODES_START + 26: // PROGRAMFILES
          smwcvesf[41]=0;
          myRegGetStr(HKEY_LOCAL_MACHINE, smwcvesf, "ProgramFilesDir", out);
          if (!*out)
            mystrcpy(out, "C:\\Program Files");
          break;

        case VAR_CODES_START + 27: // SMPROGRAMS
        case VAR_CODES_START + 28: // SMSTARTUP
        case VAR_CODES_START + 29: // DESKTOP
        case VAR_CODES_START + 30: // STARTMENU
          {
            static const char *tab[]={
              "Programs",
              "Startup",
              "Desktop",
              "Start Menu"
            };
            queryShellFolders(tab[nVarIdx-(VAR_CODES_START+27)], out);
          }
        break;

        case VAR_CODES_START + 31: // QUICKLAUNCH
          queryShellFolders("AppData", out);
          lstrcat(out, "\\Microsoft\\Internet Explorer\\Quick Launch");
          f = GetFileAttributes(out);
          if (f != (DWORD)-1 && (f & FILE_ATTRIBUTE_DIRECTORY))
            break;
        case VAR_CODES_START + 32: // TEMP
          GetTempPath(NSIS_MAX_STRLEN, out);
          break;

        case VAR_CODES_START + 33: // WINDIR
          GetWindowsDirectory(out, NSIS_MAX_STRLEN);
          break;

        case VAR_CODES_START + 34: // SYSDIR
          GetSystemDirectory(out, NSIS_MAX_STRLEN);
          break;

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
        case VAR_CODES_START + 35: // PLUGINSDIR
          mystrcpy(out, plugins_temp_dir);
          break;

        #if VAR_CODES_START + 35 >= 255
          #error "Too many variables!  Extend VAR_CODES_START!"
        #endif
#else
        #if VAR_CODES_START + 34 >= 255
          #error "Too many variables!  Extend VAR_CODES_START!"
        #endif
#endif //NSIS_CONFIG_PLUGIN_SUPPORT
      } // switch
      // remove trailing slash
      while (*out && *CharNext(out)) out++;
      if (nVarIdx > 21+VAR_CODES_START && nVarIdx != VAR_CODES_START+25 && *out == '\\') // only if not $0 to $R9, $CMDLINE, $LANGUAGE, or $HWNDPARENT
        *out = 0;
      out=CharNext(out);
    } // >= VAR_CODES_START
  } // while
  *out = 0;
  return outsave;
}
#ifdef NSIS_CONFIG_LOG

char log_text[4096];
int log_dolog;
void NSISCALL log_write(int close)
{
  extern char g_log_file[1024];
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


#endif
