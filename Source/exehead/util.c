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

char g_usrvars[24][NSIS_MAX_STRLEN];
char *state_command_line=g_usrvars[20];
char *state_install_directory=g_usrvars[21];
char *state_output_directory=g_usrvars[22];
char *state_exe_directory=g_usrvars[23];

HANDLE g_hInstance;

HANDLE NSISCALL myCreateProcess(char *cmd, char *dir)
{
  PROCESS_INFORMATION ProcInfo={0,};
  STARTUPINFO StartUp={sizeof(StartUp),};
  if (!CreateProcess(NULL, cmd, NULL, NULL, FALSE, 0, NULL, dir, &StartUp, &ProcInfo))
    return NULL;
  if (NULL != ProcInfo.hThread)  CloseHandle( ProcInfo.hThread );
  return ProcInfo.hProcess;
}

int NSISCALL my_MessageBox(const char *text, UINT type) {
  return MessageBox(g_hwnd, text, g_caption, type);
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
            update_status_text_from_lang(LANGID_DELETEFILE,buf);
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
  update_status_text_from_lang(LANGID_REMOVEDIR,buf);
  RemoveDirectory(buf);
}
#endif//NSIS_SUPPORT_RMDIR

void NSISCALL addtrailingslash(char *str)
{
  if (lastchar(str)!='\\') lstrcat(str,"\\");
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
  return ((ubuf[0]=='\\' && ubuf[1]=='\\') || (ubuf[0] && *CharNext(ubuf)==':'));
}

int NSISCALL is_valid_instpath(char *s)
{
  int ivp=0;
  // if 8 is set, req is 0, which means rootdirs are not allowed.
  int req=!(g_inst_cmnheader->misc_flags&8);
  if (s[0] == '\\' && s[1] == '\\') // \\ path
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
      if (*s == ':')
      {
        s=CharNext(s);
        if (*s == '\\')
        {
          s=CharNext(s);
          if (req || (*s && *s != '\\')) ivp++;
        }
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
          int do_write=0;
          LPSTR pszRenameSecInFile = findinmem(pszWinInit, szRenameSec,-1);
          if (pszRenameSecInFile == NULL)
          {
            mystrcpy(pszWinInit+dwFileSize, szRenameSec);
            dwFileSize += 10;
            dwRenameLinePos = dwFileSize;
            do_write++;
          }
          else
          {
            char *pszFirstRenameLine = findinmem(pszRenameSecInFile, "\n",-1)+1;
            int l=pszWinInit + dwFileSize-pszFirstRenameLine;
            if (!findinmem(pszFirstRenameLine,szRenameLine,l))
            {
              void* data=(void*)GlobalAlloc(GMEM_FIXED,l);
              mini_memcpy(data, pszFirstRenameLine, l);
              mini_memcpy(pszFirstRenameLine + cchRenameLine, data, l);
              GlobalFree((HGLOBAL)data);

              dwRenameLinePos = pszFirstRenameLine - pszWinInit;
              do_write++;
            }
          }

          if (do_write)
          {
            mini_memcpy(&pszWinInit[dwRenameLinePos], szRenameLine,cchRenameLine);
            dwFileSize += cchRenameLine;
          }

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

void NSISCALL recursive_create_directory(char *directory)
{
  char *tp;
	char *p;
  p=directory;
  while (*p == ' ') p=CharNext(p);
  if (!*p) return;
  tp=CharNext(p);
  if (*tp == ':' && tp[1] == '\\') p=tp+2;
  else if (p[0] == '\\' && p[1] == '\\')
  {
    int x;
    for (x = 0; x < 2; x ++)
    {
      while (*p != '\\' && *p) p=CharNext(p); // skip host then share
      if (*p) p=CharNext(p);
    }

  }
  else return;
  while (*p)
  {
    while (*p != '\\' && *p) p=CharNext(p);
    if (!*p) CreateDirectory(directory,NULL);
    else
    {
      *p=0;
  	  CreateDirectory(directory,NULL);
      *p++ = '\\';
    }
  }
}


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


char g_all_user_var_flag;

static void NSISCALL queryShellFolders(const char *name_, char *out)
{
  static char name[20] = "Common ";
  mystrcpy(name + 7, name_);
  {
    char f=g_all_user_var_flag;
  again:

    myRegGetStr(f?HKEY_LOCAL_MACHINE:HKEY_CURRENT_USER,
      "Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders",
      name+(f?0:7),out);
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


void NSISCALL process_string_fromtab(char *out, int offs)
{
  process_string(ps_tmpbuf,GetStringFromStringTab(offs));
  lstrcpyn(out,ps_tmpbuf,NSIS_MAX_STRLEN);
}

void NSISCALL process_string_from_lang(char *out, langid_t id)
{
  process_string_fromtab(out, GetLangString(id));
}

// Retrieve the string offset associated with the language string ID given
int NSISCALL GetLangString(langid_t id)
{
  return (int)id < 0 ?
    *((int *)cur_install_strings_table - 1 - (int)id) :
    *((int *)cur_common_strings_table + (int)id);
}

void NSISCALL myitoa(char *s, int d) { wsprintf(s,"%d",d); }

int NSISCALL myatoi(char *s)
{
  unsigned int v=0;
  if (*s == '0' && (s[1] == 'x' || s[1] == 'X'))
  {
    s+=2;
    for (;;)
    {
      int c=*s++;
      if (c >= '0' && c <= '9') c-='0';
      else if (c >= 'a' && c <= 'f') c-='a'-10;
      else if (c >= 'A' && c <= 'F') c-='A'-10;
      else break;
      v<<=4;
      v+=c;
    }
  }
  else if (*s == '0' && s[1] <= '7' && s[1] >= '0')
  {
    s++;
    for (;;)
    {
      int c=*s++;
      if (c >= '0' && c <= '7') c-='0';
      else break;
      v<<=3;
      v+=c;
    }
  }
  else
  {
    int sign=0;
    if (*s == '-') { s++; sign++; }
    for (;;)
    {
      int c=*s++ - '0';
      if (c < 0 || c > 9) break;
      v*=10;
      v+=c;
    }
    if (sign) return -(int) v;
  }
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


int NSISCALL process_string_fromtab_toint(int offs)
{
  process_string(ps_tmpbuf,GetStringFromStringTab(offs));
  return myatoi(ps_tmpbuf);
}

// Dave Laundon's simplified process_string
void NSISCALL process_string(char *out, const char *in)
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
          mystrcpy(out, g_usrvars[nVarIdx - (VAR_CODES_START + 1)]);
          break;

        case VAR_CODES_START + 25: // LANGUAGE
          myitoa(out, cur_common_strings_table->lang_id);
          break;

        case VAR_CODES_START + 26: // PROGRAMFILES
          myRegGetStr(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows\\CurrentVersion", "ProgramFilesDir", out);
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

#ifdef NSIS_SUPPORT_CREATESHORTCUT
int NSISCALL CreateShortCut(HWND hwnd, LPCSTR pszShortcutFile, LPCSTR pszIconFile, int iconindex, LPCSTR pszExe, LPCSTR pszArg, LPCSTR workingdir, int showmode, int hotkey)
{
  HRESULT hres;
  int rv=1;
  IShellLink* psl;
  hres=OleInitialize(NULL);
  if (hres != S_FALSE && hres != S_OK) return rv;

  hres = CoCreateInstance(&CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
                            &IID_IShellLink, (void **) &psl);
  if (SUCCEEDED(hres))
  {
    IPersistFile* ppf;

    hres = psl->lpVtbl->QueryInterface(psl,&IID_IPersistFile, (void **) &ppf);
    if (SUCCEEDED(hres))
    {

       hres = psl->lpVtbl->SetPath(psl,pszExe);
       psl->lpVtbl->SetWorkingDirectory(psl,workingdir);
       if (showmode) psl->lpVtbl->SetShowCmd(psl,showmode);
       if (hotkey) psl->lpVtbl->SetHotkey(psl,(unsigned short)hotkey);
       if (pszIconFile) psl->lpVtbl->SetIconLocation(psl,pszIconFile,iconindex);
       if (pszArg)
       {
         psl->lpVtbl->SetArguments(psl,pszArg);
       }

       if (SUCCEEDED(hres))
       {
          WCHAR wsz[1024];
          MultiByteToWideChar(CP_ACP, 0, pszShortcutFile, -1, wsz, 1024);
		      hres=ppf->lpVtbl->Save(ppf,(const WCHAR*)wsz,TRUE);
          if (SUCCEEDED(hres)) rv=0;
       }
      ppf->lpVtbl->Release(ppf);
    }
    psl->lpVtbl->Release(psl);
  }
  OleUninitialize();
  return rv;
}
#endif//NSIS_SUPPORT_CREATESHORTCUT
