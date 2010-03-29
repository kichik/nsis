#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#include <ctype.h>
#include <commctrl.h>

#ifndef _countof
#ifndef __cplusplus
#define _countof(_Array) (sizeof(_Array) / sizeof(_Array[0]))
#else
  extern "C++" {
    template <typename _CountofType,size_t _SizeOfArray> char (*__countof_helper(UNALIGNED _CountofType (&_Array)[_SizeOfArray]))[_SizeOfArray];
#define _countof(_Array) sizeof(*__countof_helper(_Array))
  }
#endif
#endif

/*
version 0.36
* Unicode support by Jim Park -- 08/27/2007
* This support allow Unicode *ZIP file* names but does NOT allow the archive
* to store Unicode files inside it.  That's a ZLIB limitation that I can't
* do much about.

version 0.35
* drag & drop support

version 0.34
* preserve zip timestamps

version 0.33
* Added solid compression checkbox

version 0.32
* Fixed codepage problems

version 0.31 (by Joost Verburg)
* LZMA compression support
* Fixed compression setting

version 0.31 (by Joost Verburg)
* Based on header files
* Improved interface
* Modern UI support
* New script code
* Immproved folder detection

portions Copyright © 1999-2001 Miguel Garrido (mgarrido01@hotmail.com)

*/

extern "C"
{
#include "zlib/unzip.h"
};
#include "resource.h"

const TCHAR *g_errcaption=_T("Zip2Exe Error");

HINSTANCE g_hInstance;
HWND g_hwnd;
HANDLE g_hThread;
TCHAR g_cmdline[1024];
int g_extracting;
int g_compressor;
int g_compressor_solid;
int g_mui;
int g_zipfile_size;

TCHAR *g_options=_T("");//_T("/V3");

static BOOL CALLBACK DlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

int WINAPI _tWinMain(HINSTANCE hInstance, HINSTANCE hPrevInst,
                   LPTSTR lpszCmdParam, int nCmdShow)
{
  g_hInstance=hInstance;

  InitCommonControls();
  return DialogBox(hInstance,MAKEINTRESOURCE(IDD_DIALOG1),GetDesktopWindow(),DlgProc);
}
TCHAR tempzip_path[1024];


int made;

static void doRMDir(TCHAR *buf)
{
  HANDLE h;
  WIN32_FIND_DATA fd;
  TCHAR *p=buf;
  while (*p) p++;
  lstrcpy(p,_T("\\*.*"));
  h = FindFirstFile(buf,&fd);
  if (h != INVALID_HANDLE_VALUE)
  {
    do
    {
      if (fd.cFileName[0] != _T('.') ||
          (fd.cFileName[1] != _T('.') && fd.cFileName[1]))
      {
        lstrcpy(p+1,fd.cFileName);
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
          SetFileAttributes(buf,fd.dwFileAttributes^FILE_ATTRIBUTE_READONLY);
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) doRMDir(buf);
        else
        {
          DeleteFile(buf);
        }
      }
    } while (FindNextFile(h,&fd));
    FindClose(h);
  }
  p[0]=0; // fix buffer
  RemoveDirectory(buf);
}

static void doMKDir(TCHAR *directory)
{
  TCHAR *p, *p2;
  TCHAR buf[MAX_PATH];
  if (!*directory) return;
  lstrcpy(buf,directory);
  p=buf; while (*p) p++;
  while (p >= buf && *p != _T('\\')) p--;
  p2 = buf;
  if (p2[1] == _T(':')) p2+=4;
  else if (p2[0] == _T('\\') && p2[1] == _T('\\'))
  {
    p2+=2;
    while (*p2 && *p2 != _T('\\')) p2++;
    if (*p2) p2++;
    while (*p2 && *p2 != _T('\\')) p2++;
    if (*p2) p2++;
  }
  if (p >= p2)
  {
    *p=0;
    doMKDir(buf);
  }
  CreateDirectory(directory,NULL);
}



void tempzip_cleanup(HWND hwndDlg, int err)
{
  if (tempzip_path[0]) doRMDir(tempzip_path);
  tempzip_path[0]=0;
  if (err)
  {
    SendDlgItemMessage(hwndDlg,IDC_ZIPINFO_FILES,LB_RESETCONTENT,0,0);
    EnableWindow(GetDlgItem(hwndDlg,IDOK),0);
    SetDlgItemText(hwndDlg,IDC_ZIPINFO_SUMMARY,_T(""));
    SetDlgItemText(hwndDlg,IDC_ZIPFILE,_T(""));
    SetDlgItemText(hwndDlg,IDC_OUTFILE,_T(""));
  }
}

int tempzip_make(HWND hwndDlg, TCHAR *fn)
{
  TCHAR buf[MAX_PATH];
  GetTempPath(MAX_PATH,buf);
  GetTempFileName(buf,_T("z2e"),GetTickCount(),tempzip_path);
  if (!CreateDirectory(tempzip_path,NULL))
  {
    GetTempPath(MAX_PATH,tempzip_path);
    _tcscat(tempzip_path,_T("\\nsi"));
    if (!CreateDirectory(tempzip_path,NULL))
    {
      tempzip_path[0]=0;
      MessageBox(hwndDlg,_T("Error creating temporary directory"),g_errcaption,MB_OK|MB_ICONSTOP);
      return 1;
    }
  }
  FILE *fp=_tfopen(fn,_T("rb"));
  if (fp)
  {
    fseek(fp,0,SEEK_END);
    g_zipfile_size=ftell(fp);
    fclose(fp);
  }
  else g_zipfile_size=0;
  unzFile f;
  f = unzOpen(fn);
  if (!f || unzGoToFirstFile(f) != UNZ_OK)
  {
    if (f) unzClose(f);
    MessageBox(hwndDlg,_T("Error opening ZIP file"),g_errcaption,MB_OK|MB_ICONSTOP);
    return 1;
  }

  int nf=0, nkb=0;
  g_extracting=1;
  do {
    TCHAR filename[MAX_PATH];
    unz_file_info info;

    unzGetCurrentFileInfo(f,&info,filename,sizeof(filename),NULL,0,NULL,0);

    // was zip created on MS-DOS/Windows?
    if ((info.version & 0xFF00) == 0)
    {
      OemToCharBuff(filename, filename, strlen(filename));
    }

    if (filename[0] &&
        filename[_tcsclen(filename)-1] != _T('\\') &&
        filename[_tcsclen(filename)-1] != _T('/'))
    {
      TCHAR *pfn=filename;
      while (*pfn)
      {
        if (*pfn == _T('/')) *pfn=_T('\\');
        pfn++;
      }
      pfn=filename;
      if (pfn[1] == _T(':') && pfn[2] == _T('\\')) pfn+=3;
      while (*pfn == _T('\\')) pfn++;

      TCHAR out_filename[1024];
      lstrcpy(out_filename,tempzip_path);
      lstrcat(out_filename,_T("\\"));
      lstrcat(out_filename,pfn);
      if (_tcsstr(pfn,_T("\\")))
      {
        TCHAR buf[1024];
        lstrcpy(buf,out_filename);
        TCHAR *p=buf+_tcsclen(buf);
        while (p > buf && *p != _T('\\')) p--;
        *p=0;
        if (buf[0]) doMKDir(buf);
      }

      if (unzOpenCurrentFile(f) == UNZ_OK)
      {
        SendDlgItemMessage(hwndDlg,IDC_ZIPINFO_FILES,LB_ADDSTRING,0,(LPARAM)pfn);
        FILE *fp;
        int l;
        fp = _tfopen(out_filename,_T("wb"));
        if (fp)
        {
          do
          {
            // Jim Park: Local buf, no need to TCHAR
            char buf[1024];
            l=unzReadCurrentFile(f,buf,sizeof(buf));
            if (l > 0)
            {
              if (fwrite(buf,1,l,fp) != (unsigned int)l)
              {
                unzClose(f);
                fclose(fp);
                MessageBox(hwndDlg,_T("Error writing output file(s)"),g_errcaption,MB_OK|MB_ICONSTOP);
                g_extracting=0;
                return 1;
              }
              nkb++;
            }
          } while (l > 0);

          fclose(fp);

          {
            // set file time
            HANDLE hf = CreateFile(out_filename, GENERIC_WRITE, 0, 0, OPEN_ALWAYS, 0, 0);
            if (hf != INVALID_HANDLE_VALUE)
            {
              FILETIME ft, lft;
              DosDateTimeToFileTime(HIWORD(info.dosDate), LOWORD(info.dosDate), &ft);
              LocalFileTimeToFileTime(&ft, &lft);
              SetFileTime(hf, 0, 0, &lft);
              CloseHandle(hf);
            }
          }
        }
        else
        {
          unzClose(f);
          MessageBox(hwndDlg,_T("Error opening output file(s)"),g_errcaption,MB_OK|MB_ICONSTOP);
          g_extracting=0;
          return 1;
        }
        nf++;
        wsprintf(buf,_T("Extracting: %d files, %dKB"),nf,nkb);
        SetDlgItemText(hwndDlg,IDC_ZIPINFO_SUMMARY,buf);
        MSG msg;
        int quit=0;
        while (PeekMessage(&msg,NULL,0,0,PM_REMOVE))
        {
          if (msg.message == WM_DESTROY && msg.hwnd == g_hwnd)
          {
            quit++;
            break;
          }
          TranslateMessage(&msg);
          DispatchMessage(&msg);
        }
        unzCloseCurrentFile(f);
        if (quit) break;
      }
      else
      {
        unzClose(f);
        MessageBox(hwndDlg,_T("Error extracting from ZIP file"),g_errcaption,MB_OK|MB_ICONSTOP);
        g_extracting=0;
        return 1;
      }
    }
  } while (unzGoToNextFile(f) == UNZ_OK);

  g_extracting=0;
  wsprintf(buf,_T("Extracted: %d files, %dKB"),nf,nkb);
  SetDlgItemText(hwndDlg,IDC_ZIPINFO_SUMMARY,buf);
  unzClose(f);
  return 0;
}

TCHAR *gp_winamp = _T("(WINAMP DIRECTORY)");
TCHAR *gp_winamp_plugins = _T("(WINAMP PLUG-INS DIRECTORY)");
TCHAR *gp_winamp_vis = _T("(WINAMP VIS PLUG-INS DIRECTORY)");
TCHAR *gp_winamp_dsp = _T("(WINAMP DSP PLUG-INS DIRECTORY)");
TCHAR *gp_winamp_skins = _T("(WINAMP SKINS DIRECTORY)");
TCHAR *gp_poi = _T("(PATH OF INSTALLER)");


void wnd_printf(const TCHAR *str)
{
  if (!*str) return;
  TCHAR existing_text[32000];
  existing_text[0]=0;
  UINT l=GetDlgItemText(g_hwnd, IDC_OUTPUTTEXT, existing_text, 32000);
  l+=_tcsclen(str);

  TCHAR *p=existing_text;
  existing_text[31000]=0;
  while (l > 31000 && *p)
  {
    while (*p != _T('\r') && *p != _T('\n') && *p)
    {
      p++;
      l--;
    }
    while (*p == _T('\r') || *p == _T('\n'))
    {
      p++;
      l--;
    }
  }

  TCHAR buf[31000];
  lstrcpy(buf,p);
  lstrcpy(existing_text,buf);
  lstrcat(existing_text,str);

  SetDlgItemText(g_hwnd, IDC_OUTPUTTEXT, existing_text);
  SendDlgItemMessage(g_hwnd, IDC_OUTPUTTEXT, EM_LINESCROLL, 0, SendDlgItemMessage(g_hwnd, IDC_OUTPUTTEXT, EM_GETLINECOUNT, 0, 0)); // scroll to the last line of the textbox

}

void ErrorMessage(TCHAR *str)  //display detailed error info
{
  LPVOID msg;
  FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
    NULL,
    GetLastError(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
    (LPTSTR) &msg,
    0,
    NULL
    );
  wnd_printf(str);
  wnd_printf(_T(": "));
  wnd_printf((TCHAR*)msg);
  LocalFree(msg);
}

DWORD WINAPI ThreadProc(LPVOID p) // thread that will start & monitor makensis
{
  TCHAR buf[1024];           //i/o buffer
  STARTUPINFO si={sizeof(si),};
  SECURITY_ATTRIBUTES sa={sizeof(sa),};
  SECURITY_DESCRIPTOR sd={0,};               //security information for pipes
  PROCESS_INFORMATION pi={0,};
  HANDLE newstdout=0,read_stdout=0;         //pipe handles

  OSVERSIONINFO osv={sizeof(osv)};
  GetVersionEx(&osv);
  if (osv.dwPlatformId == VER_PLATFORM_WIN32_NT)        //initialize security descriptor (Windows NT)
  {
    InitializeSecurityDescriptor(&sd,SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(&sd, true, NULL, false);
    sa.lpSecurityDescriptor = &sd;
  }
  else sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = true;         //allow inheritable handles

  if (!CreatePipe(&read_stdout,&newstdout,&sa,0))  //create stdout pipe
  {
    ErrorMessage(_T("CreatePipe"));
    PostMessage(g_hwnd,WM_USER+1203,0,1);
    return 1;
  }

  GetStartupInfo(&si);      //set startupinfo for the spawned process
  /*
    The dwFlags member tells CreateProcess how to make the process.
    STARTF_USESTDHANDLES validates the hStd* members. STARTF_USESHOWWINDOW
    validates the wShowWindow member.
  */
  si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_HIDE;
  si.hStdOutput = newstdout;
  si.hStdError = newstdout;     //set the new handles for the child process

  // *******************************************************************
  // If there is a command line in the config file, use it for create process

  //spawn the child process
  if (!CreateProcess(NULL,g_cmdline,NULL,NULL,TRUE,CREATE_NEW_CONSOLE,
      NULL,tempzip_path,&si,&pi))
  {
    ErrorMessage(_T("CreateProcess"));
    wnd_printf(_T("\r\nPlease make sure the path to makensis.exe is correct."));
    CloseHandle(newstdout);
    CloseHandle(read_stdout);
    PostMessage(g_hwnd,WM_USER+1203,0,1);
    return 1;
  }

  DWORD exit=0;  //process exit code
  DWORD bread;   //bytes read
  DWORD avail;   //bytes available

  // Number of bytes available in the buffer.
  const int bufBytesAvail = sizeof(buf)-sizeof(TCHAR);

  memset(buf,0,sizeof(buf));
  while (1)      //main program loop
  {
    PeekNamedPipe(read_stdout,buf,bufBytesAvail,&bread,&avail,NULL);

    //check to see if there is any data to read from stdout
    if (bread != 0)
    {
      memset(buf,0,sizeof(buf));
      if (avail > bufBytesAvail)
      {
        while (bread >= bufBytesAvail)
        {
          ReadFile(read_stdout,buf,bufBytesAvail,&bread,NULL);  //read the stdout pipe
          wnd_printf(buf);
          memset(buf,0,sizeof(buf));
        }
      }
      else
      {
        ReadFile(read_stdout,buf,bufBytesAvail,&bread,NULL);
        wnd_printf(buf);
      }
    }

    GetExitCodeProcess(pi.hProcess,&exit);      //while the process is running
    if (exit != STILL_ACTIVE)
      break;

    Sleep(100);
  }
  CloseHandle(pi.hThread);
  CloseHandle(pi.hProcess);
  CloseHandle(newstdout);
  CloseHandle(read_stdout);


  wsprintf(buf,_T("(source ZIP size was %d bytes)\r\n"),g_zipfile_size);
  wnd_printf(buf);

  PostMessage(g_hwnd,WM_USER+1203,0,0);
  return 0;
}


TCHAR nsifilename[MAX_PATH];



void makeEXE(HWND hwndDlg)
{
  TCHAR buf[2048];
  GetTempPath(MAX_PATH,buf);
  GetTempFileName(buf,_T("zne"),0,nsifilename);
  FILE *fp=fopen(nsifilename,_T("w"));
  if (!fp)
  {
    MessageBox(hwndDlg,_T("Error writing .NSI file"),g_errcaption,MB_OK|MB_ICONSTOP);
    PostMessage(g_hwnd,WM_USER+1203,0,0);
    return;
  }
  GetDlgItemText(hwndDlg,IDC_INSTNAME,buf,sizeof(buf));
  _ftprintf(fp,_T("!define ZIP2EXE_NAME `%s`\n"),buf);
  GetDlgItemText(hwndDlg,IDC_OUTFILE,buf,sizeof(buf));
  _ftprintf(fp,_T("!define ZIP2EXE_OUTFILE `%s`\n"),buf);
  if (g_compressor == 1)
    _ftprintf(fp,_T("!define ZIP2EXE_COMPRESSOR_ZLIB\n"));
  if (g_compressor == 2)
    _ftprintf(fp,_T("!define ZIP2EXE_COMPRESSOR_BZIP2\n"));
  if (g_compressor == 3)
    _ftprintf(fp,_T("!define ZIP2EXE_COMPRESSOR_LZMA\n"));
  if (g_compressor_solid == 1)
    _ftprintf(fp,_T("!define ZIP2EXE_COMPRESSOR_SOLID\n"));
  GetDlgItemText(hwndDlg,IDC_INSTPATH,buf,sizeof(buf));
  int iswinamp=0;
  TCHAR *iswinampmode=NULL;
  if (!_tcscmp(buf,gp_poi)) lstrcpy(buf,_T("$EXEDIR"));

  if (!_tcscmp(buf,gp_winamp))
  {
    iswinamp=1;
  }
  if (!_tcscmp(buf,gp_winamp_plugins))
  {
    iswinamp=1;
    _ftprintf(fp,_T("!define ZIP2EXE_INSTALLDIR_PLUGINS\n"));
  }
  if (!_tcscmp(buf,gp_winamp_vis))
  {
    iswinamp=1;
    iswinampmode=_T("VisDir");
  }
  if (!_tcscmp(buf,gp_winamp_dsp))
  {
    iswinamp=1;
    iswinampmode=_T("DSPDir");
  }
  if (!_tcscmp(buf,gp_winamp_skins))
  {
    iswinamp=1;
    iswinampmode=_T("SkinDir");
    _ftprintf(fp,_T("!define ZIP2EXE_INSTALLDIR_SKINS\n"));
  }

  if (iswinamp)
  {
    _ftprintf(fp,_T("!define ZIP2EXE_INSTALLDIR_WINAMP\n"));

    if (iswinampmode)
    {
      _ftprintf(fp,_T("!define ZIP2EXE_INSTALLDIR_WINAMPMODE `%s`\n"),iswinampmode);
    }
  }
  else  // set out path to $INSTDIR
  {
    _ftprintf(fp,_T("!define ZIP2EXE_INSTALLDIR `%s`\n"),buf);
  }

  _ftprintf(fp,_T("!include `${NSISDIR}\\Contrib\\zip2exe\\Base.nsh`\n"));
  _ftprintf(fp,_T("!include `${NSISDIR}\\Contrib\\zip2exe\\%s.nsh`\n"),g_mui?_T("Modern"):_T("Classic"));

  _ftprintf(fp,_T("!insertmacro SECTION_BEGIN\n"));
  _ftprintf(fp,_T("File /r `%s\\*.*`\n"),tempzip_path);
  _ftprintf(fp,_T("!insertmacro SECTION_END\n"));

  fclose(fp);

  TCHAR g_makensis_path[MAX_PATH];
  TCHAR *p=g_makensis_path;
  GetModuleFileName(g_hInstance,g_makensis_path,sizeof(g_makensis_path));
  while (*p) p++;
  while (p >= g_makensis_path && *p != _T('\\')) p--;
  _tcscpy(p+1,_T("makensis.exe"));

  WIN32_FIND_DATA fd;
  HANDLE h=FindFirstFile(g_makensis_path,&fd);
  if (h==INVALID_HANDLE_VALUE)
  {
    if ((p-g_makensis_path>4)&&(_totlower(*(p-1))==_T('n'))&&(_totlower(*(p-2))==_T('i'))&&(_totlower(*(p-3))==_T('b'))&&(*(p-4)==_T('\\')))
    {
      p -= 4;
      _tcscpy(p+1,_T("makensis.exe"));
      h=FindFirstFile(g_makensis_path,&fd);
      if (h==INVALID_HANDLE_VALUE)
      {
        MessageBox(hwndDlg,_T("Error finding makensis.exe."),g_errcaption,MB_OK|MB_ICONSTOP);
        PostMessage(g_hwnd,WM_USER+1203,0,0);
        return;
      }
    }
  }
  if (h!=INVALID_HANDLE_VALUE) FindClose(h);



  wsprintf(g_cmdline,_T("\"%s\" %s \"%s\""),g_makensis_path,g_options,nsifilename);
  DWORD id;
  g_hThread=CreateThread(NULL,0,ThreadProc,0,0,&id);

}

void SetZip(HWND hwndDlg, TCHAR *path)
{
  TCHAR buf2[1024];
  lstrcpy(buf2,path);
  tempzip_cleanup(hwndDlg,1);
  SetDlgItemText(hwndDlg,IDC_ZIPFILE,path);
  TCHAR *t=path+lstrlen(path);
  while (t > path && *t != _T('\\') && *t != _T('.')) t--;
  {
    TCHAR *p=t;
    while (p >= path && *p != _T('\\')) p--;
    p++;
    *t=0;
    SetDlgItemText(hwndDlg,IDC_INSTNAME,p[0]?p:_T("Stuff"));
  }
  _tcscpy(t,_T(".exe"));
  SetDlgItemText(hwndDlg,IDC_OUTFILE,path);
  if (tempzip_make(hwndDlg,buf2)) tempzip_cleanup(hwndDlg,1);
  else
  {
    EnableWindow(GetDlgItem(hwndDlg,IDOK),1);
  }
}

BOOL CALLBACK DlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static int ids[]={IDC_INFO,IDC_NSISICON,IDC_SZIPFRAME,IDC_BROWSE,IDC_ZIPFILE,IDC_ZIPINFO_SUMMARY,IDC_ZIPINFO_FILES,IDC_OFRAME,IDC_INAMEST,
                        IDC_INSTNAME,IDC_INSTPATH,IDC_OEFST,IDC_OUTFILE,IDC_BROWSE2,IDC_COMPRESSOR,IDC_ZLIB,IDC_BZIP2,IDC_LZMA,IDC_SOLID,IDC_INTERFACE,IDC_MODERNUI,IDC_CLASSICUI};
  static HICON hIcon;
  static HFONT hFont;
  if (uMsg == WM_DESTROY) { if (hIcon) DeleteObject(hIcon); hIcon=0; if (hFont) DeleteObject(hFont); hFont=0; }
  switch (uMsg)
  {
    case WM_INITDIALOG:
      g_hwnd=hwndDlg;
      CheckDlgButton(hwndDlg,IDC_LZMA,BST_CHECKED);
      CheckDlgButton(hwndDlg,IDC_MODERNUI,BST_CHECKED);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_poi);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)_T("$TEMP"));
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)_T("$SYSDIR"));
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)_T("$WINDIR"));
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)_T("$DESKTOP"));
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)_T("$DESKTOP\\YourNameHere"));
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)_T("$PROGRAMFILES\\YourNameHere"));
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)_T("$STARTMENU"));
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)_T("$SMPROGRAMS"));

      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp_plugins);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp_vis);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp_dsp);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp_skins);

      SetDlgItemText(hwndDlg,IDC_INSTPATH,gp_poi);

      hIcon=LoadIcon(g_hInstance,MAKEINTRESOURCE(IDI_ICON1));
      SetClassLong(hwndDlg,GCL_HICON,(long)hIcon);

      hFont=CreateFont(15,0,0,0,FW_NORMAL,0,0,0,DEFAULT_CHARSET,
              OUT_CHARACTER_PRECIS,
              CLIP_DEFAULT_PRECIS,
              DEFAULT_QUALITY,FIXED_PITCH|FF_DONTCARE,_T("Courier New"));
      SendDlgItemMessage(hwndDlg,IDC_OUTPUTTEXT,WM_SETFONT,(WPARAM)hFont,0);

      DragAcceptFiles(hwndDlg,TRUE);
    return 1;
    case WM_CLOSE:
      if (!g_hThread)
      {
        tempzip_cleanup(hwndDlg,0);
        EndDialog(hwndDlg,1);
      }
    break;
    case WM_USER+1203:

      if (g_hThread)
      {
        if (!lParam) ShowWindow(GetDlgItem(hwndDlg,IDC_TEST),SW_SHOWNA);
        CloseHandle(g_hThread);
        g_hThread=0;
      }
      made=1;
      ShowWindow(GetDlgItem(hwndDlg,IDC_BACK),SW_SHOWNA);
      EnableWindow(GetDlgItem(hwndDlg,IDOK),1);
      if (nsifilename[0]) DeleteFile(nsifilename);
      nsifilename[0]=0;
    break;
    case WM_DROPFILES:
    {
      TCHAR dropped_file[MAX_PATH]=_T("");
      if (DragQueryFile((HDROP)wParam,(UINT)-1,NULL,0)==1)
      {
        DragQueryFile((HDROP)wParam,0,dropped_file,MAX_PATH);
        if (lstrlen(dropped_file)>0)
        {
          SetZip(hwndDlg,dropped_file);
        }
      }
      else
      {
        MessageBox(hwndDlg,_T("Dropping more than one zip file at a time is not supported"),g_errcaption,MB_OK|MB_ICONSTOP);
      }
      DragFinish((HDROP)wParam);
      return TRUE;
    }
    case WM_COMMAND:
      switch (LOWORD(wParam))
      {
        case IDC_BROWSE:
          if (!g_extracting) {
            OPENFILENAME l={sizeof(l),};
            TCHAR buf[1024];
            l.hwndOwner = hwndDlg;
            l.lpstrFilter = _T("ZIP Files\0*.zip\0All Files\0*.*\0");
            l.lpstrFile = buf;
            l.nMaxFile = 1023;
            l.lpstrTitle = _T("Open ZIP File");
            l.lpstrDefExt = _T("zip");
            l.lpstrInitialDir = NULL;
            l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER|OFN_PATHMUSTEXIST;
            buf[0]=0;
            if (GetOpenFileName(&l))
            {
              SetZip(hwndDlg,buf);
            }
          }
        break;
        case IDC_BROWSE2:
          {
            OPENFILENAME l={sizeof(l),};
            TCHAR buf[1024];
            l.hwndOwner = hwndDlg;
            l.lpstrFilter = _T("Executables\0*.exe\0All Files\0*.*\0");
            l.lpstrFile = buf;
            l.nMaxFile = 1023;
            l.lpstrTitle = _T("Select Output EXE File");
            l.lpstrDefExt = _T("exe");
            l.lpstrInitialDir = NULL;
            l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER;
            GetDlgItemText(hwndDlg,IDC_OUTFILE,buf,sizeof(buf));
            if (GetSaveFileName(&l))
            {
              SetDlgItemText(hwndDlg,IDC_OUTFILE,buf);
            }
          }
        break;
        case IDC_BACK:
          if (!g_hThread)
          {
            made=0;
            ShowWindow(GetDlgItem(hwndDlg,IDC_BACK),SW_HIDE);
            ShowWindow(GetDlgItem(hwndDlg,IDC_TEST),SW_HIDE);
            ShowWindow(GetDlgItem(hwndDlg,IDC_OUTPUTTEXT),SW_HIDE);
            {
              for (size_t x = 0; x < _countof(ids); x ++)
                ShowWindow(GetDlgItem(hwndDlg,ids[x]),SW_SHOWNA);
              SetDlgItemText(hwndDlg,IDOK,_T("&Generate"));
              EnableWindow(GetDlgItem(hwndDlg,IDOK),1);
            }
          }
        break;
        case IDC_TEST:
          if (!g_hThread) {
            TCHAR buf[1024];
            GetDlgItemText(hwndDlg,IDC_OUTFILE,buf,_countof(buf));
            ShellExecute(hwndDlg,_T("open"),buf,_T(""),_T(""),SW_SHOW);
          }
        break;
        case IDOK:
          if (!g_hThread)
          {
            if (!made)
            {
              if (IsDlgButtonChecked(hwndDlg,IDC_ZLIB))
                g_compressor = 1;
              if (IsDlgButtonChecked(hwndDlg,IDC_BZIP2))
                g_compressor = 2;
              if (IsDlgButtonChecked(hwndDlg,IDC_LZMA))
                g_compressor = 3;
              if (IsDlgButtonChecked(hwndDlg,IDC_SOLID))
                g_compressor_solid = 1;
              else
                g_compressor_solid = 0;
              g_mui=!IsDlgButtonChecked(hwndDlg,IDC_CLASSICUI);
              SetDlgItemText(g_hwnd, IDC_OUTPUTTEXT, _T(""));
              for (size_t x = 0; x < _countof(ids); x ++)
                ShowWindow(GetDlgItem(hwndDlg,ids[x]),SW_HIDE);
              ShowWindow(GetDlgItem(hwndDlg,IDC_OUTPUTTEXT),SW_SHOWNA);
              SetDlgItemText(hwndDlg,IDOK,_T("&Close"));
              EnableWindow(GetDlgItem(hwndDlg,IDOK),0);

              makeEXE(hwndDlg);
            }
            else
            {
              tempzip_cleanup(hwndDlg,0);
              EndDialog(hwndDlg,0);
            }
          }
        break;
      }
    break;
  }
  return 0;
}
