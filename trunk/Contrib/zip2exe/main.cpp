#include <windows.h>
#include <stdio.h>

// portions Copyright © 1999-2001 Miguel Garrido (mgarrido01@hotmail.com)

extern "C"
{
#include "zlib/unzip.h"
};
#include "resource.h"

const char *g_errcaption="ZIP2EXE Error";

HINSTANCE g_hInstance;
HWND g_hwnd;
HANDLE g_hThread;
char g_cmdline[1024];
char g_makensis_path[MAX_PATH];
int g_extracting;
int g_zipfile_size;

char *g_options="";//"/V3";

static BOOL CALLBACK DlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam); 


int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInst,
                   LPSTR lpszCmdParam, int nCmdShow)
{
  g_hInstance=hInstance;


  return DialogBox(hInstance,MAKEINTRESOURCE(IDD_DIALOG1),GetDesktopWindow(),DlgProc);
}
char tempzip_path[1024];


int made;

static void doRMDir(char *buf)
{
  HANDLE h;
  WIN32_FIND_DATA fd;
  char *p=buf;
  while (*p) p++;
  lstrcpy(p,"\\*.*");
  h = FindFirstFile(buf,&fd);
  if (h != INVALID_HANDLE_VALUE) 
  {
    do
    {
      if (fd.cFileName[0] != '.' ||
          (fd.cFileName[1] != '.' && fd.cFileName[1]))
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

static void doMKDir(char *directory)
{
	char *p, *p2;
	char buf[MAX_PATH];
  if (!*directory) return;
	lstrcpy(buf,directory);
  p=buf; while (*p) p++;
	while (p >= buf && *p != '\\') p--;
	p2 = buf;
	if (p2[1] == ':') p2+=4;
	else if (p2[0] == '\\' && p2[1] == '\\')
	{
		p2+=2;
		while (*p2 && *p2 != '\\') p2++;
		if (*p2) p2++;
		while (*p2 && *p2 != '\\') p2++;
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
    SetDlgItemText(hwndDlg,IDC_ZIPINFO_SUMMARY,"");
    SetDlgItemText(hwndDlg,IDC_ZIPFILE,"");
    SetDlgItemText(hwndDlg,IDC_OUTFILE,"");
  }
}

int tempzip_make(HWND hwndDlg, char *fn)
{
  char buf[MAX_PATH];
  GetTempPath(MAX_PATH,buf);
  GetTempFileName(buf,"z2e",GetTickCount(),tempzip_path);
  if (!CreateDirectory(tempzip_path,NULL))
  {
    GetTempPath(MAX_PATH,tempzip_path);
    strcat(tempzip_path,"\\nsi");
    if (!CreateDirectory(tempzip_path,NULL))
    {
      tempzip_path[0]=0;
      MessageBox(hwndDlg,"Error creating temporary directory",g_errcaption,MB_OK|MB_ICONSTOP);
      return 1;
    }
  }
  FILE *fp=fopen(fn,"rb");
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
    MessageBox(hwndDlg,"Error opening ZIP file",g_errcaption,MB_OK|MB_ICONSTOP);
    return 1;
  }
	
  int nf=0, nkb=0;
  g_extracting=1;
	do {
		char filename[MAX_PATH];
	  unzGetCurrentFileInfo(f,NULL,filename,sizeof(filename),NULL,0,NULL,0);
    if (filename[0] && 
        filename[strlen(filename)-1] != '\\' && 
        filename[strlen(filename)-1] != '/')
    {
      char *pfn=filename;
      while (*pfn)
      {
        if (*pfn == '/') *pfn='\\';
        pfn++;
      }
      pfn=filename;
      if (pfn[1] == ':' && pfn[2] == '\\') pfn+=3;
      while (*pfn == '\\') pfn++;

      char out_filename[1024];
      lstrcpy(out_filename,tempzip_path);
      lstrcat(out_filename,"\\");
      lstrcat(out_filename,pfn);
      if (strstr(pfn,"\\"))
      {
        char buf[1024];
        lstrcpy(buf,out_filename);
        char *p=buf+strlen(buf);
        while (p > buf && *p != '\\') p--;
        *p=0;
        if (buf[0]) doMKDir(buf);
      }

  		if (unzOpenCurrentFile(f) == UNZ_OK)
	  	{
        SendDlgItemMessage(hwndDlg,IDC_ZIPINFO_FILES,LB_ADDSTRING,0,(LPARAM)pfn);
			  FILE *fp;
			  int l;
			  fp = fopen(out_filename,"wb");
			  if (fp)
			  {
				  do
				  {
					  char buf[1024];
					  l=unzReadCurrentFile(f,buf,sizeof(buf));
					  if (l > 0) 
            {
              if (fwrite(buf,1,l,fp) != (unsigned int)l)
              {
                unzClose(f);
                fclose(fp);
                MessageBox(hwndDlg,"Error writing output file(s)",g_errcaption,MB_OK|MB_ICONSTOP);
                g_extracting=0;
                return 1;
              }
              nkb++;
            }
				  } while (l > 0);

				  fclose(fp);
			  }
        else
        {
          unzClose(f);
          MessageBox(hwndDlg,"Error opening output file(s)",g_errcaption,MB_OK|MB_ICONSTOP);
          g_extracting=0;
          return 1;
        }
        nf++;
        wsprintf(buf,"Extracting: %d files, %dKB",nf,nkb);
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
        MessageBox(hwndDlg,"Error extracting from ZIP file",g_errcaption,MB_OK|MB_ICONSTOP);
        g_extracting=0;
        return 1;
      }
    }
  } while (unzGoToNextFile(f) == UNZ_OK);
  
  g_extracting=0;
  wsprintf(buf,"Extracted: %d files, %dKB",nf,nkb);
  SetDlgItemText(hwndDlg,IDC_ZIPINFO_SUMMARY,buf);
  unzClose(f);
  return 0;
}

char *gp_winamp = "(WINAMP DIRECTORY)";
char *gp_winamp_plugins = "(WINAMP PLUG-INS DIRECTORY)";
char *gp_winamp_vis = "(WINAMP VIS PLUG-INS DIRECTORY)";
char *gp_winamp_dsp = "(WINAMP DSP PLUG-INS DIRECTORY)";
char *gp_winamp_skins = "(WINAMP SKINS DIRECTORY)";
char *gp_poi = "(PATH OF INSTALLER)";


void wnd_printf(const char *str)
{
  if (!*str) return;
	char existing_text[32000];
  existing_text[0]=0;
	UINT l=GetDlgItemText(g_hwnd, IDC_OUTPUTTEXT, existing_text, 32000);
  l+=strlen(str);

  char *p=existing_text;
  existing_text[31000]=0;
  while (l > 31000 && *p)
  {
    while (*p != '\r' && *p != '\n' && *p)
    {
      p++;
      l--;
    }
    while (*p == '\r' || *p == '\n')
    {
      p++;
      l--;
    }
  }

  char buf[31000];
  lstrcpy(buf,p);
  lstrcpy(existing_text,buf);
  lstrcat(existing_text,str);

	SetDlgItemText(g_hwnd, IDC_OUTPUTTEXT, existing_text);
	SendDlgItemMessage(g_hwnd, IDC_OUTPUTTEXT, EM_LINESCROLL, 0, SendDlgItemMessage(g_hwnd, IDC_OUTPUTTEXT, EM_GETLINECOUNT, 0, 0)); // scroll to the last line of the textbox

}

void ErrorMessage(char *str)  //display detailed error info
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
  wnd_printf(": ");
  wnd_printf((char*)msg);
	LocalFree(msg);
}

DWORD WINAPI ThreadProc(LPVOID p) // thread that will start & monitor wwwinamp
{
	char buf[1024];           //i/o buffer
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
		ErrorMessage("CreatePipe");
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
		ErrorMessage("CreateProcess");
		wnd_printf("\r\nPlease make sure the path to makensis.exe is correct.");
		CloseHandle(newstdout);
		CloseHandle(read_stdout);
    PostMessage(g_hwnd,WM_USER+1203,0,1);
		return 1;
	}
	
	unsigned long exit=0;  //process exit code
	unsigned long bread;   //bytes read
	unsigned long avail;   //bytes available
	
  memset(buf,0,sizeof(buf));
	while (1)      //main program loop
	{
		PeekNamedPipe(read_stdout,buf,1023,&bread,&avail,NULL);
		//check to see if there is any data to read from stdout
		if (bread != 0)
		{
      memset(buf,0,sizeof(buf));
			if (avail > 1023)
			{
				while (bread >= 1023)
				{
					ReadFile(read_stdout,buf,1023,&bread,NULL);  //read the stdout pipe
					wnd_printf(buf);
          memset(buf,0,sizeof(buf));
				}
			}
			else 
			{
				ReadFile(read_stdout,buf,1023,&bread,NULL);
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


  wsprintf(buf,"(source ZIP size was %d bytes)\r\n",g_zipfile_size);
  wnd_printf(buf);
	
  PostMessage(g_hwnd,WM_USER+1203,0,0);
  return 0;
}


char nsifilename[MAX_PATH];



void makeEXE(HWND hwndDlg)
{
  char buf[2048];
  GetTempPath(MAX_PATH,buf);
  GetTempFileName(buf,"zne",0,nsifilename);
  FILE *fp=fopen(nsifilename,"w");
  if (!fp)
  {
    MessageBox(hwndDlg,"Error writing .NSI file",g_errcaption,MB_OK|MB_ICONSTOP);
    return;
  }
  GetDlgItemText(hwndDlg,IDC_INSTNAME,buf,sizeof(buf));
  fprintf(fp,"Name `%s`\n",buf);
  fprintf(fp,"Caption `%s Self Extractor`\n",buf);
  GetDlgItemText(hwndDlg,IDC_OUTFILE,buf,sizeof(buf));
  fprintf(fp,"OutFile `%s`\n",buf);
  GetDlgItemText(hwndDlg,IDC_INSTPATH,buf,sizeof(buf));
  char *outpath = "$INSTDIR";
  int iswinamp=0;
  char *iswinampmode=NULL;
  if (!strcmp(buf,gp_poi)) lstrcpy(buf,"$EXEDIR");
  
  if (!strcmp(buf,gp_winamp))
  {
    iswinamp=1;
    fprintf(fp,"Function SetMyOutPath\n"
               "  SetOutPath $INSTDIR\n"
               "FunctionEnd\n");
  }
  if (!strcmp(buf,gp_winamp_plugins))
  {
    iswinamp=1;
    fprintf(fp,"Function SetMyOutPath\n"
               "  SetOutPath $INSTDIR\\Plugins\n"
               "FunctionEnd\n");
  }
  if (!strcmp(buf,gp_winamp_vis))
  {
    iswinamp=1;
    iswinampmode="VisDir";
  }
  if (!strcmp(buf,gp_winamp_dsp))
  {
    iswinamp=1;
    iswinampmode="DSPDir";
  }
  if (!strcmp(buf,gp_winamp_skins))
  {
    iswinamp=1;
    iswinampmode="SkinDir";
  }

  if (iswinamp)
  {
    fprintf(fp,"InstallDir `$PROGRAMFILES\\Winamp`\n");
    fprintf(fp,"InstallDirRegKey HKEY_LOCAL_MACHINE `Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Winamp` `UninstallString`\n");

    fprintf(fp,"Function .onVerifyInstDir\n"
               "  IfFileExists $INSTDIR\\winamp.exe WinampInstalled\n"
               "    Abort\n"
               "  WinampInstalled:\n"
               "FunctionEnd\n");

    if (iswinampmode)
    {
      fprintf(fp,"Function SetMyOutPath\n"
                 "  StrCpy $1 $INSTDIR\\Plugins\n"
                 "  ReadINIStr $9 $INSTDIR\\winamp.ini Winamp %s\n"
                 "  StrCmp $9 '' End\n"
                 "    IfFileExists $9 0 End\n"
                 "      StrCpy $1 $9\n"
                 "  End:\n"
                 "  SetOutPath $1\n"
                 "FunctionEnd\n",iswinampmode);
    }
  }
  else  // set out path to $INSTDIR
  {
    fprintf(fp,"InstallDir `%s`\n",buf);
    fprintf(fp,"Function SetMyOutPath\n"
               "  SetOutPath $INSTDIR\n"
               "FunctionEnd\n");
  }

  GetDlgItemText(hwndDlg,IDC_DESCTEXT,buf,sizeof(buf));
  fprintf(fp,"DirText `%s`\n",buf);

  fprintf(fp,"Section\n");
  fprintf(fp,"Call SetMyOutPath\n");
  fprintf(fp,"File /r `%s\\*.*`\n",tempzip_path);
  fprintf(fp,"SectionEnd\n");
  fclose(fp);

  wsprintf(g_cmdline,"\"%s\" %s \"%s\"",g_makensis_path,g_options,nsifilename);


  DWORD id;
  g_hThread=CreateThread(NULL,0,ThreadProc,0,0,&id);

}


BOOL CALLBACK DlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static int ids[]={IDC_SZIPFRAME,IDC_BROWSE,IDC_ZIPFILE,IDC_ZIPINFO_SUMMARY,IDC_ZIPINFO_FILES,IDC_OFRAME,IDC_INAMEST,
                        IDC_INSTNAME,IDC_DTEXTST,IDC_DESCTEXT,IDC_DEPST,IDC_INSTPATH,IDC_OEFST,IDC_OUTFILE,IDC_BROWSE2,IDC_BROWSE3,IDC_COMPILER};
  static HICON hIcon;
  static HFONT hFont;
  if (uMsg == WM_DESTROY) { if (hIcon) DeleteObject(hIcon); hIcon=0; if (hFont) DeleteObject(hFont); hFont=0; }
  switch (uMsg)
  {
    case WM_INITDIALOG:
      g_hwnd=hwndDlg;
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_poi);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)"$TEMP");
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)"$SYSDIR");
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)"$WINDIR");
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)"$DESKTOP");
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)"$DESKTOP\\Poop");
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)"$PROGRAMFILES\\Poop");
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)"$STARTMENU");
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)"$SMPROGRAMS");

      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp_plugins);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp_vis);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp_dsp);
      SendDlgItemMessage(hwndDlg,IDC_INSTPATH,CB_ADDSTRING,0,(LPARAM)gp_winamp_skins);
     
      SetDlgItemText(hwndDlg,IDC_INSTPATH,gp_poi);
      SetDlgItemText(hwndDlg,IDC_DESCTEXT,"Select the folder where you would like to extract the files to:");
		  hIcon=LoadIcon(g_hInstance,MAKEINTRESOURCE(IDI_ICON1));
		  SetClassLong(hwndDlg,GCL_HICON,(long)hIcon);      

      hFont=CreateFont(15,0,0,0,FW_NORMAL,0,0,0,DEFAULT_CHARSET,
              OUT_CHARACTER_PRECIS,
              CLIP_DEFAULT_PRECIS,
              DEFAULT_QUALITY,FIXED_PITCH|FF_DONTCARE,"Courier New");
      SendDlgItemMessage(hwndDlg,IDC_OUTPUTTEXT,WM_SETFONT,(WPARAM)hFont,0);
      {
        char *p=g_makensis_path;
        GetModuleFileName(g_hInstance,g_makensis_path,sizeof(g_makensis_path));
        while (*p) p++;
        while (p >= g_makensis_path && *p != '\\') p--;
        strcpy(++p,"makensis.exe");
      }
      SetDlgItemText(hwndDlg,IDC_COMPILER,g_makensis_path);
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
        if (!lParam)
        {
          ShowWindow(GetDlgItem(hwndDlg,IDC_TEST),SW_SHOWNA);            
        }
        made=1;
        ShowWindow(GetDlgItem(hwndDlg,IDC_BACK),SW_SHOWNA);
        EnableWindow(GetDlgItem(hwndDlg,IDOK),1);
        CloseHandle(g_hThread);
        g_hThread=0;
        if (nsifilename[0]) DeleteFile(nsifilename);
        nsifilename[0]=0;
      }
    break;
    case WM_COMMAND:
      switch (LOWORD(wParam))
      {
        case IDC_BROWSE:
          if (!g_extracting) {
            OPENFILENAME l={sizeof(l),};
            char buf[1024];
            l.hwndOwner = hwndDlg;
            l.lpstrFilter = "ZIP files\0*.zip\0All files\0*.*\0";
            l.lpstrFile = buf;
            l.nMaxFile = 1023;
            l.lpstrTitle = "Open ZIP file";
            l.lpstrDefExt = "zip";
            l.lpstrInitialDir = NULL;
            l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER|OFN_PATHMUSTEXIST;  	        
            buf[0]=0;
            if (GetOpenFileName(&l)) 
            {
              char buf2[1024];
              lstrcpy(buf2,buf);
              tempzip_cleanup(hwndDlg,1);
              SetDlgItemText(hwndDlg,IDC_ZIPFILE,buf);
              char *t=buf+strlen(buf);
              while (t > buf && *t != '\\' && *t != '.') t--;
              {
                char *p=t;
                while (p >= buf && *p != '\\') p--;
                p++;
                *t=0;
                SetDlgItemText(hwndDlg,IDC_INSTNAME,p[0]?p:"Stuff");
              }
              strcpy(t,".exe");
              SetDlgItemText(hwndDlg,IDC_OUTFILE,buf);
              if (tempzip_make(hwndDlg,buf2)) tempzip_cleanup(hwndDlg,1);
              else
              {
                EnableWindow(GetDlgItem(hwndDlg,IDOK),1);
              }
            }
          }          
        break;
        case IDC_BROWSE2:
          {
            OPENFILENAME l={sizeof(l),};
            char buf[1024];
            l.hwndOwner = hwndDlg;
            l.lpstrFilter = "EXE files\0*.exe\0All files\0*.*\0";
            l.lpstrFile = buf;
            l.nMaxFile = 1023;
            l.lpstrTitle = "Select output EXE file";
            l.lpstrDefExt = "exe";
            l.lpstrInitialDir = NULL;
            l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER;  	        
            GetDlgItemText(hwndDlg,IDC_OUTFILE,buf,sizeof(buf));
            if (GetSaveFileName(&l)) 
            {
              SetDlgItemText(hwndDlg,IDC_OUTFILE,buf);
            }
          }   
        break;
        case IDC_BROWSE3:
          {
            OPENFILENAME l={sizeof(l),};
            char buf[1024];
            l.hwndOwner = hwndDlg;
            l.lpstrFilter = "Makensis EXE files (Makensis*.exe)\0Makensis*.exe\0All files\0*.*\0";
            l.lpstrFile = buf;
            l.nMaxFile = 1023;
            l.lpstrTitle = "Select compiler EXE file";
            l.lpstrDefExt = "exe";
            l.lpstrInitialDir = NULL;
            l.Flags = OFN_HIDEREADONLY|OFN_EXPLORER|OFN_PATHMUSTEXIST;  	        
            GetDlgItemText(hwndDlg,IDC_COMPILER,buf,sizeof(buf));
            if (GetOpenFileName(&l)) 
            {
              SetDlgItemText(hwndDlg,IDC_COMPILER,buf);
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
              int x;
              for (x = 0; x < sizeof(ids)/sizeof(ids[0]); x ++)
                ShowWindow(GetDlgItem(hwndDlg,ids[x]),SW_SHOWNA);            
              SetDlgItemText(hwndDlg,IDOK,"Convert");
              EnableWindow(GetDlgItem(hwndDlg,IDOK),1);
            }
          }
        break;
        case IDC_TEST:
          if (!g_hThread) {
            char buf[1024];
            GetDlgItemText(hwndDlg,IDC_OUTFILE,buf,sizeof(buf));
            ShellExecute(hwndDlg,"open",buf,"","",SW_SHOW);
          }
        break;
        case IDOK:
          if (!g_hThread)
          {
            if (!made)
            {
              GetDlgItemText(hwndDlg,IDC_COMPILER,g_makensis_path,sizeof(g_makensis_path));
              SetDlgItemText(g_hwnd, IDC_OUTPUTTEXT, "");
              int x;
              for (x = 0; x < sizeof(ids)/sizeof(ids[0]); x ++)
                ShowWindow(GetDlgItem(hwndDlg,ids[x]),SW_HIDE);            
              ShowWindow(GetDlgItem(hwndDlg,IDC_OUTPUTTEXT),SW_SHOWNA);
              SetDlgItemText(hwndDlg,IDOK,"Close");
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