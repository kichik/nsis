/*
  NSIS-DL 1.2 - http downloading DLL for NSIS
  Copyright (C) 2001-2002 Yaroslav Faybishenko & Justin Frankel

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.



  Note: this source code is pretty hacked together right now, improvements
  and cleanups will come later.

  */

#include <windows.h>
#include <stdio.h>
#include <commctrl.h>

#include "netinc.h"
#include "util.h"
#include "resource.h"
#include "httpget.h"

int g_timeout_ms=30000;

void *operator new( unsigned int num_bytes )
{
  return GlobalAlloc(GPTR,num_bytes);
}
void operator delete( void *p ) { if (p) GlobalFree(p); }

typedef struct _stack_t {
  struct _stack_t *next;
  char             text[1]; // this should be the length of string_size
} stack_t;

static int  popstring(char *str); // 0 on success, 1 on empty stack
static void setuservariable(int varnum, char *var);

enum
{
INST_0,         // $0
INST_1,         // $1
INST_2,         // $2
INST_3,         // $3
INST_4,         // $4
INST_5,         // $5
INST_6,         // $6
INST_7,         // $7
INST_8,         // $8
INST_9,         // $9
INST_R0,        // $R0
INST_R1,        // $R1
INST_R2,        // $R2
INST_R3,        // $R3
INST_R4,        // $R4
INST_R5,        // $R5
INST_R6,        // $R6
INST_R7,        // $R7
INST_R8,        // $R8
INST_R9,        // $R9
INST_CMDLINE,   // $CMDLINE
INST_INSTDIR,   // $INSTDIR
INST_OUTDIR,    // $OUTDIR
INST_EXEDIR,    // $EXEDIR
__INST_LAST
};


HANDLE    hModule;
HWND      g_parent;
HWND      g_dialog;
HWND      g_childwnd;
int       g_stringsize;
stack_t **g_stacktop;
char     *g_variables;
static int       g_cancelled;

BOOL CALLBACK DownloadDialogProc(HWND   hwndDlg, 
								 UINT   uMsg,    
								 WPARAM wParam,
								 LPARAM lParam)
{
	return 0;
}


static void *lpWndProcOld;

static LRESULT CALLBACK ParentWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	if (message == WM_COMMAND && LOWORD(wParam) == IDCANCEL)
  {
		g_cancelled = 1;
    return 0;
  }
  return CallWindowProc((long (__stdcall *)(struct HWND__ *,unsigned int,unsigned int,long))lpWndProcOld,hwnd,message,wParam,lParam);
}


BOOL APIENTRY DllMain( HANDLE _hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{

  switch (ul_reason_for_call)
	{
		case DLL_PROCESS_ATTACH:
		case DLL_THREAD_ATTACH:
			hModule = _hModule;
			JNL::open_socketlib ();
			break;

		case DLL_THREAD_DETACH:
		case DLL_PROCESS_DETACH:
			JNL::close_socketlib ();
			break;

    }
	
	return TRUE;
}


static int g_file_size;

static void progress_callback(char *msg, int read_bytes)
{
  if (g_dialog)
  {
	  HWND hwndProgressBar = GetDlgItem (g_dialog, IDC_PROGRESS1);

	  SetDlgItemText (g_dialog, IDC_STATIC2, msg);
	  if (g_file_size) SendMessage(hwndProgressBar, PBM_SETPOS, (WPARAM)MulDiv(read_bytes,30000,g_file_size), 0);
  }
}

static int getProxyInfo(char *out)
{
  DWORD v=0;
	HKEY hKey;
  if (RegOpenKeyEx(HKEY_CURRENT_USER,"Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings",0,KEY_READ,&hKey) == ERROR_SUCCESS)
  {
		DWORD l = 4;
		DWORD t;
    if (RegQueryValueEx(hKey,"ProxyEnable",NULL,&t,(unsigned char *)&v,&l) == ERROR_SUCCESS && t == REG_DWORD)
    {
      l=8192;
      if (RegQueryValueEx(hKey,"ProxyServer",NULL,&t,(unsigned char *)out,&l ) != ERROR_SUCCESS || t != REG_SZ) 
      { 
        v=0; 
        *out=0; 
      }
    }
    else v=0;
    out[8192-1]=0;
    RegCloseKey(hKey);
  }
  return v;
}


extern char *_strstr(char *i, char *s);

static
void downloadFile(char         *url, 
				   HANDLE hFile, 
				   char         **error)
{
	static char buf[8192];
  char *p=NULL;
  if (getProxyInfo(buf))
  {
    p=_strstr(buf,"http=");
    if (!p) p=buf;
    else 
    {
      p+=5;
      char *tp=_strstr(p,";");
      if (tp) *tp=0;
    }
  }
  DWORD start_time=GetTickCount();
  JNL_HTTPGet *get=new JNL_HTTPGet(JNL_CONNECTION_AUTODNS,16384,(p&&p[0])?p:NULL);
	int         st;
	int         has_printed_headers = 0;
	int         cl;
	int         len;
	int         sofar = 0;
  DWORD last_recv_time=start_time;

	get->addheader ("User-Agent: NSISDL/1.2 (Mozilla)");
	get->addheader ("Accept: */*");

	get->connect (url);

	while (1) {
    if (g_dialog)
    {
      MSG msg;
      while (PeekMessage(&msg,g_dialog,0,0,PM_REMOVE))
      {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
      } 
    }
	
    Sleep(25);

		if (g_cancelled) break;

		st = get->run ();

		if (st == -1) {
      *error=get->geterrorstr();
			break;
		} else if (st == 1) {
			if (sofar < cl)
        *error="download incomplete";
			break;
		} else {

			if (get->get_status () == 0) {
				// progressFunc ("Connecting ...", 0);
        if (last_recv_time+g_timeout_ms < GetTickCount())
        {
				  *error = "Timed out on connecting.";
				  break;
        }

			} else if (get->get_status () == 1) {

				progress_callback("Reading headers", 0);
        if (last_recv_time+g_timeout_ms < GetTickCount())
        {
				  *error = "Timed out on getting headers.";
				  break;
        }

			} else if (get->get_status () == 2) {

				if (! has_printed_headers) {
					has_printed_headers = 1;
          last_recv_time=GetTickCount();

					cl = get->content_length ();
					if (cl == 0) {
						*error = "Server did not specify content length.";
						break;
					} else if (g_dialog) {
            	HWND hwndProgressBar = GetDlgItem (g_dialog, IDC_PROGRESS1);
            	SendMessage(hwndProgressBar, PBM_SETRANGE, 0, MAKELPARAM(0,30000));
              g_file_size=cl;
					}
				}

				while ((len = get->bytes_available ()) > 0) {
					if (len > 8192)
						len = 8192;
					len = get->get_bytes (buf, len);
					if (len > 0) {
            last_recv_time=GetTickCount();
            DWORD dw;
            WriteFile(hFile,buf,len,&dw,NULL);
						sofar += len;
            int time_sofar=(GetTickCount()-start_time)/1000;
            int bps=sofar/(time_sofar?time_sofar:1);
            int remain=MulDiv(time_sofar,cl,sofar) - time_sofar;
            char *rtext="second";
            if (remain >= 60) 
            {
              remain/=60;
              rtext="minute";
              if (remain >= 60)
              {
                remain/=60;
                rtext="hour";
              }
            }
						wsprintf (buf, 
								  "%dkB (%d%%) of %dkB @ %d.%01dkB/s", 
                  sofar/1024,
								  MulDiv(100,sofar,cl),
								  cl/1024,
								  bps/1024,((bps*10)/1024)%10
                  );
            if (remain) wsprintf(buf+lstrlen(buf)," (%d %s%s remaining)",
                  remain,
                  rtext,
                  remain==1?"":"s"
                  );
						progress_callback(buf, sofar);
					} else {
						if (sofar < cl)
							*error = "Server aborted.";

						break;
					}
				}
        if (GetTickCount() > last_recv_time+g_timeout_ms)
        {
				  *error = "Downloading timed out.";
				  break;
        }

			} else {
				*error = "Bad response status.";
				break;
			}
		}
		
	}

  if (*error)
  {
    char *t=*error;
		*error = (char *)GlobalAlloc(GPTR,strlen(t)+1);
    lstrcpy(*error,t);
  }
  delete get;
}

extern "C" 
{

__declspec(dllexport) void download (HWND   parent,
						  int    stringsize, 
						  char   *variables, 
						  stack_t **stacktop)
{
	static char buf[1024];
	static char url[1024];
	static char filename[1024];
  int wasen=0;
  HWND hwndL=0;
  HWND hwndB=0;

	g_parent     = parent;
	g_stringsize = stringsize;
	g_variables  = variables;
	g_stacktop   = stacktop;

	popstring (filename);
  lstrcpyn(buf, filename, 10);
  if (!lstrcmp(buf, "/TIMEOUT=")) {
    g_timeout_ms=my_atoi(filename+9);
    popstring (filename);
  }
	popstring (url);

  HANDLE hFile = CreateFile(filename,GENERIC_WRITE,FILE_SHARE_READ,NULL,CREATE_ALWAYS,0,NULL);

	if (hFile == INVALID_HANDLE_VALUE) {
 		wsprintf (buf, "Unable to open %s", filename);
  	setuservariable(INST_0, buf);
	} else {  
    if (g_parent)
    {
      g_childwnd=FindWindowEx(g_parent,NULL,"#32770",NULL);
      hwndL=GetDlgItem(g_childwnd,1016);
      hwndB=GetDlgItem(g_childwnd,1027);
      if (hwndL && IsWindowVisible(hwndL)) ShowWindow(hwndL,SW_HIDE);
      else hwndL=NULL;
      if (hwndB && IsWindowVisible(hwndB)) ShowWindow(hwndB,SW_HIDE);
      else hwndB=NULL;

      wasen=EnableWindow(GetDlgItem(g_parent,IDCANCEL),1);
	    lpWndProcOld = (void *) GetWindowLong(g_parent,GWL_WNDPROC);
	    SetWindowLong(g_parent,GWL_WNDPROC,(long)ParentWndProc);

	    g_dialog = CreateDialog((HINSTANCE)hModule, 
							      MAKEINTRESOURCE(IDD_DIALOG1),
							      g_childwnd,
							      DownloadDialogProc);
      if (g_dialog)
      {
        RECT r;
        GetWindowRect(GetDlgItem(g_childwnd,1016),&r);
        ScreenToClient(g_childwnd,(LPPOINT)&r);
        SetWindowPos(g_dialog,0,r.left,r.top,0,0,SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);
        ShowWindow(g_dialog,SW_SHOWNA);
        char *p=filename;
        while (*p) p++;
        while (*p != '\\' && p != filename) p=CharPrev(filename,p);
	      wsprintf(buf,"Downloading %s", p+1);
        SetDlgItemText(g_childwnd,1006,buf);

	      wsprintf(buf,"Connecting ...");
	      SetDlgItemText (g_dialog, IDC_STATIC2, buf);
      }
    }


		char *error=NULL;
		
		downloadFile(url, hFile, &error);

    CloseHandle(hFile);
    if (g_parent)
    {
      if (g_dialog) DestroyWindow(g_dialog);
      if (lpWndProcOld)
        SetWindowLong(g_parent,GWL_WNDPROC,(long)lpWndProcOld);
      if (g_childwnd)
      {
        if (hwndB) ShowWindow(hwndB,SW_SHOWNA);
        if (hwndL) ShowWindow(hwndL,SW_SHOWNA);
      }
      if (wasen) EnableWindow(GetDlgItem(g_parent,IDCANCEL),0);
    }



    if (g_cancelled) {
   		setuservariable(INST_0, "cancel");
			DeleteFile(filename);
		} else if (error == NULL) {
			setuservariable(INST_0, "success");
		} else {
			DeleteFile(filename);
			setuservariable(INST_0, error);
		}
    if (error) GlobalFree(error);
  }
}


__declspec(dllexport) void download_quiet(HWND   parent,
						  int    stringsize, 
						  char   *variables, 
						  stack_t **stacktop)
{
  download(NULL,stringsize,variables,stacktop);
}

}

// utility functions (not required but often useful)
static
int popstring(char *str)
{
  stack_t *th;
  if (!g_stacktop || !*g_stacktop) return 1;
  th=(*g_stacktop);
  lstrcpy(str,th->text);
  *g_stacktop = th->next;
  GlobalFree((HGLOBAL)th);
  return 0;
}

static
void setuservariable(int varnum, char *var)
{
	if (var != NULL && varnum >= 0 && varnum < __INST_LAST) {
		lstrcpy (g_variables + varnum*g_stringsize, var);

	}
}
