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

  IMPORTANT: The dialog must have the style "No Parent Notify"

  */
#include <windows.h>
#include <stdio.h>
#include <commctrl.h>

#include "netinc.h"
#include "util.h"
#include "resource.h"
#include "httpget.h"
#include "../exdll/exdll.h"

void *operator new( unsigned int num_bytes )
{
  return GlobalAlloc(GPTR,num_bytes);
}
void operator delete( void *p ) { if (p) GlobalFree(p); }


HANDLE    hModule;
HWND      g_hwndProgressBar;
static int  g_cancelled;
long lBusy;
ULONG idThreadOwner;
ULONG ulRefCount;
static void *lpWndProcOld;

BOOL CALLBACK DownloadDialogProc(HWND   hwndDlg, 
                 UINT   uMsg,    
                 WPARAM wParam,
                 LPARAM lParam)
{
  return 0;
}

BOOL TryEnterCS()
{
   DWORD CurThreadID = ::GetCurrentThreadId();
   BOOL bRet = TRUE;
   long *plBusy = &lBusy;
   while (::InterlockedExchange(plBusy, 1) != 0)
   {
       Sleep(0);
   }

   if (idThreadOwner == 0)
   {
      idThreadOwner = CurThreadID;
      ulRefCount = 1;
   }
   else if (idThreadOwner == CurThreadID)
   {
      ulRefCount++;
   }
   else
   {
      bRet = FALSE;
   }

   ::InterlockedExchange(plBusy, 0);   

   return bRet;
}

void LeaveCS()
{
   long *plBusy = &lBusy;
   while (::InterlockedExchange(plBusy, 1) != 0)
   {
     Sleep(0);
   }
   if (idThreadOwner == ::GetCurrentThreadId())
   {
      if (--ulRefCount == 0)
      {
         // No owner from now
         idThreadOwner = 0;
      }
   }
   ::InterlockedExchange(plBusy, 0);
}

static LRESULT CALLBACK ParentWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  LRESULT Res = 0;
  while ( !TryEnterCS() ) Sleep(0);
  if (message == WM_COMMAND && LOWORD(wParam) == IDCANCEL)
  {
    g_cancelled = 1;
  }
  else
    Res = CallWindowProc((long (__stdcall *)(struct HWND__ *,unsigned int,unsigned int,long))lpWndProcOld,hwnd,message,wParam,lParam);
  LeaveCS();
  return Res;
}

BOOL APIENTRY DllMain( HANDLE _hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
           )
{
  hModule = _hModule;
  return TRUE;
}


static int g_file_size;
static DWORD g_dwLastTick = 0;
void progress_callback(HWND dlg, char *msg, int read_bytes)
{
  // flicker reduction by A. Schiffler
  DWORD dwLastTick = g_dwLastTick;
  DWORD dwThisTick = GetTickCount();
  if (dlg)
  {
    if (dwThisTick - dwLastTick > 500)
    {
      SetDlgItemText (dlg, IDC_STATIC2, msg);
      dwLastTick = dwThisTick;
    }
    if (g_file_size) SendMessage(g_hwndProgressBar, PBM_SETPOS, (WPARAM)MulDiv(read_bytes,30000,g_file_size), 0);
    g_dwLastTick = dwLastTick;
  }
}

extern char *_strstr(char *i, char *s);
#define strstr _strstr

extern "C" 
{

__declspec(dllexport) void download (HWND   parent,
              int    string_size, 
              char   *variables, 
              stack_t **stacktop)
{
  char buf[1024];
  char url[1024];
  char filename[1024];
  int wasen=0;
  HWND hwndAux;
  HWND hwndL=0;
  HWND hwndB=0;
  HWND dlg=0;
  HWND childwnd=0;
  BOOL bSuccess=FALSE;
  RECT r, cr, orig_childRc;
  int timeout_ms=30000;

  JNL_HTTPGet *get = 0;

  char *error=NULL;

  static char szDownloading[1024];//= "Downloading %s";
  static char szConnecting[1024];//= "Connecting ...";
  static char szSecond[1024];//= "second";
  static char szMinute[1024];//= "minute";
  static char szHour[1024];//= "hour";
  static char szPlural[1024];//= "s";
  static char szProgress[1024];//= "%dkB (%d%%) of %dkB @ %d.%01dkB/s";
  static char szRemaining[1024];//= " (%d %s%s remaining)";

  EXDLL_INIT();

  popstring(url);
  if (!lstrcmpi(url, "/TRANSLATE")) {
    popstring(szDownloading);
    popstring(szConnecting);
    popstring(szSecond);
    popstring(szMinute);
    popstring(szHour);
    popstring(szPlural);
    popstring(szProgress);
    popstring(szRemaining);
    popstring(url);
  }
  else {
    lstrcpy(szDownloading, "Downloading %s");
    lstrcpy(szConnecting, "Connecting ...");
    lstrcpy(szSecond, "second");
    lstrcpy(szMinute, "minute");
    lstrcpy(szHour, "hour");
    lstrcpy(szPlural, "s");
    lstrcpy(szProgress, "%dkB (%d%%) of %dkB @ %d.%01dkB/s");
    lstrcpy(szRemaining, " (%d %s%s remaining)");
  }
  lstrcpyn(buf, url, 10);
  if (!lstrcmpi(buf, "/TIMEOUT=")) {
    timeout_ms=my_atoi(url+9);
    popstring(url);
  }
  popstring(filename);

  HANDLE hFile = CreateFile(filename,GENERIC_WRITE,FILE_SHARE_READ,NULL,CREATE_ALWAYS,0,NULL);

  if (hFile == INVALID_HANDLE_VALUE) {
    wsprintf (buf, "Unable to open %s", filename);
    error=buf;
  } else {
    if (parent)
    {
      childwnd=FindWindowEx(parent,NULL,"#32770",NULL);
      hwndL=GetDlgItem(childwnd,1016);
      hwndB=GetDlgItem(childwnd,1027);
      if ( IsWindowVisible(hwndL) ) ShowWindow(hwndL,SW_HIDE);
      else hwndL=NULL;
      if (IsWindowVisible(hwndB)) ShowWindow(hwndB,SW_HIDE);
      else hwndB=NULL;

      wasen=EnableWindow(GetDlgItem(parent,IDCANCEL),1);

      lpWndProcOld = (void *)SetWindowLong(parent,GWL_WNDPROC,(long)ParentWndProc);

      dlg = CreateDialog((HINSTANCE)hModule, 
                    MAKEINTRESOURCE(IDD_DIALOG1),
                    parent,
                    DownloadDialogProc);
      if (dlg)
      {
        int pbid = IDC_PROGRESS1;
        HWND hwPb = GetDlgItem(childwnd, 1004);

        // Set progress bar style
        if (GetWindowLong(hwPb, GWL_STYLE) & PBS_SMOOTH)
          pbid = IDC_PROGRESS2;

        HWND pb = g_hwndProgressBar = GetDlgItem(dlg, pbid);

        long c;

        if (hwPb)
        {
          c = SendMessage(hwPb, PBM_SETBARCOLOR, 0, 0);
          SendMessage(hwPb, PBM_SETBARCOLOR, 0, c);
          SendMessage(pb, PBM_SETBARCOLOR, 0, c);

          c = SendMessage(hwPb, PBM_SETBKCOLOR, 0, 0);
          SendMessage(hwPb, PBM_SETBKCOLOR, 0, c);
          SendMessage(pb, PBM_SETBKCOLOR, 0, c);
        }

        ShowWindow(pb, SW_SHOW);

        GetWindowRect(childwnd,&orig_childRc);
        GetWindowRect(dlg,&cr);
        ScreenToClient(dlg,(LPPOINT)&cr);
        ScreenToClient(dlg,((LPPOINT)&cr)+1);
        
        hwndAux = GetDlgItem(childwnd,1016);
        GetWindowRect(hwndAux,&r);
        ScreenToClient(childwnd,(LPPOINT)&r);
        ScreenToClient(childwnd,((LPPOINT)&r)+1);
        SetWindowPos(childwnd,0,0,0,r.right-r.left,r.top,SWP_NOACTIVATE|SWP_NOZORDER|SWP_NOMOVE);
        
        GetWindowRect(hwndAux,&r);
        ScreenToClient(parent,(LPPOINT)&r);
        ScreenToClient(parent,((LPPOINT)&r)+1);
        SetWindowPos(dlg,0,r.left,r.top,r.right-r.left,cr.bottom-cr.top,SWP_NOACTIVATE|SWP_NOZORDER|SWP_SHOWWINDOW);

        hwndAux = GetDlgItem(dlg,IDC_STATIC2);
        GetWindowRect(hwndAux,&cr);
        ScreenToClient(dlg,(LPPOINT)&cr);
        ScreenToClient(dlg,((LPPOINT)&cr)+1);
        SetWindowPos(hwndAux,0,0,0,r.right-r.left,cr.bottom-cr.top,SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOZORDER);

        hwndAux = GetDlgItem(dlg,pbid);
        GetWindowRect(hwndAux,&cr);
        ScreenToClient(dlg,(LPPOINT)&cr);
        ScreenToClient(dlg,((LPPOINT)&cr)+1);
        SetWindowPos(hwndAux,0,0,0,r.right-r.left,cr.bottom-cr.top,SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOZORDER);

        char *p=filename;
        while (*p) p++;
        while (*p != '\\' && p != filename) p=CharPrev(filename,p);
        wsprintf(buf,szDownloading, p!=filename?p+1:p);
        SetDlgItemText(childwnd,1006,buf);
        SetDlgItemText(dlg, IDC_STATIC2, szConnecting);

        // set font
        long hFont = SendMessage(parent, WM_GETFONT, 0, 0);
        SendDlgItemMessage(dlg, pbid, WM_SETFONT, hFont, 0);
        SendDlgItemMessage(dlg, IDC_STATIC2, WM_SETFONT, hFont, 0);
      }
    }
    {
      WSADATA wsaData;
      WSAStartup(MAKEWORD(1, 1), &wsaData);

      static char main_buf[8192];
      char *buf=main_buf;
      char *p=NULL;
      
      HKEY hKey;
      if (RegOpenKeyEx(HKEY_CURRENT_USER,"Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings",0,KEY_READ,&hKey) == ERROR_SUCCESS)
      {
        DWORD l = 4;
        DWORD t;
        DWORD v;
        if (RegQueryValueEx(hKey,"ProxyEnable",NULL,&t,(unsigned char *)&v,&l) == ERROR_SUCCESS && t == REG_DWORD && v)
        {
          l=8192;
          if (RegQueryValueEx(hKey,"ProxyServer",NULL,&t,(unsigned char *)buf,&l ) == ERROR_SUCCESS && t == REG_SZ)
          {
            p=strstr(buf,"http=");
            if (!p) p=buf;
            else {
              p+=5;
            }
            char *tp=strstr(p,";");
            if (tp) *tp=0;
            char *p2=strstr(p,"=");
            if (p2) p=0; // we found the wrong proxy
          }
        }
        buf[8192-1]=0;
        RegCloseKey(hKey);
      }

      DWORD start_time=GetTickCount();
      get=new JNL_HTTPGet(JNL_CONNECTION_AUTODNS,16384,(p&&p[0])?p:NULL);
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
        do
        {
          if ( dlg )
          {
            MSG msg;
            while (PeekMessage(&msg,dlg,0,0,PM_REMOVE))
            {
              TranslateMessage(&msg);
              DispatchMessage(&msg);
            }
          }
        }
        while (!TryEnterCS()); // Process messages
        if ((g_cancelled || error) && dlg) 
          DestroyWindow(dlg);
        LeaveCS();

        if ( g_cancelled || error )
        {
          if (parent)
          {
            SetWindowLong(parent,GWL_WNDPROC,(long)lpWndProcOld);
            
            if (hwndB) ShowWindow(hwndB,SW_SHOWNA);
            if (hwndL) ShowWindow(hwndL,SW_SHOWNA);
            
            SetWindowPos(childwnd,0,0,0,orig_childRc.right-orig_childRc.left,orig_childRc.bottom-orig_childRc.top,SWP_NOACTIVATE|SWP_NOZORDER|SWP_NOMOVE);
            
            if (wasen) EnableWindow(GetDlgItem(parent,IDCANCEL),0);
          }
          if ( !error )
            error = "cancel";
          break;
        }
      
        Sleep(25);

        st = get->run ();

        if (st == -1) {
          error=get->geterrorstr();
          //break;
        } else if (st == 1) {
          if (sofar < cl)
            error="download incomplete";
          else
          {
            bSuccess=TRUE;
            error = "success";
          }
          //break;
        } else {

          if (get->get_status () == 0) {
            // progressFunc ("Connecting ...", 0);
            if (last_recv_time+timeout_ms < GetTickCount())
            {
              error = "Timed out on connecting.";
              //break;
            }

          } else if (get->get_status () == 1) {

            progress_callback(dlg, "Reading headers", 0);
            if (last_recv_time+timeout_ms < GetTickCount())
            {
              error = "Timed out on getting headers.";
              //break;
            }

          } else if (get->get_status () == 2) {

            if (! has_printed_headers) {
              has_printed_headers = 1;
              last_recv_time=GetTickCount();

              cl = get->content_length ();
              if (cl == 0) {
                error = "Server did not specify content length.";
                //break;
              } else if (dlg) {
                  SendMessage(g_hwndProgressBar, PBM_SETRANGE, 0, MAKELPARAM(0,30000));
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
                char *rtext=szSecond;
                if (remain >= 60) 
                {
                  remain/=60;
                  rtext=szMinute;
                  if (remain >= 60)
                  {
                    remain/=60;
                    rtext=szHour;
                  }
                }
                wsprintf (buf, 
                      szProgress,
                      sofar/1024,
                      MulDiv(100,sofar,cl),
                      cl/1024,
                      bps/1024,((bps*10)/1024)%10
                      );
                if (remain) wsprintf(buf+lstrlen(buf),szRemaining,
                      remain,
                      rtext,
                      remain==1?"":szPlural
                      );
                progress_callback(dlg, buf, sofar);
              } else {
                if (sofar < cl)
                  error = "Server aborted.";
                //break;
              }
            }
            if (GetTickCount() > last_recv_time+timeout_ms)
            {
              error = "Downloading timed out.";
              //break;
            }

          } else {
            error = "Bad response status.";
          }
        }
        
      }

      WSACleanup();
    }
    
    CloseHandle(hFile);
  }
  
  if (g_cancelled || !bSuccess) {
    DeleteFile(filename);
  }

  pushstring(error);
    
  if (get) delete get;
}


__declspec(dllexport) void download_quiet(HWND   parent,
              int    stringsize, 
              char   *variables, 
              stack_t **stacktop)
{
  g_hwndProgressBar=0;
  download(NULL,stringsize,variables,stacktop);
}

} //extern "C"