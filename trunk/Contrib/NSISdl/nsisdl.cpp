/*
  NSIS-DL 1.3 - http downloading DLL for NSIS
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
*/
#include <windows.h>
#include <stdio.h>
#include <commctrl.h>

#include "netinc.h"
#include "util.h"
#include "httpget.h"
#include "../exdll/exdll.h"

void *operator new( unsigned int num_bytes )
{
  return GlobalAlloc(GPTR,num_bytes);
}
void operator delete( void *p ) { if (p) GlobalFree(p); }


HMODULE     hModule;
HWND        g_hwndProgressBar;
HWND        g_hwndStatic;
static int  g_cancelled;
static void *lpWndProcOld;

static UINT uMsgCreate;

HWND childwnd;
HWND hwndL;
HWND hwndB;

static LRESULT CALLBACK ParentWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  if (uMsgCreate && message == uMsgCreate)
  {
    if (wParam)
    {
      childwnd = FindWindowEx((HWND) lParam, NULL, "#32770", NULL);
      hwndL = GetDlgItem(childwnd, 1016);
      hwndB = GetDlgItem(childwnd, 1027);
      HWND hwndP = GetDlgItem(childwnd, 1004);
      HWND hwndS = GetDlgItem(childwnd, 1006);
      if (childwnd && hwndP && hwndS)
      {
        if (IsWindowVisible(hwndL))
          ShowWindow(hwndL, SW_HIDE);
        else
          hwndL = NULL;
        if (IsWindowVisible(hwndB))
          ShowWindow(hwndB, SW_HIDE);
        else
          hwndB = NULL;

        RECT wndRect, ctlRect;

        GetClientRect(childwnd, &wndRect);

        GetWindowRect(hwndS, &ctlRect);

        HWND s = g_hwndStatic = CreateWindow(
          "STATIC",
          "",
          WS_CHILD | WS_CLIPSIBLINGS | SS_CENTER,
          0,
          wndRect.bottom / 2 - (ctlRect.bottom - ctlRect.top) / 2,
          wndRect.right,
          ctlRect.bottom - ctlRect.top,
          childwnd,
          NULL,
          hModule,
          NULL
        );

        DWORD dwStyle = WS_CHILD | WS_CLIPSIBLINGS;
        dwStyle |= GetWindowLong(hwndP, GWL_STYLE) & PBS_SMOOTH;
        
        GetWindowRect(hwndP, &ctlRect);

        HWND pb = g_hwndProgressBar = CreateWindow(
          "msctls_progress32",
          "",
          dwStyle,
          0,
          wndRect.bottom / 2 + (ctlRect.bottom - ctlRect.top) / 2,
          wndRect.right,
          ctlRect.bottom - ctlRect.top,
          childwnd,
          NULL,
          hModule,
          NULL
        );

        long c;

        c = SendMessage(hwndP, PBM_SETBARCOLOR, 0, 0);
        SendMessage(hwndP, PBM_SETBARCOLOR, 0, c);
        SendMessage(pb, PBM_SETBARCOLOR, 0, c);

        c = SendMessage(hwndP, PBM_SETBKCOLOR, 0, 0);
        SendMessage(hwndP, PBM_SETBKCOLOR, 0, c);
        SendMessage(pb, PBM_SETBKCOLOR, 0, c);

        // set font
        long hFont = SendMessage((HWND) lParam, WM_GETFONT, 0, 0);
        SendMessage(pb, WM_SETFONT, hFont, 0);
        SendMessage(s, WM_SETFONT, hFont, 0);

        ShowWindow(pb, SW_SHOWNA);
        ShowWindow(s, SW_SHOWNA);
      }
      else
        childwnd = NULL;
    }
    else if (childwnd)
    {
      if (g_hwndStatic)
      {
        DestroyWindow(g_hwndStatic);
        g_hwndStatic = NULL;
      }
      if (g_hwndProgressBar)
      {
        DestroyWindow(g_hwndProgressBar);
        g_hwndProgressBar = NULL;
      }
      if (hwndB)
      {
        ShowWindow(hwndB, SW_SHOWNA);
        hwndB = NULL;
      }
      if (hwndL)
      {
        ShowWindow(hwndL, SW_SHOWNA);
        hwndL = NULL;
      }

      childwnd = NULL;
    }
  }
  else if (message == WM_COMMAND && LOWORD(wParam) == IDCANCEL)
  {
    g_cancelled = 1;
  }
  else
  {
    return CallWindowProc(
      (WNDPROC) lpWndProcOld,
      hwnd,
      message,
      wParam,
      lParam
    );
  }
  return 0;
}

BOOL APIENTRY DllMain(HINSTANCE _hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
  hModule = _hModule;
  return TRUE;
}

static int g_file_size;
static DWORD g_dwLastTick = 0;
void progress_callback(char *msg, int read_bytes)
{
  // flicker reduction by A. Schiffler
  DWORD dwLastTick = g_dwLastTick;
  DWORD dwThisTick = GetTickCount();
  if (childwnd)
  {
    if (dwThisTick - dwLastTick > 500)
    {
      SetWindowText(g_hwndStatic, msg);
      dwLastTick = dwThisTick;
    }
    if (g_file_size)
      SendMessage(g_hwndProgressBar, PBM_SETPOS, (WPARAM) MulDiv(read_bytes, 30000, g_file_size), 0);
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
  BOOL bSuccess=FALSE;
  int timeout_ms=30000;

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

  if (hFile == INVALID_HANDLE_VALUE)
  {
    wsprintf(buf, "Unable to open %s", filename);
    error = buf;
  }
  else
  {
    HWND hwndPrevFocus;
    BOOL fCancelDisabled;

    if (parent)
    {
      uMsgCreate = RegisterWindowMessage("nsisdl create");

      lpWndProcOld = (void *)SetWindowLong(parent,GWL_WNDPROC,(long)ParentWndProc);

      SendMessage(parent, uMsgCreate, TRUE, (LPARAM) parent);

      // set initial text
      char *p = filename;
      while (*p) p++;
      while (*p != '\\' && p != filename) p = CharPrev(filename, p);
      wsprintf(buf, szDownloading, p != filename ? p + 1 : p);
      SetDlgItemText(childwnd, 1006, buf);
      SetWindowText(g_hwndStatic, szConnecting);

      // enable the cancel button
      hwndPrevFocus = GetFocus();
      fCancelDisabled = EnableWindow(GetDlgItem(parent, IDCANCEL), TRUE);
      SendMessage(parent, WM_NEXTDLGCTL, (WPARAM)GetDlgItem(parent, IDCANCEL), TRUE);
    }
    {
      WSADATA wsaData;
      WSAStartup(MAKEWORD(1, 1), &wsaData);

      JNL_HTTPGet *get = 0;

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
        if (g_cancelled)
            error = "cancel";

        if (error)
        {
          if (parent)
          {
            SendMessage(parent, uMsgCreate, FALSE, (LPARAM) parent);
            SetWindowLong(parent, GWL_WNDPROC, (long)lpWndProcOld);

            // Prevent wierd stuff happening if the cancel button happens to be
            // pressed at the moment we are finishing
            SendMessage(GetDlgItem(parent, IDCANCEL), BM_SETSTATE, FALSE, 0);
            // Restore the previous focus and cancel button states
            SendMessage(parent, WM_NEXTDLGCTL, (WPARAM)hwndPrevFocus, TRUE);
            if (fCancelDisabled)
              EnableWindow(GetDlgItem(parent, IDCANCEL), FALSE);
          }
          break;
        }

        Sleep(25);

        st = get->run ();

        if (st == -1) {
          lstrcpyn(url, get->geterrorstr(), sizeof(url));
          error = url;
        } else if (st == 1) {
          if (sofar < cl)
            error="download incomplete";
          else
          {
            bSuccess=TRUE;
            error = "success";
          }
        } else {

          if (get->get_status () == 0) {
            // progressFunc ("Connecting ...", 0);
            if (last_recv_time+timeout_ms < GetTickCount())
              error = "Timed out on connecting.";

          } else if (get->get_status () == 1) {

            progress_callback("Reading headers", 0);
            if (last_recv_time+timeout_ms < GetTickCount())
              error = "Timed out on getting headers.";

          } else if (get->get_status () == 2) {

            if (! has_printed_headers) {
              has_printed_headers = 1;
              last_recv_time=GetTickCount();

              cl = get->content_length ();
              if (cl == 0)
                error = "Server did not specify content length.";
              else if (g_hwndProgressBar) {
                SendMessage(g_hwndProgressBar, PBM_SETRANGE, 0, MAKELPARAM(0, 30000));
                g_file_size = cl;
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
                progress_callback(buf, sofar);
              } else {
                if (sofar < cl)
                  error = "Server aborted.";
              }
            }
            if (GetTickCount() > last_recv_time+timeout_ms)
              error = "Downloading timed out.";

          } else {
            error = "Bad response status.";
          }
        }

      }

      // Clean up the connection then release winsock
      if (get) delete get;
      WSACleanup();
    }

    CloseHandle(hFile);
  }

  if (g_cancelled || !bSuccess) {
    DeleteFile(filename);
  }

  pushstring(error);
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
