#include <windows.h>
#include "../ExDLL/exdll.h"

HINSTANCE g_hInstance;

HBITMAP g_hbm;
int sleep_val;
int g_rv=-1;

static LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_CREATE)
  {
 	  BITMAP bm;
    RECT vp;
    GetObject(g_hbm, sizeof(bm), (LPSTR)&bm);
    SystemParametersInfo(SPI_GETWORKAREA, 0, &vp, 0);
    SetWindowLong(hwnd,GWL_STYLE,0);
    SetWindowPos(hwnd,NULL,
      vp.left+(vp.right-vp.left-bm.bmWidth)/2,
      vp.top+(vp.bottom-vp.top-bm.bmHeight)/2,
      bm.bmWidth,bm.bmHeight,
      SWP_NOZORDER);
    ShowWindow(hwnd,SW_SHOW);
    SetTimer(hwnd,1,sleep_val,NULL);
    return 0;
  }
  if (uMsg == WM_PAINT)
  {
    PAINTSTRUCT ps;
    RECT r;
    HDC curdc=BeginPaint(hwnd,&ps);
    HDC hdc=CreateCompatibleDC(curdc);
    HBITMAP oldbm;
    GetClientRect(hwnd,&r);
    oldbm=(HBITMAP)SelectObject(hdc,g_hbm);
    BitBlt(curdc,r.left,r.top,r.right-r.left,r.bottom-r.top,hdc,0,0,SRCCOPY);
    SelectObject(hdc,oldbm);
    DeleteDC(hdc);
    EndPaint(hwnd,&ps);
    return 0;
  }
  if (uMsg == WM_CLOSE) return 0;
  if (uMsg == WM_TIMER || uMsg == WM_LBUTTONDOWN)
  {
    g_rv=(uMsg == WM_LBUTTONDOWN);
    DestroyWindow(hwnd);
  }
  return DefWindowProc(hwnd,uMsg,wParam,lParam);
}

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  g_hInstance=hInst;
	return TRUE;
}

void __declspec(dllexport) show(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  char fn[MAX_PATH];
  char temp[64];
  char *sleep=temp;

 
  EXDLL_INIT();

  popstring(sleep);
  popstring(fn);

  sleep_val=0;
  while (*sleep >= '0' && *sleep <= '9')
  {
    sleep_val*=10;
    sleep_val+=*sleep++-'0';
  }

  if (fn[0] && sleep_val>0)
  {
    MSG msg;
    char classname[4]="_sp";
    static WNDCLASS wc;
    wc.lpfnWndProc = WndProc;
    wc.hInstance = g_hInstance;
    wc.hCursor = LoadCursor(NULL,IDC_ARROW);
    wc.lpszClassName = classname;
    if (RegisterClass(&wc)) 
    {
      char fn2[MAX_PATH];
      lstrcpy(fn2,fn);
      lstrcat(fn,".bmp");
      lstrcat(fn2,".wav");
      g_hbm=LoadImage(NULL,fn,IMAGE_BITMAP,0,0,LR_CREATEDIBSECTION|LR_LOADFROMFILE);
      if (g_hbm) 
      {
        HWND myWnd;

        PlaySound(fn2,NULL,SND_ASYNC|SND_FILENAME|SND_NODEFAULT);

        myWnd = CreateWindowEx(WS_EX_TOOLWINDOW,classname,classname,
          0,0,0,0,0,(HWND)hwndParent,NULL,g_hInstance,NULL);

        while (IsWindow(myWnd) && GetMessage(&msg,myWnd,0,0))
        {
          DispatchMessage(&msg);
        }

        // Stop currently playing wave, we want to exit
        PlaySound(0,0,0);

        DeleteObject(g_hbm);

        UnregisterClass(classname, g_hInstance);

      }
    }
  }
  wsprintf(temp,"%d",g_rv);
  pushstring(temp);
}
