#include <windows.h>
#include <windowsx.h>
//#pragma comment(lib, "winmm.lib")

HBITMAP g_hbm;
int sleep_val;
int g_rv;




HBITMAP CreateBitmapMask(HBITMAP hbmColour, COLORREF crTransparent)
{
    HDC hdcMem, hdcMem2;
    HBITMAP hbmMask;
    BITMAP bm;

    // Create monochrome (1 bit) mask bitmap.  

    GetObject(hbmColour, sizeof(BITMAP), &bm);
    hbmMask = CreateBitmap(bm.bmWidth, bm.bmHeight, 1, 1, NULL);

    // Get some HDCs that are compatible with the display driver

    hdcMem = CreateCompatibleDC(0);
    hdcMem2 = CreateCompatibleDC(0);

    SelectBitmap(hdcMem, hbmColour);
    SelectBitmap(hdcMem2, hbmMask);

    // Set the background colour of the colour image to the colour
    // you want to be transparent.
    SetBkColor(hdcMem, crTransparent);

    // Copy the bits from the colour image to the B+W mask... everything
    // with the background colour ends up white while everythig else ends up
    // black...Just what we wanted.

    BitBlt(hdcMem2, 0, 0, bm.bmWidth, bm.bmHeight, hdcMem, 0, 0, SRCCOPY);

    // Take our new mask and use it to turn the transparent colour in our
    // original colour image to black so the transparency effect will
    // work right.
    BitBlt(hdcMem, 0, 0, bm.bmWidth, bm.bmHeight, hdcMem2, 0, 0, SRCINVERT);

    // Clean up.

    DeleteDC(hdcMem);
    DeleteDC(hdcMem2);

    return hbmMask;
}




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
//    HBITMAP oldbm;
	HBITMAP g_hbmMask;
    GetClientRect(hwnd,&r);
//oldbm=(HBITMAP)SelectObject(hdc,g_hbm);
g_hbmMask = CreateBitmapMask(g_hbm, RGB(0, 255, 0));
	SelectObject(hdc, g_hbmMask);
    BitBlt(curdc,r.left,r.top,r.right-r.left,r.bottom-r.top,hdc,0,0, SRCAND);
    SelectObject(hdc, g_hbm);
    BitBlt(curdc,r.left,r.top,r.right-r.left,r.bottom-r.top,hdc,0,0, SRCPAINT);

//    BitBlt(curdc,r.left,r.top,r.right-r.left,r.bottom-r.top,hdc,0,0,SRCCOPY);
//    SelectObject(hdc,oldbm);
    DeleteDC(hdc);
    EndPaint(hwnd,&ps);
    return 0;
  }
  if (uMsg == WM_CLOSE) return 0;
  if (uMsg == WM_DESTROY)
  {
    PostQuitMessage(0);
    return 0;
  }
  if (uMsg == WM_TIMER || uMsg == WM_LBUTTONDOWN)
  {
    g_rv=(uMsg == WM_LBUTTONDOWN);
    DestroyWindow(hwnd);
  }
  return DefWindowProc(hwnd,uMsg,wParam,lParam);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInst,LPSTR lpszCmdParam, int nCmdShow)
{
  char fn[MAX_PATH];
  int hwndParent;
  char *o=fn;

  hInstance=GetModuleHandle(NULL);
  lpszCmdParam=GetCommandLine();
  if (*lpszCmdParam == '\"')
  {
    do
    {
      lpszCmdParam++; 
    } while (*lpszCmdParam != '\"' && *lpszCmdParam);
    if (*lpszCmdParam) lpszCmdParam++;
  }
  else 
  {
    do
    {
      lpszCmdParam++; 
    } while (*lpszCmdParam != ' ' && *lpszCmdParam);
  }
  while (*lpszCmdParam == ' ') lpszCmdParam++;
  sleep_val=0;
  while (*lpszCmdParam >= '0' && *lpszCmdParam <= '9')
  {
    sleep_val*=10;
    sleep_val += *lpszCmdParam++-'0';
  }

  while (*lpszCmdParam == ' ') lpszCmdParam++;
  hwndParent=0;
  while (*lpszCmdParam >= '0' && *lpszCmdParam <= '9')
  {
    hwndParent*=10;
    hwndParent += *lpszCmdParam++-'0';
  }

  while (*lpszCmdParam == ' ') lpszCmdParam++;
  while (*lpszCmdParam)
  {
    *o++=*lpszCmdParam++;
  }
  *o=0;

  if (fn[0] && sleep_val>0)
  {
    MSG msg;
    char classname[4]="_sp";
    static WNDCLASS wc;
	  wc.lpfnWndProc = WndProc;
	  wc.hInstance = hInstance;
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
        BOOL s=0;
        HANDLE f=CreateFile(fn2,0,0,NULL,OPEN_EXISTING,0,NULL);
        if (f != INVALID_HANDLE_VALUE) { CloseHandle(f); s=PlaySound(fn2,NULL,SND_ASYNC|SND_FILENAME); }

        CreateWindowEx(WS_EX_TOOLWINDOW,classname,classname,
          0,0,0,0,0,(HWND)hwndParent,NULL,hInstance,NULL);

        while (GetMessage(&msg,NULL,0,0))
        {
          DispatchMessage(&msg);
        }

        if (s) PlaySound(NULL,0,0);

        DeleteObject(g_hbm);
      }
    }
  }
  ExitProcess(g_rv);
}

