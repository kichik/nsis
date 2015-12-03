// Unicode support by Jim Park -- 08/22/2007
// For layered windows
#define _WIN32_WINNT 0x0500

#include <windows.h>
#include <windowsx.h>
#include <nsis/pluginapi.h> // nsis plugin

#ifndef LWA_COLORKEY
#  define LWA_COLORKEY 1
#  define LWA_ALPHA 2
#endif


#define RESOLUTION 32 // 30 fps ;) (32? I like SHR more than iDIV ;)

HINSTANCE g_hInstance;
HBITMAP g_hbm;
BITMAP bm;
int g_rv;
int resolution;
int sleep_val, fadein_val, fadeout_val, state, timeleft, keycolor, alphaparam;
const TCHAR classname[4] = _T("_sp");

typedef BOOL(WINAPI*SetLayeredWindowAttributes_T)(HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags);
SetLayeredWindowAttributes_T SetLayeredWindowAttributesProc;
#define IsLayeredWnd() ( SetLayeredWindowAttributesProc != NULL )

static LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  PAINTSTRUCT ps;
  RECT r;
  HDC curdc = NULL;
  HDC hdc;
  HBITMAP oldbm;

  switch (uMsg) {
  case WM_CREATE:
    SystemParametersInfo(SPI_GETWORKAREA, 0, &r, 0);
    SetWindowLongPtr(hwnd, GWL_STYLE, 0);
    SetWindowPos(hwnd, NULL,
                 r.left + (r.right - r.left - bm.bmWidth) / 2,
                 r.top + (r.bottom - r.top - bm.bmHeight) / 2,
                 bm.bmWidth, bm.bmHeight, SWP_NOZORDER | SWP_SHOWWINDOW);
    return 0;

  case WM_PAINT:
    curdc = BeginPaint(hwnd, &ps);
    hdc = CreateCompatibleDC(curdc);
    GetClientRect(hwnd, &r);

    oldbm = SelectObject(hdc, g_hbm);
    BitBlt(curdc, r.left, r.top, r.right - r.left, r.bottom - r.top, hdc,
           0, 0, SRCCOPY);

    SelectObject(hdc, oldbm);
    DeleteDC(hdc);
    EndPaint(hwnd, &ps);

  case WM_CLOSE:
    return 0;

  case WM_TIMER:
  case WM_LBUTTONDOWN:
    g_rv = (uMsg == WM_LBUTTONDOWN);
    DestroyWindow(hwnd);
    break;
  }
  return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

void SetTransparentRegion(HWND myWnd)
{
  HDC dc;
  int x, y;
  HRGN region, cutrgn;
  BITMAPINFO bmi;
  int size = bm.bmWidth * bm.bmHeight * 4;
  int *bmp = GlobalAlloc(GPTR, size);
  int *bmp_orig = bmp;
  bmi.bmiHeader.biBitCount = 32;
  bmi.bmiHeader.biCompression = BI_RGB;
  bmi.bmiHeader.biHeight = bm.bmHeight;
  bmi.bmiHeader.biPlanes = 1;
  bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
  bmi.bmiHeader.biWidth = bm.bmWidth;
  bmi.bmiHeader.biClrUsed = 0;
  bmi.bmiHeader.biClrImportant = 0;

  dc = CreateCompatibleDC(NULL);
  SelectObject(dc, g_hbm);

  x = GetDIBits(dc, g_hbm, 0, bm.bmHeight, bmp, &bmi, DIB_RGB_COLORS);

  region = CreateRectRgn(0, 0, bm.bmWidth, bm.bmHeight);

  // Search for transparent pixels 
  for (y = bm.bmHeight - 1; y >= 0; y--) {
    for (x = 0; x < bm.bmWidth;) {
      if ((*bmp & 0xFFFFFF) == keycolor) {
        int j = x;
        while ((x < bm.bmWidth) && ((*bmp & 0xFFFFFF) == keycolor)) {
          bmp++, x++;
        }

        // Cut transparent pixels from the original region
        cutrgn = CreateRectRgn(j, y, x, y + 1);
        CombineRgn(region, region, cutrgn, RGN_XOR);
        DeleteObject(cutrgn);
      } else {
        bmp++, x++;
      }
    }
  }

  // Set resulting region.
  SetWindowRgn(myWnd, region, TRUE);
  DeleteObject(region);
  DeleteObject(dc);
  GlobalFree(bmp_orig);
}

BOOL WINAPI DllMain(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  g_hInstance = hInst;
  return TRUE;
}

void CALLBACK TimeProc(UINT uID, UINT uMsg, DWORD_PTR dwUser, DWORD_PTR dw1, DWORD_PTR dw2)
{
  int call = -1;
  switch (state) {
    // FadeIN
  case 0:
    if (timeleft == 0) {
      timeleft = sleep_val;
      state++;
      if (IsLayeredWnd())
        call = 255;
    } else {
      call = ((fadein_val - timeleft) * 255) / fadein_val;
      break;
    }
    // Sleep
  case 1:
    if (timeleft == 0) {
      timeleft = fadeout_val;
      state++;
      // fadeout
    } else
      break;
    // FadeOUT
  case 2:
    if (timeleft == 0) {
      PostMessage((HWND) dwUser, WM_TIMER, 0, 0);
      return;
    } else {
      call = ((timeleft) * 255) / fadeout_val;
      break;
    }
  }
  // Transparency value aquired, and could be set...
  if ((call >= 0) && IsLayeredWnd())
  {
    SetLayeredWindowAttributesProc((HWND) dwUser, keycolor, (BYTE) call, alphaparam);
  }

  // Time is running out...
  timeleft--;
}

void __declspec(dllexport) show(HWND hwndParent, int string_size,
                                TCHAR *variables, stack_t ** stacktop)
{
  DEVMODE dm;
  TCHAR fn[MAX_PATH], uselayerwnd;
  TCHAR temp[64];
  FARPROC slwa;

  g_rv = -1;
  resolution = RESOLUTION;

  EXDLL_INIT();

  popstring(temp);
  sleep_val = myatoi(temp);
  popstring(temp);
  fadein_val = myatoi(temp);
  popstring(temp);
  fadeout_val = myatoi(temp);
  popstring(temp);
  keycolor = myatoi(temp);
  popstring(fn);

  dm.dmSize = sizeof(DEVMODE);
  EnumDisplaySettings(NULL, ENUM_CURRENT_SETTINGS, &dm);
#ifdef _WIN64
  slwa = (FARPROC) SetLayeredWindowAttributes;
#else
  slwa = GetProcAddress(LoadLibraryA("USER32"), "SetLayeredWindowAttributes");
#endif

  // Check for winXP/2k at 32 bpp transparency
  uselayerwnd = slwa && dm.dmBitsPerPel >= 32 && keycolor == -1;

  if (!uselayerwnd) {
    // Fading+transparency is unsupported at old windows versions...
    resolution = sleep_val + fadein_val + fadeout_val;
    fadeout_val = fadein_val = 0;
    sleep_val = 1;
    state = 1; // skip fade in
  } else {
    // div them by resolution
    sleep_val >>= 5;
    fadein_val >>= 5;
    fadeout_val >>= 5;
    state = 0;

    alphaparam = LWA_ALPHA | ((keycolor == -1) ? (0) : (LWA_COLORKEY));
    keycolor =
        ((keycolor & 0xFF) << 16) + (keycolor & 0xFF00) +
        ((keycolor & 0xFF0000) >> 16);
  }

  if (fn[0] && ((sleep_val + fadein_val + fadeout_val) > 0)) {
    MSG msg;
    static WNDCLASS wc;
    wc.lpfnWndProc = WndProc;
    wc.hInstance = g_hInstance;
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.lpszClassName = classname;
    if (RegisterClass(&wc)) {
      TCHAR fn2[MAX_PATH];

      fn2[0] = _T('\0'), lstrcat(fn2, fn);
      lstrcat(fn2, _T(".wav"));
      lstrcat(fn, _T(".bmp"));

      g_hbm = LoadImage(NULL, fn, IMAGE_BITMAP, 0, 0,
                LR_CREATEDIBSECTION | LR_LOADFROMFILE);
      if (g_hbm) {
        HWND myWnd;
        UINT timerEvent;

        // Get Bitmap Information
        GetObject(g_hbm, sizeof(bm), & bm);

        myWnd = CreateWindowEx(WS_EX_TOOLWINDOW |
                           (uselayerwnd ? (WS_EX_LAYERED) : (0)), classname,
                           classname, 0, 0, 0, 0, 0, (HWND) hwndParent,
                           NULL, g_hInstance, NULL);

        // Set transparency / key color
        if (uselayerwnd) {
          SetLayeredWindowAttributesProc = (SetLayeredWindowAttributes_T) slwa;

          // Use win2k method
          SetLayeredWindowAttributesProc(myWnd, keycolor,
                                         (BYTE) ((fadein_val > 0) ? (0) : (255)),
                                         alphaparam);
        } else if (keycolor != -1) {
          // transparency mode                                
          SetTransparentRegion(myWnd);
        }

        PlaySound(fn2, NULL, SND_ASYNC | SND_FILENAME | SND_NODEFAULT);

        // Start up timer...
        timeleft = fadein_val;
        timerEvent =
            timeSetEvent(resolution, RESOLUTION / 4, TimeProc,
                         (DWORD_PTR) myWnd, TIME_PERIODIC);

        while (IsWindow(myWnd) && GetMessage(&msg, myWnd, 0, 0)) {
          DispatchMessage(&msg);
        }

        // Kill the timer...
        timeKillEvent(timerEvent);

        // Stop currently playing wave, we want to exit
        PlaySound(0, 0, 0);

        DeleteObject(g_hbm);
      }
      // We should UnRegister class, since Windows NT series never does this by itself
      UnregisterClass(wc.lpszClassName, wc.hInstance);
    }
  }
  wsprintf(temp, _T("%d"), g_rv);
  pushstring(temp);
}
