/* I modified the window styles to eliminate the annoying title bar.
   8th February 2003 Ximon Eighteen aka Sunjammer */


#include <Windows.h>
#include <Mmsystem.h>
#include "../exdll/exdll.h"

int x, y;
char temp[MAX_PATH];
HBITMAP hBitmap;
HWND hWndImage, hWndParent;

HINSTANCE g_hInstance;

void *oldProc;
LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam);
int myatoi(char *s);
extern "C" void __declspec(dllexport) SetImage(HWND hwndParent, int string_size, char *variables, stack_t **stacktop);

extern "C" void __declspec(dllexport) Init(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) {
  hWndParent = hwndParent;

  SetImage(hwndParent, string_size, variables, stacktop);

  WNDCLASSEX wc = {
    sizeof(WNDCLASSEX),
    CS_VREDRAW|CS_HREDRAW,
    WndProc,
    0,
    0,
    g_hInstance,
    0,//LoadIcon(GetModuleHandle(0), MAKEINTRESOURCE(103)),
    0,
    (HBRUSH)GetStockObject(WHITE_BRUSH),
    0,
    "NSISBGImage",
    0
  };
  if (!RegisterClassEx(&wc)) {
    pushstring("can't register class");
    return;
  }

  hWndImage = CreateWindowEx(
    WS_EX_TOOLWINDOW|WS_EX_LEFT|WS_EX_LTRREADING|WS_EX_RIGHTSCROLLBAR,
    "NSISBGImage",
    0,
    WS_POPUPWINDOW|WS_VISIBLE|WS_CLIPSIBLINGS|WS_OVERLAPPED,
    (GetSystemMetrics(SM_CXSCREEN)-x)/2,
    (GetSystemMetrics(SM_CYSCREEN)-y)/2,
    x,
    y,
    0,
    0,
    g_hInstance,
    0
  );
  if (!hWndImage) {
    pushstring("can't create window");
    return;
  }

  SetWindowLong(hWndImage, GWL_STYLE, WS_VISIBLE);

  oldProc = (void *)SetWindowLong(hwndParent, GWL_WNDPROC, (long)WndProc);
}

extern "C" void __declspec(dllexport) SetImage(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) {
  EXDLL_INIT();

  popstring(temp);
  if (!lstrcmp(temp, "/FILLSCREEN")) {
    x = GetSystemMetrics(SM_CXSCREEN);
    y = GetSystemMetrics(SM_CYSCREEN);
    popstring(temp);
  }
  else x = y = 0;

  BITMAP bitmap;

  if (hBitmap) DeleteObject((HGDIOBJ)hBitmap);
  hBitmap = (HBITMAP)LoadImage(0, temp, IMAGE_BITMAP, x, y, LR_LOADFROMFILE);
  if (!hBitmap) {
    pushstring("can't load bitmap");
    return;
  }

  GetObject(hBitmap, sizeof(bitmap), (LPSTR)&bitmap);
  x = x ? x : bitmap.bmWidth;
  y = y ? y : bitmap.bmHeight;

  if (hWndImage) {
    SetWindowPos(
      hWndImage,
      hWndParent,
      (GetSystemMetrics(SM_CXSCREEN)-x)/2,
      (GetSystemMetrics(SM_CYSCREEN)-y)/2,
      x,
      y,
      SWP_NOACTIVATE
    );
    RedrawWindow(hWndImage, 0, 0, RDW_INVALIDATE | RDW_UPDATENOW);
  }
}

extern "C" void __declspec(dllexport) Destroy(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) {
  SendMessage(hWndImage, WM_CLOSE, 0, 0);
  UnregisterClass("NSISBGImage", g_hInstance);
}

extern "C" void __declspec(dllexport) Sound(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) {
  DWORD flags = SND_FILENAME|SND_NODEFAULT;
  g_stacktop=stacktop;
  popstring(temp);
  if (lstrcmp(temp, "/WAIT"))
    flags |= SND_ASYNC;
  else
    popstring(temp);
  PlaySound(temp, 0, flags);
}

BOOL WINAPI _DllMainCRTStartup(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
  g_hInstance=hInst;
  return TRUE;
}

int myatoi(char *s)
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

LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
  if (hWndImage && hwnd != hWndImage) {
    if (message == WM_WINDOWPOSCHANGED) {
      LPWINDOWPOS wp = (LPWINDOWPOS) lParam;
      if (!(wp->flags & SWP_NOZORDER)) {
        CallWindowProc(
          (long (__stdcall *)(struct HWND__ *,unsigned int,unsigned int,long))oldProc,
          hwnd,
          message,
          wParam,
          lParam
        );
        SetWindowPos(hWndImage, hWndParent, 0, 0, 0, 0, SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOSIZE);
        return 0;
      }
    }
    return CallWindowProc(
      (long (__stdcall *)(struct HWND__ *,unsigned int,unsigned int,long))oldProc,
      hwnd,
      message,
      wParam,
      lParam
    );
  }
  switch (message) {
    case WM_PAINT:
      {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint(hwnd, &ps);
        HDC cdc = CreateCompatibleDC(hdc);
        HGDIOBJ hOldObject = SelectObject(cdc, hBitmap);
        RECT cRect;
        GetClientRect(hwnd, &cRect);
        BitBlt(hdc, cRect.left, cRect.top, cRect.right - cRect.left, cRect.bottom - cRect.top, cdc, 0, 0, SRCCOPY);
        SelectObject(cdc, hOldObject);
        DeleteDC(cdc);
        EndPaint(hwnd, &ps);
      }
      break;
    case WM_WINDOWPOSCHANGING:
      {
        LPWINDOWPOS wp = (LPWINDOWPOS) lParam;
        wp->flags |= SWP_NOACTIVATE;
        wp->hwndInsertAfter = hWndParent;
        break;
      }
    case WM_DESTROY:
      SetWindowLong(hWndParent, GWL_WNDPROC, (long)oldProc);
    default:
      return DefWindowProc(hwnd, message, wParam, lParam);
  }
  return 0;
}