#include <windows.h>
#include "../exdll/exdll.h"
#include "../../Source/exehead/resource.h"

// Turn a pair of chars into a word
// Turn four chars into a dword
#ifdef __BIG_ENDIAN__ // Not very likely, but, still...
#define CHAR2_TO_WORD(a,b) (((WORD)(b))|((a)<<8))
#define CHAR4_TO_DWORD(a,b,c,d)	(((DWORD)CHAR2_TO_WORD(c,d))|(CHAR2_TO_WORD(a,b)<<16))
#else
#define CHAR2_TO_WORD(a,b) (((WORD)(a))|((b)<<8))
#define CHAR4_TO_DWORD(a,b,c,d)	(((DWORD)CHAR2_TO_WORD(a,b))|(CHAR2_TO_WORD(c,d)<<16))
#endif

HINSTANCE hInstance;
HWND hwBanner;
HWND hwParent;

char buf[1024];

unsigned int myatoi(char *s);
long oldProc;

BOOL CALLBACK bannerProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (hwndDlg == hwParent)
  {
    if (uMsg == WM_SIZE)
    {
      ShowWindow(hwBanner, wParam == SIZE_MINIMIZED ? SW_HIDE : SW_SHOW);
    }
    return CallWindowProc(
      (WNDPROC) oldProc,
      hwndDlg,
      uMsg,
      wParam,
      lParam
    );
  }
  if (uMsg == WM_INITDIALOG)
  {
    popstring(buf);
    while (*(int*)buf == CHAR4_TO_DWORD('/','s','e','t'))
    {
      unsigned int id;

      popstring(buf);
      id = myatoi(buf);
      
      popstring(buf);
      SetDlgItemText(hwndDlg, id, buf);
      
      popstring(buf);
    }
    SetWindowText(hwndDlg, buf);
    SetDlgItemText(hwndDlg, IDC_STR, buf);
  }
  if (uMsg == WM_CLOSE)
  {
    hwBanner = 0;
    DestroyWindow(hwndDlg);
  }
  return 0;
}

DWORD WINAPI BannerCreator(void *lpParameter)
{
  oldProc = SetWindowLong((HWND) lpParameter, GWL_WNDPROC, (long)bannerProc);

  hwParent = (HWND) lpParameter;

  hwBanner = CreateDialog(
    GetModuleHandle(0),
    MAKEINTRESOURCE(IDD_VERIFY),
    (HWND) lpParameter,
    bannerProc
  );

  {
    BOOL bRet;
    MSG msg;

    while (hwBanner && (bRet = GetMessage(&msg, NULL, 0, 0)))
    { 
      if (bRet != -1)
      { 
        TranslateMessage(&msg);
        DispatchMessage(&msg);
      }
    }
  }

  return 0;
}

void __declspec(dllexport) show(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  DWORD dwThreadId;

  g_stringsize = string_size;
  g_stacktop = stacktop;

  CreateThread(0, 0, BannerCreator, (void *) hwndParent, 0, &dwThreadId);
}

void __declspec(dllexport) destroy(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  if (oldProc)
    SetWindowLong(hwndParent, GWL_WNDPROC, oldProc);

  SendMessage(hwBanner, WM_CLOSE, 0, 0);
}

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
	return TRUE;
}

unsigned int myatoi(char *s)
{
  unsigned int v=0;

  for (;;)
  {
    unsigned int c=*s++;
    if (c >= '0' && c <= '9') c-='0';
    else break;
    v*=10;
    v+=c;
  }
  return v;
}