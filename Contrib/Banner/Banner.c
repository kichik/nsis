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
HANDLE hThread;

char buf[1024];

unsigned int myatoi(char *s);

BOOL CALLBACK BannerProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG)
  {
    int iMainStringSet = 0;

    popstring(buf);
    while (*(int*)buf == CHAR4_TO_DWORD('/','s','e','t') && !buf[4])
    {
      unsigned int id;
      popstring(buf);
      id = myatoi(buf);
      popstring(buf);
      SetDlgItemText(hwndDlg, id, buf);
      popstring(buf);

      if (id == IDC_STR)
        iMainStringSet++;
    }

    SetWindowText(hwndDlg, buf);
    if (!iMainStringSet)
      SetDlgItemText(hwndDlg, IDC_STR, buf);

    if (!*buf)
      SetWindowLong(hwndDlg, GWL_EXSTYLE, GetWindowLong(hwndDlg, GWL_EXSTYLE) | WS_EX_TOOLWINDOW);
  }
  if (uMsg == WM_CLOSE)
  {
    DestroyWindow(hwndDlg);
  }
  return 0;
}

DWORD WINAPI BannerThread(LPVOID lpParameter)
{
  HWND hwndParent = (HWND) lpParameter;
  HWND lhwBanner;
  MSG msg;
  //BOOL bRet;

  lhwBanner = CreateDialog(
    GetModuleHandle(0),
    MAKEINTRESOURCE(IDD_VERIFY),
    hwndParent,
    BannerProc
  );

  while (IsWindow(lhwBanner))
  {
    if (PeekMessage(&msg, lhwBanner, 0, 0, PM_REMOVE))
    {
      DispatchMessage(&msg);
    }
    else
    {
      hwBanner = lhwBanner;
      WaitMessage();
    }
  }

  hwBanner = NULL;

  return 0;
}

void __declspec(dllexport) show(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  EXDLL_INIT();

  {
    DWORD dwThreadId;

    hwBanner = NULL;

    hThread = CreateThread(0, 0, BannerThread, (LPVOID) hwndParent, 0, &dwThreadId);

    // wait for the window to initalize and for the stack operations to finish
    while (hThread && !hwBanner)
    {
      Sleep(10);
    }

    CloseHandle(hThread);

    ShowWindow(hwBanner, SW_SHOW);
  }
}

void __declspec(dllexport) getWindow(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  wsprintf(buf, "%u", hwBanner);
  pushstring(buf);
}

void __declspec(dllexport) destroy(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  if (!hwBanner)
    return;

  PostMessage(hwBanner, WM_CLOSE, 0, 0);

  if (!hwndParent)
  {
    // create a dummy window on the thread the NSIS window will be created on and set it
    // as the foreground window so this thread will return to be the foreground window
    HWND hwTemp;

    AttachThreadInput(GetWindowThreadProcessId(hwndParent, 0), GetCurrentThreadId(), TRUE);

    hwTemp = CreateWindowEx(
      WS_EX_TOOLWINDOW,
      "STATIC",
      "",
      WS_VISIBLE | WS_POPUP,
      -1,
      -1,
      1,
      1,
      0,
      0,
      hInstance,
      0
    );
    SetForegroundWindow(hwTemp);
    DestroyWindow(hwTemp);

    AttachThreadInput(GetWindowThreadProcessId(hwndParent, 0), GetCurrentThreadId(), FALSE);
  }

  // Wait for the thread to finish
  while (hwBanner)
    Sleep(25);
}

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  hInstance = hInst;
  if (hwBanner && ul_reason_for_call == DLL_PROCESS_DETACH)
  {
    destroy(0, 0, 0, 0);
  }
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