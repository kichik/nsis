#include <windows.h>
#include <nsis/pluginapi.h> // nsis plugin
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
BOOL bFailed;

char buf[1024];

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
      id = myatou(buf);
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

BOOL ProcessMessages()
{
  BOOL processed = FALSE;
  MSG msg;

  while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
  {
    DispatchMessage(&msg);
    processed = TRUE;
  }

  return processed;
}

DWORD WINAPI BannerThread(LPVOID lpParameter)
{
  HWND hwndParent = (HWND) lpParameter;
  HWND lhwBanner;

  // This right here is the mother of all evils when it comes to
  // foreground windows. The dialog is created in another thread
  // and there can only be one thread holding the right to set the
  // foreground window. So long as this thread exists and has an
  // active window, another thread from the same process can steal
  // its thunder. But if the window and the thread are destroyed,
  // the foreground rights pass on to another process. To avoid
  // this situation that could cause the installer to show up on
  // the background if Banner is used in .onInit, we don't let
  // CreateDialog show the window and instead do this in the
  // original thread. This is done by not specifying WS_VISIBLE
  // for IDD_VERIFY.

  lhwBanner = CreateDialog(
    GetModuleHandle(0),
    MAKEINTRESOURCE(IDD_VERIFY),
    hwndParent,
    BannerProc
  );
  if (!lhwBanner)
  {
    bFailed = TRUE;
    return 0;
  }

  while (IsWindow(lhwBanner))
  {
    if (!ProcessMessages())
    {
      hwBanner = lhwBanner;
      WaitMessage();
    }
  }

  hwBanner = NULL;

  return 0;
}

static UINT_PTR PluginCallback(enum NSPIM msg)
{
  return 0;
}

void __declspec(dllexport) show(HWND hwndParent, int string_size, char *variables, stack_t **stacktop, extra_parameters *extra)
{
  EXDLL_INIT();

  extra->RegisterPluginCallback(hInstance, PluginCallback);

  {
    DWORD dwThreadId;
    DWORD dwMainThreadId = GetCurrentThreadId();

    hwBanner = NULL;

    if (!IsWindowVisible(hwndParent))
      hwndParent = 0;

    bFailed = FALSE;

    hThread = CreateThread(0, 0, BannerThread, (LPVOID) hwndParent, 0, &dwThreadId);

    // wait for the window to initalize and for the stack operations to finish
    while (hThread && !hwBanner && !bFailed)
    {
      ProcessMessages();
      Sleep(10);
    }

    CloseHandle(hThread);

    if (AttachThreadInput(dwMainThreadId, dwThreadId, TRUE))
    {
      // Activates and displays a window
      ShowWindow(hwBanner, SW_SHOW);
      AttachThreadInput(dwMainThreadId, dwThreadId, FALSE);
    }
    else
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

  // Wait for the thread to finish
  while (hwBanner)
  {
    ProcessMessages();
    Sleep(25);
  }
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
