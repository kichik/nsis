#include <windows.h>
#include "../exdll/exdll.h"
#include "../../Source/exehead/resource.h"

HINSTANCE hInstance;
HWND hwBanner;

char buf[1024];

BOOL CALLBACK bannerProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG)
  {
    popstring(buf);
    SetWindowText(hwndDlg,buf);
    SetDlgItemText(hwndDlg,IDC_STR,buf);
  }
  return 0;
}

void __declspec(dllexport) show(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  EXDLL_INIT();

  hwBanner = CreateDialog(
    GetModuleHandle(0),
    MAKEINTRESOURCE(IDD_VERIFY),
    hwndParent,
    bannerProc
  );
}

void __declspec(dllexport) destroy(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  DestroyWindow(hwBanner);
}

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
	return TRUE;
}