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

char buf[1024];

unsigned int myatoi(char *s);

BOOL CALLBACK bannerProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG)
  {
    popstring(buf);
    while (*(int*)buf == CHAR4_TO_DWORD('/','s','e','t')) {
      unsigned int id;
      popstring(buf);
      id = myatoi(buf);
      popstring(buf);
      SetDlgItemText(hwndDlg,id,buf);
      popstring(buf);
    }
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