#include <windows.h>
#include "../exdll/exdll.h"

//cleaned up by JF 9/21/02

int my_atoi(char *s);


HINSTANCE g_hInstance;

HWND mygetwnd()
{
  if (g_stacktop && *g_stacktop)
  {
    HWND h=(HWND)my_atoi((*g_stacktop)->text);
    popstring(NULL);
    return h;
  }
  return 0;
}

void __declspec(dllexport) Show(HWND hwndParent, int string_size, 
                                      char *variables, stack_t **stacktop)
{
  EXDLL_INIT()
  {
    HWND hwnd_ctrl=mygetwnd();
    if (hwnd_ctrl && IsWindow(hwnd_ctrl)) ShowWindow(hwnd_ctrl,1);
  }
}

void __declspec(dllexport) Hide(HWND hwndParent, int string_size, char *variables, stack_t **stacktop){
  EXDLL_INIT()
  {
    HWND hwnd_ctrl=mygetwnd();
    if (hwnd_ctrl && IsWindow(hwnd_ctrl)) ShowWindow(hwnd_ctrl,0);
  }
}

void __declspec(dllexport) Enable(HWND hwndParent, int string_size, char *variables, stack_t **stacktop){
  EXDLL_INIT()
  {
    HWND hwnd_ctrl=mygetwnd();
    if (hwnd_ctrl && IsWindow(hwnd_ctrl)) EnableWindow(hwnd_ctrl,TRUE);
  }
}

void __declspec(dllexport) Disable(HWND hwndParent, int string_size, char *variables, stack_t **stacktop){
  EXDLL_INIT()
  {
    HWND hwnd_ctrl=mygetwnd();
    if (hwnd_ctrl && IsWindow(hwnd_ctrl)) EnableWindow(hwnd_ctrl,0);
  }
}

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  g_hInstance=hInst;
	return TRUE;
}

int my_atoi(char *s)
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


