#include <windows.h>
#include "resource.h"

// JF> updated usage
// call like this:
// LangDLL:LangDialog "Window Title" "Window subtext" <number of languages>[F] language_text language_id ... [font_size font_face]
// ex:
//  LangDLL:LangDialog "Language Selection" "Choose a language" 2 French 1036 English 1033
// or (the F after the 2 means we're supplying font information)
//  LangDLL:LangDialog "Language Selection" "Choose a language" 2F French 1036 English 1033 12 Garamond


#include "../exdll/exdll.h"

int myatoi(char *s);

HINSTANCE g_hInstance;
HWND g_hwndParent;

char temp[1024];
char g_wndtitle[1024], g_wndtext[1024];
int dofont;

int langs_num;

struct lang {
  char *name;
  char *id;
} *langs;

BOOL CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  int i, size;
  static HFONT font;
  switch (uMsg) {
  	case WM_INITDIALOG:
      for (i = langs_num - 1; i >= 0; i--) {
        SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_ADDSTRING, 0, (LPARAM)langs[i].name);
      }
      SetDlgItemText(hwndDlg, IDC_TEXT, g_wndtext);
      SetWindowText(hwndDlg, g_wndtitle);
      SendDlgItemMessage(hwndDlg, IDC_APPICON, STM_SETICON, (LPARAM)LoadIcon(GetModuleHandle(0),MAKEINTRESOURCE(103)), 0);
      for (i = 0; i < langs_num; i++) {
        if (!lstrcmp(langs[i].id, getuservariable(INST_LANG))) {
          SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_SETCURSEL, langs_num-i-1, 0);
          break;
        }
      }
      if (dofont && !popstring(temp))
      {
        size = myatoi(temp);
        if (!popstring(temp)) {
          LOGFONT f = {0,};
          f.lfHeight = -MulDiv(size, GetDeviceCaps(GetDC(hwndDlg), LOGPIXELSY), 72);
          lstrcpy(f.lfFaceName, temp);
          font = CreateFontIndirect(&f);
          SendMessage(hwndDlg, WM_SETFONT, (WPARAM)font, 1);
          SendDlgItemMessage(hwndDlg, IDOK, WM_SETFONT, (WPARAM)font, 1);
          SendDlgItemMessage(hwndDlg, IDCANCEL, WM_SETFONT, (WPARAM)font, 1);
          SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, WM_SETFONT, (WPARAM)font, 1);
          SendDlgItemMessage(hwndDlg, IDC_TEXT, WM_SETFONT, (WPARAM)font, 1);
        }
      }
      ShowWindow(hwndDlg, SW_SHOW);
      break;
    case WM_COMMAND:
      switch (LOWORD(wParam)) {
      	case IDOK:
          pushstring(langs[langs_num-SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_GETCURSEL, 0, 0)-1].id);
          EndDialog(hwndDlg, 0);
          break;
        case IDCANCEL:
          pushstring("cancel");
          EndDialog(hwndDlg, 1);
          break;
      }
      break;
    case WM_CLOSE:
      if (font) DeleteObject(font);
      pushstring("cancel");
      EndDialog(hwndDlg, 1);
      break;
    default:
      return 0;
  }
  return 1;
}

void __declspec(dllexport) LangDialog(HWND hwndParent, int string_size, 
                                      char *variables, stack_t **stacktop)
{
  g_hwndParent=hwndParent;
  EXDLL_INIT();

  {
    int i;

    if (popstring(g_wndtitle)) return;
    if (popstring(g_wndtext)) return;

    if (popstring(temp)) return;
    langs_num = myatoi(temp);
    {
      char *p=temp;
      while (*p) if (*p++ == 'F') dofont=1;
    }

    if (!langs_num) return;

    langs = (struct lang *)GlobalAlloc(GPTR, langs_num*sizeof(struct lang));

    for (i = 0; i < langs_num; i++) {
      if (popstring(temp)) return;
      langs[i].name = GlobalAlloc(GPTR, lstrlen(temp)+1);
      lstrcpy(langs[i].name, temp);

      if (popstring(temp)) return;
      langs[i].id = GlobalAlloc(GPTR, lstrlen(temp)+1);
      lstrcpy(langs[i].id, temp);
    }

    DialogBox(g_hInstance, MAKEINTRESOURCE(IDD_DIALOG), 0, DialogProc);
  }
}

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
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