#include <windows.h>
#include "resource.h"

// JF> updated usage
// call like this:
// LangDLL:LangDialog "Window Title" "Window subtext" <number of languages>[F] language_text language_id ... [font_size font_face]
// ex:
//  LangDLL:LangDialog "Language Selection" "Choose a language" 2 French 1036 English 1033
// or (the F after the 2 means we're supplying font information)
//  LangDLL:LangDialog "Language Selection" "Choose a language" 2F French 1036 English 1033 12 Garamond


#include "../ExDLL/exdll.h"

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
      // add languages
      for (i = langs_num - 1; i >= 0; i--) {
        SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_ADDSTRING, 0, (LPARAM)langs[i].name);
      }
      // set texts
      SetDlgItemText(hwndDlg, IDC_TEXT, g_wndtext);
      SetWindowText(hwndDlg, g_wndtitle);
      SendDlgItemMessage(hwndDlg, IDC_APPICON, STM_SETICON, (LPARAM)LoadIcon(GetModuleHandle(0),MAKEINTRESOURCE(103)), 0);
      // select the current language
      for (i = 0; i < langs_num; i++) {
        if (!lstrcmp(langs[i].id, getuservariable(INST_LANG))) {
          SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_SETCURSEL, langs_num-i-1, 0);
          break;
        }
      }
      // set font
      if (dofont && !popstring(temp)) {
        size = myatoi(temp);
        if (!popstring(temp)) {
          LOGFONT f = {0,};
          if (lstrcmp(temp, "MS Shell Dlg")) {
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
      }
      // show window
      ShowWindow(hwndDlg, SW_SHOW);
      break;
    case WM_COMMAND:
      switch (LOWORD(wParam)) {
      	case IDOK:
          // push result on the stack
          pushstring(langs[langs_num-SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_GETCURSEL, 0, 0)-1].id);
          // end dialog
          EndDialog(hwndDlg, 0);
          break;
        case IDCANCEL:
          // push "cancel" on the stack
          pushstring("cancel");
          // end dialog
          EndDialog(hwndDlg, 0);
          break;
      }
      break;
    case WM_DESTROY:
      // clean up
      if (font) DeleteObject(font);
      break;
    default:
      return FALSE; // message not processed
  }
  return TRUE; // message processed
}

void __declspec(dllexport) LangDialog(HWND hwndParent, int string_size, 
                                      char *variables, stack_t **stacktop)
{
  g_hwndParent=hwndParent;
  EXDLL_INIT();

  {
    int i;
    BOOL pop_empty_string = FALSE;

    // get texts
    if (popstring(g_wndtitle)) return;
    if (popstring(g_wndtext)) return;

    // get flags
    if (popstring(temp)) return;
    if (*temp == 'A') {
      // automatic language count
      stack_t *th;
      langs_num=0;
      th=(*g_stacktop);
      while (th && th->text[0]) {
        langs_num++;
        th = th->next;
      }
      if (!th) return;
      langs_num /= 2;
      pop_empty_string = TRUE;
    } else {
      // use counts languages
      langs_num = myatoi(temp);
    }

    {
      // parse font flag
      char *p=temp;
      while (*p) if (*p++ == 'F') dofont=1;
    }

    // zero languages?
    if (!langs_num) return;

    // allocate language struct
    langs = (struct lang *)GlobalAlloc(GPTR, langs_num*sizeof(struct lang));
    if (!langs) return;

    // fill language struct
    for (i = 0; i < langs_num; i++) {
      if (popstring(temp)) return;
      langs[i].name = GlobalAlloc(GPTR, lstrlen(temp)+1);
      if (!langs[i].name) return;
      lstrcpy(langs[i].name, temp);

      if (popstring(temp)) return;
      langs[i].id = GlobalAlloc(GPTR, lstrlen(temp)+1);
      if (!langs[i].id) return;
      lstrcpy(langs[i].id, temp);
    }

    // pop the empty string to keep the stack clean
    if (pop_empty_string) {
      if (popstring(temp)) return;
    }

    // start dialog
    DialogBox(g_hInstance, MAKEINTRESOURCE(IDD_DIALOG), 0, DialogProc);

    // free structs
    for (i = 0; i < langs_num; i++) {
      GlobalFree(langs[i].name);
      GlobalFree(langs[i].id);
    }
    GlobalFree(langs);
  }
}

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  g_hInstance=hInst;
	return TRUE;
}

int myatoi(char *s)
{
  unsigned int v=0;
  for (;;) {
    int c=*s++ - '0';
    if (c < 0 || c > 9) break;
    v*=10;
    v+=c;
  }
  return (int)v;
}
