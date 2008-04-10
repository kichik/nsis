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
int docp;

int langs_num;
int visible_langs_num;

struct lang {
  char *name;
  char *id;
  UINT cp;
} *langs;

BOOL CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  int i, size;
  char *selected_language = NULL;
  static HFONT font;
  switch (uMsg) {
  	case WM_INITDIALOG:
      // add languages
      for (i = visible_langs_num - 1; i >= 0; i--) {
        int cbi;

        cbi = SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_ADDSTRING, 0, (LPARAM) langs[i].name);
        SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_SETITEMDATA, cbi, (LPARAM) langs[i].id);

        // remember selected language
        if (!lstrcmp(langs[i].id, getuservariable(INST_LANG))) {
          selected_language = langs[i].name;
        }
      }
      // select the current language
      if (selected_language)
        SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_SELECTSTRING, (WPARAM) -1, (LPARAM) selected_language);
      else
        SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_SETCURSEL, 0, 0);
      // set texts
      SetDlgItemText(hwndDlg, IDC_TEXT, g_wndtext);
      SetWindowText(hwndDlg, g_wndtitle);
      SendDlgItemMessage(hwndDlg, IDC_APPICON, STM_SETICON, (LPARAM)LoadIcon(GetModuleHandle(0),MAKEINTRESOURCE(103)), 0);
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
          i = SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_GETCURSEL, 0, 0);
          i = SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_GETITEMDATA, i, 0);
          if (i != CB_ERR && i) {
            pushstring((char *) i);
          } else {
            // ?!
            pushstring("cancel");
          }
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

    // parse flags
    {
      char *p=temp;
      while (*p)
      {
        if (*p == 'F') dofont=1; // parse font flag
        if (*p == 'C') docp=1;   // parse codepage flag
        p++;
      }
    }
 
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
      if (docp)
        langs_num /= 3;
      else
        langs_num /= 2;
      pop_empty_string = TRUE;
    } else {
      // use counts languages
      langs_num = myatoi(temp);
    }

    // zero languages?
    if (!langs_num) return;

    // initialize visible languages count
    visible_langs_num = 0;

    // allocate language struct
    langs = (struct lang *)GlobalAlloc(GPTR, langs_num*sizeof(struct lang));
    if (!langs) return;

    // fill language struct
    for (i = 0; i < langs_num; i++) {
      if (popstring(temp)) return;
      langs[visible_langs_num].name = GlobalAlloc(GPTR, lstrlen(temp)+1);
      if (!langs[visible_langs_num].name) return;
      lstrcpy(langs[visible_langs_num].name, temp);

      if (popstring(temp)) return;
      langs[visible_langs_num].id = GlobalAlloc(GPTR, lstrlen(temp)+1);
      if (!langs[visible_langs_num].id) return;
      lstrcpy(langs[visible_langs_num].id, temp);

      if (docp)
      {
        if (popstring(temp)) return;
        langs[visible_langs_num].cp = myatoi(temp);
      }

      if (!docp || langs[visible_langs_num].cp == GetACP() || langs[visible_langs_num].cp == 0)
      {
        visible_langs_num++;
      }
      else
      {
        GlobalFree(langs[visible_langs_num].name);
        GlobalFree(langs[visible_langs_num].id);
      }
    }

    // pop the empty string to keep the stack clean
    if (pop_empty_string) {
      if (popstring(temp)) return;
    }

    // start dialog
    if (visible_langs_num > 1)
    {
      DialogBox(g_hInstance, MAKEINTRESOURCE(IDD_DIALOG), 0, DialogProc);
    }
    else if (visible_langs_num == 0)
    {
      pushstring("");
    }
    else
    {
      pushstring(langs[0].id);
    }

    // free structs
    for (i = 0; i < visible_langs_num; i++) {
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
