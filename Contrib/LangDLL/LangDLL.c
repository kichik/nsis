#include <windows.h>
#include "resource.h"

typedef struct _stack_t {
  struct _stack_t *next;
  char text[1]; // this should be the length of string_size
} stack_t;

int popstring(char *str); // 0 on success, 1 on empty stack
void pushstring(char *str);

enum
{
  INST_0,         // $0
  INST_1,         // $1
  INST_2,         // $2
  INST_3,         // $3
  INST_4,         // $4
  INST_5,         // $5
  INST_6,         // $6
  INST_7,         // $7
  INST_8,         // $8
  INST_9,         // $9
  INST_R0,        // $R0
  INST_R1,        // $R1
  INST_R2,        // $R2
  INST_R3,        // $R3
  INST_R4,        // $R4
  INST_R5,        // $R5
  INST_R6,        // $R6
  INST_R7,        // $R7
  INST_R8,        // $R8
  INST_R9,        // $R9
  INST_CMDLINE,   // $CMDLINE
  INST_INSTDIR,   // $INSTDIR
  INST_OUTDIR,    // $OUTDIR
  INST_EXEDIR,    // $EXEDIR
  INST_LANG,      // $LANGUAGE
  __INST_LAST
};

char *getuservariable(int varnum);
int myatoi(char *s);

HINSTANCE g_hInstance;
HWND g_hwndParent;
int g_stringsize;
stack_t **g_stacktop;
char *g_variables;

char temp[1024];

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
      if (!popstring(temp))
        SetDlgItemText(hwndDlg, IDC_TEXT, temp);
      if (!popstring(temp))
        SetWindowText(hwndDlg, temp);
      SendDlgItemMessage(hwndDlg, IDC_APPICON, STM_SETICON, (LPARAM)LoadIcon(GetModuleHandle(0),MAKEINTRESOURCE(103)), 0);
      for (i = 0; i < langs_num; i++) {
        if (!lstrcmp(langs[i].id, getuservariable(INST_LANG))) {
          SendDlgItemMessage(hwndDlg, IDC_LANGUAGE, CB_SETCURSEL, langs_num-i-1, 0);
          break;
        }
      }
      if (!popstring(temp))
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
  g_stringsize=string_size;
  g_stacktop=stacktop;
  g_variables=variables;

  {
    int i;
    popstring(temp);
    langs_num = myatoi(temp);

    langs = (struct lang *)GlobalAlloc(GPTR, langs_num*sizeof(struct lang));

    for (i = 0; i < langs_num; i++) {
      if (popstring(temp)) return;
      langs[i].id = GlobalAlloc(GPTR, lstrlen(temp)+1);
      lstrcpy(langs[i].id, temp);
      if (popstring(temp)) return;
      langs[i].name = GlobalAlloc(GPTR, lstrlen(temp)+1);
      lstrcpy(langs[i].name, temp);
    }

    DialogBox(g_hInstance, MAKEINTRESOURCE(IDD_DIALOG), 0, DialogProc);
  }
}

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  g_hInstance=hInst;
	return TRUE;
}

// utility functions (not required but often useful)
int popstring(char *str)
{
  stack_t *th;
  if (!g_stacktop || !*g_stacktop) return 1;
  th=(*g_stacktop);
  lstrcpy(str,th->text);
  *g_stacktop = th->next;
  GlobalFree((HGLOBAL)th);
  return 0;
}

void pushstring(char *str)
{
  stack_t *th;
  if (!g_stacktop) return;
  th=(stack_t*)GlobalAlloc(GPTR,sizeof(stack_t)+g_stringsize);
  lstrcpyn(th->text,str,g_stringsize);
  th->next=*g_stacktop;
  *g_stacktop=th;
}

char *getuservariable(int varnum)
{
  if (varnum < 0 || varnum >= __INST_LAST) return NULL;
  return g_variables+varnum*g_stringsize;
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