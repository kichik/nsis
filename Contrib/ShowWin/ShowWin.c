#include <windows.h>

typedef struct _stack_t {
  struct _stack_t *next;
  char text[1]; // this should be the length of string_size
} stack_t;

int popstring(char *str); // 0 on success, 1 on empty stack
void pushstring(char *str);
int my_atoi(char *s);

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


HINSTANCE g_hInstance;
HWND g_hwndParent;
int g_stringsize;
stack_t **g_stacktop;
char *g_variables;

void __declspec(dllexport) Show(HWND hwndParent, int string_size, 
                                      char *variables, stack_t **stacktop)
{
  g_hwndParent=hwndParent;
  g_stringsize=string_size;
  g_stacktop=stacktop;
  g_variables=variables;

  // do your stuff here
  {
    HWND hwnd_ctrl;
    char* str_var;
    str_var = (char *)GlobalAlloc(GPTR, sizeof(char)*string_size+1);
    if (!popstring(str_var)) {
      hwnd_ctrl = (HWND)my_atoi(str_var);
      if (IsWindow(hwnd_ctrl)) {
        ShowWindow(hwnd_ctrl,1);
      } else {
        pushstring("error");
      }
    } else {
      pushstring("error");
    }
  }
}

void __declspec(dllexport) Hide(HWND hwndParent, int string_size, char *variables, stack_t **stacktop){
  g_hwndParent=hwndParent;
  g_stringsize=string_size;
  g_stacktop=stacktop;
  g_variables=variables;
  {
    HWND hwnd_ctrl;
    char* str_var;
    str_var = (char *)GlobalAlloc(GPTR, sizeof(char)*string_size+1);
    if (!popstring(str_var)) {
      hwnd_ctrl = (HWND)my_atoi(str_var);
      if (IsWindow(hwnd_ctrl)) {
        ShowWindow(hwnd_ctrl,0);
      } else {
        pushstring("error");
      }
    } else {
      pushstring("error");
    }
  }
}

void __declspec(dllexport) Enable(HWND hwndParent, int string_size, char *variables, stack_t **stacktop){
  g_hwndParent=hwndParent;
  g_stringsize=string_size;
  g_stacktop=stacktop;
  g_variables=variables;
  {
    HWND hwnd_ctrl;
    char* str_var;
    str_var = (char *)GlobalAlloc(GPTR, sizeof(char)*string_size+1);
    if (!popstring(str_var)) {
      hwnd_ctrl = (HWND)my_atoi(str_var);
      if (IsWindow(hwnd_ctrl)) {
        EnableWindow(hwnd_ctrl,TRUE);
      } else {
        pushstring("error");
      }
    } else {
      pushstring("error");
    }
  }
}

void __declspec(dllexport) Disable(HWND hwndParent, int string_size, char *variables, stack_t **stacktop){
  g_hwndParent=hwndParent;
  g_stringsize=string_size;
  g_stacktop=stacktop;
  g_variables=variables;
  {
    HWND hwnd_ctrl;
    char* str_var;
    str_var = (char *)GlobalAlloc(GPTR, sizeof(char)*string_size+1);
    if (!popstring(str_var)) {
      hwnd_ctrl = (HWND)my_atoi(str_var);
      if (IsWindow(hwnd_ctrl)) {
        EnableWindow(hwnd_ctrl,FALSE);
      } else {
        pushstring("error");
      }
    } else {
      pushstring("error");
    }
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


