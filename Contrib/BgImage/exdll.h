#ifndef _EXDLL_H_
#define _EXDLL_H_

// only include this file from one place in your DLL.
// (it is all static, if you use it in two places it will fail)

#define EXDLL_INIT()           {  \
        g_hwndParent=hwndParent;  \
        g_stringsize=string_size; \
        g_stacktop=stacktop;      \
        /*g_variables=variables;*/ }


typedef struct _stack_t {
  struct _stack_t *next;
  char text[1]; // this should be the length of string_size
} stack_t;


static int g_stringsize;
static stack_t **g_stacktop;
static HWND g_hwndParent;

static int popstring(char *str); // 0 on success, 1 on empty stack
static void pushstring(char *str);

// utility functions (not required but often useful)
static int popstring(char *str)
{
  stack_t *th;
  if (!g_stacktop || !*g_stacktop) return 1;
  th=(*g_stacktop);
  lstrcpy(str,th->text);
  *g_stacktop = th->next;
  GlobalFree((HGLOBAL)th);
  return 0;
}

static void pushstring(char *str)
{
  stack_t *th;
  if (!g_stacktop) return;
  th=(stack_t*)GlobalAlloc(GPTR,sizeof(stack_t)+g_stringsize);
  lstrcpyn(th->text,str,g_stringsize);
  th->next=*g_stacktop;
  *g_stacktop=th;
}

#endif//_EXDLL_H_