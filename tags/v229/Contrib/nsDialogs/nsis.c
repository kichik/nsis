#include <windows.h>

#include "nsis.h"

int g_stringsize;
stack_t **g_stacktop;
char *g_variables;

int NSDFUNC myatoi(const char *s)
{
  int v=0;
  if (*s == '0' && (s[1] == 'x' || s[1] == 'X'))
  {
    s++;
    for (;;)
    {
      int c=*(++s);
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
    for (;;)
    {
      int c=*(++s);
      if (c >= '0' && c <= '7') c-='0';
      else break;
      v<<=3;
      v+=c;
    }
  }
  else
  {
    int sign=0;
    if (*s == '-') sign++; else s--;
    for (;;)
    {
      int c=*(++s) - '0';
      if (c < 0 || c > 9) break;
      v*=10;
      v+=c;
    }
    if (sign) v = -v;
  }

  // Support for simple ORed expressions
  if (*s == '|') 
  {
      v |= myatoi(s+1);
  }

  return v;
}

int NSDFUNC popstring(char *str, int size)
{
  stack_t *th;
  if (!g_stacktop || !*g_stacktop) return 1;
  th=(*g_stacktop);
  lstrcpyn(str,th->text,size?size:g_stringsize);
  *g_stacktop = th->next;
  GlobalFree((HGLOBAL)th);
  return 0;
}

void NSDFUNC pushstring(const char *str)
{
  stack_t *th;
  if (!g_stacktop) return;
  th=(stack_t*)GlobalAlloc(GPTR,sizeof(stack_t)+g_stringsize);
  lstrcpyn(th->text,str,g_stringsize);
  th->next=*g_stacktop;
  *g_stacktop=th;
}

int NSDFUNC popint()
{
  char buf[1024];
  if (popstring(buf,sizeof(buf)))
    return 0;

  return myatoi(buf);
}

void NSDFUNC pushint(int value)
{
	char buffer[1024];
	wsprintf(buffer, "%d", value);
	pushstring(buffer);
}
