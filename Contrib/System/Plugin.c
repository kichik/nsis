#include "stdafx.h"
#include "Plugin.h"
#include "System.h"

HWND g_hwndParent;
int g_stringsize;
stack_t **g_stacktop;
char *g_variables;

int popstring(char *str)
{
        stack_t *th;
        if (!g_stacktop || !*g_stacktop) return 0;
        th=(*g_stacktop);
        lstrcpy(str,th->text);
        *g_stacktop = th->next;
        GlobalFree((HGLOBAL)th);
        return 1;
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

void setuservariable(int varnum, char *var)
{
        if (var != NULL && varnum >= 0 && varnum < __INST_LAST) {
                lstrcpy (g_variables + varnum*g_stringsize, var);

        }
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

int popint(int *value)
{
	char buffer[1024];
	if (popstring(buffer) == 0) return FALSE;
	*value = myatoi(buffer);
	return TRUE;
}

void pushint(int value)
{
	char buffer[1024];
	wsprintf(buffer, "%d", value);
	pushstring(buffer);
}

#ifdef _DEBUG
void main()
{
}
#endif