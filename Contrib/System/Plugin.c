#include "stdafx.h"
#include "Plugin.h"
#include "System.h"

HWND g_hwndParent;
int g_stringsize;
stack_t **g_stacktop;
char *g_variables;

char *AllocString()
{
    return (char*) GlobalAlloc(GPTR,g_stringsize);
}

char *AllocStr(char *str)
{
    return lstrcpy(AllocString(), str);
}

char* popstring()
{
        char *str;
        stack_t *th;

        if (!g_stacktop || !*g_stacktop) return NULL;
        th=(*g_stacktop);

        str = AllocString();
        lstrcpy(str,th->text);

        *g_stacktop = th->next;
        GlobalFree((HGLOBAL)th);
        return str;
}

char *pushstring(char *str)
{
        stack_t *th;
        if (!g_stacktop) return str;
        th=(stack_t*)GlobalAlloc(GPTR,sizeof(stack_t)+g_stringsize);
        lstrcpyn(th->text,str,g_stringsize);
        th->next=*g_stacktop;
        *g_stacktop=th;
        return str;
}

char *getuservariable(int varnum)
{
        if (varnum < 0 || varnum >= __INST_LAST) return AllocString();
        return AllocStr(g_variables+varnum*g_stringsize);
}

void setuservariable(int varnum, char *var)
{
        if (var != NULL && varnum >= 0 && varnum < __INST_LAST) {
                lstrcpy (g_variables + varnum*g_stringsize, var);

        }
}

// Updated for int64 and simple bitwise operations
__int64 myatoi(char *s)
{
  __int64 v=0;
  // Check for right input
  if (!s) return 0;
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

void myitoa64(__int64 i, char *buffer)
{
    char buf[128], *b = buf;

    if (i < 0)
    {
        *(buffer++) = '-';
        i = -i;
    }
    while (i > 0) 
    {
        *(b++) = '0' + ((char) (i%10));
        i /= 10;
    }
    while (b > buf) *(buffer++) = *(--b);
    *buffer = 0;
}

int popint()
{
    int value;
	char *str;
	if ((str = popstring()) == NULL) return -1;
	value = (int) myatoi(str);
    GlobalFree(str);
	return value;
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