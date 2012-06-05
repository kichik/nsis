// Unicode support by Jim Park -- 08/23/2007

#include "stdafx.h"
#include "Plugin.h"
#include "Buffers.h"
#include "System.h"

HWND g_hwndParent;

TCHAR *AllocString()
{
    return (TCHAR*) GlobalAlloc(GPTR,g_stringsize*sizeof(TCHAR));
}

TCHAR *AllocStr(TCHAR *str)
{
    return lstrcpyn(AllocString(), str, g_stringsize);
}

TCHAR* system_popstring()
{
        TCHAR *str;
        stack_t *th;

        if (!g_stacktop || !*g_stacktop) return NULL;
        th=(*g_stacktop);

        str = AllocString();
        lstrcpy(str,th->text);

        *g_stacktop = th->next;
        GlobalFree((HGLOBAL)th);
        return str;
}

TCHAR *system_pushstring(TCHAR *str)
{
        stack_t *th;
        if (!g_stacktop) return str;
        th=(stack_t*)GlobalAlloc(GPTR,sizeof(stack_t)+(g_stringsize*sizeof(TCHAR)));
        lstrcpyn(th->text,str,g_stringsize);
        th->next=*g_stacktop;
        *g_stacktop=th;
        return str;
}

TCHAR *system_getuservariable(int varnum)
{
        if (varnum < 0 || varnum >= __INST_LAST) return AllocString();
        return AllocStr(g_variables+varnum*g_stringsize);
}

TCHAR *system_setuservariable(int varnum, TCHAR *var)
{
        if (var != NULL && varnum >= 0 && varnum < __INST_LAST) {
                lstrcpy (g_variables + varnum*g_stringsize, var);
        }
        return var;
}

// Updated for int64 and simple bitwise operations
__int64 myatoi64(TCHAR *s)
{
  __int64 v=0;
  // Check for right input
  if (!s) return 0;
  if (*s == _T('0') && (s[1] == _T('x') || s[1] == _T('X')))
  {
    s++;
    for (;;)
    {
      int c=*(++s);
      if (c >= _T('0') && c <= _T('9')) c-=_T('0');
      else if (c >= _T('a') && c <= _T('f')) c-=_T('a')-10;
      else if (c >= _T('A') && c <= _T('F')) c-=_T('A')-10;
      else break;
      v<<=4;
      v+=c;
    }
  }
  else if (*s == _T('0') && s[1] <= _T('7') && s[1] >= _T('0'))
  {
    for (;;)
    {
      int c=*(++s);
      if (c >= _T('0') && c <= _T('7')) c-=_T('0');
      else break;
      v<<=3;
      v+=c;
    }
  }
  else
  {
    int sign=0;
    if (*s == _T('-')) sign++; else s--;
    for (;;)
    {
      int c=*(++s) - _T('0');
      if (c < 0 || c > 9) break;
      v*=10;
      v+=c;
    }
    if (sign) v = -v;
  }

  // Support for simple ORed expressions
  if (*s == _T('|')) 
  {
      v |= myatoi64(s+1);
  }

  return v;
}

void myitoa64(__int64 i, TCHAR *buffer)
{
    TCHAR buf[128], *b = buf;

    if (i < 0)
    {
        *(buffer++) = _T('-');
        i = -i;
    }
    if (i == 0) *(buffer++) = _T('0');
    else 
    {
        while (i > 0) 
        {
            *(b++) = _T('0') + ((TCHAR) (i%10));
            i /= 10;
        }
        while (b > buf) *(buffer++) = *(--b);
    }
    *buffer = 0;
}

int system_popint()
{
    int value;
    TCHAR *str;
    if ((str = system_popstring()) == NULL) return -1;
    value = myatoi(str);
    GlobalFree(str);
    return value;
}

void system_pushint(int value)
{
    TCHAR buffer[80];
    wsprintf(buffer, _T("%d"), value);
    system_pushstring(buffer);
}

void *copymem(void *output, void *input, size_t cbSize)
{
  BYTE *out = (BYTE*) output;
  BYTE *in = (BYTE*) input;
  if ((input != NULL) && (output != NULL))
  {
    while (cbSize-- > 0) *(out++) = *(in++);
  }
  return output;
}

HANDLE GlobalCopy(HANDLE Old)
{
    size_t size = GlobalSize(Old);
    return copymem(GlobalAlloc(GPTR, size), Old, size);
}

UINT_PTR NSISCallback(enum NSPIM msg)
{
  return 0;
}

#ifdef _DEBUG
void main()
{
}
#endif
