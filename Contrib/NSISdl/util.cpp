/*
** JNetLib
** Copyright (C) 2000-2001 Nullsoft, Inc.
** Author: Justin Frankel
** File: util.cpp - JNL implementation of basic network utilities
** License: see jnetlib.h
*/

#include "netinc.h"

#include "util.h"

int my_atoi(char *s)
{
  int sign=0;
  int v=0;
  if (*s == '-') { s++; sign++; }
  for (;;)
  {
    int c=*s++ - '0';
    if (c < 0 || c > 9) break;
    v*=10;
    v+=c;
  }
  if (sign) return -(int) v;
  return (int)v;
}

// Updated for int64 and simple bitwise operations
__int64 myatoi64(char *s)
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
      v |= myatoi64(s+1);
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
    if (i == 0) *(buffer++) = '0';
    else 
    {
        while (i > 0) 
        {
            *(b++) = '0' + ((char) (i%10));
            i /= 10;
        }
        while (b > buf) *(buffer++) = *(--b);
    }
    *buffer = 0;
}

void mini_memset(void *o,char i,int l)
{
  char *oo=(char*)o;
  while (l-- > 0) *oo++=i;
}
void mini_memcpy(void *o,void*i,int l)
{
  char *oo=(char*)o;
  char *ii=(char*)i;
  while (l-- > 0) *oo++=*ii++;
}
