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