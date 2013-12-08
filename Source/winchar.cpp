/*
 * winchar.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2013 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "Platform.h"
#include "winchar.h"
#include "util.h"
#include "utf.h"

#include <stdexcept>
#include <cassert>

using std::runtime_error;

int WinWStrICmpASCII(const WINWCHAR *a, const char *b)
{
  int diff = 0;
  do
    diff = static_cast<int>(S7ChLwr(*a)) - static_cast<int>(S7ChLwr(*b++));
  while (*a++ && !diff);
  return diff;
}
int WinWStrNICmpASCII(const WINWCHAR *a, const char *b, size_t n)
{
  int diff = 0;
  for ( ; n--; ++a, ++b )
  {
    diff = static_cast<int>(S7ChLwr(*a)) - static_cast<int>(S7ChLwr(*b));
    if (diff || !*a) break;
  }
  return diff;
}

#ifndef _WIN32
size_t WinWStrLen(const WINWCHAR *s)
{
  return StrLenUTF16(s);
}

WINWCHAR* WinWStrCpy(WINWCHAR *d, const WINWCHAR *s)
{
  WINWCHAR *ret = d;
  while (*s) *d++ = *s++;
  *d = 0;
  return ret;
}

WINWCHAR* WinWStrNCpy(WINWCHAR *d, const WINWCHAR *s, size_t n)
{
  WINWCHAR *ret = d;
  while (n && *s) *d++ = *s++, n--;
  while (n--) *d++ = 0;
  return ret;
}

int WinWStrCmp(const WINWCHAR *a, const WINWCHAR *b)
{
  int diff = 0;
  do
    diff = static_cast<int>(*a) - static_cast<int>(*b++);
  while (*a++ && !diff);
  return diff;
}

WINWCHAR* WinWStrDupFromWinWStr(const WINWCHAR *s)
{
  WINWCHAR *d = (WINWCHAR*) malloc((WinWStrLen(s) + 1) * sizeof(WINWCHAR));
  if (d) WinWStrCpy(d, s);
  assert(!d || !IS_INTRESOURCE(d)); // DialogTemplate strings can be ATOMs
  return d;
}

WINWCHAR* WinWStrDupFromTChar(const TCHAR *s)
{
  WCToUTF16LEHlpr cnv;
  if (!cnv.Create(s)) throw runtime_error("Unicode conversion failed");
  return (WINWCHAR*) cnv.Detach();
}

int WinWStrToInt(const WINWCHAR *s)
{
  unsigned int v = 0, base = 10, top = '9';
  int sign = 1;
  if (*s == _T('-')) ++s, sign = -1;
  for ( unsigned int c;; )
  {
    if ((c = *s++) >= '0' && c <= top) c -= '0'; else break;
    v *= base, v += c;
  }
  return ((int)v) * sign;
} 
#endif // ~!_WIN32

#if 0
WCHAR *wcsdup_fromansi(const char* s, unsigned int codepage/* = CP_ACP*/)
{
  int l = MultiByteToWideChar(codepage, 0, s, -1, 0, 0);
  if (l == 0)
    throw runtime_error("Unicode conversion failed");

  WCHAR *ws = new WCHAR[l + 1];
  if (MultiByteToWideChar(codepage, 0, s, -1, ws, l + 1) == 0)
    throw runtime_error("Unicode conversion failed");
  return ws;
}
#endif
