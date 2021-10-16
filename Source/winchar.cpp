/*
 * winchar.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
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

WINWCHAR* WinWStrDupFromChar(const char *s, unsigned int cp)
{
  int cch = MultiByteToWideChar(cp, 0, s, -1, 0, 0);
  wchar_t *p = (wchar_t*) malloc(cch * sizeof(wchar_t));
  if (p)
  {
    MultiByteToWideChar(cp, 0, s, -1, p, cch);
#ifndef _WIN32
    wchar_t *p2 = (wchar_t*) WinWStrDupFromWC(p);
    free(p), p = p2;
#endif
  }
  return (WINWCHAR*) p;
}

#ifndef _WIN32
size_t WinWStrLen(const WINWCHAR *s)
{
#ifdef MAKENSIS // Only makensis implements all the functions in utf.cpp
  return StrLenUTF16(s);
#else
  return sizeof(wchar_t) == 2 ? wcslen((wchar_t*)s) : InlineStrLenUTF16(s);
#endif
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

WINWCHAR* WinWStrDupFromWC(const wchar_t *s)
{
#ifdef MAKENSIS
  WCToUTF16LEHlpr cnv;
  if (!cnv.Create(s)) throw runtime_error("Unicode conversion failed");
  return (WINWCHAR*) cnv.Detach();
#else
  // NOTE: Anything outside the ASCII range will not convert correctly!
  size_t cch = wcslen(s);
  WINWCHAR* p = (WINWCHAR*) malloc(++cch * 2);
  if (p) for (size_t i = 0; i < cch; ++i) p[i] = FIX_ENDIAN_INT16(s[i]);
  return p;
#endif
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
