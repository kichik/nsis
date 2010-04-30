/*
 * winchar.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2009 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Reviewed for Unicode support by Jim Park -- 08/13/2007
 */

#include "Platform.h"
#include "winchar.h"
#include "util.h"

#include <stdexcept>

using std::runtime_error;

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

#if 0 // Needed by some RTL missing wchar string functions ?
WCHAR *wcscpy(WCHAR *ws1, const WCHAR *ws2)
{
  WCHAR *ret = ws1;

  while (*ws2)
  {
    *ws1++ = *ws2++;
  }

  *ws1 = 0;

  return ret;
}

WCHAR *wcsncpy(WCHAR *ws1, const WCHAR *ws2, size_t n)
{
  WCHAR *ret = ws1;

  while (n && *ws2)
  {
    *ws1++ = *ws2++;
    n--;
  }

  while (n--)
  {
    *ws1++ = 0;
  }

  return ret;
}

size_t wcslen(const WCHAR *ws)
{
  size_t len = 0;

  while (*ws++)
  {
    len++;
  }

  return len;
}

WCHAR *_wcsdup(const WCHAR *ws)
{
  WCHAR *dup = (WCHAR*) malloc(sizeof(WCHAR)*(wcslen(ws)+1)];
  wcscpy(dup, ws);
  return dup;
}

int wcscmp(const WCHAR *ws1, const WCHAR *ws2)
{
  int diff = 0;

  do
  {
    diff = static_cast<int>(*ws1) - static_cast<int>(*ws2);
  }
  while (*ws1++ && *ws2++ && !diff);

  return diff;
}
#endif
