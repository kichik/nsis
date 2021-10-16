// tstring.cpp
//
// This file is a part of Unicode NSIS.
//
// Copyright (C) 2007-2021 Jim Park
//
// Licensed under the zlib/libpng license (the "License");
// you may not use this file except in compliance with the License.
//
// This software is provided 'as-is', without any expressed or implied
// warranty.
//
// Provides TSTRING support.

#ifdef _UNICODE

#include "tstring.h"
#include "util.h"
#include <vector>
#include <stdio.h>

void CtoTString::Init(const char* str, UINT cp)
{
#if defined(_UNICODE) && !defined(_WIN32)
  if (CP_ACP == cp)
  {
    assert(NSISRT_free_is_STDC_free());
    m_wStr = NSISRT_mbtowc(str); // Should be faster than iconv
    return ;
  }
#endif
  int len = MultiByteToWideChar(cp, 0, str, -1, NULL, 0);
  m_wStr = (wchar_t*) malloc(len*sizeof(wchar_t));
  if (m_wStr) MultiByteToWideChar(cp, 0, str, -1, m_wStr, len);
}

void CtoTString::Init(const char* str)
{
  Init(str, CP_ACP);
}

CtoTString::~CtoTString() { free(m_wStr); m_wStr = 0; }



void TtoCString::Init(const wchar_t* str)
{
#if defined(_UNICODE) && !defined(_WIN32)
  assert(NSISRT_free_is_STDC_free());
  m_cStr = NSISRT_wctomb(str); // Should be faster than iconv
  return ;
#endif
  int len = WideCharToMultiByte(CP_ACP, 0, str, -1, NULL, 0, 0, 0);
  m_cStr = (char*) malloc(len);
  if (m_cStr) WideCharToMultiByte(CP_ACP, 0, str, -1, m_cStr, len, 0, 0);
}

TtoCString::~TtoCString() { free(m_cStr); m_cStr = 0; }


#endif
