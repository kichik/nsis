// tstring.h
//
// This file is a part of Unicode NSIS.
//
// Copyright (C) 2007-2009 Jim Park
//
// Licensed under the zlib/libpng license (the "License");
// you may not use this file except in compliance with the License.
//
// This software is provided 'as-is', without any expressed or implied
// warranty.
//
// Provides TSTRING support.

/* 
   Unicode support by Jim Park -- 07/23/2007 
*/

#ifndef _TSTRING_
#define _TSTRING_

#include "Platform.h"
#include "tchar.h"
#include <string>

/* Abstract string type as well. */
#ifdef _UNICODE
typedef std::wstring     tstring;
typedef std::wofstream   tofstream;
typedef std::wifstream   tifstream;
// Use the following macros to open text files.
FILE* FileOpenUnicodeText(const TCHAR* file, const TCHAR* mode, BOOL* unicode);
#define FOPENTEXT(file, mode)           FileOpenUnicodeText(file, _T(mode), NULL)
#define FOPENTEXT2(file, mode, unicode) FileOpenUnicodeText(file, _T(mode), unicode)
#else
typedef std::string      tstring;
typedef std::ofstream    tofstream;
typedef std::ifstream    tifstream;
// Use the following macros to open text files.
#define FOPENTEXT(file, mode)           fopen(file, mode)
#define FOPENTEXT2(file, mode, unicode) (*unicode=FALSE, fopen(file, mode))
#endif

#ifndef _UNICODE
#define CtoTString(str) (str)
#define CtoTString2(str,cp) (str)
#define TtoCString(str) (str)
#else
#define CtoTString2(str,cp) CtoTString(str,cp)

// This is a helpful little function for converting exceptions or
// other system type things that come back ANSI and must be
// utilized as either ANSI or WCHAR depending on _UNICODE.
class CtoTString
{
public:
  CtoTString(const char* str)
  {
    int len = MultiByteToWideChar(CP_ACP, 0, str, -1, NULL, 0);
    m_wStr = (wchar_t*) GlobalAlloc(GPTR, len*sizeof(wchar_t));
    MultiByteToWideChar(CP_ACP, 0, str, -1, m_wStr, len);
  }
  CtoTString(const char* str, UINT cp)
  {
    int len = MultiByteToWideChar(cp, 0, str, -1, NULL, 0);
    m_wStr = (wchar_t*) GlobalAlloc(GPTR, len*sizeof(wchar_t));
    MultiByteToWideChar(cp, 0, str, -1, m_wStr, len);
  }
  CtoTString(const std::string& str)
  {
    int len = MultiByteToWideChar(CP_ACP, 0, str.c_str(), str.length()+1, NULL, 0);
    m_wStr = (wchar_t*) GlobalAlloc(GPTR, len*sizeof(wchar_t));
    MultiByteToWideChar(CP_ACP, 0, str.c_str(), str.length()+1, m_wStr, len);
  }

  ~CtoTString() { GlobalFree(m_wStr); m_wStr = 0; }

  operator const wchar_t*() { return m_wStr; }

private:
  wchar_t* m_wStr;
};

// There may be system things that require C strings but we
// may actually have Unicode strings.
class TtoCString
{
public:
  TtoCString(const wchar_t* wStr)
  {
    int len = WideCharToMultiByte(CP_ACP, 0, wStr, -1, NULL, 0, 0, 0);
    m_cStr = (char*) GlobalAlloc(GPTR, len);
    WideCharToMultiByte(CP_ACP, 0, wStr, -1, m_cStr, len, 0, 0);
  }
  TtoCString(const tstring& wStr)
  {
    int len = WideCharToMultiByte(CP_ACP, 0, wStr.c_str(), wStr.length()+1, NULL, 0, 0, 0);
    m_cStr = (char*) GlobalAlloc(GPTR, len);
    WideCharToMultiByte(CP_ACP, 0, wStr.c_str(), wStr.length()+1, m_cStr, len, 0, 0);
  }

  ~TtoCString() { GlobalFree(m_cStr); m_cStr = 0; }

  operator const char*() { return m_cStr; }

private:
  char* m_cStr;
};
#endif // _UNICODE

#endif
