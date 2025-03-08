// tstring.h
//
// This file is a part of Unicode NSIS.
//
// Copyright (C) 2007-2025 Jim Park
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

#ifndef NSIS_TSTRING___H__
#define NSIS_TSTRING___H__

#include "Platform.h"
#include "tchar.h"
#include <string>

/* Abstract string type as well. */
#ifdef _UNICODE
typedef std::wstring     tstring;
typedef std::wofstream   tofstream;
typedef std::wifstream   tifstream;
#else
typedef std::string      tstring;
typedef std::ofstream    tofstream;
typedef std::ifstream    tifstream;
#endif

#ifndef _UNICODE
#define CtoTString(str) (str)
#define CtoTString2(str,cp) (str)
#define TtoCString(str) (str)
#define CtoTStrParam CtoTString
#else
#define CtoTString2(str,cp) CtoTString((str),(cp))
#define CtoTStrParam(str) ( (const TCHAR*) CtoTString((str)) ) // Use this when passing as a vararg parameter


// This is a helpful little function for converting exceptions or
// other system type things that come back ANSI and must be
// utilized as either ANSI or wchar_t depending on _UNICODE.
class CtoTString
{
  void Init(const char* str, UINT cp);
  void Init(const char* str);
public:
  CtoTString(const char* str) { Init(str); }
  CtoTString(const char* str, UINT cp) { Init(str, cp); }
  CtoTString(const std::string& str) { Init(str.c_str()); }

  ~CtoTString();

  operator const wchar_t*() const { return m_wStr; }
  inline const wchar_t*GetTStr() const { return m_wStr; }

private:
  wchar_t* m_wStr;
};

// There may be system things that require C strings but we
// may actually have Unicode strings.
class TtoCString
{
  void Init(const wchar_t* str);
public:
  TtoCString(const wchar_t* wStr) { Init(wStr); }
  TtoCString(const tstring& wStr) { Init(wStr.c_str()); }

  ~TtoCString();

  operator const char*() const { return m_cStr; };

private:
  char* m_cStr;
};
#endif // _UNICODE

#define PosixBug_CtoTString CtoTString
#define PosixBug_TtoCString TtoCString

#endif // NSIS_TSTRING___H__
