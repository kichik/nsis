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
#define FOPENTEXT2(file, mode, unicode) (*(unicode)=FALSE, fopen(file, mode))
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
// utilized as either ANSI or WCHAR depending on _UNICODE.
class CtoTString
{
public:
  CtoTString(const char* str);
  CtoTString(const char* str, UINT cp);
  CtoTString(const std::string& str);

  ~CtoTString();

  operator const wchar_t*() const;
  inline const wchar_t*GetTStr() const;

private:
  wchar_t* m_wStr;
};

// There may be system things that require C strings but we
// may actually have Unicode strings.
class TtoCString
{
public:
  TtoCString(const wchar_t* wStr);
  TtoCString(const tstring& wStr);

  ~TtoCString();

  operator const char*() const;

private:
  char* m_cStr;
};
#endif // _UNICODE

#endif // NSIS_TSTRING___H__
