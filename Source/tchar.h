/*
 * tchar.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * For Unicode support by Jim Park -- 08/30/2007
 */

// Jim Park: Only those we use are listed here.

#pragma once
#ifdef _UNICODE

#include <wchar.h>

#ifndef _T
#define __T(x)   L ## x
#define _T(x)    __T(x)
#define _TEXT(x) __T(x)
#endif

#if !defined(_WIN32) && defined(EXEHEAD)
typedef unsigned short TCHAR, _TUCHAR;
#else
// MinGW typedefs TCHAR and _TCHAR inside #ifndef _TCHAR_DEFINED
// MSVC typedefs TCHAR inside #ifndef _TCHAR_DEFINED 
// and _TCHAR and _T*CHAR inside #ifndef __TCHAR_DEFINED.
// We don't want to break MSVCs _TSCHAR and _TXCHAR so we don't protect our typedef...
#if defined(_WIN32) && ((_MSC_VER>1 && (_MSC_VER<1400 || !defined(_NATIVE_WCHAR_T_DEFINED))) || !defined(_WCHAR_T_DEFINED))
// VC6 knows about __wchar_t but does not support it. /Zc:wchar_t is on by default starting with VC8.
// VC7.1 supports __wchar_t but using it causes problems with conversions from WCHAR (unsigned short)?
typedef unsigned short TCHAR, _TUCHAR;
#else
typedef wchar_t TCHAR, _TUCHAR;
#endif
#endif


// program
#define _tmain      wmain
#define _tWinMain   wWinMain
#define _tenviron   _wenviron
#define __targv     __wargv

// printfs
#define _ftprintf   fwprintf
#define _sntprintf  _snwprintf
#if (defined(_MSC_VER) && (_MSC_VER<=1310||_MSC_FULL_VER<=140040310)) || defined(__MINGW32__)
#	define _stprintf   swprintf
#else
#	define _stprintf   _swprintf
#endif
#define _tprintf    wprintf
#define _vftprintf  vfwprintf
#define _vsntprintf _vsnwprintf
#if defined(_MSC_VER) && (_MSC_VER<=1310||_MSC_FULL_VER<=140040310)
#	define _vstprintf  vswprintf
#else
#	define _vstprintf  _vswprintf
#endif

// scanfs
#define _tscanf     wscanf
#define _stscanf    swscanf

// string manipulations
#define _tcscat     wcscat
#define _tcschr     wcschr
#define _tcsclen    wcslen
#define _tcscpy     wcscpy
#define _tcsdup     _wcsdup
#define _tcslen     wcslen
#define _tcsrchr(s,c) ( wcsrchr((wchar_t*)(s), (c)) ) // bug #1085: If <cwchar> is included the compiler has problems picking the correct overloaded function
#define _tcsstr     wcsstr
#define _tcstok     wcstok

// string comparisons
#define _tcscmp     wcscmp
#define _tcsicmp    _wcsicmp
#define _tcsncicmp  _wcsnicmp
#define _tcsncmp    wcsncmp
#define _tcsnicmp   _wcsnicmp

// upper / lower
#define _tcslwr     _wcslwr
#define _tcsupr     _wcsupr
#define _totlower   towlower
#define _totupper   towupper

// conversions to numbers
#define _tcstoi64   _wcstoi64
#define _tcstol     wcstol
#define _tcstoul    wcstoul
#if !defined(_WIN32) || !defined(_MSC_VER) || (defined(_MSC_VER) && ((_MSC_VER<=1200) || defined(_DLL))) // _wtof does not exist in older versions of MSVCRT.dll
#  define _tstof    my_wtof
#else
#  define _tstof    _wtof
#endif
#define _tstoi      _wtoi
#define _tstoi64    _wtoi64
#define _ttoi       _wtoi
#define _ttoi64     _wtoi64
#define _ttol       _wtol

// conversion from numbers to strings
//Only VC has this? #define _itot _itow
#define _ltot       _ltow
#define _i64tot     _i64tow
#define _ui64tot    _ui64tow

// file manipulations
#define _tfopen     _wfopen
#define _topen      _wopen
#define _tremove    _wremove
#define _tunlink    _wunlink
#define _tstat      _wstat

// reading and writing to i/o
#define _fgettc     fgetwc
#define _fgetts     fgetws
#define _fputts     fputws
#define _gettchar   getwchar

// directory
#define _tchdir     _wchdir

// environment
#define _tgetenv    _wgetenv
#define _tsystem    _wsystem

// time
#define _tcsftime   wcsftime

#else // ANSI

#ifndef _T
#define _T(x)    x
#define _TEXT(x) x
#endif

typedef char            TCHAR;
typedef unsigned char   _TUCHAR;


// program
#define _tmain      main
#define _tWinMain   WinMain
#define _tenviron   environ
#define __targv     __argv

// printfs
#define _ftprintf   fprintf
#define _sntprintf  _snprintf
#define _stprintf   sprintf
#define _tprintf    printf
#define _vftprintf  vfprintf
#define _vsntprintf _vsnprintf
#define _vstprintf  vsprintf

// scanfs
#define _tscanf     scanf
#define _stscanf    sscanf

// string manipulations
#define _tcscat     strcat
#define _tcschr     strchr
#define _tcsclen    strlen
#define _tcscnlen   strnlen
#define _tcscpy     strcpy
#define _tcsdup     _strdup
#define _tcslen     strlen
#define _tcsrchr    strrchr
#define _tcsstr     strstr
#define _tcstok     strtok

// string comparisons
#define _tcscmp     strcmp
#define _tcsncmp    strncmp
#define _tcsncicmp  _strnicmp
#ifdef _WIN32
#define _tcsicmp    _stricmp
#define _tcsnicmp   _strnicmp
#else
#define _tcsicmp    strcasecmp
#define _tcsnicmp   strncasecmp
#endif

// upper / lower
#define _tcslwr     _strlwr
#define _tcsupr     _strupr

#define _totupper   toupper
#define _totlower   tolower

// conversions to numbers
#define _tcstol     strtol
#define _tcstoul    strtoul
#define _tstof      atof
#define _tstoi      atoi
#define _tstoi64    _atoi64
#define _tstoi64    _atoi64
#define _ttoi       atoi
#define _ttoi64     _atoi64
#define _ttol       atol

// conversion from numbers to strings
#define _i64tot     _i64toa
#define _itot       _itoa
#define _ltot       _ltoa
#define _ui64tot    _ui64toa

// file manipulations
#define _tfopen     fopen
#define _topen      _open
#define _tremove    remove
#define _tunlink    _unlink
#define _tstat      stat // _stat on Windows?

// reading and writing to i/o
#define _fgettc     fgetc
#define _fgetts     fgets
#define _fputts     fputs
#define _gettchar   getchar

// directory
#define _tchdir     _chdir

// environment
#define _tgetenv    getenv
#define _tsystem    system

// time
#define _tcsftime   strftime

#endif

// is functions (the same in Unicode / ANSI on MinGW? MSVC does not agree!)
#ifndef _istascii
#if defined(_MSC_VER) && defined(_UNICODE)
#define _istascii iswascii
#else
#define _istascii __isascii
#endif
#endif
