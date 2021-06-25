/*
 * Platform.h
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
 *
 * Unicode support by Jim Park -- 08/13/2007
 */

#ifndef ___PLATFORM__H___
#define ___PLATFORM__H___

// some definitions for non Win32 platforms were taken from MinGW's free Win32 library


#if defined(__cplusplus) && defined(MAKENSIS) && (!defined(_MSC_VER) || _MSC_VER > 1200)
template<class T> class NSISCHARTYPE{ T _c; public: NSISCHARTYPE(){} NSISCHARTYPE(T c):_c(c){} operator T()const{ return _c; } };
typedef NSISCHARTYPE<unsigned short> WINWCHAR; // WINWCHAR is always UTF16LE and should not be passed to wcs* functions
#else
typedef unsigned short WINWCHAR;
#endif


// includes

#include "tchar.h"

#ifdef _WIN32
#include <windows.h>
#include <commctrl.h>
#include <shellapi.h>
#include <shlwapi.h>
#include <shlobj.h>
#else
#  include <stdint.h>
#  ifndef EXEHEAD
#    include <string.h>
#    include <stdlib.h>
#  endif
// basic types
typedef uint8_t BYTE, *PBYTE, *LPBYTE;
typedef uint16_t WORD, *LPWORD;
typedef uint32_t DWORD, *LPDWORD;
typedef int16_t SHORT;
typedef uint16_t USHORT;
typedef uint32_t UINT;
typedef uint32_t UINT32;
typedef int32_t INT;
typedef int32_t INT32;
typedef int32_t LONG;
typedef uint32_t ULONG;
typedef int64_t INT64, LARGE_INTEGER;
typedef uint64_t UINT64, ULARGE_INTEGER;
typedef intptr_t INT_PTR;
typedef uintptr_t UINT_PTR;
typedef int BOOL, *LPBOOL;
typedef short VARIANT_BOOL;
typedef void VOID;
typedef void *LPVOID;
typedef char CHAR, *PCHAR, *LPCH, *PCH, *NPSTR, *LPSTR, *PSTR;
typedef unsigned char UCHAR;
typedef const char *LPCCH, *PCSTR, *LPCSTR;
typedef unsigned short WCHAR, OLECHAR, *PWCHAR, *LPWCH, *PWCH, *NWPSTR, *LPWSTR, *PWSTR, *BSTR;
typedef const unsigned short *LPCWCH, *PCWCH, *LPCWSTR, *PCWSTR, *LPCOLESTR;
#ifndef _tctime
#define _tctime _wctime
#else
#define _tctime ctime
#endif
// basic stuff
typedef void * HANDLE;
typedef HANDLE HWND;
typedef HANDLE HMODULE;
typedef unsigned long HKEY;
// some gdi
typedef DWORD COLORREF;
typedef HANDLE HBRUSH;
// bool
#  define FALSE 0
#  define TRUE 1
// more
typedef WORD LANGID;
// ULONGLONG
#ifdef __GNUC__
#define _HAVE_INT64
#define _INTEGRAL_MAX_BITS 64
#undef __int64
#define __int64 long long
#elif defined(__WATCOMC__) && (defined(_INTEGRAL_MAX_BITS) && _INTEGRAL_MAX_BITS >= 64 )
#define _HAVE_INT64
#endif /* __GNUC__/__WATCOMC */
#if defined(_HAVE_INT64) || (defined(_INTEGRAL_MAX_BITS) && _INTEGRAL_MAX_BITS >= 64)
typedef __int64 LONGLONG;
typedef unsigned __int64 DWORDLONG;
#else
typedef double LONGLONG,DWORDLONG;
#endif
typedef LONGLONG *PLONGLONG;
typedef DWORDLONG *PDWORDLONG;
typedef DWORDLONG ULONGLONG,*PULONGLONG;

// function mapping
#define _strdup strdup
#define _snprintf snprintf
#define _vsnprintf vsnprintf

#endif // ?WIN32

#ifndef INT_MAX
#include <limits.h>
#endif


// mingw32 and w64-mingw32 do not define ULONG_PTR
// but rather declare ULONG_PTR via typedef (see basetsd.h)
#if !defined(__MINGW32__) && !defined(ULONG_PTR)
#  ifndef _WIN64
#    define ULONG_PTR unsigned long
#  endif
#endif

#ifdef __cplusplus
#include <algorithm>
#if defined(_MSC_VER) && ( _MSC_VER <= 1200 || (defined(_MIN) && _MSC_FULL_VER >= 140000000 && _MSC_FULL_VER <= 140040310) )
#define STD_MIN std::_MIN
#define STD_MAX std::_MAX
#else
#define STD_MIN (std::min) // This works even when windows.h defines min/max
#define STD_MAX (std::max)
#endif
#endif

#ifndef COUNTOF
#define COUNTOF(a) (sizeof(a)/sizeof(a[0]))
#endif

#ifndef __BIG_ENDIAN__
# define FIX_ENDIAN_INT64(x) (x)
# define FIX_ENDIAN_INT32_INPLACE(x) ((void)(x))
# define FIX_ENDIAN_INT32(x) (x)
# define FIX_ENDIAN_INT16_INPLACE(x) ((void)(x))
# define FIX_ENDIAN_INT16(x) (x)
# define BE2HE32(x) SWAP_ENDIAN_INT32(x)
#else
# define FIX_ENDIAN_INT64(x) SWAP_ENDIAN_INT64(x)
# define FIX_ENDIAN_INT32_INPLACE(x) ((x) = SWAP_ENDIAN_INT32(x))
# define FIX_ENDIAN_INT32(x) SWAP_ENDIAN_INT32(x)
# define FIX_ENDIAN_INT16_INPLACE(x) ((x) = SWAP_ENDIAN_INT16(x))
# define FIX_ENDIAN_INT16(x) SWAP_ENDIAN_INT16(x)
# define BE2HE32(x) (x)
#endif
#define SWAP_ENDIAN_INT64(x) ( \
  (((x)&0xFF00000000000000) >> 56) | \
  (((x)&0x00FF000000000000) >> 40) | \
  (((x)&0x0000FF0000000000) >> 24) | \
  (((x)&0x000000FF00000000) >>  8) | \
  (((x)&0x00000000FF000000) <<  8) | \
  (((x)&0x0000000000FF0000) << 24) | \
  (((x)&0x000000000000FF00) << 40) | \
  (((x)&0x00000000000000FF) << 56) )
#define SWAP_ENDIAN_INT32(x) ( \
  (((x)&0xFF000000) >> 24) | \
  (((x)&0x00FF0000) >>  8) | \
  (((x)&0x0000FF00) <<  8) | \
  (((x)&0x000000FF) << 24) )
#define SWAP_ENDIAN_INT16(x) ( \
  (((x)&0xFF00) >> 8) | \
  (((x)&0x00FF) << 8) )

// script path separator

#  define PATH_SEPARATOR_STR _T("\\")
#  define PATH_SEPARATOR_C _T('\\')

// system specific characters

#ifdef _WIN32
#  define PLATFORM_PATH_SEPARATOR_STR _T("\\")
#  define PLATFORM_PATH_SEPARATOR_C _T('\\')
#  define OPT_STR _T("/")
#  define OPT_C _T('/')
#  define IS_OPT(a) (a[0]==OPT_C||a[0]==_T('-'))
#else
#  define PLATFORM_PATH_SEPARATOR_STR _T("/")
#  define PLATFORM_PATH_SEPARATOR_C _T('/')
#  define OPT_STR _T("-")
#  define OPT_C _T('-')
#  define IS_OPT(a) (a[0]==OPT_C)
#endif

// attributes

#ifdef _MSC_VER
#  define FORCE_INLINE __forceinline
#else
#  ifdef __GNUC__
#    if __GNUC__ < 3
#      define FORCE_INLINE inline
#    else
#      define FORCE_INLINE inline __attribute__ ((always_inline))
#    endif
#  else
#    define FORCE_INLINE inline
#  endif
#endif

#if defined(__GNUC__)
#define UNUSED __attribute__((unused))
#else
#define UNUSED
#endif

// macros

#ifndef _WIN32
#  ifndef FIELD_OFFSET
#    define FIELD_OFFSET(t,f) ((UINT_PTR)&(((t*)0)->f))
#  endif
#  ifndef MAKEINTRESOURCEA
#    define MAKEINTRESOURCEA(i) ((LPSTR)((ULONG_PTR)((WORD)(ULONG_PTR)(i))))
#  endif
#  ifndef MAKEINTRESOURCEW
#    define MAKEINTRESOURCEW(i) ((LPWSTR)((ULONG_PTR)((WORD)(ULONG_PTR)(i))))
#  endif
#  ifndef MAKEINTRESOURCE
#    ifdef _UNICODE
#      define MAKEINTRESOURCE MAKEINTRESOURCEW
#    else
#      define MAKEINTRESOURCE MAKEINTRESOURCEA
#    endif
#  endif
#  ifndef IMAGE_FIRST_SECTION
#    define IMAGE_FIRST_SECTION(h) ( PIMAGE_SECTION_HEADER( (ULONG_PTR) h + \
                                     FIELD_OFFSET(IMAGE_NT_HEADERS, OptionalHeader) + \
                                     FIX_ENDIAN_INT16(PIMAGE_NT_HEADERS(h)->FileHeader.SizeOfOptionalHeader) ) )
#  endif
#  ifndef RGB
#    define RGB(r,g,b) ((DWORD)(((BYTE)(r)|((WORD)(g)<<8))|(((DWORD)(BYTE)(b))<<16)))
#  endif
#  ifndef LOBYTE
#    define LOBYTE(w) ((BYTE)(w))
#    define HIBYTE(w) ((BYTE)(((WORD)(w)>>8)&0xFF))
#  endif
#  ifndef MAKEWORD
#    define MAKEWORD(a,b) ((WORD)(((BYTE)(a))|(((WORD)((BYTE)(b)))<<8)))
#  endif
#  ifndef MAKELONG
#    define MAKELONG(a,b) ((DWORD)(((WORD)(a))|(((DWORD)((WORD)(b)))<<16)))
#  endif
#endif
#ifndef IS_INTRESOURCE
#  define IS_INTRESOURCE(_r) (((ULONG_PTR)(_r) >> 16) == 0)
#endif

#ifndef IS_HIGH_SURROGATE
#  define IS_HIGH_SURROGATE(wch) (((wch) >= 0xd800) && ((wch) <= 0xdbff))
#endif

// functions

// Anders: MSVC's swprintf is non standard, use _snwprintf when you really mean swprintf
#if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_snwprintf)
#  define _snwprintf swprintf // (wchar_t*,size_t,const wchar_t*,...)
#endif
#ifndef _WIN32
#  define _vsnwprintf vswprintf // (wchar_t*,size_t,const wchar_t*,va_list)
#endif

// Jim Park: These str functions will probably never be encountered with all my
// Unicode changes.  And if they were used, these would probably be wrong.
#ifndef _WIN32
#  define stricmp strcasecmp
#  define strcmpi strcasecmp
#  define strnicmp strncasecmp
#  define CopyMemory memcpy
#  define ZeroMemory(x, y) memset(x, 0, y)
#endif

#ifndef _WIN64
#  ifndef GCLP_HICON
#    define GCLP_HICON GCL_HICON
#    define SetClassLongPtr SetClassLong
#  endif
#endif

// defines

#ifndef MEM_LARGE_PAGES
#  define MEM_LARGE_PAGES 0x20000000
#endif

#ifndef WC_NO_BEST_FIT_CHARS
#  define WC_NO_BEST_FIT_CHARS 0x400
#endif

#ifndef IDC_HAND
#  define IDC_HAND MAKEINTRESOURCE(32649)
#endif

#ifndef BIF_NEWDIALOGSTYLE
#  define BIF_NEWDIALOGSTYLE 0x0040
#endif

#ifndef TVITEM
#  define TVITEM TV_ITEM
#endif

#ifndef TVM_SETITEMHEIGHT
#  define TVM_SETITEMHEIGHT (TV_FIRST + 27)
#endif

#ifndef TVM_GETITEMHEIGHT
#  define TVM_GETITEMHEIGHT (TV_FIRST + 28)
#endif

#ifndef LVS_EX_LABELTIP
#  define LVS_EX_LABELTIP 0x00004000
#endif

#ifndef EXEHEAD
#  ifndef SF_TEXT
#    define SF_TEXT 0x0001
#  endif
#  ifndef SF_RTF
#    define SF_RTF 0x0002
#  endif
#  ifndef SF_UNICODE
#    define SF_UNICODE 0x0010
#  endif
#endif

#ifdef __GNUC__
#  undef INVALID_FILE_ATTRIBUTES
#endif
#ifndef INVALID_FILE_ATTRIBUTES
#  define INVALID_FILE_ATTRIBUTES ((DWORD) -1)
#endif

// shell folders

#ifdef _WIN32
#  include <shlobj.h>
#endif

#ifndef CSIDL_FLAG_CREATE
#  define CSIDL_FLAG_CREATE 0x8000
#endif

#ifndef CSIDL_PROGRAMS
#  define CSIDL_PROGRAMS 0x2
#endif
#ifndef CSIDL_COMMON_PROGRAMS
#  define CSIDL_COMMON_PROGRAMS 0x17
#endif
#ifndef CSIDL_PRINTERS
#  define CSIDL_PRINTERS 0x4
#endif
#ifndef CSIDL_PERSONAL
#  define CSIDL_PERSONAL 0x5
#endif
#ifndef CSIDL_COMMON_DOCUMENTS
#  define CSIDL_COMMON_DOCUMENTS 0x2E
#endif
#ifndef CSIDL_FAVORITES
#  define CSIDL_FAVORITES 0x6
#endif
#ifndef CSIDL_COMMON_FAVORITES
#  define CSIDL_COMMON_FAVORITES 0x1F
#endif
#ifndef CSIDL_STARTUP
#  define CSIDL_STARTUP 0x7
#endif
#ifndef CSIDL_COMMON_STARTUP
#  define CSIDL_COMMON_STARTUP 0x18
#endif
#ifndef CSIDL_RECENT
#  define CSIDL_RECENT 0x8
#endif
#ifndef CSIDL_SENDTO
#  define CSIDL_SENDTO 0x9
#endif
#ifndef CSIDL_STARTMENU
#  define CSIDL_STARTMENU 0xB
#endif
#ifndef CSIDL_COMMON_STARTMENU
#  define CSIDL_COMMON_STARTMENU 0x16
#endif
#ifndef CSIDL_DESKTOPDIRECTORY
#  define CSIDL_DESKTOPDIRECTORY 0x10
#endif
#ifndef CSIDL_COMMON_DESKTOPDIRECTORY
#  define CSIDL_COMMON_DESKTOPDIRECTORY 0x19
#endif
#ifndef CSIDL_NETHOOD
#  define CSIDL_NETHOOD 0x13
#endif
#ifndef CSIDL_FONTS
#  define CSIDL_FONTS 0x14
#endif
#ifndef CSIDL_TEMPLATES
#  define CSIDL_TEMPLATES 0x15
#endif
#ifndef CSIDL_COMMON_TEMPLATES
#  define CSIDL_COMMON_TEMPLATES 0x2D
#endif
#ifndef CSIDL_APPDATA  
#  define CSIDL_APPDATA 0x1A
#endif
#ifndef CSIDL_COMMON_APPDATA
#  define CSIDL_COMMON_APPDATA 0x23
#endif
#ifndef CSIDL_LOCAL_APPDATA
#  define CSIDL_LOCAL_APPDATA 0x1C
#endif
#ifndef CSIDL_PRINTHOOD
#  define CSIDL_PRINTHOOD 0x1B
#endif
#ifndef CSIDL_ALTSTARTUP   
#  define CSIDL_ALTSTARTUP 0x1D
#endif
#ifndef CSIDL_COMMON_ALTSTARTUP
#  define CSIDL_COMMON_ALTSTARTUP 0x1E
#endif
#ifndef CSIDL_INTERNET_CACHE  
#  define CSIDL_INTERNET_CACHE 0x20
#endif
#ifndef CSIDL_COOKIES
#  define CSIDL_COOKIES 0x21
#endif
#ifndef CSIDL_HISTORY
#  define CSIDL_HISTORY 0x22
#endif
#ifndef CSIDL_WINDOWS
#  define CSIDL_WINDOWS 0x24
#endif
#ifndef CSIDL_SYSTEM
#  define CSIDL_SYSTEM 0x25
#endif
#ifndef CSIDL_PROGRAM_FILES
#  define CSIDL_PROGRAM_FILES 0x26
#endif
#ifndef CSIDL_PROGRAM_FILES_COMMON
#  define CSIDL_PROGRAM_FILES_COMMON 0x2B
#endif
#ifndef CSIDL_MYPICTURES
#  define CSIDL_MYPICTURES 0x27
#endif
#ifndef CSIDL_COMMON_PICTURES
#  define CSIDL_COMMON_PICTURES 0x36
#endif
#ifndef CSIDL_PROFILE
#  define CSIDL_PROFILE 0x28
#endif
#ifndef CSIDL_ADMINTOOLS
#  define CSIDL_ADMINTOOLS 0x30
#endif
#ifndef CSIDL_COMMON_ADMINTOOLS
#  define CSIDL_COMMON_ADMINTOOLS 0x2F
#endif
#ifndef CSIDL_MYMUSIC
#  define CSIDL_MYMUSIC 0xD
#endif
#ifndef CSIDL_COMMON_MUSIC
#  define CSIDL_COMMON_MUSIC 0x35
#endif
#ifndef CSIDL_MYVIDEO
#  define CSIDL_MYVIDEO 0xE
#endif
#ifndef CSIDL_COMMON_VIDEO
#  define CSIDL_COMMON_VIDEO 0x37
#endif
#ifndef CSIDL_RESOURCES
#  define CSIDL_RESOURCES 0x38
#endif
#ifndef CSIDL_RESOURCES_LOCALIZED
#  define CSIDL_RESOURCES_LOCALIZED 0x39
#endif
#ifndef CSIDL_CDBURN_AREA
#  define CSIDL_CDBURN_AREA 0x3B
#endif

#ifndef SHGFP_TYPE_CURRENT
  #define SHGFP_TYPE_CURRENT 0
#endif

// other shell stuff

#ifndef SHACF_FILESYSTEM
#  define SHACF_FILESYSTEM 1
#endif

#ifndef SEE_MASK_NOCLOSEPROCESS
#define SEE_MASK_NOCLOSEPROCESS 0x00000040
#define SEE_MASK_FLAG_NO_UI     0x00000400
#define SEE_MASK_FLAG_DDEWAIT   0x00000100
#endif

// other stuff

#ifndef CP_ACP
#  define CP_ACP 0
#  define CP_OEMCP 1
#endif
#ifndef CP_UTF8
#  define CP_UTF8 65001
#endif

#ifndef COLOR_BTNFACE
#  define COLOR_BTNFACE 15
#endif
#ifndef COLOR_WINDOW
#  define COLOR_WINDOW 5
#endif
#ifndef COLOR_HOTLIGHT
#  define COLOR_HOTLIGHT 26
#endif

// resources

#ifndef RT_CURSOR
#  define RT_CURSOR MAKEINTRESOURCE(1)
#  define RT_GROUP_CURSOR MAKEINTRESOURCE(1 + 11)
#endif
#ifndef RT_BITMAP
#  define RT_BITMAP MAKEINTRESOURCE(2)
#endif
#ifndef RT_ICON
#  define RT_ICON MAKEINTRESOURCE(3)
#endif
#ifndef RT_DIALOG
#  define RT_DIALOG MAKEINTRESOURCE(5)
#endif
#ifndef RT_GROUP_ICON
#  define RT_GROUP_ICON MAKEINTRESOURCE(3 + 11)
#endif
#ifndef RT_VERSION
#  define RT_VERSION MAKEINTRESOURCE(16)
#endif

// version

#ifndef VS_FILE_INFO
#  define VS_FILE_INFO RT_VERSION
#endif
#ifndef VS_VERSION_INFO
#  define VS_VERSION_INFO 1
#endif
#ifndef VS_FFI_SIGNATURE
#  define VS_FFI_SIGNATURE 0xFEEF04BD
#endif

// message box

#ifndef MB_OK
#  define MB_OK 0
#  define MB_OKCANCEL 1
#  define MB_ABORTRETRYIGNORE 2
#  define MB_YESNOCANCEL 3
#  define MB_YESNO 4
#  define MB_RETRYCANCEL 5
#  define MB_DEFBUTTON1 0
#  define MB_DEFBUTTON2 256
#  define MB_DEFBUTTON3 512
#  define MB_DEFBUTTON4 768
#  define MB_ICONSTOP 16
#  define MB_ICONQUESTION 32
#  define MB_ICONEXCLAMATION 48
#  define MB_ICONINFORMATION 64
#  define MB_USERICON 128
#  define MB_SETFOREGROUND 0x10000
#  define MB_TOPMOST 0x40000
#  define MB_RIGHT 0x80000
#  define MB_RTLREADING 0x100000
#endif

#ifndef IDOK
#  define IDOK 1
#  define IDCANCEL 2
#  define IDABORT 3
#  define IDRETRY 4
#  define IDIGNORE 5
#  define IDYES 6
#  define IDNO 7
#endif

// window styles

#ifndef _WIN32
#  define WS_CHILD 0x40000000
#  define WS_VISIBLE 0x10000000

#  define BS_CHECKBOX 2
#  define BS_LEFT 256
#  define BS_LEFTTEXT 32
#  define BS_RADIOBUTTON 4
#  define BS_RIGHT 512
#  define BS_USERBUTTON 8

#  define ES_LEFT 0
#  define ES_CENTER 1
#  define ES_RIGHT 2

#  define SS_BITMAP 14
#  define SS_CENTER 1
#  define SS_CENTERIMAGE 512
#  define SS_ICON 3
#  define SS_LEFT 0
#  define SS_LEFTNOWORDWRAP 0xc
#  define SS_RIGHT 2
#  define SS_RIGHTJUST 0x400
#  define SS_USERITEM 10
#  define SS_TYPEMASK 0x0000001FL

#  define DS_FIXEDSYS 8
#  define DS_SETFONT 64

#  define WS_EX_RIGHT 0x1000
#  define WS_EX_RIGHTSCROLLBAR 0
#  define WS_EX_RTLREADING 0x2000
#  define WS_EX_LEFTSCROLLBAR 0x4000
#  define WS_EX_LAYOUTRTL 0x00400000

#  define TVS_RTLREADING 64

#  define PBS_SMOOTH 1
#endif

#ifndef SS_REALSIZECONTROL
#  define SS_REALSIZECONTROL 0x0040
#endif

#ifndef DS_SHELLFONT
#  define DS_SHELLFONT (DS_SETFONT | DS_FIXEDSYS)
#endif

// brush styles

#ifndef BS_SOLID
#  define BS_SOLID 0
#endif
#ifndef BS_NULL
#  define BS_NULL 1
#endif

// reg
#ifndef HKEY_CLASSES_ROOT
#  define HKEY_CLASSES_ROOT ((HKEY)0x80000000)
#  define HKEY_CURRENT_USER ((HKEY)0x80000001)
#  define HKEY_LOCAL_MACHINE ((HKEY)0x80000002)
#  define HKEY_USERS ((HKEY)0x80000003)
#  define HKEY_PERFORMANCE_DATA ((HKEY)0x80000004)
#  define HKEY_CURRENT_CONFIG ((HKEY)0x80000005)
#  define HKEY_DYN_DATA ((HKEY)0x80000006)
#endif

#ifndef KEY_WOW64_32KEY
#  define KEY_WOW64_32KEY 0x200
#endif
#ifndef KEY_WOW64_64KEY
#  define KEY_WOW64_64KEY 0x100
#endif

#ifndef REG_SZ
#  define REG_NONE 0
#  define REG_SZ 1
#  define REG_EXPAND_SZ 2
#  define REG_BINARY 3
#  define REG_DWORD 4
#  define REG_MULTI_SZ 7
#endif

// show modes

#ifndef SW_SHOWNORMAL
#  define SW_HIDE 0
#  define SW_SHOWNORMAL 1
#  define SW_SHOWMINIMIZED 2
#  define SW_SHOWMAXIMIZED 3
#  define SW_SHOWNOACTIVATE 4
#  define SW_SHOW 5
#  define SW_SHOWMINNOACTIVE 7
#  define SW_SHOWNA 8
#  define SW_RESTORE 9
#  define SW_SHOWDEFAULT 10
#endif

// hotkeys

#ifndef HOTKEYF_SHIFT
#  define HOTKEYF_SHIFT 1
#  define HOTKEYF_CONTROL 2
#  define HOTKEYF_ALT 4
#  define HOTKEYF_EXT 8
#endif

// vk
#ifndef VK_F1
#  define VK_F1 0x70
#endif

// gdi

#ifndef OPAQUE
#  define OPAQUE 2
#endif
#ifndef TRANSPARENT
#  define TRANSPARENT 1
#endif
#ifndef LF_FACESIZE
#  define LF_FACESIZE 32
#endif
#ifndef FW_NORMAL
#  define FW_NORMAL 400
#endif
#ifndef FW_BOLD
#  define FW_BOLD 700
#endif
#ifndef DEFAULT_CHARSET
#  define DEFAULT_CHARSET 1
#endif
#ifndef SHIFTJIS_CHARSET
#  define SHIFTJIS_CHARSET 128
#endif
#ifndef OUT_DEFAULT_PRECIS
#  define OUT_DEFAULT_PRECIS 0
#endif
#ifndef CLIP_DEFAULT_PRECIS
#  define CLIP_DEFAULT_PRECIS 0
#endif
#ifndef DEFAULT_QUALITY
#  define DEFAULT_QUALITY 0
#endif
#ifndef DEFAULT_PITCH
#  define DEFAULT_PITCH 0
#endif

// file ops

#ifndef FOF_SILENT
#  define FOF_SILENT 4
#  define FOF_NOCONFIRMATION 16
#  define FOF_FILESONLY 128
#  define FOF_SIMPLEPROGRESS 256
#  define FOF_NOCONFIRMMKDIR 512
#endif
#ifndef FOF_NOERRORUI
#  define FOF_NOERRORUI 0x0400
#endif

// file attribs

#ifndef FILE_ATTRIBUTE_READONLY
#  define FILE_ATTRIBUTE_READONLY 0x00000001
#  define FILE_ATTRIBUTE_HIDDEN 0x00000002
#  define FILE_ATTRIBUTE_SYSTEM 0x00000004
#  define FILE_ATTRIBUTE_ARCHIVE 0x00000020
#  define FILE_ATTRIBUTE_NORMAL 0x00000080
#  define FILE_ATTRIBUTE_TEMPORARY 0x00000100
#  define FILE_ATTRIBUTE_OFFLINE 0x00001000
#endif
#ifndef FILE_ATTRIBUTE_NOT_CONTENT_INDEXED
#  define FILE_ATTRIBUTE_NOT_CONTENT_INDEXED 0x002000
#endif


// fopen
#ifndef GENERIC_READ
#  define GENERIC_READ 0x80000000
#  define GENERIC_WRITE 0x40000000
#endif

#ifndef CREATE_NEW
#  define CREATE_NEW 1
#  define CREATE_ALWAYS 2
#  define OPEN_EXISTING 3
#  define OPEN_ALWAYS 4
#endif

// fseek

#ifndef FILE_BEGIN
#  define FILE_BEGIN 0
#  define FILE_CURRENT 1
#  define FILE_END 2
#endif

// PE

#ifndef _WIN32
#  define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16
#  ifndef __BIG_ENDIAN__
#    define IMAGE_DOS_SIGNATURE 0x5A4D
#    define IMAGE_NT_SIGNATURE 0x00004550
#  else
#    define IMAGE_DOS_SIGNATURE 0x4D5A
#    define IMAGE_NT_SIGNATURE 0x50450000
#  endif
#  define IMAGE_FILE_DLL 8192
#  define IMAGE_DIRECTORY_ENTRY_EXPORT 0
#  define IMAGE_SIZEOF_SHORT_NAME 8
#endif
#ifndef IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE
#define IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE 0x0040 // ASLR
#endif
#ifndef IMAGE_DLLCHARACTERISTICS_NX_COMPAT
#define IMAGE_DLLCHARACTERISTICS_NX_COMPAT 0x0100 // DEP
#endif
#ifndef IMAGE_DLLCHARACTERISTICS_NO_SEH
#define IMAGE_DLLCHARACTERISTICS_NO_SEH 0x0400
#endif
#ifndef IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE
#define IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE 0x8000
#endif
#ifndef IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA
#define IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA 0x0020 // HEASLR
#endif

// structures

#ifndef _WIN32
typedef struct _LOGFONT {
  LONG lfHeight;
  LONG lfWidth;
  LONG lfEscapement;
  LONG lfOrientation;
  LONG lfWeight;
  BYTE lfItalic;
  BYTE lfUnderline;
  BYTE lfStrikeOut;
  BYTE lfCharSet;
  BYTE lfOutPrecision;
  BYTE lfClipPrecision;
  BYTE lfQuality;
  BYTE lfPitchAndFamily;
  TCHAR lfFaceName[LF_FACESIZE];
} LOGFONT;
#  pragma pack(2)
typedef struct _IMAGE_DOS_HEADER {
  WORD e_magic;
  WORD e_cblp;
  WORD e_cp;
  WORD e_crlc;
  WORD e_cparhdr;
  WORD e_minalloc;
  WORD e_maxalloc;
  WORD e_ss;
  WORD e_sp;
  WORD e_csum;
  WORD e_ip;
  WORD e_cs;
  WORD e_lfarlc;
  WORD e_ovno;
  WORD e_res[4];
  WORD e_oemid;
  WORD e_oeminfo;
  WORD e_res2[10];
  LONG e_lfanew;
} IMAGE_DOS_HEADER,*PIMAGE_DOS_HEADER;
#  pragma pack()
#  pragma pack(4)
typedef struct _IMAGE_FILE_HEADER {
  WORD Machine;
  WORD NumberOfSections;
  DWORD TimeDateStamp;
  DWORD PointerToSymbolTable;
  DWORD NumberOfSymbols;
  WORD SizeOfOptionalHeader;
  WORD Characteristics;
} IMAGE_FILE_HEADER, *PIMAGE_FILE_HEADER;
typedef struct _IMAGE_DATA_DIRECTORY {
  DWORD VirtualAddress;
  DWORD Size;
} IMAGE_DATA_DIRECTORY,*PIMAGE_DATA_DIRECTORY;
typedef struct _IMAGE_OPTIONAL_HEADER32 {
  WORD Magic;
  BYTE MajorLinkerVersion;
  BYTE MinorLinkerVersion;
  DWORD SizeOfCode;
  DWORD SizeOfInitializedData;
  DWORD SizeOfUninitializedData;
  DWORD AddressOfEntryPoint;
  DWORD BaseOfCode;
  DWORD BaseOfData;
  DWORD ImageBase;
  DWORD SectionAlignment;
  DWORD FileAlignment;
  WORD MajorOperatingSystemVersion;
  WORD MinorOperatingSystemVersion;
  WORD MajorImageVersion;
  WORD MinorImageVersion;
  WORD MajorSubsystemVersion;
  WORD MinorSubsystemVersion;
  DWORD Reserved1;
  DWORD SizeOfImage;
  DWORD SizeOfHeaders;
  DWORD CheckSum;
  WORD Subsystem;
  WORD DllCharacteristics;
  DWORD SizeOfStackReserve;
  DWORD SizeOfStackCommit;
  DWORD SizeOfHeapReserve;
  DWORD SizeOfHeapCommit;
  DWORD LoaderFlags;
  DWORD NumberOfRvaAndSizes;
  IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} IMAGE_OPTIONAL_HEADER32,*PIMAGE_OPTIONAL_HEADER32;
typedef struct _IMAGE_OPTIONAL_HEADER64 {
  WORD Magic;
  BYTE MajorLinkerVersion;
  BYTE MinorLinkerVersion;
  DWORD SizeOfCode;
  DWORD SizeOfInitializedData;
  DWORD SizeOfUninitializedData;
  DWORD AddressOfEntryPoint;
  DWORD BaseOfCode;
  ULONGLONG ImageBase;
  DWORD SectionAlignment;
  DWORD FileAlignment;
  WORD MajorOperatingSystemVersion;
  WORD MinorOperatingSystemVersion;
  WORD MajorImageVersion;
  WORD MinorImageVersion;
  WORD MajorSubsystemVersion;
  WORD MinorSubsystemVersion;
  DWORD Win32VersionValue;
  DWORD SizeOfImage;
  DWORD SizeOfHeaders;
  DWORD CheckSum;
  WORD Subsystem;
  WORD DllCharacteristics;
  ULONGLONG SizeOfStackReserve;
  ULONGLONG SizeOfStackCommit;
  ULONGLONG SizeOfHeapReserve;
  ULONGLONG SizeOfHeapCommit;
  DWORD LoaderFlags;
  DWORD NumberOfRvaAndSizes;
  IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} IMAGE_OPTIONAL_HEADER64,*PIMAGE_OPTIONAL_HEADER64;
typedef struct _IMAGE_NT_HEADERS32 {
  DWORD Signature;
  IMAGE_FILE_HEADER FileHeader;
  IMAGE_OPTIONAL_HEADER32 OptionalHeader;
} IMAGE_NT_HEADERS32,*PIMAGE_NT_HEADERS32;
typedef struct _IMAGE_NT_HEADERS64 {
  DWORD Signature;
  IMAGE_FILE_HEADER FileHeader;
  IMAGE_OPTIONAL_HEADER64 OptionalHeader;
} IMAGE_NT_HEADERS64,*PIMAGE_NT_HEADERS64;
#ifdef _WIN64
typedef IMAGE_OPTIONAL_HEADER64  IMAGE_OPTIONAL_HEADER;
typedef PIMAGE_OPTIONAL_HEADER64 PIMAGE_OPTIONAL_HEADER;
typedef IMAGE_NT_HEADERS64 IMAGE_NT_HEADERS;
typedef PIMAGE_NT_HEADERS64 PIMAGE_NT_HEADERS;
#else
typedef IMAGE_OPTIONAL_HEADER32  IMAGE_OPTIONAL_HEADER;
typedef PIMAGE_OPTIONAL_HEADER32 PIMAGE_OPTIONAL_HEADER;
typedef IMAGE_NT_HEADERS32  IMAGE_NT_HEADERS;
typedef PIMAGE_NT_HEADERS32 PIMAGE_NT_HEADERS;
#endif
#ifndef __BIG_ENDIAN__
#  define IMAGE_NT_OPTIONAL_HDR32_MAGIC 0x010b
#  define IMAGE_NT_OPTIONAL_HDR64_MAGIC 0x020b
#else
#  define IMAGE_NT_OPTIONAL_HDR32_MAGIC 0x0b01
#  define IMAGE_NT_OPTIONAL_HDR64_MAGIC 0x0b02
#endif
typedef struct _IMAGE_SECTION_HEADER {
  BYTE Name[IMAGE_SIZEOF_SHORT_NAME];
 union {
    DWORD PhysicalAddress;
    DWORD VirtualSize;
  } Misc;
  DWORD VirtualAddress;
  DWORD SizeOfRawData;
  DWORD PointerToRawData;
  DWORD PointerToRelocations;
  DWORD PointerToLinenumbers;
  WORD NumberOfRelocations;
  WORD NumberOfLinenumbers;
  DWORD Characteristics;
} IMAGE_SECTION_HEADER,*PIMAGE_SECTION_HEADER;
typedef struct _IMAGE_EXPORT_DIRECTORY {
  DWORD Characteristics;
  DWORD TimeDateStamp;
  WORD MajorVersion;
  WORD MinorVersion;
  DWORD Name;
  DWORD Base;
  DWORD NumberOfFunctions;
  DWORD NumberOfNames;
  DWORD AddressOfFunctions;
  DWORD AddressOfNames;
  DWORD AddressOfNameOrdinals;
} IMAGE_EXPORT_DIRECTORY,*PIMAGE_EXPORT_DIRECTORY;
typedef struct tagVS_FIXEDFILEINFO {
  DWORD dwSignature;
  DWORD dwStrucVersion;
  DWORD dwFileVersionMS;
  DWORD dwFileVersionLS;
  DWORD dwProductVersionMS;
  DWORD dwProductVersionLS;
  DWORD dwFileFlagsMask;
  DWORD dwFileFlags;
  DWORD dwFileOS;
  DWORD dwFileType;
  DWORD dwFileSubtype;
  DWORD dwFileDateMS;
  DWORD dwFileDateLS;
} VS_FIXEDFILEINFO;
#  pragma pack()
#endif


// MinGW does not implement the unicode CRT startup functions
#if (defined(_UNICODE) && defined(_WIN32)) && defined(__MINGW32__)
#  define NSIS_ENTRYPOINT_TMAIN \
    int _tmain(int argc,WCHAR**argv); \
    EXTERN_C int main(int ac,char**cav) { \
      WCHAR**av=CommandLineToArgvW(GetCommandLineW(),&ac); \
      if (!av) { \
        _tprintf(_T("wmain: Error %u\n"),ac = GetLastError()); \
        return ac; \
      } \
      ac = _tmain(ac,av); \
      /*LEAK: LocalFree(av);*/ \
      return ac; \
    }
#  define NSIS_ENTRYPOINT_SIMPLEGUI \
     int WINAPI _tWinMain(HINSTANCE hI,HINSTANCE hOld,LPTSTR cl,int sc); \
     EXTERN_C int WINAPI WinMain(HINSTANCE hI,HINSTANCE hOld,char*cl,int sc) \
     {return _tWinMain(hI,0,0,sc);}
#  ifdef __cplusplus
#    define NSIS_ENTRYPOINT_GUINOCRT \
       EXTERN_C void NSISWinMainNOCRT(); \
       int WINAPI WinMain(HINSTANCE hI,HINSTANCE hOld,char*cl,int sc) \
       {NSISWinMainNOCRT();return 0;}
#  endif
#endif
#ifndef NSIS_ENTRYPOINT_TMAIN
#  define NSIS_ENTRYPOINT_TMAIN
#endif
#ifndef NSIS_ENTRYPOINT_SIMPLEGUI // _tWinMain with valid hInstance, calls ExitProcess
#  define NSIS_ENTRYPOINT_SIMPLEGUI
#endif
#ifndef NSIS_ENTRYPOINT_GUINOCRT
#  define NSIS_ENTRYPOINT_GUINOCRT
#endif


#if defined(__clang__) && defined(__cplusplus) && __cplusplus < 201103L
#  define NSIS_CXX_THROWSPEC(throwspec) throw(throwspec) // Use exception specifications to avoid operator new missing-exception-spec warning
#else
#  define NSIS_CXX_THROWSPEC(ignoredthrowspec) // Ignore c++ exception specifications
#endif
#if defined(__cplusplus) && __cplusplus >= 201103L
#  define NSIS_CXX_NOEXCEPT() noexcept(true)
#else
#  define NSIS_CXX_NOEXCEPT() throw() // Can't specialize __declspec(nothrow) because MSVC requires it before the function name
#endif
#if defined(_MSC_VER) && _MSC_VER <= 1200
#  define NSIS_CXX_TYPENAME // VC6 can't handle typename in some places but GCC requires it
#else
#  define NSIS_CXX_TYPENAME typename
#endif
#define BUGBUG64TRUNCATE(cast,xpr) ( (cast) (xpr) )

/*
_tprintf on Windows/MSVCRT treats %s as TCHAR* and on POSIX %s is always char*!
Always use our NPRI* (NsisPRInt*[Narrow|Wide]) defines in format strings when calling 
functions from tchar.h (Similar to the way <inttypes.h> works)

Example: _tprintf(_T("%") NPRIs _T(" %") NPRIws _T("\n"), _T("Hello"), L"World");
*/
#ifdef _WIN32
#  define NPRIs _T("s")
#  define NPRIns _T("hs")
#  define NPRIws _T("ls") // ws also works, not sure which is most compatible
#  ifndef _WIN64
#    define NPRIp _T(".8x")
#    define NPRIpN ".8x"
#  endif
#else  // !_WIN32
#  define NPRIns _T("s")
#  define NPRIws _T("ls")
#  ifdef _UNICODE
#    define NPRIs _T("ls")
#  else // !_UNICODE
#    define NPRIs _T("s")
#  endif // ~_UNICODE
#endif // ~_WIN32
#ifndef NPRIp
#  define NPRIp _T("p")
#  define NPRIpN "p"
#endif


// Disable deprecated warnings (Windows SDK for Windows 8.1)
#ifdef _MSC_VER
#if _MSC_VER >= 1500
FORCEINLINE DWORD NoDepr_GetVersion() { __pragma(warning(push))__pragma(warning(disable:4996)) DWORD r = GetVersion(); __pragma(warning(pop)) return r; }
#define GetVersion NoDepr_GetVersion
FORCEINLINE BOOL NoDepr_GetVersionExA(OSVERSIONINFOA*p) { __pragma(warning(push))__pragma(warning(disable:4996)) BOOL r = GetVersionExA(p); __pragma(warning(pop)) return r; }
#define GetVersionExA NoDepr_GetVersionExA
FORCEINLINE BOOL NoDepr_GetVersionExW(OSVERSIONINFOW*p) { __pragma(warning(push))__pragma(warning(disable:4996)) BOOL r = GetVersionExW(p); __pragma(warning(pop)) return r; }
#define GetVersionExW NoDepr_GetVersionExW
#endif //~ _MSC_VER >= 1500
#endif //~ _MSC_VER


#ifdef __cplusplus
namespace STL
{
  template<class M> struct mapped_type { typedef typename M::value_type::second_type type; }; // VC6 uses referent_type and not mapped_type
}
#endif //~ __cplusplus

#endif // EOF
