#ifndef ___PLATFORM__H___
#define ___PLATFORM__H___

// includes

#ifdef _WIN32
#  ifndef _WIN32_IE
#    define _WIN32_IE 0x0400
#  endif
#  include <Windows.h>
#  include <commctrl.h>
#else
#  define WORD unsigned short
#  define DWORD unsigned long
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

// Added by Dave Laundon 19th August 2002
// For all internal functions, use of stdcall calling convention moves the
// responsibility for tidying the stack to callee from caller, reducing the code
// involved considerably.  Gives an instant saving of 0.5K.
// NB - the zlib and bzip2 portions have been given the same treatment, but with
// project compiler-options settings and/or project-wide defines.
// NB - safer for NSIS's routines to be defined explicitly to avoid problems
// calling DLL functions.
#if defined(_WIN32) && ((_MSC_VER >= 800) || defined(_STDCALL_SUPPORTED))
#  define NSISCALL  __stdcall   // Ordinary functions
#  define NSISCALLV __cdecl     // Variable-argument-list functions
#else
#  ifdef __GNUC__
#    define NSISCALL  __attribute__((__stdcall__))   // Ordinary functions
#    define NSISCALLV __attribute__((__cdecl__))     // Variable-argument-list functions
#  else
#    define NSISCALL
#    define NSISCALLV
#  endif
#endif

// defines

#ifndef FOF_NOERRORUI
#define FOF_NOERRORUI 0x0400
#endif

#ifndef DS_SHELLFONT
#define DS_SHELLFONT (DS_SETFONT | DS_FIXEDSYS)
#endif

#ifndef ULONG_PTR
#define ULONG_PTR DWORD
#endif

#ifndef IS_INTRESOURCE
#define IS_INTRESOURCE(_r) (((ULONG_PTR)(_r) >> 16) == 0)
#endif

#ifndef IDC_HAND
#define IDC_HAND MAKEINTRESOURCE(32649)
#endif

#ifndef BIF_NEWDIALOGSTYLE
#define BIF_NEWDIALOGSTYLE 0x0040
#endif

#ifndef TVITEM
#define TVITEM TV_ITEM
#endif

#ifndef TVM_SETITEMHEIGHT
#define TVM_SETITEMHEIGHT (TV_FIRST + 27)
#endif

#ifndef TVM_GETITEMHEIGHT
#define TVM_GETITEMHEIGHT (TV_FIRST + 28)
#endif

#ifndef LVS_EX_LABELTIP
#define LVS_EX_LABELTIP 0x00004000
#endif

#ifdef __GNUC__  // ((DWORD)-1) may cause parsing errors with MinGW
#  ifdef INVALID_FILE_ATTRIBUTES	// updated win32api may also set as (DWORD)-1
#    undef INVALID_FILE_ATTRIBUTES
#  endif
#  define INVALID_FILE_ATTRIBUTES ((unsigned long)-1)
#else
#  ifndef INVALID_FILE_ATTRIBUTES
#    define INVALID_FILE_ATTRIBUTES ((DWORD)-1)
#  endif
#endif

#ifndef CSIDL_FLAG_CREATE
#define CSIDL_FLAG_CREATE 0x8000
#endif

#ifndef CSIDL_WINDOWS
#define CSIDL_WINDOWS 0x0024
#endif

#ifndef CSIDL_SYSTEM
#define CSIDL_SYSTEM 0x0025
#endif

#ifndef CSIDL_PROGRAM_FILES
#define CSIDL_PROGRAM_FILES 0x0026
#endif

#ifndef CSIDL_PROGRAM_FILES_COMMON
#define CSIDL_PROGRAM_FILES_COMMON 0x002b
#endif

#ifndef CSIDL_COMMON_DOCUMENTS
#define CSIDL_COMMON_DOCUMENTS 0x002e
#endif

#ifndef CSIDL_RESOURCES
#define CSIDL_RESOURCES 0x0038
#endif

#ifndef CSIDL_RESOURCES_LOCALIZED
#define CSIDL_RESOURCES_LOCALIZED 0x0039
#endif

#ifndef CSIDL_COMMON_ADMINTOOLS
#define CSIDL_COMMON_ADMINTOOLS 0x002f
#endif

#ifndef CSIDL_ADMINTOOLS
#define CSIDL_ADMINTOOLS 0x0030
#endif

#ifndef CSIDL_MYPICTURES
#define CSIDL_MYPICTURES 0x0027
#endif

#ifndef CSIDL_COMMON_PICTURES
#define CSIDL_COMMON_PICTURES 0x0036
#endif

#ifndef CSIDL_MYMUSIC
#define CSIDL_MYMUSIC 0x000d
#endif

#ifndef CSIDL_COMMON_MUSIC
#define CSIDL_COMMON_MUSIC 0x0035
#endif

#ifndef CSIDL_MYVIDEO
#define CSIDL_MYVIDEO 0x000e
#endif

#ifndef CSIDL_COMMON_VIDEO
#define CSIDL_COMMON_VIDEO 0x0037
#endif

#ifndef CSIDL_CDBURN_AREA
#define CSIDL_CDBURN_AREA 0x003b
#endif

#ifndef CSIDL_TEMPLATES
#define CSIDL_TEMPLATES 0x0015
#endif

#ifndef CSIDL_COMMON_TEMPLATES
#define CSIDL_COMMON_TEMPLATES 0x002d
#endif

#ifndef CSIDL_PROFILE
#define CSIDL_PROFILE 0x0028
#endif

#ifndef CSIDL_COMMON_APPDATA
#define CSIDL_COMMON_APPDATA 0x0023
#endif

#endif
