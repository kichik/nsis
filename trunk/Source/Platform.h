#ifndef ___PLATFORM__H___
#define ___PLATFORM__H___

// some definitions for non Win32 platforms were taken from MinGW's free Win32 library

// includes

#ifdef _WIN32
#  ifndef _WIN32_IE
#    define _WIN32_IE 0x0400
#  endif
#  include <windows.h>
#  include <commctrl.h>
#else
#  ifndef EXEHEAD
#    include <string.h>
#    include <stdlib.h>
#  endif
// basic types
typedef unsigned char BYTE, *PBYTE, *LPBYTE;
typedef unsigned short WORD, *LPWORD;
typedef unsigned long DWORD, *LPDWORD;
typedef short SHORT;
typedef unsigned short USHORT;
typedef unsigned int UINT;
typedef unsigned int UINT32;
typedef int INT;
typedef int INT32;
typedef long LONG;
typedef unsigned long ULONG;
typedef long long INT64, LARGE_INTEGER;
typedef unsigned long long UINT64, ULARGE_INTEGER;
typedef int BOOL;
typedef void VOID;
typedef void *LPVOID;
typedef char CHAR, *PCHAR, *LPCH, *PCH, *NPSTR, *LPSTR, *PSTR;
typedef unsigned char UCHAR;
typedef const char *LPCCH, *PCSTR, *LPCSTR;
typedef unsigned short WCHAR, *PWCHAR, *LPWCH, *PWCH, *NWPSTR, *LPWSTR, *PWSTR;
typedef const unsigned short *LPCWCH, *PCWCH, *LPCWSTR, *PCWSTR;
typedef unsigned int UINT_PTR;
// basic stuff
typedef unsigned long HANDLE;
typedef unsigned long HKEY;
// some gdi
typedef unsigned long COLORREF;
typedef unsigned long HBRUSH;
// bool
#  define FALSE 0
#  define TRUE 1
// more
typedef WORD LANGID;
#endif


// script path separator

#  define PATH_SEPARATOR_STR "\\"
#  define PATH_SEPARATOR_C '\\'

// system specific separator

#ifdef _WIN32
#  define PLATFORM_PATH_SEPARATOR_STR "\\"
#  define PLATFORM_PATH_SEPARATOR_C '\\'
#else
#  define PLATFORM_PATH_SEPARATOR_STR "/"
#  define PLATFORM_PATH_SEPARATOR_C '/'
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
#  if defined(__GNUC__) && defined(__i386__)
#    define NSISCALL  __attribute__((__stdcall__))   // Ordinary functions
#    define NSISCALLV __attribute__((__cdecl__))     // Variable-argument-list functions
#  else
#    define NSISCALL
#    define NSISCALLV
#  endif
#endif

// macros

#ifndef _WIN32
#  ifndef min
#    define min(x,y) ((x<y)?x:y)
#  endif
#  ifndef max
#    define max(x,y) ((x>y)?x:y)
#  endif
#  ifndef FIELD_OFFSET
#    define FIELD_OFFSET(t,f) ((LONG)&(((t*)0)->f))
#  endif
#  ifndef MAKEINTRESOURCE
#    define MAKEINTRESOURCE(i) (LPSTR)((DWORD)((WORD)(i)))
#  endif
#  ifndef IMAGE_FIRST_SECTION
#    define IMAGE_FIRST_SECTION(h) ((PIMAGE_SECTION_HEADER) ((DWORD)h+FIELD_OFFSET(IMAGE_NT_HEADERS,OptionalHeader)+((PIMAGE_NT_HEADERS)(h))->FileHeader.SizeOfOptionalHeader))
#  endif
#  ifndef RGB
#    define RGB(r,g,b) ((DWORD)(((BYTE)(r)|((WORD)(g)<<8))|(((DWORD)(BYTE)(b))<<16)))
#  endif
#  ifndef MAKELONG
#    define MAKELONG(a,b) ((LONG)(((WORD)(a))|(((DWORD)((WORD)(b)))<<16)))
#  endif
#endif
#ifndef IS_INTRESOURCE
#  define IS_INTRESOURCE(_r) (((ULONG_PTR)(_r) >> 16) == 0)
#endif

// functions

#ifndef _WIN32
#  define stricmp strcasecmp
#  define strcmpi strcasecmp
#  define strnicmp strncasecmp
#  define CopyMemory memcpy
#  define ZeroMemory(x, y) memset(x, 0, y)
#endif

// defines

#ifndef FOF_NOERRORUI
#  define FOF_NOERRORUI 0x0400
#endif

#ifndef ULONG_PTR
#  define ULONG_PTR DWORD
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
#    define SF_TEXT 1
#  endif
#  ifndef SF_RTF
#    define SF_RTF 2
#  endif
#endif

#ifdef __GNUC__
#  undef INVALID_FILE_ATTRIBUTES
#endif
#ifndef INVALID_FILE_ATTRIBUTES
#  define INVALID_FILE_ATTRIBUTES ((unsigned long) -1)
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

#ifndef CP_ACP
#  define CP_ACP 0
#endif

#ifndef COLOR_BTNFACE
#  define COLOR_BTNFACE 15
#endif
#ifndef COLOR_WINDOW
#  define COLOR_WINDOW 5
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
#  define RT_GROUP_ICON MAKEINTRESOURCE(14)
#endif
#ifndef RT_VERSION
#  define RT_VERSION MAKEINTRESOURCE(16)
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
#  define MB_SETFOREGROUND 0x10000
#  define MB_TOPMOST 0x40000
#  define MB_RIGHT 0x80000
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
#  define DS_SHELLFONT (DS_SETFONT | DS_FIXEDSYS)

#  define WS_EX_RIGHT 0x1000
#  define WS_EX_RIGHTSCROLLBAR 0
#  define WS_EX_RTLREADING 0x2000

#  define TVS_RTLREADING 64

#  define PBS_SMOOTH 1
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

// show modes

#ifndef SW_SHOWNORMAL
#  define SW_HIDE 0
#  define SW_SHOWNORMAL 1
#  define SW_SHOWMINIMIZED 2
#  define SW_SHOWMAXIMIZED 3
#  define SW_SHOWNOACTIVATE 4
#  define SW_SHOWMINNOACTIVE 7
#  define SW_SHOWNA 8
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

// registry

#ifndef REG_SZ
#  define REG_SZ 1
#  define REG_EXPAND_SZ 2
#  define REG_BINARY 3
#  define REG_DWORD 4
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
#  define IMAGE_DOS_SIGNATURE 0x5A4D
#  define IMAGE_NT_SIGNATURE 0x00004550
#  define IMAGE_FILE_DLL 8192
#  define IMAGE_DIRECTORY_ENTRY_EXPORT 0
#  define IMAGE_SIZEOF_SHORT_NAME 8
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
  CHAR lfFaceName[LF_FACESIZE];
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
typedef struct _IMAGE_OPTIONAL_HEADER {
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
} IMAGE_OPTIONAL_HEADER,*PIMAGE_OPTIONAL_HEADER;
typedef struct _IMAGE_NT_HEADERS {
  DWORD Signature;
  IMAGE_FILE_HEADER FileHeader;
  IMAGE_OPTIONAL_HEADER OptionalHeader;
} IMAGE_NT_HEADERS,*PIMAGE_NT_HEADERS;
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
#  pragma pack()
#endif

#endif
