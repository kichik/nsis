/*
 * util.h
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
 * Unicode support by Jim Park -- 08/10/2007
 */

#ifndef _UTIL_H_
#define _UTIL_H_

#include "tstring.h" // for std::string

#include "boost/scoped_ptr.hpp" // for boost::scoped_ptr
#include "ResourceEditor.h"

#ifndef _WIN32
#  include <iconv.h>
#  include <stdio.h>
#  include <unistd.h>
#endif


// these are the standard pause-before-quit stuff.
extern int g_dopause;
extern void dopause(void);

extern double my_wtof(const wchar_t *str);
extern unsigned int my_strncpy(TCHAR*Dest, const TCHAR*Src, unsigned int cchMax);

// Adds the bitmap in filename using resource editor re as id id.
// If width or height are specified it will also make sure the bitmap is in that size
int update_bitmap(CResourceEditor* re, WORD id, const TCHAR* filename, int width=0, int height=0, int maxbpp=0);

size_t my_strftime(TCHAR *s, size_t max, const TCHAR  *fmt, const struct tm *tm);

bool GetDLLVersion(const tstring& filepath, DWORD& high, DWORD& low);

tstring get_full_path(const tstring& path);
tstring get_dir_name(const tstring& path);
tstring get_file_name(const tstring& path);
tstring get_executable_dir(const TCHAR *argv0);
tstring remove_file_extension(const tstring& path);
tstring lowercase(const tstring&);

tstring get_string_prefix(const tstring& str, const tstring& separator);
tstring get_string_suffix(const tstring& str, const tstring& separator);

int sane_system(const TCHAR *command);

void PrintColorFmtMsg(unsigned int type, const TCHAR *fmtstr, va_list args);
void FlushOutputAndResetPrintColor();
#ifdef _WIN32
#define ResetPrintColor() FlushOutputAndResetPrintColor() // For reset ONLY use PrintColorFmtMsg(0,NULL ...
#define SetPrintColorWARN() PrintColorFmtMsg(1|0x10, NULL, (va_list)NULL)
#define SetPrintColorERR() PrintColorFmtMsg(2|0x10, NULL, (va_list)NULL)
#else
#define ResetPrintColor()
#define SetPrintColorWARN()
#define SetPrintColorERR()
#endif
inline void PrintColorFmtMsg_WARN(const TCHAR *fmtstr, ...)
{
  va_list val;
  va_start(val,fmtstr);
  PrintColorFmtMsg(1, fmtstr, val);
  va_end(val);
}
inline void PrintColorFmtMsg_ERR(const TCHAR *fmtstr, ...)
{
  va_list val;
  va_start(val,fmtstr);
  PrintColorFmtMsg(2, fmtstr, val);
  va_end(val);
}



#ifndef _WIN32
TCHAR *CharPrev(const TCHAR *s, const TCHAR *p);
char *CharNextA(const char *s);
WCHAR *CharNextW(const WCHAR *s);
char *CharNextExA(WORD codepage, const char *s, int flags);
int wsprintf(TCHAR *s, const TCHAR *format, ...);
int WideCharToMultiByte(UINT CodePage, DWORD dwFlags, LPCWSTR lpWideCharStr,
    int cchWideChar, LPSTR lpMultiByteStr, int cbMultiByte, LPCSTR lpDefaultChar,
    LPBOOL lpUsedDefaultChar);
int MultiByteToWideChar(UINT CodePage, DWORD dwFlags, LPCSTR lpMultiByteStr,
    int cbMultiByte, LPWSTR lpWideCharStr, int cchWideChar);
BOOL IsValidCodePage(UINT CodePage);
#ifdef _UNICODE
#define CharNext CharNextW
#else
#define CharNext CharNextA
#endif

TCHAR *my_convert(const TCHAR *path);
void my_convert_free(TCHAR *converted_path);
int my_open(const TCHAR *pathname, int flags);

#define OPEN(a, b) my_open(a, b)

#else

#define my_convert(x) (x)
#define my_convert_free(x)

#define OPEN(a, b) _topen(a, b)

#endif

FILE* my_fopen(const TCHAR *path, const char *mode);
#define FOPEN(a, b) my_fopen((a), (b))

// round a value up to be a multiple of 512
// assumption: T is an int type
template <class T>
inline T align_to_512(const T x) {
  return (x+511) & ~511;
}

// ================
// ResourceManagers
// ================

// When a ResourceManager instance goes out of scope, it will run
// _FREE_RESOURCE on the resource.
// Example use:
// int fd = open(..);
// assert(fd != -1);
// MANAGE_WITH(fd, close);

class BaseResourceManager {
protected:
	BaseResourceManager() {}
public:
	virtual ~BaseResourceManager() {}
};

template <typename _RESOURCE, typename _FREE_RESOURCE>
class ResourceManager : public BaseResourceManager {
public:
  ResourceManager(_RESOURCE& resource) : m_resource(resource) {}
  virtual ~ResourceManager() { m_free_resource(m_resource); };
private: // members
  _RESOURCE& m_resource;
  _FREE_RESOURCE m_free_resource;
private: // don't copy instances
  ResourceManager(const ResourceManager&);
  void operator=(const ResourceManager&);
};

#define RM_MANGLE_FREEFUNC(freefunc) \
	__free_with_##freefunc

#define RM_DEFINE_FREEFUNC(freefunc) \
struct RM_MANGLE_FREEFUNC(freefunc) { \
  template <typename T> void operator()(T& x) { freefunc(x); } \
}

typedef boost::scoped_ptr<BaseResourceManager> ResourceManagerPtr;

template<typename _FREE_RESOURCE, typename _RESOURCE>
void createResourceManager(_RESOURCE& resource, ResourceManagerPtr& ptr) {
	ptr.reset(new ResourceManager<_RESOURCE, _FREE_RESOURCE>(resource));
}

#define RM_MANGLE_RESOURCE(resource) resource##_autoManager
#define MANAGE_WITH(resource, freefunc) \
	ResourceManagerPtr RM_MANGLE_RESOURCE(resource); \
		createResourceManager<RM_MANGLE_FREEFUNC(freefunc)>( \
      resource, RM_MANGLE_RESOURCE(resource))

// Add more resource-freeing functions here when you need them
RM_DEFINE_FREEFUNC(close);
RM_DEFINE_FREEFUNC(CloseHandle);
RM_DEFINE_FREEFUNC(fclose);
RM_DEFINE_FREEFUNC(free);
RM_DEFINE_FREEFUNC(my_convert_free);

// Auto path conversion
#ifndef _WIN32
#  define PATH_CONVERT(x) x = my_convert(x); MANAGE_WITH(x, my_convert_free);
#else
#  define PATH_CONVERT(x)
#endif

// Platform detection
bool Platform_SupportsUTF8Conversion();

#endif //_UTIL_H_
