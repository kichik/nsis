/*
 * winchar.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2013 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef INC_NSIS_WINCHAR
#define INC_NSIS_WINCHAR
#include "Platform.h"

#define MAKEINTRESOURCEWINW(i) ( (WINWCHAR*) MAKEINTRESOURCEW(i) )

int WinWStrICmpASCII(const WINWCHAR *a, const char *b);
int WinWStrNICmpASCII(const WINWCHAR *a, const char *b, size_t n);
#ifdef _WIN32
inline size_t WinWStrLen(const WINWCHAR *s) { return wcslen((wchar_t*)s); }
inline WINWCHAR* WinWStrCpy(WINWCHAR *d, const WINWCHAR *s) { return (WINWCHAR*)wcscpy((wchar_t*)d, (wchar_t*)s); }
inline WINWCHAR* WinWStrNCpy(WINWCHAR *d, const WINWCHAR *s, size_t n) { return (WINWCHAR*)wcsncpy((wchar_t*)d, (wchar_t*)s, n); }
inline int WinWStrCmp(const WINWCHAR *a, const WINWCHAR *b) { return wcscmp((wchar_t*)a, (wchar_t*)b); }
inline WINWCHAR* WinWStrDupFromWinWStr(const WINWCHAR *s) { return (WINWCHAR*)wcsdup((wchar_t*)s); }
inline WINWCHAR* WinWStrDupFromTChar(const wchar_t *s) { return WinWStrDupFromWinWStr((WINWCHAR*)s); }
inline int WinWStrToInt(const WINWCHAR *s) { return _wtoi((wchar_t*)s); }
#else // !_WIN32
size_t WinWStrLen(const WINWCHAR *s);
WINWCHAR* WinWStrCpy(WINWCHAR *d, const WINWCHAR *s);
WINWCHAR* WinWStrNCpy(WINWCHAR *d, const WINWCHAR *s, size_t n);
int WinWStrCmp(const WINWCHAR *a, const WINWCHAR *b);
WINWCHAR* WinWStrDupFromWinWStr(const WINWCHAR *s);
WINWCHAR* WinWStrDupFromTChar(const TCHAR *s);
int WinWStrToInt(const WINWCHAR *s);
#endif // ~_WIN32

#ifdef _UNICODE
inline WINWCHAR* WinWStrDupFromTChar(const TCHAR *s, unsigned int codepage) { return WinWStrDupFromTChar(s); }
#endif

#endif // ~INC_NSIS_WINCHAR
