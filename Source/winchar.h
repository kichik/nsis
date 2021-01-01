/*
 * winchar.h
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
inline WINWCHAR* WinWStrDupFromWC(const wchar_t *s) { return WinWStrDupFromWinWStr((WINWCHAR*)s); }
inline int WinWStrToInt(const WINWCHAR *s) { return _wtoi((wchar_t*)s); }
#else // !_WIN32
size_t WinWStrLen(const WINWCHAR *s);
WINWCHAR* WinWStrCpy(WINWCHAR *d, const WINWCHAR *s);
WINWCHAR* WinWStrNCpy(WINWCHAR *d, const WINWCHAR *s, size_t n);
int WinWStrCmp(const WINWCHAR *a, const WINWCHAR *b);
WINWCHAR* WinWStrDupFromWinWStr(const WINWCHAR *s);
WINWCHAR* WinWStrDupFromWC(const wchar_t *s);
int WinWStrToInt(const WINWCHAR *s);
#endif // ~_WIN32

WINWCHAR* WinWStrDupFromChar(const char *s, unsigned int cp);
inline WINWCHAR* WinWStrDupFromChar(const char *s) { return WinWStrDupFromChar(s, CP_ACP); }

#ifdef _UNICODE
inline WINWCHAR* WinWStrDupFromTChar(const wchar_t *s) { return WinWStrDupFromWC(s); }
inline WINWCHAR* WinWStrDupFromTChar(const wchar_t *s, unsigned int codepage) { return WinWStrDupFromTChar(s); }
#else
inline WINWCHAR* WinWStrDupFromTChar(const char *s, unsigned int cp) { return WinWStrDupFromChar(s, cp); }
inline WINWCHAR* WinWStrDupFromTChar(const char *s) { return WinWStrDupFromChar(s, CP_ACP); }
#endif

#endif // ~INC_NSIS_WINCHAR
