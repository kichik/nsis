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
 *
 * Reviewed for Unicode support by Jim Park -- 07/31/2007
 */

#include "Platform.h"

WCHAR *wcsdup_fromansi(const char* s, unsigned int codepage = CP_ACP);
#ifdef _UNICODE
#define wcsdup_fromTchar(s, codepage) _wcsdup(s)      // codepage is not used in this mode
#else
#define wcsdup_fromTchar(s, codepage) wcsdup_fromansi(s, codepage)
#endif

#if 0 // Needed by some RTL missing wchar string functions ?
WCHAR *wcscpy(WCHAR *ws1, const WCHAR *ws2);
WCHAR *wcsncpy(WCHAR *ws1, const WCHAR *ws2, size_t n);
size_t wcslen(const WCHAR *ws);
WCHAR *_wcsdup(const WCHAR *ws);
int wcscmp(const WCHAR *ws1, const WCHAR *ws2);
#endif
