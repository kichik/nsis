/*
 * winchar.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "Platform.h"

WCHAR *winchar_fromansi(const char* s, unsigned int codepage = CP_ACP);
char *winchar_toansi(const WCHAR* ws, unsigned int codepage = CP_ACP);
WCHAR *winchar_strcpy(WCHAR *ws1, const WCHAR *ws2);
WCHAR *winchar_strncpy(WCHAR *ws1, const WCHAR *ws2, size_t n);
size_t winchar_strlen(WCHAR *ws);
WCHAR *winchar_strdup(WCHAR *ws);
int winchar_strcmp(const WCHAR *ws1, const WCHAR *ws2);
int winchar_stoi(const WCHAR *ws);
