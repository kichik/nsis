/*
 * BinInterop.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2017-2018 Anders Kjersem
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 */

#ifndef NSIS_BININTEROP_H
#define NSIS_BININTEROP_H

#include "Platform.h"
#include "tchar.h"
#include <stdio.h> // FILE*

FILE* MSTLB_fopen(const TCHAR*filepath, size_t*pResId = 0);
bool GetTLBVersion(const TCHAR *filepath, DWORD &high, DWORD &low);

bool GetDLLVersion(const TCHAR *filepath, DWORD &high, DWORD &low);

#endif //~ NSIS_BININTEROP_H
