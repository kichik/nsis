/*
 * crc32.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2006 Nullsoft and Contributors
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

#ifndef ___CRC32__H___
#define ___CRC32__H___

typedef UINT32 crc32_t;

#ifdef __cplusplus
extern "C"
#endif
crc32_t NSISCALL CRC32(crc32_t crc, const unsigned char *buf, unsigned int len);

#endif//!___CRC32__H___
