/*
 * Types.h
 * 
 * This file is a part of LZMA compression module for NSIS.
 * 
 * Original LZMA SDK Copyright (C) 1999-2006 Igor Pavlov
 * Modifications Copyright (C) 2003-2021 Amir Szekely <kichik@netvision.net.il>
 * 
 * Licensed under the Common Public License version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef __COMMON_TYPES_H
#define __COMMON_TYPES_H

#ifdef _WIN32
#include <windows.h>

#ifndef _7ZIP_BYTE_DEFINED
#define _7ZIP_BYTE_DEFINED
typedef UINT8 Byte;
#endif 

#ifndef _7ZIP_INT16_DEFINED
#define _7ZIP_INT16_DEFINED
typedef INT16 Int16;
#endif 

#ifndef _7ZIP_UINT16_DEFINED
#define _7ZIP_UINT16_DEFINED
typedef UINT16 UInt16;
#endif 

#ifndef _7ZIP_INT32_DEFINED
#define _7ZIP_INT32_DEFINED
typedef INT32 Int32;
#endif 

#ifndef _7ZIP_UINT32_DEFINED
#define _7ZIP_UINT32_DEFINED
typedef UINT32 UInt32;
#endif 

#ifndef _7ZIP_INT64_DEFINED
#define _7ZIP_INT64_DEFINED
typedef INT64 Int64;
#endif 

#ifndef _7ZIP_UINT64_DEFINED
#define _7ZIP_UINT64_DEFINED
typedef UINT64 UInt64;
#endif 

#else // !_WIN32

#include <stdint.h>

#ifndef _7ZIP_BYTE_DEFINED
#define _7ZIP_BYTE_DEFINED
typedef uint8_t Byte;
#endif 

#ifndef _7ZIP_INT16_DEFINED
#define _7ZIP_INT16_DEFINED
typedef int16_t Int16;
#endif 

#ifndef _7ZIP_UINT16_DEFINED
#define _7ZIP_UINT16_DEFINED
typedef uint16_t UInt16;
#endif 

#ifndef _7ZIP_INT32_DEFINED
#define _7ZIP_INT32_DEFINED
typedef int32_t Int32;
#endif 

#ifndef _7ZIP_UINT32_DEFINED
#define _7ZIP_UINT32_DEFINED
typedef uint32_t UInt32;
#endif 

#ifndef _7ZIP_INT64_DEFINED
#define _7ZIP_INT64_DEFINED
typedef int64_t Int64;
#endif 

#ifndef _7ZIP_UINT64_DEFINED
#define _7ZIP_UINT64_DEFINED
typedef uint64_t UInt64;
#endif 

#endif

#endif
