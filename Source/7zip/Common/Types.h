/*
 * Types.h
 * 
 * This file is a part of LZMA compression module for NSIS.
 * 
 * Original LZMA SDK Copyright (C) 1999-2006 Igor Pavlov
 * Modifications Copyright (C) 2003-2006 Amir Szekely <kichik@netvision.net.il>
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

#ifndef _7ZIP_BYTE_DEFINED
#define _7ZIP_BYTE_DEFINED
typedef unsigned char Byte;
#endif 

#ifndef _7ZIP_INT16_DEFINED
#define _7ZIP_INT16_DEFINED
typedef short Int16;
#endif 

#ifndef _7ZIP_UINT16_DEFINED
#define _7ZIP_UINT16_DEFINED
typedef unsigned short UInt16;
#endif 

#ifndef _7ZIP_INT32_DEFINED
#define _7ZIP_INT32_DEFINED
typedef int Int32;
#endif 

#ifndef _7ZIP_UINT32_DEFINED
#define _7ZIP_UINT32_DEFINED
typedef unsigned int UInt32;
#endif 

#ifdef _MSC_VER

#ifndef _7ZIP_INT64_DEFINED
#define _7ZIP_INT64_DEFINED
typedef __int64 Int64;
#endif 

#ifndef _7ZIP_UINT64_DEFINED
#define _7ZIP_UINT64_DEFINED
typedef unsigned __int64 UInt64;
#endif 

#else

#ifndef _7ZIP_INT64_DEFINED
#define _7ZIP_INT64_DEFINED
typedef long long int Int64;
#endif 

#ifndef _7ZIP_UINT64_DEFINED
#define _7ZIP_UINT64_DEFINED
typedef unsigned long long int UInt64;
#endif 

#endif

#endif
