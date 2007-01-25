/*
 * This file is a part of the zlib compression module for NSIS.
 * 
 * Copyright and license information can be found below.
 * Modifications Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * The original zlib source code is available at
 * http://www.zlib.net/
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

/* zutil.h -- internal interface and configuration of the compression library
 * Copyright (C) 1995-1998 Jean-loup Gailly.
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

/* WARNING: this file should *not* be used by applications. It is
   part of the implementation of the compression library and is
   subject to change. Applications should only use zlib.h.
 */

/* @(#) $Id: ZUTIL.H,v 1.5 2007/01/13 17:28:23 kichik Exp $ */

#ifndef _Z_UTIL_H
#define _Z_UTIL_H

#include "../Platform.h"
#include "ZLIB.H"

#ifndef local
#  define local static
#endif

typedef unsigned char  uch;
typedef uch FAR uchf;
typedef unsigned short ush;
typedef ush FAR ushf;
typedef unsigned long  ulg;

#ifndef DEF_WBITS
#  define DEF_WBITS MAX_WBITS
#endif
/* default windowBits for decompression. MAX_WBITS is for compression only */

#define DEF_MEM_LEVEL  MAX_MEM_LEVEL

#define STORED_BLOCK 0
#define STATIC_TREES 1
#define DYN_TREES    2
/* The three kinds of block type */

#define MIN_MATCH  3
#define MAX_MATCH  258
/* The minimum and maximum match lengths */

#define PRESET_DICT 0x20 /* preset dictionary flag in zlib header */


#ifdef EXEHEAD
#  ifdef _WIN32
#    include "../exehead/util.h"
#    define zmemcpy mini_memcpy
#  else
#    define zmemcpy memcpy
#  endif
#else
#  define zmemcpy memcpy
#  define zmemzero(a,b) memset(a,0,b)
#endif

#define Assert(cond,msg)
#define Trace(x)
#define Tracev(x)
#define Tracevv(x)
#define Tracec(c,x)
#define Tracecv(c,x)

#define ZALLOC(strm, items, size) malloc((items)*(size))
#define ZFREE(strm, addr)  { if (addr) free(addr); }
#define TRY_FREE(s, p) { ZFREE(s, p); }
#define ERR_RETURN(strm,err) return (err)

#endif /* _Z_UTIL_H */
