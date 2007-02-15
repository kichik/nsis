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

/* zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.1.3, July 9th, 1998

  Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu


*/

#ifndef _ZLIB_H
#define _ZLIB_H

#include "ZCONF.H"
#include "ZUTIL.H"

#ifdef __cplusplus
extern "C" {
#endif


#ifdef EXEHEAD

typedef struct inflate_huft_s FAR inflate_huft;



typedef enum {        /* waiting for "i:"=input, "o:"=output, "x:"=nothing */
      CODES_START,    /* x: set up for LEN */
      CODES_LEN,      /* i: get length/literal/eob next */
      CODES_LENEXT,   /* i: getting length extra (have base) */
      CODES_DIST,     /* i: get distance next */
      CODES_DISTEXT,  /* i: getting distance extra */
      CODES_COPY,     /* o: copying bytes in window, waiting for space */
      CODES_LIT,      /* o: got literal, waiting for output space */
      CODES_WASH,     /* o: got eob, possibly still output waiting */
      //CODES_END,      /* x: got eob and all data flushed */
      //CODES_BADCODE,  /* x: got error */
      
      TYPE,     /* get type bits (3, including end bit) */
      LENS,     /* get lengths for stored */
      STORED,   /* processing stored block */
      TABLE,    /* get table lengths */
      BTREE,    /* get bit lengths tree for a dynamic block */
      DTREE,    /* get length, distance trees for a dynamic block */
      CODES,    /* processing fixed or dynamic block */
      DRY,      /* output remaining window bytes */
      DONE,     /* finished last block, done */
      BAD       /* got a data error--stuck here */
} inflate_mode;

/* inflate codes private state */
struct inflate_codes_state {

  /* mode */
  //inflate_mode mode;      /* current inflate_codes mode */

  /* mode dependent information */
  uInt len;
  union {
    struct {
      inflate_huft *tree;       /* pointer into tree */
      uInt need;                /* bits needed */
    } code;             /* if LEN or DIST, where in tree */
    uInt lit;           /* if LIT, literal */
    struct {
      uInt get;                 /* bits to get for extra */
      uInt dist;                /* distance back to copy from */
    } copy;             /* if EXT or COPY, where and how much */
  } sub;                /* submode */

  /* mode independent information */
  Byte lbits;           /* ltree bits decoded per branch */
  Byte dbits;           /* dtree bits decoder per branch */
  inflate_huft *ltree;          /* literal/length/eob tree */
  inflate_huft *dtree;          /* distance tree */

};

struct inflate_huft_s {
  union {
    struct {
      Byte Exop;        /* number of extra bits or operation */
      Byte Bits;        /* number of bits in this code or subcode */
    } what;
  } word;
  unsigned short base;            /* literal, length base, distance base,
                           or table offset */
};

#define MANY 1440

typedef struct inflate_codes_state inflate_codes_statef;

struct inflate_blocks_state {

  /* mode */
  inflate_mode  mode;    /* current inflate_block mode */

  /* mode dependent information */
  union {
    uInt left;          /* if STORED, bytes left to copy */
    struct {
      uInt table;               /* table lengths (14 bits) */
      uInt index;               /* index into blens (or border) */
      uIntf t_blens[258+31+31];             /* bit lengths of codes */
      uInt bb;                  /* bit length tree depth */
      inflate_huft *tb;         /* bit length decoding tree */
    } trees;            /* if DTREE, decoding info for trees */
    struct {
      inflate_codes_statef t_codes;
    } decode;           /* if CODES, current state */
  } sub;                /* submode */

  uInt last;            /* DRY if this block is the last block, TYPE otherwise */

  /* mode independent information */
  uInt bitk;            /* bits in bit buffer */
  uLong bitb;           /* bit buffer */
  inflate_huft hufts[MANY];  /* single malloc for tree space */
  Bytef window[1 << MAX_WBITS];        /* sliding window */
  Bytef *end;           /* one byte after sliding window */
  Bytef *read;          /* window read pointer */
  Bytef *write;         /* window write pointer */
  uLong check;          /* check on output */

};



#else
struct internal_state;
#endif

typedef struct z_stream_s {
    Bytef    *next_in;  /* next input byte */
    uInt     avail_in;  /* number of bytes available at next_in */
#ifndef EXEHEAD
    uLong    total_in;  /* total nb of input bytes read so far */
#endif

    Bytef    *next_out; /* next output byte should be put there */
    uInt     avail_out; /* remaining free space at next_out */
#ifndef EXEHEAD
    uLong    total_out; /* total nb of bytes output so far */
#endif

//    char     *msg;      /* last error message, NULL if no error */
    //struct internal_state FAR *state; /* not visible by applications */
#ifdef EXEHEAD
    struct inflate_blocks_state blocks;            /* current inflate_blocks state */
#else
    struct internal_state *state;
#endif

} z_stream;

typedef z_stream FAR *z_streamp;


#define Z_NO_FLUSH      0
#define Z_PARTIAL_FLUSH 1 /* will be removed, use Z_SYNC_FLUSH instead */
#define Z_SYNC_FLUSH    2
#define Z_FULL_FLUSH    3
#define Z_FINISH        4
/* Allowed flush values; see deflate() below for details */

#define Z_OK            0
#define Z_STREAM_END    1
#define Z_NEED_DICT     2
#define Z_ERRNO        (-1)

#ifndef EXEHEAD

#define Z_STREAM_ERROR (-2)
#define Z_DATA_ERROR   (-3)
#define Z_MEM_ERROR    (-4)
#define Z_BUF_ERROR    (-5)
#define Z_VERSION_ERROR (-6)

#else
// EXEHEAD doesn't need a specific return code, just < 0

#define Z_STREAM_ERROR  Z_ERRNO
#define Z_DATA_ERROR    Z_ERRNO
#define Z_MEM_ERROR     Z_ERRNO
#define Z_BUF_ERROR     Z_ERRNO
#define Z_VERSION_ERROR Z_ERRNO

#endif
/* Return codes for the compression/decompression functions. Negative
 * values are errors, positive values are used for special but normal events.
 */

#define Z_NO_COMPRESSION         0
#define Z_BEST_SPEED             1
#define Z_BEST_COMPRESSION       9
#define Z_DEFAULT_COMPRESSION  (-1)
/* compression levels */

#define Z_FILTERED            1
#define Z_HUFFMAN_ONLY        2
#define Z_DEFAULT_STRATEGY    0
/* compression strategy; see deflateInit2() below for details */

#define Z_BINARY   0
#define Z_ASCII    1
#define Z_UNKNOWN  2
/* Possible values of the data_type field */

#define Z_DEFLATED   8
/* The deflate compression method (the only one supported in this version) */

#define Z_NULL  0  /* for initializing zalloc, zfree, opaque */


ZEXTERN int ZEXPORT deflate OF((z_streamp strm, int flush));

ZEXTERN int ZEXPORT deflateEnd OF((z_streamp strm));


#define inflateInit(x) inflateReset(x)
int ZEXPORT inflate(z_streamp z);
ZEXTERN int ZEXPORT inflateEnd OF((z_streamp strm));
ZEXTERN int ZEXPORT deflateInit2 OF((z_streamp strm,
                                     int  level,
                                     int  method,
                                     int  windowBits,
                                     int  memLevel,
                                     int  strategy));


ZEXTERN int ZEXPORT deflateReset OF((z_streamp strm));

//void ZEXPORT inflateReset OF((
  //  z_streamp));
#define inflateReset(z) \
{ \
  (z)->blocks.mode = TYPE; \
  (z)->blocks.bitk = (z)->blocks.bitb = 0; \
  (z)->blocks.read = (z)->blocks.write = (z)->blocks.window; \
  (z)->blocks.end = (z)->blocks.window + (1 << DEF_WBITS); \
}

                        /* various hacks, don't look :) */

/* deflateInit and inflateInit are macros to allow checking the zlib version
 * and the compiler's view of z_stream:
 */
ZEXTERN int ZEXPORT deflateInit_ OF((z_streamp strm, int level,
                                     const char *version, int stream_size));
//ZEXTERN int ZEXPORT inflateInit_ OF((z_streamp strm,
  //                                   const char *version, int stream_size));
ZEXTERN int ZEXPORT deflateInit2_ OF((z_streamp strm, int  level, int  method,
                                      int windowBits, int memLevel,
                                      int strategy, const char *version,
                                      int stream_size));

#define deflateInit(strm, level) \
        deflateInit_((strm), (level),       "", sizeof(z_stream))


#ifdef __cplusplus
}
#endif

#endif /* _ZLIB_H */
