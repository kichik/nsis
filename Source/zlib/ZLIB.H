/*
  Note: this version of zlib has been hacked up quite a bit in order to reduce
  the size of the EXE header and to remove what we didn't need.
  For the complete real thing, go to:

  http://www.info-zip.org/pub/infozip/zlib/

  Peace,

  -Justin
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

#include "zconf.h"

#ifdef __cplusplus
extern "C" {
#endif


struct internal_state;

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
    struct internal_state FAR *state; /* not visible by applications */

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
#define Z_STREAM_ERROR (-2)
#define Z_DATA_ERROR   (-3)
#define Z_MEM_ERROR    (-4)
#define Z_BUF_ERROR    (-5)
#define Z_VERSION_ERROR (-6)
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
ZEXTERN int ZEXPORT inflateInit OF((z_streamp strm));

int ZEXPORT inflate(z_streamp z);
ZEXTERN int ZEXPORT inflateEnd OF((z_streamp strm));
ZEXTERN int ZEXPORT deflateInit2 OF((z_streamp strm,
                                     int  level,
                                     int  method,
                                     int  windowBits,
                                     int  memLevel,
                                     int  strategy));


ZEXTERN int ZEXPORT deflateReset OF((z_streamp strm));
int ZEXPORT inflateReset(z_streamp z);


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
ZEXTERN int ZEXPORT inflateInit2_ OF((z_streamp strm, int  windowBits,
                                      const char *version, int stream_size));
#define deflateInit(strm, level) \
        deflateInit_((strm), (level),       "", sizeof(z_stream))


#ifdef __cplusplus
}
#endif

#endif /* _ZLIB_H */
