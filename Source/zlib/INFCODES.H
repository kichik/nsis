/* infcodes.h -- header to use infcodes.c
 * Copyright (C) 1995-1998 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h 
 */

struct inflate_codes_state;
typedef struct inflate_codes_state FAR inflate_codes_statef;

extern inflate_codes_statef *inflate_codes_new OF((
    uInt, uInt,
    inflate_huft *, inflate_huft *));

extern int inflate_codes OF((
    z_streamp ,
    int));

#define inflate_codes_free(z,c) ZFREE(z,c)

