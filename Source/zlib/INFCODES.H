/* infcodes.h -- header to use infcodes.c
 * Copyright (C) 1995-1998 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h 
 */

extern void inflate_codes_new OF((
    inflate_codes_statef *, uInt, uInt,
    inflate_huft *, inflate_huft *));

extern int inflate_codes OF((
    z_streamp ,
    int));