/* inftrees.h -- header to use inftrees.c
 * Copyright (C) 1995-1998 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h 
 */


typedef struct inflate_huft_s FAR inflate_huft;

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

extern int inflate_trees_bits OF((
    uIntf *,
    uIntf *,
    inflate_huft * FAR *,
    inflate_huft *,
    z_streamp));

extern int inflate_trees_dynamic OF((
    uInt,
    uInt,
    uIntf *,
    uIntf *,
    uIntf *,
    inflate_huft * FAR *,
    inflate_huft * FAR *,
    inflate_huft *,
    z_streamp));

extern int inflate_trees_fixed OF((
    uIntf *,
    uIntf *,
    inflate_huft * FAR *,
    inflate_huft * FAR *,
    z_streamp)); 
