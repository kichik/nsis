/* inftrees.h -- header to use inftrees.c
 * Copyright (C) 1995-1998 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h 
 */


extern int inflate_trees_bits OF((
    uIntf *,
    uIntf *,
    inflate_huft * FAR *,
    inflate_huft *));

extern int inflate_trees_dynamic OF((
    uInt,
    uInt,
    uIntf *,
    uIntf *,
    uIntf *,
    inflate_huft * FAR *,
    inflate_huft * FAR *,
    inflate_huft *));

extern int inflate_trees_fixed OF((
    uIntf *,
    uIntf *,
    inflate_huft * FAR *,
    inflate_huft * FAR *)); 
