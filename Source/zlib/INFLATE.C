#include "../exehead/config.h"
#ifdef NSIS_COMPRESS_USE_ZLIB
/* inflate.c -- zlib interface to inflate modules
 * Copyright (C) 1995-1998 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h

 * this has been HEAVILY modified for NSIS use, and is no longer compatible
 * with the stock zlib.

 */

#include "zutil.h"
#include "infblock.h"

struct inflate_blocks_state { int dummy; }; /* for buggy compilers */


/* inflate private state */
struct internal_state {

  /* mode dependent information */
  union {
    uInt method;        /* if FLAGS, method byte */
    struct {
      uLong was;                /* computed check value */
      uLong need;               /* stream check value */
    } check;            /* if CHECK, check values to compare */
    uInt marker;        /* if BAD, inflateSync's marker bytes count */
  } sub;        /* submode */

  inflate_blocks_statef blocks;            /* current inflate_blocks state */
};


int ZEXPORT inflateReset(z_streamp z)
{
  if (z == Z_NULL || z->state == Z_NULL)
    return Z_STREAM_ERROR;
  z->total_in = z->total_out = 0;
  inflate_blocks_reset(&z->state->blocks, z, Z_NULL);
  return Z_OK;
}


int ZEXPORT inflateInit(z_streamp z)
{
  int inflate_blocks_getssize(void);
  void inflate_blocks_init(z_streamp z,inflate_blocks_statef *s);

  if ((z->state =
    (struct internal_state FAR *) ZALLOC(z,1,sizeof(struct internal_state)+inflate_blocks_getssize())) == Z_NULL)
    return Z_MEM_ERROR;

  inflate_blocks_init(z,&z->state->blocks);

  return Z_OK;
}


int ZEXPORT inflate(z_streamp z)
{
  return inflate_blocks(&z->state->blocks, z, Z_OK);
}

#endif
