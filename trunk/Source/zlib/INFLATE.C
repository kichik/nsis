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
#include "inftrees.h"
#include "infcodes.h"
#include "infutil.h"



int ZEXPORT inflateReset(z_streamp z)
{
  if (z == Z_NULL || z->state == Z_NULL)
    return Z_STREAM_ERROR;
  z->total_in = z->total_out = 0;
  inflate_blocks_reset(&z->state->blocks, z, Z_NULL);
  return Z_OK;
}

static struct internal_state __mstate;

int ZEXPORT inflateInit(z_streamp z)
{
  void inflate_blocks_init(z_streamp z,struct inflate_blocks_state *s);

  z->state=&__mstate;
//  if ((z->state =
  //  (struct internal_state FAR *) ZALLOC(z,1,sizeof(struct internal_state))) == Z_NULL)
    //return Z_MEM_ERROR;

  inflate_blocks_init(z,&z->state->blocks);

  return Z_OK;
}


int ZEXPORT inflate(z_streamp z)
{
  return inflate_blocks(&z->state->blocks, z, Z_OK);
}

#endif
