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

int ZEXPORT inflateInit(z_streamp z)
{
  z->blocks.end = z->blocks.window + (1 << DEF_WBITS);
  z->blocks.mode = TYPE;

  inflateReset(z);

  return Z_OK;
}


#endif
