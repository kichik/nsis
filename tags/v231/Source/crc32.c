/*
 * crc32.c
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "Platform.h"
#include "crc32.h"
#include "exehead/config.h"
#ifdef NSIS_CONFIG_CRC_SUPPORT

// this is based on the (slow,small) CRC32 implementation from zlib.
crc32_t NSISCALL CRC32(crc32_t crc, const unsigned char *buf, unsigned int len)
{
    static crc32_t crc_table[256];

    if (!crc_table[1])
    {
      crc32_t c;
      int n, k;

      for (n = 0; n < 256; n++)
      {
        c = (crc32_t)n;
        for (k = 0; k < 8; k++) c = (c >> 1) ^ (c & 1 ? 0xedb88320L : 0);
        crc_table[n] = c;
      }
    }

    crc = crc ^ 0xffffffffL;
    while (len-- > 0) {
      crc = crc_table[(crc ^ (*buf++)) & 0xff] ^ (crc >> 8);
    }
    return crc ^ 0xffffffffL;
}

#endif
