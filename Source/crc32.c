#include "exehead/config.h"
#ifdef NSIS_CONFIG_CRC_SUPPORT

// this is based on the (slow,small) CRC32 implementation from zlib.

static unsigned long crc_table[256];

static void make_crc_table()
{
  unsigned long c;
  int n, k;

  for (n = 0; n < 256; n++)
  {
    c = (unsigned long)n;
    for (k = 0; k < 8; k++) c = (c >> 1) ^ (c & 1 ? 0xedb88320L : 0);
    crc_table[n] = c;
  }
}


// actually CRC32, but we put it in here so we don't
// have to modify the other code.
unsigned long CRC32(unsigned long crc, const unsigned char *buf, unsigned int len)
{
    if (!crc_table[1]) make_crc_table();
    crc = crc ^ 0xffffffffL;
    while (len-- > 0) {
      crc = crc_table[(crc ^ (*buf++)) & 0xff] ^ (crc >> 8);    
    }
    return crc ^ 0xffffffffL;
}

#endif