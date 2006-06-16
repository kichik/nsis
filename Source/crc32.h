#include "Platform.h"

#ifndef ___CRC32__H___
#define ___CRC32__H___

typedef UINT32 crc32_t;

#ifdef __cplusplus
extern "C"
#endif
crc32_t NSISCALL CRC32(crc32_t crc, const unsigned char *buf, unsigned int len);

#endif//!___CRC32__H___
