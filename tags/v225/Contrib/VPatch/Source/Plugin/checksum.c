//---------------------------------------------------------------------------
// checksum.c
//---------------------------------------------------------------------------
//                           -=* VPatch *=-
//---------------------------------------------------------------------------
// Copyright (C) 2001-2005 Koen van de Sande / Van de Sande Productions
//---------------------------------------------------------------------------
// Website: http://www.tibed.net/vpatch
//
// This software is provided 'as-is', without any express or implied
// warranty.  In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

#include "checksum.h"

#ifdef _MSC_VER
#  define FORCE_INLINE __forceinline
#else
#  ifdef __GNUC__
#    if __GNUC__ < 3
#      define FORCE_INLINE inline
#    else
#      define FORCE_INLINE inline __attribute__ ((always_inline))
#    endif
#  else
#    define FORCE_INLINE inline
#  endif
#endif

/* ------------------------ CRC32 checksum calculation ----------------- */

UINT CRCTable[256];
BOOL bInitCRC = FALSE;

FORCE_INLINE void InitCRC() {
  int i, j; unsigned long c;
  for (c = i = 0; i < 256; c = ++i) {
    for (j = 0; j < 8; j++) {
      if (c & 1) c = (c>>1) ^ 0xEDB88320;
      else       c >>= 1;
    }
    CRCTable[i] = c;
  }
  bInitCRC = TRUE;
}

#define CRCBLOCKSIZE    4096

BOOL FileCRC(HANDLE hFile, DWORD *crc) {
  static BYTE crcblock[CRCBLOCKSIZE];
  DWORD read;
  BYTE *p;

  UINT c = 0xFFFFFFFF;
  if (bInitCRC == FALSE)
    InitCRC();
  
  SetFilePointer(hFile, 0, NULL, FILE_BEGIN);
  do {
    if (ReadFile(hFile, crcblock, CRCBLOCKSIZE, &read, NULL) == FALSE)
      return FALSE;
    for (p = crcblock; p < crcblock + read; p++)
      c = CRCTable[(c & 0xFF) ^ *p] ^ (c >> 8);
  } while (read);

  *crc = (c ^ 0xFFFFFFFF);

  return TRUE;
}

void CRC32ToString(char* string, DWORD crc) {
  int i = 0;
  int j = 7;
  int a1, a2;
  for(i = 0; i < 4; i++) {
    a1 = (crc >> 4) % 16;
    a2 = crc % 16;
    string[j--] = a2 < 10 ? ('0' + a2) : ('A' + a2 - 10);
    string[j--] = a1 < 10 ? ('0' + a1) : ('A' + a1 - 10);
    crc = crc >> 8;
  }
}

/* ------------------------ MD5 checksum calculation ----------------- */

#define MD5BLOCKSIZE    16384

BOOL FileMD5(HANDLE hFile, md5_byte_t digest[16]) {
  static BYTE md5block[MD5BLOCKSIZE];
  DWORD read;

  md5_state_t state;
  
  md5_init(&state);
 
  SetFilePointer(hFile, 0, NULL, FILE_BEGIN);
  do {
    if (ReadFile(hFile, md5block, MD5BLOCKSIZE, &read, NULL) == FALSE)
      return FALSE;
    md5_append(&state, md5block, read);
  } while (read);

  md5_finish(&state, digest);
  return TRUE;
}

void MD5ToString(char* string, md5_byte_t checksum[16]) {
  int i = 0;
  int j = 0;
  int a1, a2;
  for(i = 0; i < 16; i++) {
    a1 = (checksum[i] >> 4) % 16;
    a2 = checksum[i] % 16;
    string[j++] = a1 < 10 ? ('0' + a1) : ('A' + a1 - 10);
    string[j++] = a2 < 10 ? ('0' + a2) : ('A' + a2 - 10);
  }
}
