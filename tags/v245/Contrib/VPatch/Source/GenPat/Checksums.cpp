//---------------------------------------------------------------------------
// Checksums.cpp
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

#include "Checksums.h"

/* ------------------------ CRC32 checksum calculation ----------------- */

uint32_t CRCTable[256];
bool bInitCRC = false;

void InitCRC() {
  int i, j; unsigned long c;
  for (c = i = 0; i < 256; c = ++i) {
    for (j = 0; j < 8; j++) {
      if (c & 1)	c = (c>>1) ^ 0xEDB88320;
      else		c >>= 1;
    }
    CRCTable[i] = c;
  }
  bInitCRC = true;
}

crc32_t streamCRC32(bistream& data) {
  if(!bInitCRC) InitCRC();

  const int CRCBLOCKSIZE = 16384;

  uint8_t block[CRCBLOCKSIZE];
  unsigned int read;
  uint8_t *p;

  crc32_t crc = 0xFFFFFFFF;
  while(data.good()) {
    data.read(reinterpret_cast<char*>(block), CRCBLOCKSIZE);
    read = data.gcount();
    for (p = block; p < block + read; p++)
      crc = CRCTable[(crc & 0xFF) ^ *p] ^ (crc >> 8);
  }
  crc = (crc ^ 0xFFFFFFFF);
  return crc;
}

/* ------------------------ MD5 checksum calculation ----------------- */

void streamMD5(bistream& data, md5_byte_t digest[16]) {
  const int MD5BLOCKSIZE = 16384;
  uint8_t md5block[MD5BLOCKSIZE];
  unsigned int read;

  md5_state_t state;

  md5_init(&state);

  while(data.good()) {
    data.read(reinterpret_cast<char*>(md5block), MD5BLOCKSIZE);
    read = data.gcount();
    md5_append(&state, md5block, read);
  }

  md5_finish(&state, digest);
}

TChecksum::TChecksum(std::string& fileName) : mode(MD5) {
  bifstream data;
  data.open(fileName.c_str(), ios::binary | ios::in);
  data.seekg(0, ios::beg);
  crc = streamCRC32(data);
  data.close();

  bifstream data2;
  data2.open(fileName.c_str(), ios::binary | ios::in);
  data2.seekg(0, ios::beg);
  streamMD5(data2, digest);
  data2.close();
}

void TChecksum::loadMD5(md5_byte_t newdigest[16]) {
  mode = MD5;
  for(int i = 0; i < 16; i++) {
    digest[i] = newdigest[i];
  }
}
void TChecksum::loadCRC32(crc32_t newcrc) {
  mode = CRC32;
  crc = newcrc;
}
bool TChecksum::operator==(const TChecksum& b) {
  if(mode != b.mode) throw "Checksums in different mode: MD5/CRC32";
  if(mode == MD5) {
    for(int md5index = 0; md5index < 16; md5index++) {
      if(digest[md5index] != b.digest[md5index]) break;
      if(md5index == 15) return true;
    }
    return false;
  } else {
    return (crc == b.crc);
  }
}
