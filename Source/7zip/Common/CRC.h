/*
 * CRC.h
 * 
 * This file is a part of LZMA compression module for NSIS.
 * 
 * Original LZMA SDK Copyright (C) 1999-2006 Igor Pavlov
 * Modifications Copyright (C) 2003-2006 Amir Szekely <kichik@netvision.net.il>
 * 
 * Licensed under the Common Public License version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef __COMMON_CRC_H
#define __COMMON_CRC_H

#include <stddef.h>
#include "Types.h"

class CCRC
{
  UInt32 _value;
public:
	static UInt32 Table[256];
	static void InitTable();

  CCRC():  _value(0xFFFFFFFF){};
  void Init() { _value = 0xFFFFFFFF; }
  void UpdateByte(Byte v);
  void UpdateUInt16(UInt16 v);
  void UpdateUInt32(UInt32 v);
  void UpdateUInt64(UInt64 v);
  void Update(const void *data, size_t size);
  UInt32 GetDigest() const { return _value ^ 0xFFFFFFFF; } 
  static UInt32 CalculateDigest(const void *data, size_t size)
  {
    CCRC crc;
    crc.Update(data, size);
    return crc.GetDigest();
  }
  static bool VerifyDigest(UInt32 digest, const void *data, size_t size)
  {
    return (CalculateDigest(data, size) == digest);
  }
};

#endif
