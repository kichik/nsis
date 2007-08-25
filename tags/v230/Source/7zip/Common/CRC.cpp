/*
 * CRC.cpp
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

#include "StdAfx.h"

#include "CRC.h"

static const UInt32 kCRCPoly = 0xEDB88320;

UInt32 CCRC::Table[256];

void CCRC::InitTable()
{
  for (UInt32 i = 0; i < 256; i++)
  {
    UInt32 r = i;
    for (int j = 0; j < 8; j++)
      if (r & 1) 
        r = (r >> 1) ^ kCRCPoly;
      else     
        r >>= 1;
    CCRC::Table[i] = r;
  }
}

class CCRCTableInit
{
public:
  CCRCTableInit() { CCRC::InitTable(); }
} g_CRCTableInit;

void CCRC::UpdateByte(Byte b)
{
  _value = Table[((Byte)(_value)) ^ b] ^ (_value >> 8);
}

void CCRC::UpdateUInt16(UInt16 v)
{
  UpdateByte(Byte(v));
  UpdateByte(Byte(v >> 8));
}

void CCRC::UpdateUInt32(UInt32 v)
{
  for (int i = 0; i < 4; i++)
    UpdateByte((Byte)(v >> (8 * i)));
}

void CCRC::UpdateUInt64(UInt64 v)
{
  for (int i = 0; i < 8; i++)
    UpdateByte((Byte)(v >> (8 * i)));
}

void CCRC::Update(const void *data, size_t size)
{
  UInt32 v = _value;
  const Byte *p = (const Byte *)data;
  for (; size > 0 ; size--, p++)
    v = Table[((Byte)(v)) ^ *p] ^ (v >> 8);
  _value = v;
}
