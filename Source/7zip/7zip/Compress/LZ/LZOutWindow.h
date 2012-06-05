/*
 * LZOutWindow.h
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

#ifndef __LZ_OUT_WINDOW_H
#define __LZ_OUT_WINDOW_H

#include "../../IStream.h"
#include "../../Common/OutBuffer.h"

#ifndef _NO_EXCEPTIONS
typedef COutBufferException CLZOutWindowException;
#endif

class CLZOutWindow: public COutBuffer
{
public:
  void Init(bool solid = false);
  
  // distance >= 0, len > 0, 
  bool CopyBlock(UInt32 distance, UInt32 len)
  {
    UInt32 pos = _pos - distance - 1;
    if (distance >= _pos)
    {
      if (!_overDict || distance >= _bufferSize)
        return false;
      pos += _bufferSize;
    }
    do
    {
      if (pos == _bufferSize)
        pos = 0;
      _buffer[_pos++] = _buffer[pos++];
      if (_pos == _limitPos)
        FlushWithCheck();  
    }
    while(--len != 0);
    return true;
  }
  
  void PutByte(Byte b)
  {
    _buffer[_pos++] = b;
    if (_pos == _limitPos)
      FlushWithCheck();  
  }
  
  Byte GetByte(UInt32 distance) const
  {
    UInt32 pos = _pos - distance - 1;
    if (pos >= _bufferSize)
      pos += _bufferSize;
    return _buffer[pos]; 
  }
};

#endif
