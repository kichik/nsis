/*
 * OutBuffer.h
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

#ifndef __OUTBUFFER_H
#define __OUTBUFFER_H

#include "../IStream.h"
#include "../../Common/MyCom.h"

#ifndef _NO_EXCEPTIONS
struct COutBufferException
{
  HRESULT ErrorCode;
  COutBufferException(HRESULT errorCode): ErrorCode(errorCode) {}
};
#endif

class COutBuffer
{
protected:
  Byte *_buffer;
  UInt32 _pos;
  UInt32 _limitPos;
  UInt32 _streamPos;
  UInt32 _bufferSize;
  CMyComPtr<ISequentialOutStream> _stream;
  UInt64 _processedSize;
  Byte  *_buffer2;
  bool _overDict;

  HRESULT FlushPart();
  void FlushWithCheck();
public:
  #ifdef _NO_EXCEPTIONS
  HRESULT ErrorCode;
  #endif

  COutBuffer(): _buffer(0), _pos(0), _stream(0), _buffer2(0) {}
  ~COutBuffer() { Free(); }
  
  bool Create(UInt32 bufferSize);
  void Free();

  void SetMemStream(Byte *buffer) { _buffer2 = buffer; }
  void SetStream(ISequentialOutStream *stream);
  void Init();
  HRESULT Flush();
  void ReleaseStream() {  _stream.Release(); }

  void WriteByte(Byte b)
  {
    _buffer[_pos++] = b;
    if(_pos == _limitPos)
      FlushWithCheck();
  }
  void WriteBytes(const void *data, size_t size)
  {
    for (size_t i = 0; i < size; i++)
      WriteByte(((const Byte *)data)[i]);
  }

  UInt64 GetProcessedSize() const;
};

#endif
