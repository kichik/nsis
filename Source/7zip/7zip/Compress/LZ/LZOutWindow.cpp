// LZOutWindow.cpp

#include "StdAfx.h"

#include "../../../Common/Alloc.h"
#include "LZOutWindow.h"

bool CLZOutWindow::Create(UInt32 windowSize)
{
  _pos = 0;
  _streamPos = 0;
  const UInt32 kMinBlockSize = 1;
  if (windowSize < kMinBlockSize)
    windowSize = kMinBlockSize;
  if (_buffer != 0 && _windowSize == windowSize)
    return true;
  Free();
  _windowSize = windowSize;
  _buffer = (Byte *)::BigAlloc(windowSize);
  return (_buffer != 0);
}

void CLZOutWindow::Free()
{
  ::BigFree(_buffer);
  _buffer = 0;
}

void CLZOutWindow::Init(ISequentialOutStream *stream, bool solid)
{
  // ReleaseStream();
  _stream = stream;
  // _stream->AddRef();

  if(!solid)
  {
    _streamPos = 0;
    _pos = 0;
  }
  #ifdef _NO_EXCEPTIONS
  ErrorCode = S_OK;
  #endif
}

/*
void CLZOutWindow::ReleaseStream()
{
  if(_stream != 0)
  {
    // Flush(); // Test it
    _stream->Release();
    _stream = 0;
  }
}
*/

void CLZOutWindow::FlushWithCheck()
{
  HRESULT result = Flush();
  #ifdef _NO_EXCEPTIONS
  ErrorCode = result;
  #else
  if (result != S_OK)
    throw CLZOutWindowException(result);
  #endif
}

HRESULT CLZOutWindow::Flush()
{
  UInt32 size = _pos - _streamPos;
  if(size == 0)
    return S_OK;
  #ifdef _NO_EXCEPTIONS
  if (ErrorCode != S_OK)
    return ErrorCode;
  #endif

  UInt32 processedSize;
  HRESULT result = _stream->Write(_buffer + _streamPos, size, &processedSize);
  if (result != S_OK)
    return result;
  if (size != processedSize)
    return E_FAIL;
  if (_pos >= _windowSize)
    _pos = 0;
  _streamPos = _pos;
  return S_OK;
}
