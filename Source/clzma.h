#ifndef __CLZMA_H__
#define __CLZMA_H__

#include "compressor.h"
#include "7zip/7zip/IStream.h"
#include "7zip/7zip/Compress/LZMA/LZMAEncoder.h"
#include "7zip/Common/MyCom.h"

class CCyclicBuffer
{
  BYTE *Buffer;
  UINT32 BufferSize;
  UINT32 Pos;
  UINT32 UsedSize;
public:
  void Free()
  {
    if (Buffer != 0)
    {
      delete []Buffer;
      Buffer = 0;
    }
  }
  CCyclicBuffer(): Buffer(0) {}
  ~CCyclicBuffer() { Free(); }
  bool Create(UINT32 bufferSize)
  {
    if (Buffer != 0 && bufferSize == bufferSize)
      return true;
    Free();
    BufferSize = bufferSize;
    Buffer = new BYTE[bufferSize];
    return (Buffer != 0);
  }
  void Init()
  {
    Pos = 0;
    UsedSize = 0;
  }
  UINT32 GetUsedSize() const { return UsedSize; }
  UINT32 GetAvailSize() const { return BufferSize - UsedSize; }
  UINT32 Write(const BYTE *data, UINT32 size)
  {
    if (size > GetAvailSize())
      size = GetAvailSize();
    UINT32 size1 = size;
    while(size1 > 0)
    {
      UINT32 writePos = Pos + UsedSize;
      if (writePos >= BufferSize)
        writePos -= BufferSize;
      UINT32 size2 = size1;
      if (size2 > BufferSize - writePos)
        size2 = BufferSize - writePos;
      memmove(Buffer + writePos, data, size2);
      data += size2;
      size1 -= size2;
      UsedSize += size2;
    }
    return size;
  }
  UINT32 Read(BYTE *data, UINT32 size)
  {
    if (size > UsedSize)
      size = UsedSize;
    UINT32 size1 = size;
    while (size1 > 0)
    {
      UINT32 size2 = size1;
      if (size2 > BufferSize - Pos)
        size2 = BufferSize - Pos;
      memmove(data, Buffer + Pos, size2);
      Pos += size2;
      if (Pos >= BufferSize)
        Pos -= BufferSize;
      data += size2;
      size1 -= size2;
      UsedSize -= size2;
    }
    return size;
  }
};

class CBufferInStream: 
  public ISequentialInStream,
  public CMyUnknownImp,
  public CCyclicBuffer
{
public:
  MY_UNKNOWN_IMP
  STDMETHOD(Read)(void *data, UINT32 size, UINT32 *processedSize)
  {
    return ReadPart(data, size, processedSize);
  }
  STDMETHOD(ReadPart)(void *data, UINT32 size, UINT32 *processedSize)
  {
    UINT32 temp = CCyclicBuffer::Read((BYTE *)data, size);
    if (processedSize != 0)
      *processedSize = temp;
    return S_OK;
  }
};

class CBufferOutStream: 
  public ISequentialOutStream,
  public CMyUnknownImp,
  public CCyclicBuffer
{
public:
  MY_UNKNOWN_IMP

  STDMETHOD(Write)(const void *data, UINT32 size, UINT32 *processedSize)
  {
    return WritePart(data, size, processedSize);
  }
  STDMETHOD(WritePart)(const void *data, UINT32 size, UINT32 *processedSize)
  {
    UINT32 temp = CCyclicBuffer::Write((const BYTE *)data, size);
    if (processedSize != 0)
      *processedSize = temp;
    return S_OK;
  }
};

class CLZMA : public ICompressor 
{
public:
  CLZMA(): _encoder(0) 
  {
    _encoder = new NCompress::NLZMA::CEncoder();
    _encoder->SetWriteEndMarkerMode(true);
  }
  ~CLZMA()
  {
    if (_encoder != 0)
    {
      delete _encoder;
      _encoder = 0;
    }
  }
  
  int Init(int level, UINT32 dicSize) 
  {
    _inStream.Create(dicSize * 2 + (1 << 21));
    
    // you must set it at least 1 MB and add 2 sizes of buffer from OutBuffer.h: 
    // COutBuffer::COutBuffer(UINT32 bufferSize = (1 << 20));
    _outStream.Create(3 << 20);

    _needWriteProperties = true;
    _inStream.Init();
    _outStream.Init();

    PROPID propdIDs [] = 
    {
      NCoderPropID::kAlgorithm,
      NCoderPropID::kDictionarySize,
      NCoderPropID::kNumFastBytes
    };
    const kNumProps = sizeof(propdIDs) / sizeof(propdIDs[0]);
    PROPVARIANT props[kNumProps];
    // NCoderPropID::kAlgorithm
    props[0].vt = VT_UI4;
    props[0].ulVal = 2;
    // NCoderPropID::kDictionarySize
    props[1].vt = VT_UI4;
    props[1].ulVal = dicSize;
    // NCoderPropID::kNumFastBytes
    props[2].vt = VT_UI4;
    props[2].ulVal = 64;
    if (_encoder->SetCoderProperties(propdIDs, props, kNumProps) != 0)
      return -1;
    return _encoder->SetStreams(&_inStream, &_outStream, 0, 0);
  }

  int Init(int level) 
  {
    // default dictionary size is 8MB
    return Init(level, 1 << 23);
  }
  
  int End()
  {
    _next_in = NULL;
    _avail_in = 0;
    _next_out = NULL;
    _avail_out = 0;
    _inStream.Free();
    _outStream.Free();
    return C_OK;
  }
  
  int Compress(BOOL finish) 
  {
    WriteToOutStream();
    if (_avail_in)
    {
      UINT32 written = _inStream.Write((const LPBYTE)_next_in, _avail_in);
      _next_in += written;
      _avail_in -= written;
    }
    while ((_inStream.GetAvailSize() == 0 || finish) && 
          _outStream.GetUsedSize() == 0)
    {
      UINT64 inSize, outSize;
      INT32 finished;
      if (_needWriteProperties)
      {
        if (_encoder->WriteCoderProperties(&_outStream) != 0)
          return 1;
        _needWriteProperties = false;
      }
      if (_encoder->CodeOneBlock(&inSize, &outSize, &finished) != 0)
        return 1;
      WriteToOutStream();
      if (finished != 0)
        return C_OK;
      if (_avail_out == 0)
        return C_OK;
    }
    return C_OK;
  }
  
  void SetNextIn(char *in, unsigned int size) 
  {
    _next_in = in;
    _avail_in = size;
  }
  
  void SetNextOut(char *out, unsigned int size) 
  {
    _next_out = out;
    _avail_out = size;
  }
  
  virtual char* GetNextOut() { return _next_out; }
  virtual unsigned int GetAvailIn() { return _avail_in; }
  virtual unsigned int GetAvailOut() { return _avail_out; }
  const char* GetName() { return "lzma"; }
  
private:
  NCompress::NLZMA::CEncoder *_encoder;
  CBufferInStream _inStream;
  CBufferOutStream _outStream;
  char *_next_in;
  UINT32 _avail_in;
  char *_next_out;
  UINT32 _avail_out;
  bool _needWriteProperties;

  void WriteToOutStream() 
  {
    UINT32 temp = _outStream.Read((BYTE *)_next_out, _avail_out);
    _next_out += temp;
    _avail_out -= temp;
  }
};

#endif