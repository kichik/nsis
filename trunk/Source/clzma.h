#ifndef __CLZMA_H__
#define __CLZMA_H__

#include "compressor.h"
#include "7zip/7zip/IStream.h"
#include "7zip/7zip/Compress/LZMA/LZMAEncoder.h"
#include "7zip/Common/MyCom.h"

#ifndef _WIN32
#  include <sched.h>
#endif

// implemented in build.cpp - simply calls CompressReal
#ifdef _WIN32
DWORD WINAPI lzmaCompressThread(LPVOID lpParameter);
#else
void *lzmaCompressThread(void *arg);
#endif

class CLZMA:
  public ICompressor,
  public ISequentialInStream,
  public ISequentialOutStream,
  public CMyUnknownImp
{
private:
  NCompress::NLZMA::CEncoder *_encoder;

#ifdef _WIN32
  HANDLE hCompressionThread;
#else
  pthread_t hCompressionThread;
#endif

  BYTE *next_in; /* next input byte */
  UINT avail_in; /* number of bytes available at next_in */

  BYTE *next_out; /* next output byte should be put there */
  UINT avail_out; /* remaining free space at next_out */

  int res;

  BOOL finish;

  long sync_state;
  BOOL compressor_finished;

#ifndef _WIN32
#  define Sleep(x) sched_yield()

  pthread_mutex_t mutex;

  // these two lock every targer passed to them, but that's ok becuase
  // we use only one target anyway...
  long InterlockedExchange(long volatile* target, long value)
  {
    long oldvalue;
    pthread_mutex_lock(&mutex);
    oldvalue = *target;
    *target = value;
    pthread_mutex_unlock(&mutex);
    return oldvalue;
  }

  long InterlockedCompareExchange(long volatile* destination, long exchange, long comperand)
  {
    long oldvalue;
    pthread_mutex_lock(&mutex);
    oldvalue = *destination;
    if (oldvalue == comperand)
      *destination = exchange;
    pthread_mutex_unlock(&mutex);
    return oldvalue;
  }
#endif

public:
  MY_UNKNOWN_IMP

  CLZMA(): _encoder(NULL)
  {
    _encoder = new NCompress::NLZMA::CEncoder();
    _encoder->SetWriteEndMarkerMode(true);
#ifdef _WIN32
    hCompressionThread = NULL;
#else
    hCompressionThread = 0;
#endif
    compressor_finished = FALSE;
    finish = FALSE;
#ifndef _WIN32
    pthread_mutex_init(&mutex, 0);
#endif
    compressor_finished = 1;
    sync_state = 0;
    End();
  }

  virtual ~CLZMA()
  {
#ifndef _WIN32
    pthread_mutex_destroy(&mutex);
#endif
    End();
    if (_encoder)
    {
      delete _encoder;
      _encoder = NULL;
    }
  }

  int Init(int level, UINT32 dicSize)
  {
    End();

    sync_state = 1;

    compressor_finished = FALSE;
    finish = FALSE;

    res = C_OK;

    PROPID propdIDs [] =
    {
      NCoderPropID::kAlgorithm,
      NCoderPropID::kDictionarySize,
      NCoderPropID::kNumFastBytes
    };
    const int kNumProps = sizeof(propdIDs) / sizeof(propdIDs[0]);
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
    return _encoder->SetStreams(this, this, 0, 0);
  }

  int Init(int level)
  {
    // default dictionary size is 8MB
    return Init(level, 8 << 20);
  }

  int End()
  {
    // is compressor not finished and waiting for input/output?
    if (hCompressionThread && !compressor_finished && sync_state == 0)
    {
      // kill compression thread
      avail_in = 0;
      avail_out = 0;

      InterlockedExchange(&sync_state, 1);
      while (InterlockedCompareExchange(&sync_state, 2, 0) != 0)
        Sleep(1);
    }
#ifdef _WIN32
    if (hCompressionThread)
    {
      CloseHandle(hCompressionThread);
      hCompressionThread = NULL;
    }
#else
    hCompressionThread = 0;
#endif
    SetNextOut(NULL, 0);
    SetNextIn(NULL, 0);
    return C_OK;
  }

  int CompressReal()
  {
    try
    {
      if (_encoder->WriteCoderProperties(this) == S_OK)
      {
        while (true)
        {
          UINT64 inSize, outSize;
          INT32 finished;
          if (_encoder->CodeOneBlock(&inSize, &outSize, &finished))
          {
            res = -2;
            break;
          }
          if (finished)
          {
            res = C_OK;
            break;
          }
        }
      }
      else
      {
        res = -2;
      }
    }
    catch (...)
    {
      res = -3;
    }

    compressor_finished = TRUE;
    InterlockedExchange(&sync_state, 0);
    return C_OK;
  }

  int Compress(BOOL flush)
  {
    if (compressor_finished)
    {
      // act like zlib when it comes to stream ending
      if (flush)
        return C_OK;
      else
        return -1;
    }

    finish = flush;

    if (!hCompressionThread)
    {
#ifdef _WIN32
      DWORD dwThreadId;

      hCompressionThread = CreateThread(0, 0, lzmaCompressThread, (LPVOID) this, 0, &dwThreadId);
      if (!hCompressionThread)
#else
      if (pthread_create(&hCompressionThread, NULL, lzmaCompressThread, (LPVOID) this))
#endif
        return -2;
    }
    else
    {
      InterlockedExchange(&sync_state, 1);
    }

    while (InterlockedCompareExchange(&sync_state, 2, 0) != 0)
      Sleep(1);

    if (compressor_finished)
    {
      return res;
    }

    return C_OK;
  }

  void GetMoreIO()
  {
    InterlockedExchange(&sync_state, 0);
    while (InterlockedCompareExchange(&sync_state, 2, 1) != 1)
      Sleep(1);
  }

  STDMETHOD(Read)(void *data, UINT32 size, UINT32 *processedSize)
  {
    return ReadPart(data, size, processedSize);
  }

  STDMETHOD(ReadPart)(void *data, UINT32 size, UINT32 *processedSize)
  {
    if (processedSize)
      *processedSize = 0;
    while (size)
    {
      if (!avail_in)
      {
        if (finish)
        {
          return S_OK;
        }
        GetMoreIO();
        if (!avail_in && finish)
        {
          return S_OK;
        }
        if (!avail_in)
          return E_ABORT;
      }
      UINT32 l = min(size, avail_in);
      memcpy(data, next_in, l);
      avail_in -= l;
      size -= l;
      next_in += l;
      data = LPBYTE(data) + l;
      if (processedSize)
        *processedSize += l;
    }
    return S_OK;
  }

  STDMETHOD(Write)(const void *data, UINT32 size, UINT32 *processedSize)
  {
    return WritePart(data, size, processedSize);
  }

  STDMETHOD(WritePart)(const void *data, UINT32 size, UINT32 *processedSize)
  {
    if (processedSize)
      *processedSize = 0;
    while (size)
    {
      if (!avail_out)
      {
        GetMoreIO();
        if (!avail_out)
          return E_ABORT;
      }
      UINT32 l = min(size, avail_out);
      memcpy(next_out, data, l);
      avail_out -= l;
      size -= l;
      next_out += l;
      data = LPBYTE(data) + l;
      if (processedSize)
        *processedSize += l;
    }
    return S_OK;
  }

  void SetNextIn(char *in, unsigned int size)
  {
    next_in = (LPBYTE) in;
    avail_in = size;
  }

  void SetNextOut(char *out, unsigned int size)
  {
    next_out = (LPBYTE) out;
    avail_out = size;
  }

  virtual char *GetNextOut() { return (char *) next_out; }
  virtual unsigned int GetAvailIn() { return avail_in; }
  virtual unsigned int GetAvailOut() { return avail_out; }
  const char *GetName() { return "lzma"; }
};

#endif
