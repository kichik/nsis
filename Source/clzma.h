#ifndef __CLZMA_H__
#define __CLZMA_H__

#ifndef _WIN32
# include <pthread.h>
#endif

#include "compressor.h"
#include "7zip/7zip/IStream.h"
#include "7zip/7zip/Compress/LZMA/LZMAEncoder.h"
#include "7zip/Common/MyCom.h"
#include "7zip/Common/Defs.h"

// implemented in build.cpp - simply calls CompressReal
#ifdef _WIN32
DWORD WINAPI lzmaCompressThread(LPVOID lpParameter);
#else
void *lzmaCompressThread(void *arg);
#endif

#define LZMA_BAD_CALL -1
#define LZMA_INIT_ERROR -2
#define LZMA_THREAD_ERROR -3
#define LZMA_IO_ERROR -4
#define LZMA_MEM_ERROR -5

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
  HANDLE hNeedIOEvent;
  HANDLE hIOReadyEvent;

  BYTE *next_in; /* next input byte */
  UINT avail_in; /* number of bytes available at next_in */

  BYTE *next_out; /* next output byte should be put there */
  UINT avail_out; /* remaining free space at next_out */

  int res;

  BOOL finish;
  BOOL compressor_finished;

#ifndef _WIN32
  struct evnet_t
  {
    pthread_cond_t cond;
    pthread_mutex_t mutex;
    bool signaled;
  };

  HANDLE CreateEvent(void *, BOOL, BOOL, char *)
  {
    evnet_t *event = (evnet_t *) malloc(sizeof(evnet_t));
    if (!event)
      return 0;
    if (pthread_cond_init(&event->cond, NULL))
    {
      free(event);
      return 0;
    }
    if (pthread_mutex_init(&event->mutex, NULL))
    {
      free(event);
      return 0;
    }
    event->signaled = false;
    return (HANDLE) event;
  }

  BOOL SetEvent(HANDLE _event)
  {
    evnet_t *event = (evnet_t *) _event;
    if (pthread_mutex_lock(&event->mutex))
      return FALSE;
    event->signaled = true;
    pthread_cond_signal(&event->cond);
    if (pthread_mutex_unlock(&event->mutex))
      return FALSE;
    return TRUE;
  }

  BOOL ResetEvent(HANDLE _event)
  {
    evnet_t *event = (evnet_t *) _event;
    event->signaled = false;
    return TRUE;
  }

  BOOL CloseHandle(HANDLE _event)
  {
    BOOL ret = TRUE;
    evnet_t *event = (evnet_t *) _event;
    if (event)
      return FALSE;
    if (pthread_cond_destroy(&event->cond))
      ret = FALSE;
    if (pthread_mutex_destroy(&event->mutex))
      ret = FALSE;
    free(event);
    return ret;
  }

#define WAIT_OBJECT_0 0
#define INFINITE 0
  DWORD WaitForSingleObject(HANDLE _event, DWORD) {
    DWORD ret = WAIT_OBJECT_0;
    evnet_t *event = (evnet_t *) _event;
    if (!event->signaled)
    {
      pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;
      if (pthread_mutex_lock(&m) || pthread_cond_wait(&event->cond, &m))
      {
        ret = !WAIT_OBJECT_0;
      }
      pthread_mutex_unlock(&m);
      pthread_mutex_destroy(&m);
    }
    if (pthread_mutex_lock(&event->mutex))
      return !WAIT_OBJECT_0;
    event->signaled = false;
    if (pthread_mutex_unlock(&event->mutex))
      return !WAIT_OBJECT_0;
    return ret;
  }

#define WaitForMultipleObjects(x, list, y, t) WaitForSingleObject(list[0], t)

#endif

  int ConvertError(HRESULT result)
  {
    if (result != S_OK)
    {
      if (result == E_OUTOFMEMORY)
        return LZMA_MEM_ERROR;
      else
        return LZMA_IO_ERROR;
    }
    return C_OK;
  }

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
    hNeedIOEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
    hIOReadyEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
    finish = FALSE;
    compressor_finished = TRUE;
    hCompressionThread = 0;
    SetNextOut(NULL, 0);
    SetNextIn(NULL, 0);
  }

  virtual ~CLZMA()
  {
    End();
    if (hNeedIOEvent)
      CloseHandle(hNeedIOEvent);
    if (hIOReadyEvent)
      CloseHandle(hIOReadyEvent);
    if (_encoder)
    {
      delete _encoder;
      _encoder = NULL;
    }
  }

  int Init(int level, UINT32 dicSize)
  {
    End();

    compressor_finished = FALSE;
    finish = FALSE;
    res = C_OK;

    if (!hNeedIOEvent || !hIOReadyEvent)
    {
      return LZMA_INIT_ERROR;
    }

    ResetEvent(hNeedIOEvent);
    ResetEvent(hIOReadyEvent);

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
      return LZMA_INIT_ERROR;
    return _encoder->SetStreams(this, this, 0, 0) == S_OK ? C_OK : LZMA_INIT_ERROR;
  }

  int Init(int level)
  {
    // default dictionary size is 8MB
    return Init(level, 8 << 20);
  }

  int End()
  {
    // has compressor not finished?
    if (hCompressionThread && !compressor_finished)
    {
      // kill compression thread
      avail_in = 0;
      avail_out = 0;
      compressor_finished = TRUE;

      SetEvent(hIOReadyEvent);
#ifdef _WIN32
      WaitForSingleObject(hCompressionThread, INFINITE);
#else
      pthread_join(hCompressionThread, NULL);
#endif
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
      HRESULT hResult = _encoder->WriteCoderProperties(this);
      if (res == S_OK)
      {
        while (true)
        {
          UINT64 inSize, outSize;
          INT32 finished;
          res = ConvertError(_encoder->CodeOneBlock(&inSize, &outSize, &finished));
          if (res != C_OK)
            break;
          if (finished)
          {
            res = C_OK;
            break;
          }
        }
      }
      else
      {
        res = ConvertError(hResult);
      }
    }
    catch (...)
    {
      res = LZMA_IO_ERROR;
    }

    compressor_finished = TRUE;
    SetEvent(hNeedIOEvent);
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
        return LZMA_BAD_CALL;
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
        return LZMA_INIT_ERROR;
    }
    else
    {
      SetEvent(hIOReadyEvent);
    }

    HANDLE waitList[2] = {hNeedIOEvent, (HANDLE) hCompressionThread};
    if (WaitForMultipleObjects(2, waitList, FALSE, INFINITE) != WAIT_OBJECT_0)
    {
      // thread ended or WaitForMultipleObjects failed
      compressor_finished = TRUE;
      SetEvent(hIOReadyEvent);
      return LZMA_THREAD_ERROR;
    }

    if (compressor_finished)
    {
      return res;
    }

    return C_OK;
  }

  void GetMoreIO()
  {
    SetEvent(hNeedIOEvent);
    if (WaitForSingleObject(hIOReadyEvent, INFINITE) != WAIT_OBJECT_0)
    {
      compressor_finished = TRUE;
      res = LZMA_THREAD_ERROR;
    }
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
        if (!avail_in)
        {
          if (finish)
          {
            return S_OK;
          }
          return E_ABORT;
        }
        if (compressor_finished)
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

  const char* GetErrStr(int err) {
    switch (err)
    {
    case LZMA_BAD_CALL:
      return "bad call";
    case LZMA_INIT_ERROR:
      return "initialization failed";
    case LZMA_THREAD_ERROR:
      return "thread synchronization error";
    case LZMA_IO_ERROR:
      return "input/output error";
    case LZMA_MEM_ERROR:
      return "not enough memory";
    default:
      return "unknown error";
    }
  }
};

#endif
