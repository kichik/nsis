#ifndef __CZLIB_H__
#define __CZLIB_H__

#include "compressor.h"
#include "zlib/ZLIB.H"

class CZlib : public ICompressor {
  public:
    virtual ~CZlib() {}

    int Init(int level, unsigned int dict_size) {
      stream = new z_stream;
      if (!stream) return Z_MEM_ERROR;
      return deflateInit(stream, level);
    }

    int End() {
      int ret = deflateEnd(stream);
      delete stream;
      return ret;
    }

    int Compress(bool finish) {
      return deflate(stream, finish?Z_FINISH:0);
    }

    void SetNextIn(char *in, unsigned int size) {
      stream->next_in = (unsigned char*)in;
      stream->avail_in = size;
    }

    void SetNextOut(char *out, unsigned int size) {
      stream->next_out = (unsigned char*)out;
      stream->avail_out = size;
    }

    virtual char* GetNextOut() {
      return (char*)stream->next_out;
    }

    virtual unsigned int GetAvailIn() {
      return stream->avail_in;
    }

    virtual unsigned int GetAvailOut() {
      return stream->avail_out;
    }

    const char* GetName() {
      return "zlib";
    }

    const char* GetErrStr(int err) {
      switch (err)
      {
      case Z_STREAM_ERROR:
        return "invalid stream - bad call";
      case Z_DATA_ERROR:
        return "data error";
      case Z_MEM_ERROR:
        return "not enough memory";
      case Z_BUF_ERROR:
        return "buffer error - bad call";
      case Z_VERSION_ERROR:
        return "version error";
      default:
        return "unknown error";
      }
    }

  private:
    z_stream *stream;
};

#endif
