#ifndef __CZLIB_H__
#define __CZLIB_H__

#include "compressor.h"
#include "zlib/ZLIB.H"

class CZlib : public ICompressor {
  public:
    int Init(int level) {
      stream = new z_stream;
      if (!stream) return Z_MEM_ERROR;
      return deflateInit(stream, level);
    }

    int End() {
      int ret = deflateEnd(stream);
      delete stream;
      return ret;
    }

    int Compress(BOOL finish) {
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

    char* GetName() {
      return "zlib";
    }

  private:
    z_stream *stream;
};

#endif