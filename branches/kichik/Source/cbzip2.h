#ifndef __CBZIP2_H__
#define __CBZIP2_H__

#include "compressor.h"
#include "bzip2/bzlib.h"

class CBzip2 : public ICompressor {
  public:
    int Init(int level) {
      stream = new bz_stream;
      if (!stream) return BZ_MEM_ERROR;
      return BZ2_bzCompressInit(stream, level, 0, 30);
    }

    int End() {
      int ret = BZ2_bzCompressEnd(stream);
      delete stream;
      return ret;
    }

    int Compress(BOOL finish) {
      return BZ2_bzCompress(stream, finish?BZ_FINISH:0);
    }

    void SetNextIn(char *in, unsigned int size) {
      stream->next_in = in;
      stream->avail_in = size;
    }

    void SetNextOut(char *out, unsigned int size) {
      stream->next_out = out;
      stream->avail_out = size;
    }

    virtual char* GetNextOut() {
      return stream->next_out;
    }

    virtual unsigned int GetAvailIn() {
      return stream->avail_in;
    }

    virtual unsigned int GetAvailOut() {
      return stream->avail_out;
    }

  private:
    bz_stream *stream;
};

#endif