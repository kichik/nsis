#ifndef __CBZIP2_H__
#define __CBZIP2_H__

#include "compressor.h"
#include "bzip2/bzlib.h"

class CBzip2 : public ICompressor {
  public:
    int Init(int level, unsigned int dict_size) {
      last_ret = !BZ_STREAM_END;
      stream = new bz_stream;
      if (!stream) return BZ_MEM_ERROR;
      return BZ2_bzCompressInit(stream, level, 0, 30);
    }

    int End() {
      int ret = BZ2_bzCompressEnd(stream);
      delete stream;
      return ret;
    }

    int Compress(bool finish) {
      // act like zlib when it comes to stream ending
      if (last_ret == BZ_STREAM_END && finish)
        return BZ_STREAM_END;
      last_ret = BZ2_bzCompress(stream, finish?BZ_FINISH:0);
      return last_ret;
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

    const char* GetName() {
      return "bzip2";
    }

    const char* GetErrStr(int err) {
      switch (err)
      {
      case BZ_SEQUENCE_ERROR:
        return "sequence error - bad call";
      case BZ_PARAM_ERROR:
        return "parameter error - bad call";
      case BZ_MEM_ERROR:
        return "not enough memory";
      case BZ_CONFIG_ERROR:
        return "config error";
      default:
        return "unknown error";
      }
    }

  private:
    bz_stream *stream;
    int last_ret;
};

#endif
