#include "decompress.h"

#define EXEHEAD
#define NSIS_CONFIG_COMPRESSION_SUPPORT

extern "C" {
#define NSIS_COMPRESS_USE_BZIP2
#include "../bzip2/bzlib.h"
#undef NSIS_COMPRESS_USE_BZIP2

#define NSIS_COMPRESS_USE_LZMA
#include "../7zip/LZMADecode.h"
#undef NSIS_COMPRESS_USE_LZMA

#define NSIS_COMPRESS_USE_ZLIB
#include "../zlib/ZLIB.H"
#undef NSIS_COMPRESS_USE_ZLIB
}

#define DECOMPRESSOR(name, type, initf, dec, u)   \
  name::name() {                                  \
    vs = new type;                                \
    memset(vs, 0, sizeof(type));                  \
  }                                               \
                                                  \
  name::~name() {                                 \
    delete (type *) vs;                           \
    vs = 0;                                       \
  }                                               \
                                                  \
  void name::setNextIn(void *buffer, int size) {  \
    type *s = (type *) vs;                        \
    s->next_in = (u *) buffer;                    \
    s->avail_in = size;                           \
  }                                               \
                                                  \
  void name::setNextOut(void *buffer, int size) { \
    type *s = (type *) vs;                        \
    s->next_out = (u *) buffer;                   \
    s->avail_out = size;                          \
  }                                               \
                                                  \
  int name::getAvailOut() {                       \
    type *s = (type *) vs;                        \
    return s->avail_out;                          \
  }                                               \
                                                  \
  void name::init() {                             \
    type *s = (type *) vs;                        \
    initf(s);                                     \
  }                                               \
                                                  \
  int name::decompress() {                        \
    type *s = (type *) vs;                        \
    return dec(s);                                \
  }

DECOMPRESSOR(lzmaDecompressor, lzma_stream, lzmaInit, lzmaDecode, unsigned char);
DECOMPRESSOR(bzip2Decompressor, DState, BZ2_bzDecompressInit, BZ2_bzDecompress, char);
DECOMPRESSOR(zlibDecompressor, z_stream, inflateReset, inflate, unsigned char);
