// RangeCoderOpt.h

#ifndef __RANGECODEROPT_H
#define __RANGECODEROPT_H

#include "RangeCoderBit.h"

#define RC_INIT_VAR \
  UINT32 range = rangeDecoder->Range; \
  UINT32 code = rangeDecoder->Code;        

#define RC_FLUSH_VAR \
  rangeDecoder->Range = range; \
  rangeDecoder->Code = code;

#define RC_NORMALIZE \
  if (range < kTopValue) \
    { range <<= 8; code = (code << 8) | rangeDecoder->Stream.ReadByte(); }

#define RC_GETBIT2(prob, modelIndex, Action0, Action1) \
    {UINT32 newBound = (range >> kNumBitModelTotalBits) * prob; \
    if (code < newBound)                               \
    {                                                             \
      Action0;                                                    \
      range = newBound;                                         \
      prob += (kBitModelTotal - prob) >> kNumMoveBits;          \
      modelIndex <<= 1;                                          \
    }                                                             \
    else                                                          \
    {                                                             \
      Action1;                                                    \
      range -= newBound;                                        \
      code -= newBound;                                          \
      prob -= (prob) >> kNumMoveBits;                           \
      modelIndex = (modelIndex + modelIndex) + 1;                       \
    }}                                                             \
    RC_NORMALIZE

//modelIndex <<= 1; if (code >= newBound) modelIndex++;

#define RC_GETBIT(prob, modelIndex) RC_GETBIT2(prob, modelIndex, ; , ;)               

#endif
