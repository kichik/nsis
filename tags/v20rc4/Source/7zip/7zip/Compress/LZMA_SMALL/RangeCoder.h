// Compression/RangeCoder.h

#ifndef __COMPRESSION_RANGECODER_H
#define __COMPRESSION_RANGECODER_H

#include "InBuffer.h"

const int kNumTopBits = 24;
const UINT32 kTopValue = (1 << kNumTopBits);

class CRangeDecoder
{
public:
  CInBuffer Stream;
  UINT32 Range;
  UINT32 Code;
  void Normalize()
  {
    while (Range < kTopValue)
    {
      Code = (Code << 8) | Stream.ReadByte();
      Range <<= 8;
    }
  }

  void Init(CLZMAStateP state)
  {
    Stream.Init(state);
    Code = 0;
    Range = UINT32(-1);
    for(int i = 0; i < 5; i++)
      Code = (Code << 8) | Stream.ReadByte();
  }

  UINT32 DecodeDirectBits(int numTotalBits)
  {
    UINT32 range = Range;
    UINT32 code = Code;        
    UINT32 result = 0;
    for (int i = numTotalBits; i > 0; i--)
    {
      range >>= 1;

      result <<= 1;
      if (code >= range)
      {
        code -= range;
        result |= 1;
      }

      /*UINT32 t = (code - range) >> 31;
      code -= range & (t - 1);
      // range = aRangeTmp + ((range & 1) & (1 - t));
      result = (result + result) | (1 - t);*/

      if (range < kTopValue)
      {
        code = (code << 8) | Stream.ReadByte();
        range <<= 8; 
      }
    }
    Range = range;
    Code = code;
    return result;
  }

  int DecodeBit(UINT32 size0, int numTotalBits)
  {
    UINT32 newBound = (Range >> numTotalBits) * size0;
    int symbol;
    if (Code < newBound)
    {
      symbol = 0;
      Range = newBound;
    }
    else
    {
      symbol = 1;
      Code -= newBound;
      Range -= newBound;
    }
    Normalize();
    return symbol;
  }
};

#endif
