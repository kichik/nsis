// RangeCoderBit.h

#ifndef __RANGECODERBIT_H
#define __RANGECODERBIT_H

#include "Types.h"
#include "RangeCoder.h"

const int kNumBitModelTotalBits  = 11;
const int kBitModelTotal = (1 << kNumBitModelTotalBits);

const int kNumMoveBits = 5;

struct CBitModel
{
  UINT32 Probability; // it's fast version on 32-bit systems
  // unsigned short Probability; // use it if you want to reduce memory twice

  void UpdateModel(int symbol)
  {
    /*
    Probability -= (Probability + ((symbol - 1) & ((1 << numMoveBits) - 1))) >> numMoveBits;
    Probability += (1 - symbol) << (kNumBitModelTotalBits - numMoveBits);
    */
    if (symbol == 0)
      Probability += (kBitModelTotal - Probability) >> kNumMoveBits;
    else
      Probability -= (Probability) >> kNumMoveBits;
  }
  void Init() { Probability = kBitModelTotal / 2; }
};

struct CBitDecoder: public CBitModel
{
  int Decode(CRangeDecoder *rangeDecoder)
  {
    UINT32 newBound = (rangeDecoder->Range >> kNumBitModelTotalBits) * Probability;
    int ret = 0;
    if (rangeDecoder->Code < newBound)
    {
      rangeDecoder->Range = newBound;
      Probability += (kBitModelTotal - Probability) >> kNumMoveBits;
    }
    else
    {
      rangeDecoder->Range -= newBound;
      rangeDecoder->Code -= newBound;
      Probability -= (Probability) >> kNumMoveBits;
      ret = 1;
    }

    if (rangeDecoder->Range < kTopValue)
    {
      rangeDecoder->Code = (rangeDecoder->Code << 8) | rangeDecoder->Stream.ReadByte();
      rangeDecoder->Range <<= 8;
    }

    return ret;
  }
};

#endif
