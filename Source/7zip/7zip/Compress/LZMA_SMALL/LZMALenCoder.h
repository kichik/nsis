// LZMALenCoder.h

#ifndef __LZMALENCODER_H
#define __LZMALENCODER_H

#include "RangeCoderBitTree.h"

const int kLenNumPosStatesBitsMax = 4;
const int kLenNumPosStatesMax = (1 << kLenNumPosStatesBitsMax);

const int kLenNumPosStatesBitsEncodingMax = 4;
const int kLenNumPosStatesEncodingMax = (1 << kLenNumPosStatesBitsEncodingMax);

const int kLenNumLowBits = 3;
const int kLenNumLowSymbols = 1 << kLenNumLowBits;
const int kLenNumMidBits = 3;
const int kLenNumMidSymbols = 1 << kLenNumMidBits;

const int kLenNumHighBits = 8;
const int kLenNumSymbolsTotal = kLenNumLowSymbols + kLenNumMidSymbols + (1 << kLenNumHighBits);

class CLZMALenDecoder
{
  CBitDecoder m_Choice;
  CBitDecoder m_Choice2;
  CBitTreeDecoder3 m_LowCoder[kLenNumPosStatesMax];
  CBitTreeDecoder3 m_MidCoder[kLenNumPosStatesMax];
  CBitTreeDecoder8 m_HighCoder; 
public:
  // void Init(int numPosStates)
  void Init()
  {
    m_Choice.Init();
    m_Choice2.Init();
    // for (int posState = 0; posState < numPosStates; posState++)
    for (int posState = 0; posState < kLenNumPosStatesMax; posState++)
    {
      m_LowCoder[posState].Init();
      m_MidCoder[posState].Init();
    }
    m_HighCoder.Init();
  }
  int Decode(CRangeDecoder *rangeDecoder, int posState)
  {
    if(m_Choice.Decode(rangeDecoder) == 0)
      return m_LowCoder[posState].Decode(rangeDecoder);
    if(m_Choice2.Decode(rangeDecoder) == 0)
      return kLenNumLowSymbols + m_MidCoder[posState].Decode(rangeDecoder);
    return kLenNumLowSymbols + kLenNumMidSymbols + m_HighCoder.Decode(rangeDecoder);
  }
};

#endif
