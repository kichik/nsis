// RangeCoderBitTree.h

#ifndef __RANGECODERBITTREE_H
#define __RANGECODERBITTREE_H

#include "RangeCoderBit.h"
#include "RangeCoderOpt.h"

class CBitTreeDecoder3
{
  CBitDecoder m_Models[1 << 3];
public:
  void Init()
  {
    for(int i = 1; i < (1 << 3); i++)
      m_Models[i].Init();
  }
  int Decode(CRangeDecoder *rangeDecoder)
  {
    int modelIndex = 1;
    #ifdef __LOC_OPT
    RC_INIT_VAR
    #endif
    #ifndef __UNROLL
    for(int bitIndex = 3; bitIndex > 0; bitIndex--)
    #endif
    {
      #ifdef __LOC_OPT
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      #ifdef __UNROLL
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      #endif
      #else
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      #ifdef __UNROLL
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      #endif
      #endif
    }
    #ifdef __LOC_OPT
    RC_FLUSH_VAR
    #endif
    return modelIndex - (1 << 3);
  };
};

class CBitTreeDecoder6
{
  CBitDecoder m_Models[1 << 6];
public:
  void Init()
  {
    for(int i = 1; i < (1 << 6); i++)
      m_Models[i].Init();
  }
  int Decode(CRangeDecoder *rangeDecoder)
  {
    int modelIndex = 1;
    #ifdef __LOC_OPT
    RC_INIT_VAR
    #endif
    #ifndef __UNROLL
    for(int bitIndex = 6; bitIndex > 0; bitIndex--)
    #endif
    {
      #ifdef __LOC_OPT
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      #ifdef __UNROLL
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      #endif
      #else
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      #ifdef __UNROLL
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      #endif
      #endif
    }
    #ifdef __LOC_OPT
    RC_FLUSH_VAR
    #endif
    return modelIndex - (1 << 6);
  };
};

class CBitTreeDecoder8
{
  CBitDecoder m_Models[1 << 8];
public:
  void Init()
  {
    for(int i = 1; i < (1 << 8); i++)
      m_Models[i].Init();
  }
  int Decode(CRangeDecoder *rangeDecoder)
  {
    int modelIndex = 1;
    #ifdef __LOC_OPT
    RC_INIT_VAR
    #endif
    for(int bitIndex = 8; bitIndex > 0; bitIndex--)
    {
      #ifdef __LOC_OPT
      RC_GETBIT(m_Models[modelIndex].Probability, modelIndex)
      #else
      modelIndex = (modelIndex + modelIndex) + m_Models[modelIndex].Decode(rangeDecoder);
      #endif
    }
    #ifdef __LOC_OPT
    RC_FLUSH_VAR
    #endif
    return modelIndex - (1 << 8);
  };
};


class CReverseBitTreeDecoder4
{
  CBitDecoder m_Models[1 << 4];
public:
  void Init()
  {
    for(int i = 1; i < (1 << 4); i++)
      m_Models[i].Init();
  }
  int Decode(CRangeDecoder *rangeDecoder)
  {
    int modelIndex = 1;
    int symbol = 0;
    #ifdef __LOC_OPT
    RC_INIT_VAR
    #endif
    #ifndef __UNROLL
    for(int bitIndex = 0; bitIndex < 4; bitIndex++)
    #endif
    {
      #ifdef __LOC_OPT
      #ifndef __UNROLL
      RC_GETBIT2(m_Models[modelIndex].Probability, modelIndex, ; , symbol |= (1 << bitIndex))
      #else
      RC_GETBIT2(m_Models[modelIndex].Probability, modelIndex, ; , symbol |= (1 << 0))
      RC_GETBIT2(m_Models[modelIndex].Probability, modelIndex, ; , symbol |= (1 << 1))
      RC_GETBIT2(m_Models[modelIndex].Probability, modelIndex, ; , symbol |= (1 << 2))
      RC_GETBIT2(m_Models[modelIndex].Probability, modelIndex, ; , symbol |= (1 << 3))
      #endif
      #else
      #ifndef __UNROLL
      int bit = m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = modelIndex + modelIndex + bit;
      symbol |= (bit << bitIndex);
      #else
      int bit = m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = modelIndex + modelIndex + bit;
      symbol |= (bit << 0);
      bit = m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = modelIndex + modelIndex + bit;
      symbol |= (bit << 1);      
      bit = m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = modelIndex + modelIndex + bit;
      symbol |= (bit << 2);      
      bit = m_Models[modelIndex].Decode(rangeDecoder);
      modelIndex = modelIndex + modelIndex + bit;
      symbol |= (bit << 3);      
      #endif
      #endif
    }
    #ifdef __LOC_OPT
    RC_FLUSH_VAR
    #endif
    return symbol;
  }
};

#endif
