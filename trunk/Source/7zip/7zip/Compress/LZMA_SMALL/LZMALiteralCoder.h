// LZMALiteralCoder.h

#ifndef __LZMALITERALCODER_H
#define __LZMALITERALCODER_H

#include "RangeCoderBit.h"
#include "RangeCoderOpt.h"

class CLZMALiteralDecoder2
{
  CBitDecoder m_Decoders[3][1 << 8];
public:
  void Init()
  {
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < (1 << 8); j++)
        m_Decoders[i][j].Init();
  }
  BYTE DecodeNormal(CRangeDecoder *rangeDecoder)
  {
    int symbol = 1;
    #ifdef __LOC_OPT
    RC_INIT_VAR
    #endif
    do
    {
      #ifdef __LOC_OPT
      RC_GETBIT(m_Decoders[0][symbol].Probability, symbol)
      #else
      symbol = (symbol + symbol) | m_Decoders[0][symbol].Decode(rangeDecoder);
      #endif
    }
    while (symbol < 0x100);
    #ifdef __LOC_OPT
    RC_FLUSH_VAR
    #endif
    return symbol;
  }
  BYTE DecodeWithMatchByte(CRangeDecoder *rangeDecoder, BYTE matchByte)
  {
    int symbol = 1;
    #ifdef __LOC_OPT
    RC_INIT_VAR
    #endif
    do
    {
      int matchBit = (matchByte >> 7) & 1;
      matchByte <<= 1;
      #ifdef __LOC_OPT
      int bit;
      RC_GETBIT2(m_Decoders[1 + matchBit][symbol].Probability, symbol, 
          bit = 0, bit = 1)
      #else
      int bit = m_Decoders[1 + matchBit][symbol].Decode(rangeDecoder);
      symbol = (symbol + symbol) | bit;
      #endif
      if (matchBit != bit)
      {
        while (symbol < 0x100)
        {
          #ifdef __LOC_OPT
          RC_GETBIT(m_Decoders[0][symbol].Probability, symbol)
          #else
          symbol = (symbol + symbol) | m_Decoders[0][symbol].Decode(rangeDecoder);
          #endif
        }
        break;
      }
    }
    while (symbol < 0x100);
    #ifdef __LOC_OPT
    RC_FLUSH_VAR
    #endif
    return symbol;
  }
};

class CLZMALiteralDecoder
{
  CLZMALiteralDecoder2 *m_Coders;
  int  m_NumPrevBits;
  int m_PosMask;
public:
  CLZMALiteralDecoder(): m_Coders(0) {}
  UINT32 Create(BYTE *memory, int numPosBits, int numPrevBits)
  {
    m_PosMask = (1 << numPosBits) - 1;
    m_NumPrevBits = numPrevBits;
    int numStates = 1 << (numPrevBits + numPosBits);
    m_Coders = (CLZMALiteralDecoder2 *)memory;
    return sizeof(CLZMALiteralDecoder2) * numStates;
  }
  void Init()
  {
    int numStates = (m_PosMask + 1) << m_NumPrevBits;
    for (int i = 0; i < numStates; i++)
      m_Coders[i].Init();
  }
  int GetState(int pos, BYTE prevByte) const
    { return ((pos & m_PosMask) << m_NumPrevBits) + (prevByte >> (8 - m_NumPrevBits)); }
  BYTE DecodeNormal(CRangeDecoder *rangeDecoder, int pos, BYTE prevByte)
    { return m_Coders[GetState(pos, prevByte)].DecodeNormal(rangeDecoder); }
  BYTE DecodeWithMatchByte(CRangeDecoder *rangeDecoder, int pos, BYTE prevByte, BYTE matchByte)
    { return m_Coders[GetState(pos, prevByte)].DecodeWithMatchByte(rangeDecoder, matchByte); }
};

#endif
