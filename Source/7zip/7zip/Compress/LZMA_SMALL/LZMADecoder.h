// LZMADecoder.h

#ifndef __LZMADECODER_H
#define __LZMADECODER_H

#include "LZMAConf.h"
#include "LZMAState.h"
#include "LZMA.h"
#include "LZMALiteralCoder.h"
#include "LZOutWindow.h"

class CPosSpecDecoder
{
  CBitDecoder m_Models[kNumFullDistances - kEndPosModelIndex];
public:
  void Init()
  {
    for(int i = 0; i < kNumFullDistances - kEndPosModelIndex; i++)
      m_Models[i].Init();
  }
  int Decode(CRangeDecoder *rangeDecoder, int slot)
  {
    int numLeveles = (slot >> 1) - 1;

    CBitDecoder *models = 
        m_Models + (((2 | (slot & 1)) << numLeveles)) - slot - 1;

    int modelIndex = 1;
    int symbol = 0;
    #ifdef __LOC_OPT
    RC_INIT_VAR
    #endif
    for(int bitIndex = 0; bitIndex < numLeveles; bitIndex++)
    {
      #ifdef __LOC_OPT
      RC_GETBIT2(models[modelIndex].Probability, modelIndex, ; , symbol |= (1 << bitIndex))
      #else
      int bit = models[modelIndex].Decode(rangeDecoder);
      modelIndex <<= 1;
      modelIndex += bit;
      symbol |= (bit << bitIndex);
      #endif
    }
    #ifdef __LOC_OPT
    RC_FLUSH_VAR
    #endif
    return symbol;
  };
};

class CLZMADecoder
{
  CLZOutWindow  m_OutWindowStream;
  CRangeDecoder m_RangeDecoder;

  struct
  {
    CBitDecoder MainChoiceDecoders[kNumStates][kLenNumPosStatesMax];
    CBitDecoder MatchRepShortChoiceDecoders[kNumStates][kLenNumPosStatesMax];
    CBitDecoder MatchChoiceDecoders[kNumStates];
    CBitDecoder MatchRepChoiceDecoders[kNumStates];
    CBitDecoder MatchRep1ChoiceDecoders[kNumStates];
    CBitDecoder MatchRep2ChoiceDecoders[kNumStates];
  } m_Decoders;

  CLZMALiteralDecoder m_LiteralDecoder;

  CBitTreeDecoder6 m_PosSlotDecoder[kNumLenToPosStates];
  CPosSpecDecoder m_PosDecoder;
  CReverseBitTreeDecoder4 m_PosAlignDecoder;
  
  CLZMALenDecoder m_LenDecoder;
  CLZMALenDecoder m_RepMatchLenDecoder;

  int m_PosStateMask;

public:
  void Create(BYTE *memoryPointer, 
      int numLiteralContextBits, 
      int numLiteralPosStateBits,
      int numPosStateBits);
  UINT32 Code(CLZMAStateP lzmaState);
};

#endif
