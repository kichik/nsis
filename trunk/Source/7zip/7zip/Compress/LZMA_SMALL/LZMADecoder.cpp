// LZMADecoder.cpp

#include "LZMADecoder.h"

UINT32 CLZMADecoder::Create(BYTE *memoryPointer,
    int numLiteralContextBits,
    int numLiteralPosStateBits,
    int numPosStateBits)
{

  int numPosStates = 1 << numPosStateBits;
  m_PosStateMask = numPosStates - 1;
  return m_LiteralDecoder.Create(memoryPointer, numLiteralPosStateBits, numLiteralContextBits);
}

UINT32 CLZMADecoder::Code(CLZMAStateP lzmaState)
{
  m_RangeDecoder.Init(lzmaState);
  m_OutWindowStream.Init(lzmaState);

  int i;
  for(i = 0; i < kNumStates; i++)
  {
    for (int j = 0; j <= m_PosStateMask; j++)
    {
      m_MainChoiceDecoders[i][j].Init();
      m_MatchRepShortChoiceDecoders[i][j].Init();
    }
    m_MatchChoiceDecoders[i].Init();
    m_MatchRepChoiceDecoders[i].Init();
    m_MatchRep1ChoiceDecoders[i].Init();
    m_MatchRep2ChoiceDecoders[i].Init();
  }

  m_LiteralDecoder.Init();

  for (i = 0; i < kNumLenToPosStates; i++)
    m_PosSlotDecoder[i].Init();
  m_PosDecoder.Init();
  m_PosAlignDecoder.Init();

  // m_LenDecoder.Init(m_PosStateMask + 1);
  // m_RepMatchLenDecoder.Init(m_PosStateMask + 1);
  m_LenDecoder.Init();
  m_RepMatchLenDecoder.Init();


  ////////////////////////
  // code

  CState state;
  state.Init();
  bool peviousIsMatch = false;
  BYTE previousByte = 0;
  // kNumRepDistances == 4
  UINT32 repDistances[kNumRepDistances] = {1, 1, 1, 1};

  /*for(i = 0 ; i < kNumRepDistances; i++)
    repDistances[i] = 1;*/

  UINT32 nowPos = 0;
  while(nowPos < 0xFFFFFFFF)
  {
    int posState = nowPos & m_PosStateMask;
    if (m_MainChoiceDecoders[state.Index][posState].Decode(&m_RangeDecoder) == 0)
    {
      state.UpdateChar();
      if(peviousIsMatch)
      {
        #ifdef __STREAM_VERSION
        BYTE matchByte = m_OutWindowStream.GetOneByte(0 - repDistances[0]);
        #else
        BYTE matchByte = *(outStream - repDistances[0]);
        #endif
        previousByte = m_LiteralDecoder.DecodeWithMatchByte(&m_RangeDecoder,
            nowPos, previousByte, matchByte);
        peviousIsMatch = false;
      }
      else
        previousByte = m_LiteralDecoder.DecodeNormal(&m_RangeDecoder,
          nowPos, previousByte);

      #ifdef __STREAM_VERSION
      m_OutWindowStream.PutOneByte(previousByte);
      #else
      *outStream++ = previousByte;
      #endif

      nowPos++;
    }
    else
    {
      peviousIsMatch = true;
      UINT32 distance;
      int len;
      if(m_MatchChoiceDecoders[state.Index].Decode(&m_RangeDecoder) == 1)
      {
        if(m_MatchRepChoiceDecoders[state.Index].Decode(&m_RangeDecoder) == 0)
        {
          if(m_MatchRepShortChoiceDecoders[state.Index][posState].Decode(&m_RangeDecoder) == 0)
          {
            state.UpdateShortRep();

            #ifdef __STREAM_VERSION
            previousByte = m_OutWindowStream.GetOneByte(0 - repDistances[0]);
            m_OutWindowStream.PutOneByte(previousByte);
            #else
            previousByte = *(outStream - repDistances[0]);
            *outStream++ = previousByte;
            #endif

            nowPos++;
            continue;
          }
          distance = repDistances[0];
        }
        else
        {
          if(m_MatchRep1ChoiceDecoders[state.Index].Decode(&m_RangeDecoder) == 0)
            distance = repDistances[1];
          else
          {
            if (m_MatchRep2ChoiceDecoders[state.Index].Decode(&m_RangeDecoder) == 0)
              distance = repDistances[2];
            else
            {
              distance = repDistances[3];
              repDistances[3] = repDistances[2];
            }
            repDistances[2] = repDistances[1];
          }
          repDistances[1] = repDistances[0];
          repDistances[0] = distance;
        }
        len = m_RepMatchLenDecoder.Decode(&m_RangeDecoder, posState);
        state.UpdateRep();
      }
      else
      {
        len = m_LenDecoder.Decode(&m_RangeDecoder, posState);
        state.UpdateMatch();
        int posSlot = m_PosSlotDecoder[GetLenToPosState2(len)].Decode(&m_RangeDecoder);
        if (posSlot >= kStartPosModelIndex)
        {
          int numDirectBits = ((posSlot >> 1) - 1);
          distance = ((2 | (posSlot & 1)) << numDirectBits);
          if (posSlot < kEndPosModelIndex)
            distance += m_PosDecoder.Decode(&m_RangeDecoder, posSlot);
          else
          {
            distance += (m_RangeDecoder.DecodeDirectBits(
                numDirectBits - kNumAlignBits) << kNumAlignBits);
            distance += m_PosAlignDecoder.Decode(&m_RangeDecoder);
          }
        }
        else
          distance = posSlot;
        distance++;
        repDistances[3] = repDistances[2];
        repDistances[2] = repDistances[1];
        repDistances[1] = repDistances[0];
        repDistances[0] = distance;
      }
      if (distance > nowPos || distance == 0)
      {
        // it's for stream version (without knowing uncompressed size)
        // if (distance >= _dictionarySizeCheck)
        if (distance == (UINT32)(0))
          break;
        return (-1);
      }

      len += kMatchMinLen;
      m_OutWindowStream.CopyBackBlock(m_OutWindowStream._pos - distance, len);
      previousByte = m_OutWindowStream.GetOneByte((UINT32)(-1));

      nowPos += len;
    }
  }
  m_OutWindowStream._windowSize = 0;
  m_OutWindowStream.Flush();
  return 0;
}
