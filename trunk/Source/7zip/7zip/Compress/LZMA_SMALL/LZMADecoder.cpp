// LZMADecoder.cpp

#include "LZMADecoder.h"

void CLZMADecoder::Create(BYTE *memoryPointer,
    int numLiteralContextBits,
    int numLiteralPosStateBits,
    int numPosStateBits)
{

  int numPosStates = 1 << numPosStateBits;
  m_PosStateMask = numPosStates - 1;
  m_LiteralDecoder.Create(memoryPointer, numLiteralPosStateBits, numLiteralContextBits);
}

UINT32 CLZMADecoder::Code(CLZMAStateP lzmaState)
{
  m_RangeDecoder.Init(lzmaState);
  m_OutWindowStream.Init(lzmaState);

  int i;
  for (i = 0; i < sizeof(m_Decoders) / sizeof(CBitDecoder); i++)
  {
    ((CBitDecoder *) &m_Decoders)[i].Init();
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
    if (!m_Decoders.MainChoiceDecoders[state.Index][posState].Decode(&m_RangeDecoder))
    {
      state.UpdateChar();
      if(peviousIsMatch)
      {
        BYTE matchByte = m_OutWindowStream.GetOneByte(0 - repDistances[0]);
        previousByte = m_LiteralDecoder.DecodeWithMatchByte(&m_RangeDecoder,
            nowPos, previousByte, matchByte);
        peviousIsMatch = false;
      }
      else
        previousByte = m_LiteralDecoder.DecodeNormal(&m_RangeDecoder,
          nowPos, previousByte);

      m_OutWindowStream.PutOneByte(previousByte);

      nowPos++;
    }
    else
    {
      peviousIsMatch = true;
      UINT32 distance;
      int len;
      if(m_Decoders.MatchChoiceDecoders[state.Index].Decode(&m_RangeDecoder))
      {
        if(!m_Decoders.MatchRepChoiceDecoders[state.Index].Decode(&m_RangeDecoder))
        {
          if(!m_Decoders.MatchRepShortChoiceDecoders[state.Index][posState].Decode(&m_RangeDecoder))
          {
            state.UpdateShortRep();

            previousByte = m_OutWindowStream.GetOneByte(0 - repDistances[0]);
            m_OutWindowStream.PutOneByte(previousByte);

            nowPos++;
            continue;
          }
          distance = repDistances[0];
        }
        else
        {
          if(!m_Decoders.MatchRep1ChoiceDecoders[state.Index].Decode(&m_RangeDecoder))
            distance = repDistances[1];
          else
          {
            if (!m_Decoders.MatchRep2ChoiceDecoders[state.Index].Decode(&m_RangeDecoder))
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
      nowPos += len;
      // CopyBackBlock
      {
        UINT32 fromPos = m_OutWindowStream._pos - distance;
        if (fromPos >= m_OutWindowStream._windowSize)
          fromPos += m_OutWindowStream._windowSize;
        while (len--)
        {
          m_OutWindowStream._buffer[m_OutWindowStream._pos++] = m_OutWindowStream._buffer[fromPos++];
          if (fromPos >= m_OutWindowStream._windowSize)
            fromPos = 0;
          m_OutWindowStream.Flush();
        }
      }
      previousByte = m_OutWindowStream.GetOneByte((UINT32)(-1));
    }
  }
  m_OutWindowStream._windowSize = 0;
  m_OutWindowStream.Flush();
  return 0;
}
