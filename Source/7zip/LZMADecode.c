/*
LzmaDecode.c
LZMA Decoder
LZMA SDK 4.01 Copyright (c) 1999-2004 Igor Pavlov (2004-02-15)

Modified by Amir Szekely
*/

#include "LzmaDecode.h"

#ifndef Byte
#define Byte unsigned char
#endif

#define kNumTopBits 24
#define kTopValue ((UInt32)1 << kNumTopBits)

#define kNumBitModelTotalBits 11
#define kBitModelTotal (1 << kNumBitModelTotalBits)
#define kNumMoveBits 5

void LZMACALL LZMAGetIO(CLZMAStateP lzmaState)
{
  InterlockedExchange(&lzmaState->sync_state, 0);
  while (InterlockedCompareExchange(&lzmaState->sync_state, 2, 1) != 1)
    Sleep(1);
}

Byte LZMACALL RangeDecoderReadByte(CLZMAStateP lzmaState)
{
  if (!lzmaState->avail_in)
  {
    LZMAGetIO(lzmaState);
    if (!lzmaState->avail_in)
    {
      lzmaState->Result = LZMA_RESULT_DATA_ERROR;
      return 0xFF;
    }
  }
  lzmaState->avail_in--;
  return (*lzmaState->next_in++);
}

#define ReadByte (RangeDecoderReadByte(lzmaState))

FORCE_INLINE void LZMACALL RangeDecoderInit(CLZMAStateP lzmaState)
{
  int i = 5;
  lzmaState->Code = 0;
  lzmaState->Range = (0xFFFFFFFF);
  while (i--)
    lzmaState->Code = (lzmaState->Code << 8) | ReadByte;
}

#define RC_INIT_VAR UInt32 range = lzmaState->Range; UInt32 code = lzmaState->Code;        
#define RC_FLUSH_VAR lzmaState->Range = range; lzmaState->Code = code;
#define RC_NORMALIZE if (range < kTopValue) { range <<= 8; code = (code << 8) | ReadByte; }

FORCE_INLINE UInt32 LZMACALL RangeDecoderDecodeDirectBits(CLZMAStateP lzmaState, int numTotalBits)
{
  RC_INIT_VAR
  UInt32 result = 0;
  int i;
  for (i = numTotalBits; i > 0; i--)
  {
    /* UInt32 t; */
    range >>= 1;

    result <<= 1;
    if (code >= range)
    {
      code -= range;
      result |= 1;
    }
    /*
    t = (code - range) >> 31;
    t &= 1;
    code -= range & (t - 1);
    result = (result + result) | (1 - t);
    */
    RC_NORMALIZE
  }
  RC_FLUSH_VAR
  return result;
}

int LZMACALL RangeDecoderBitDecode(CProb *prob, CLZMAStateP lzmaState)
{
  UInt32 bound = (lzmaState->Range >> kNumBitModelTotalBits) * *prob;
  if (lzmaState->Code < bound)
  {
    lzmaState->Range = bound;
    *prob += (kBitModelTotal - *prob) >> kNumMoveBits;
    if (lzmaState->Range < kTopValue)
    {
      lzmaState->Code = (lzmaState->Code << 8) | ReadByte;
      lzmaState->Range <<= 8;
    }
    return 0;
  }
  else
  {
    lzmaState->Range -= bound;
    lzmaState->Code -= bound;
    *prob -= (*prob) >> kNumMoveBits;
    if (lzmaState->Range < kTopValue)
    {
      lzmaState->Code = (lzmaState->Code << 8) | ReadByte;
      lzmaState->Range <<= 8;
    }
    return 1;
  }
}

#define RC_GET_BIT2(prob, mi, A0, A1) \
  UInt32 bound = (range >> kNumBitModelTotalBits) * *prob; \
  if (code < bound) \
    { A0; range = bound; *prob += (kBitModelTotal - *prob) >> kNumMoveBits; mi <<= 1; } \
  else \
    { A1; range -= bound; code -= bound; *prob -= (*prob) >> kNumMoveBits; mi = (mi + mi) + 1; } \
  RC_NORMALIZE

#define RC_GET_BIT(prob, mi) RC_GET_BIT2(prob, mi, ; , ;)               

int LZMACALL RangeDecoderBitTreeDecode(CProb *probs, int numLevels, CLZMAStateP lzmaState)
{
  int mi = 1;
  int i;
  #ifdef _LZMA_LOC_OPT
  RC_INIT_VAR
  #endif
  for(i = numLevels; i > 0; i--)
  {
    #ifdef _LZMA_LOC_OPT
    CProb *prob = probs + mi;
    RC_GET_BIT(prob, mi)
    #else
    mi = (mi + mi) + RangeDecoderBitDecode(probs + mi, lzmaState);
    #endif
  }
  #ifdef _LZMA_LOC_OPT
  RC_FLUSH_VAR
  #endif
  return mi - (1 << numLevels);
}

int LZMACALL RangeDecoderReverseBitTreeDecode(CProb *probs, int numLevels, CLZMAStateP lzmaState)
{
  int mi = 1;
  int i;
  int symbol = 0;
  #ifdef _LZMA_LOC_OPT
  RC_INIT_VAR
  #endif
  for(i = 0; i < numLevels; i++)
  {
    #ifdef _LZMA_LOC_OPT
    CProb *prob = probs + mi;
    RC_GET_BIT2(prob, mi, ; , symbol |= (1 << i))
    #else
    int bit = RangeDecoderBitDecode(probs + mi, lzmaState);
    mi = mi + mi + bit;
    symbol |= (bit << i);
    #endif
  }
  #ifdef _LZMA_LOC_OPT
  RC_FLUSH_VAR
  #endif
  return symbol;
}

FORCE_INLINE Byte LZMACALL LzmaLiteralDecode(CProb *probs, CLZMAStateP lzmaState)
{ 
  int symbol = 1;
  #ifdef _LZMA_LOC_OPT
  RC_INIT_VAR
  #endif
  do
  {
    #ifdef _LZMA_LOC_OPT
    CProb *prob = probs + symbol;
    RC_GET_BIT(prob, symbol)
    #else
    symbol = (symbol + symbol) | RangeDecoderBitDecode(probs + symbol, lzmaState);
    #endif
  }
  while (symbol < 0x100);
  #ifdef _LZMA_LOC_OPT
  RC_FLUSH_VAR
  #endif
  return symbol;
}

FORCE_INLINE Byte LZMACALL LzmaLiteralDecodeMatch(CProb *probs, CLZMAStateP lzmaState, Byte matchByte)
{ 
  int symbol = 1;
  #ifdef _LZMA_LOC_OPT
  RC_INIT_VAR
  #endif
  do
  {
    int bit;
    int matchBit = (matchByte >> 7) & 1;
    matchByte <<= 1;
    #ifdef _LZMA_LOC_OPT
    {
      CProb *prob = probs + ((1 + matchBit) << 8) + symbol;
      RC_GET_BIT2(prob, symbol, bit = 0, bit = 1)
    }
    #else
    bit = RangeDecoderBitDecode(probs + ((1 + matchBit) << 8) + symbol, lzmaState);
    symbol = (symbol << 1) | bit;
    #endif
    if (matchBit != bit)
    {
      while (symbol < 0x100)
      {
        #ifdef _LZMA_LOC_OPT
        CProb *prob = probs + symbol;
        RC_GET_BIT(prob, symbol)
        #else
        symbol = (symbol + symbol) | RangeDecoderBitDecode(probs + symbol, lzmaState);
        #endif
      }
      break;
    }
  }
  while (symbol < 0x100);
  #ifdef _LZMA_LOC_OPT
  RC_FLUSH_VAR
  #endif
  return symbol;
}

#define kNumPosBitsMax 4
#define kNumPosStatesMax (1 << kNumPosBitsMax)

#define kLenNumLowBits 3
#define kLenNumLowSymbols (1 << kLenNumLowBits)
#define kLenNumMidBits 3
#define kLenNumMidSymbols (1 << kLenNumMidBits)
#define kLenNumHighBits 8
#define kLenNumHighSymbols (1 << kLenNumHighBits)

#define LenChoice 0
#define LenChoice2 (LenChoice + 1)
#define LenLow (LenChoice2 + 1)
#define LenMid (LenLow + (kNumPosStatesMax << kLenNumLowBits))
#define LenHigh (LenMid + (kNumPosStatesMax << kLenNumMidBits))
#define kNumLenProbs (LenHigh + kLenNumHighSymbols) 

int LZMACALL LzmaLenDecode(CProb *p, CLZMAStateP lzmaState, int posState)
{
  if(RangeDecoderBitDecode(p + LenChoice, lzmaState) == 0)
    return RangeDecoderBitTreeDecode(p + LenLow +
        (posState << kLenNumLowBits), kLenNumLowBits, lzmaState);
  if(RangeDecoderBitDecode(p + LenChoice2, lzmaState) == 0)
    return kLenNumLowSymbols + RangeDecoderBitTreeDecode(p + LenMid +
        (posState << kLenNumMidBits), kLenNumMidBits, lzmaState);
  return kLenNumLowSymbols + kLenNumMidSymbols + 
      RangeDecoderBitTreeDecode(p + LenHigh, kLenNumHighBits, lzmaState);
}

#define kNumStates 12

#define kStartPosModelIndex 4
#define kEndPosModelIndex 14
#define kNumFullDistances (1 << (kEndPosModelIndex >> 1))

#define kNumPosSlotBits 6
#define kNumLenToPosStates 4

#define kNumAlignBits 4
#define kAlignTableSize (1 << kNumAlignBits)

#define kMatchMinLen 2

#define IsMatch 0
#define IsRep (IsMatch + (kNumStates << kNumPosBitsMax))
#define IsRepG0 (IsRep + kNumStates)
#define IsRepG1 (IsRepG0 + kNumStates)
#define IsRepG2 (IsRepG1 + kNumStates)
#define IsRep0Long (IsRepG2 + kNumStates)
#define PosSlot (IsRep0Long + (kNumStates << kNumPosBitsMax))
#define SpecPos (PosSlot + (kNumLenToPosStates << kNumPosSlotBits))
#define Align (SpecPos + kNumFullDistances - kEndPosModelIndex)
#define LenCoder (Align + kAlignTableSize)
#define RepLenCoder (LenCoder + kNumLenProbs)
#define Literal (RepLenCoder + kNumLenProbs)

#if Literal != LZMA_BASE_SIZE
StopCompilingDueBUG
#endif

int LZMACALL LzmaDecoderInit(CLZMAStateP lzmaState)
{
  CProb *p = (CProb *) lzmaState->DynamicData;
  UInt32 numProbs = Literal + ((UInt32)LZMA_LIT_SIZE << (lzmaState->lc + lzmaState->lp));
  if (lzmaState->DynamicDataSize < numProbs * sizeof(CProb))
    return LZMA_RESULT_NOT_ENOUGH_MEM;
  lzmaState->Result = LZMA_RESULT_OK;
  lzmaState->DictionaryPos = 0;
  lzmaState->GlobalPos = 0;
  lzmaState->Reps[0] = lzmaState->Reps[1] = lzmaState->Reps[2] = lzmaState->Reps[3] = 1;
  lzmaState->State = 0;
  lzmaState->PreviousIsMatch = 0;
  lzmaState->RemainLen = 0;
  lzmaState->Dictionary[lzmaState->DictionarySize - 1] = 0;
  while (numProbs--)
    p[numProbs] = kBitModelTotal >> 1; 
  RangeDecoderInit(lzmaState);
  return LZMA_RESULT_OK;
}

int LZMACALL LzmaDecode(CLZMAStateP lzmaState)
{
  CProb *p = (CProb *) lzmaState->DynamicData;
  int state = lzmaState->State;
  int previousIsMatch = lzmaState->PreviousIsMatch;
  Byte previousByte;
  UInt32 rep0 = lzmaState->Reps[0], rep1 = lzmaState->Reps[1], rep2 = lzmaState->Reps[2], rep3 = lzmaState->Reps[3];
  UInt32 nowPos = 0;
  UInt32 posStateMask = (1 << (lzmaState->pb)) - 1;
  UInt32 literalPosMask = (1 << (lzmaState->lp)) - 1;
  int lc = lzmaState->lc;
  int len = lzmaState->RemainLen;
  UInt32 globalPos = lzmaState->GlobalPos;

  Byte *dictionary = lzmaState->Dictionary;
  UInt32 dictionarySize = lzmaState->DictionarySize;
  UInt32 dictionaryPos = lzmaState->DictionaryPos;

  UInt32 outSize = lzmaState->avail_out;
  unsigned char *outStream = lzmaState->next_out;

  if (len == -1)
    return LZMA_RESULT_OK;

  while(len > 0 && nowPos < outSize)
  {
    UInt32 pos = dictionaryPos - rep0;
    if (pos >= dictionarySize)
      pos += dictionarySize;
    outStream[nowPos++] = dictionary[dictionaryPos] = dictionary[pos];
    if (++dictionaryPos == dictionarySize)
      dictionaryPos = 0;
    len--;
  }
  if (dictionaryPos == 0)
    previousByte = dictionary[dictionarySize - 1];
  else
    previousByte = dictionary[dictionaryPos - 1];

  while(nowPos < outSize)
  {
    int posState = (int)((nowPos + globalPos) & posStateMask);
    if (lzmaState->Result != LZMA_RESULT_OK)
      return lzmaState->Result;
    if (RangeDecoderBitDecode(p + IsMatch + (state << kNumPosBitsMax) + posState, lzmaState) == 0)
    {
      CProb *probs = p + Literal + (LZMA_LIT_SIZE * 
        ((((nowPos + globalPos) & literalPosMask) << lc) + (previousByte >> (8 - lc))));

      if (state < 4) state = 0;
      else if (state < 10) state -= 3;
      else state -= 6;
      if (previousIsMatch)
      {
        Byte matchByte;
        UInt32 pos = dictionaryPos - rep0;
        if (pos >= dictionarySize)
          pos += dictionarySize;
        matchByte = dictionary[pos];
        previousByte = LzmaLiteralDecodeMatch(probs, lzmaState, matchByte);
        previousIsMatch = 0;
      }
      else
        previousByte = LzmaLiteralDecode(probs, lzmaState);
      outStream[nowPos++] = previousByte;
      dictionary[dictionaryPos] = previousByte;
      if (++dictionaryPos == dictionarySize)
        dictionaryPos = 0;
    }
    else             
    {
      previousIsMatch = 1;
      if (RangeDecoderBitDecode(p + IsRep + state, lzmaState) == 1)
      {
        if (RangeDecoderBitDecode(p + IsRepG0 + state, lzmaState) == 0)
        {
          if (RangeDecoderBitDecode(p + IsRep0Long + (state << kNumPosBitsMax) + posState, lzmaState) == 0)
          {
            UInt32 pos;
            if ((nowPos + globalPos) == 0)
              return LZMA_RESULT_DATA_ERROR;
            state = state < 7 ? 9 : 11;
            pos = dictionaryPos - rep0;
            if (pos >= dictionarySize)
              pos += dictionarySize;
            previousByte = dictionary[pos];
            dictionary[dictionaryPos] = previousByte;
            if (++dictionaryPos == dictionarySize)
              dictionaryPos = 0;
            outStream[nowPos++] = previousByte;
            continue;
          }
        }
        else
        {
          UInt32 distance;
          if(RangeDecoderBitDecode(p + IsRepG1 + state, lzmaState) == 0)
            distance = rep1;
          else 
          {
            if(RangeDecoderBitDecode(p + IsRepG2 + state, lzmaState) == 0)
              distance = rep2;
            else
            {
              distance = rep3;
              rep3 = rep2;
            }
            rep2 = rep1;
          }
          rep1 = rep0;
          rep0 = distance;
        }
        len = LzmaLenDecode(p + RepLenCoder, lzmaState, posState);
        state = state < 7 ? 8 : 11;
      }
      else
      {
        int posSlot;
        rep3 = rep2;
        rep2 = rep1;
        rep1 = rep0;
        state = state < 7 ? 7 : 10;
        len = LzmaLenDecode(p + LenCoder, lzmaState, posState);
        posSlot = RangeDecoderBitTreeDecode(p + PosSlot +
            ((len < kNumLenToPosStates ? len : kNumLenToPosStates - 1) << 
            kNumPosSlotBits), kNumPosSlotBits, lzmaState);
        if (posSlot >= kStartPosModelIndex)
        {
          int numDirectBits = ((posSlot >> 1) - 1);
          rep0 = ((2 | ((UInt32)posSlot & 1)) << numDirectBits);
          if (posSlot < kEndPosModelIndex)
          {
            rep0 += RangeDecoderReverseBitTreeDecode(
                p + SpecPos + rep0 - posSlot - 1, numDirectBits, lzmaState);
          }
          else
          {
            rep0 += RangeDecoderDecodeDirectBits(lzmaState, 
                numDirectBits - kNumAlignBits) << kNumAlignBits;
            rep0 += RangeDecoderReverseBitTreeDecode(p + Align, kNumAlignBits, lzmaState);
          }
        }
        else
          rep0 = posSlot;
        rep0++;
      }
      if (rep0 == (UInt32)(0))
      {
        /* it's for stream version */
        len = -1;
        break;
      }
      if (rep0 > nowPos + globalPos)
      {
        return LZMA_RESULT_DATA_ERROR;
      }
      len += kMatchMinLen;
      do
      {
        UInt32 pos = dictionaryPos - rep0;
        if (pos >= dictionarySize)
          pos += dictionarySize;
        previousByte = dictionary[pos];
        dictionary[dictionaryPos] = previousByte;
        if (++dictionaryPos == dictionarySize)
          dictionaryPos = 0;
        outStream[nowPos++] = previousByte;
        len--;
      }
      while(len > 0 && nowPos < outSize);
    }
  }

  lzmaState->DictionaryPos = dictionaryPos;
  lzmaState->GlobalPos = globalPos + nowPos;
  lzmaState->Reps[0] = rep0;
  lzmaState->Reps[1] = rep1;
  lzmaState->Reps[2] = rep2;
  lzmaState->Reps[3] = rep3;
  lzmaState->State = state;
  lzmaState->PreviousIsMatch = previousIsMatch;
  lzmaState->RemainLen = len;

  return nowPos;
}

// interface

void LZMACALL lzmaInit(CLZMAStateP lzmaState)
{
  if (lzmaState->hThread)
  {
    CloseHandle(lzmaState->hThread);
    lzmaState->hThread = NULL;
  }

  lzmaState->sync_state = 1;

  lzmaState->finished = FALSE;
}

#define kPropertiesSize 5

DWORD WINAPI lzmaDecompressThread(LPVOID lpParameter)
{
  CLZMAStateP lzmaState = (CLZMAStateP) lpParameter;

  LPBYTE properties;
  BYTE firstByte;
  UINT32 dictionarySize;

  int res;

  lzmaState->res = -4;
  if (lzmaState->avail_in < kPropertiesSize)
  {
    goto finished;
  }
  properties = lzmaState->next_in;
  lzmaState->avail_in -= kPropertiesSize;
  lzmaState->next_in += kPropertiesSize;

  firstByte = properties[0];

  if (firstByte > (9*5*5))
  {
    goto finished;
  }

  lzmaState->pb = firstByte / (9*5);
  firstByte %= (9*5);
  lzmaState->lp = firstByte / 9;
  firstByte %= 9;
  lzmaState->lc = firstByte;
  
  lzmaState->DynamicDataSize = LZMA_BASE_SIZE + (LZMA_LIT_SIZE << (lzmaState->lc + lzmaState->pb));
  lzmaState->DynamicDataSize *= sizeof(CProb);

  if (!lzmaState->DynamicData || firstByte != lzmaState->FirstProp)
  {
    if (lzmaState->DynamicData)
      GlobalFree(lzmaState->DynamicData);
    lzmaState->DynamicData = LZMAAlloc(lzmaState->DynamicDataSize);
    lzmaState->FirstProp = firstByte;
  }

  dictionarySize = *(UINT32 *)(properties + 1);
  if (dictionarySize != lzmaState->DictionarySize)
  {
    if (lzmaState->Dictionary)
      GlobalFree(lzmaState->Dictionary);
    lzmaState->Dictionary = LZMAAlloc(dictionarySize);
    lzmaState->DictionarySize = dictionarySize;
  }

  LzmaDecoderInit(lzmaState);

  for (;;)
  {
    res = LzmaDecode(lzmaState);
    if (res <= 0)
      break;

    lzmaState->next_out += res;
    lzmaState->avail_out -= res;

    LZMAGetIO(lzmaState);
  }

  lzmaState->res = 1;
  if (res < 0)
    lzmaState->res = res;

finished:

  lzmaState->finished = TRUE;
  InterlockedExchange(&lzmaState->sync_state, 0);

  return 0;
}

int LZMACALL lzmaDecompress(CLZMAStateP lzmaState)
{
  if (lzmaState->finished)
    return lzmaState->res;

  if (!lzmaState->hThread)
  {
    DWORD dwThreadId;
    lzmaState->hThread = CreateThread(0, 0, lzmaDecompressThread, (LPVOID) lzmaState, 0, &dwThreadId);
    if (!lzmaState->hThread)
      return -4;
  }
  else
    InterlockedExchange(&lzmaState->sync_state, 1);

  while (InterlockedCompareExchange(&lzmaState->sync_state, 2, 0) != 0)
    Sleep(1);

  if (lzmaState->finished)
    return lzmaState->res;

  return 0;
}