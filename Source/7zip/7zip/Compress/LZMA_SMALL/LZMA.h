// LZMA.h

#include "LZMALenCoder.h"

#ifndef __LZMA_H
#define __LZMA_H

const int kNumRepDistances = 4;

const int  kNumStates = 12;

const BYTE kLiteralNextStates[kNumStates] = {0, 0, 0, 0, 1, 2, 3, 4,  5,  6,   4, 5};
const BYTE kMatchNextStates[kNumStates]   = {7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10};
const BYTE kRepNextStates[kNumStates]     = {8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11};
const BYTE kShortRepNextStates[kNumStates]= {9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11};

class CState
{
public:
  int Index;
  void Init()           { Index = 0; }
  void UpdateChar()     { Index = kLiteralNextStates[Index]; }
  void UpdateMatch()    { Index = kMatchNextStates[Index]; }
  void UpdateRep()      { Index = kRepNextStates[Index]; }
  void UpdateShortRep() { Index = kShortRepNextStates[Index]; }
};

const int kNumPosSlotBits = 6; 

const int kNumLenToPosStates = 4;

/*
inline int GetLenToPosState(int len)
{
  if (len < kNumLenToPosStates + 2)
    return len - 2;
  return kNumLenToPosStates - 1;
}
*/

inline int GetLenToPosState2(int len)
{
  if (len < kNumLenToPosStates)
    return len;
  return kNumLenToPosStates - 1;
}

const int kMatchMinLen = 2;
const int kMatchMaxLen = kMatchMinLen + kLenNumSymbolsTotal - 1;

const int kNumAlignBits = 4;
const int kAlignTableSize = 1 << kNumAlignBits;

const int kStartPosModelIndex = 4;
const int kEndPosModelIndex = 14;
const int kNumPosModels = kEndPosModelIndex - kStartPosModelIndex;
const int kNumFullDistances = 1 << (kEndPosModelIndex / 2);

const int kNumLitPosStatesBitsEncodingMax = 4;
const int kNumLitContextBitsMax = 8;

#endif
