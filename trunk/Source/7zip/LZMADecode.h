/* 
LzmaDecode.h
LZMA Decoder interface
LZMA SDK 4.01 Copyright (c) 1999-2004 Igor Pavlov (2004-02-15)
*/

#ifndef __LZMADECODE_H
#define __LZMADECODE_H

#include "../Platform.h"
#include "../exehead/util.h"

/* #define _LZMA_PROB32 */
/* It can increase speed on some 32-bit CPUs, 
   but memory usage will be doubled in that case */

#define _LZMA_LOC_OPT
/* Enable local speed optimizations inside code */

#ifndef LZMACALL
#  define LZMACALL
#endif

#ifndef FORCE_INLINE
#  define FORCE_INLINE
#endif

#ifndef UInt32
#ifdef _LZMA_UINT32_IS_ULONG
#define UInt32 unsigned long
#else
#define UInt32 unsigned int
#endif
#endif

#ifdef _LZMA_PROB32
#define CProb UInt32
#else
#define CProb unsigned short
#endif

#define LZMA_RESULT_OK 0
#define LZMA_RESULT_DATA_ERROR -1
// we don't really care what the problem is...
// #define LZMA_RESULT_NOT_ENOUGH_MEM -2
#define LZMA_RESULT_NOT_ENOUGH_MEM LZMA_RESULT_DATA_ERROR

#define LZMA_BASE_SIZE 1846
#define LZMA_LIT_SIZE 768

/* 
bufferSize = (LZMA_BASE_SIZE + (LZMA_LIT_SIZE << (lc + lp)))* sizeof(CProb)
by default CProb is unsigned short, 
but if specify _LZMA_PROB_32, CProb will be UInt32(unsigned int)
*/

typedef struct
{
  unsigned char FirstProp;

  unsigned char *DynamicData;
  UInt32 DynamicDataSize;

  unsigned char *Dictionary;
  UInt32 DictionarySize;
  UInt32 DictionaryPos;

  // range coder
  UInt32 Range;
  UInt32 Code;
  int Result;

  // others
  UInt32 GlobalPos;
  UInt32 Reps[4];
  int lc;
  int lp;
  int pb;
  int State;
  int PreviousIsMatch;
  int RemainLen;

  // io
  unsigned char *next_in;  /* next input byte */
  unsigned int avail_in;  /* number of bytes available at next_in */

  unsigned char *next_out; /* next output byte should be put there */
  unsigned int avail_out; /* remaining free space at next_out */

  // sync
  HANDLE hThread;
  long sync_state;

  // finish
  int finished;
  int res;
} CLZMAState, *CLZMAStateP;

int LZMACALL LzmaDecoderInit(CLZMAStateP lzmaState);
int LZMACALL LzmaDecode(CLZMAStateP lzmaState);

void LZMACALL lzmaInit(CLZMAStateP lzmaState);
int LZMACALL lzmaDecompress(CLZMAStateP lzmaState);

#define LZMAAlloc my_GlobalAlloc
#define LZMAFree GlobalFree
#define LZMAMemCopy mini_memcpy

#endif
