// lzmaNSIS.h

#include <windows.h>

#include "lzmaNSIS.h"
#include "7zip/Compress/LZMA_SMALL/LZMAConf.h"
#include "7zip/Compress/LZMA_SMALL/LZMADecoder.h"

void __stdcall LZMAGetIO(CLZMAStateP lzmaState)
{
  LeaveCriticalSection(&lzmaState->cs);
  while (!lzmaState->it_locked)
    Sleep(0);
  
  lzmaState->dt_locked = FALSE;

  EnterCriticalSection(&lzmaState->cs);
  lzmaState->dt_locked = TRUE;

  while (lzmaState->it_locked)
    Sleep(0);
}

extern "C"
{

void __stdcall lzmaInit(CLZMAStateP lzmaState)
{
  if (lzmaState->hThread)
  {
    CloseHandle(lzmaState->hThread);
    lzmaState->hThread = NULL;
  }

  if (!lzmaState->lzmaDecoder)
    lzmaState->lzmaDecoder = LZMAAlloc(sizeof(CLZMADecoder));

  if (!lzmaState->cs_initialized)
  {
    InitializeCriticalSection(&lzmaState->cs);
    lzmaState->cs_initialized = TRUE;
  }

  lzmaState->it_locked = TRUE;
  lzmaState->dt_locked = FALSE;

  lzmaState->finished = FALSE;
}

DWORD WINAPI lzmaDecompressThread(LPVOID lpParameter)
{
  CLZMAStateP lzmaState = (CLZMAStateP) lpParameter;
  CLZMADecoder *lzmaDecodeder = (CLZMADecoder *) lzmaState->lzmaDecoder;

  EnterCriticalSection(&lzmaState->cs);
  lzmaState->dt_locked = TRUE;

  while (lzmaState->it_locked)
    Sleep(0);

  {
    const kPropertiesSize = 5;
    lzmaState->res = -4;
    if (lzmaState->avail_in < kPropertiesSize)
    {
      goto finished;
    }
    LPBYTE properties = lzmaState->next_in;
    lzmaState->avail_in -= kPropertiesSize;
    lzmaState->next_in += kPropertiesSize;

    BYTE firstByte = properties[0];

    if (firstByte > (9*5*5))
    {
      goto finished;
    }

    int numPosStateBits = firstByte / (9*5);
    firstByte %= (9*5);
    int numLiteralPosStateBits = firstByte / 9;
    firstByte %= 9;
    int numLiteralContextBits = firstByte;
    
    int memSize = (1 << (numLiteralContextBits + numLiteralPosStateBits)) * sizeof(CLZMALiteralDecoder2);

    if (lzmaState->DynamicData == 0 || firstByte != lzmaState->FirstProp)
    {
      if (lzmaState->DynamicData != 0)
        LZMAFree(lzmaState->DynamicData);
      lzmaState->DynamicData = LZMAAlloc(memSize);
      lzmaState->FirstProp = firstByte;
    }

    lzmaDecodeder->Create((LPBYTE) lzmaState->DynamicData, 
      numLiteralContextBits, numLiteralPosStateBits, numPosStateBits);

    UINT32 dictionarySize = 0;
    for (int i = 0; i < 4; i++)
      dictionarySize += ((UINT32)properties[1 + i]) << (i * 8);
    if (lzmaState->Dictionary == 0 || dictionarySize != lzmaState->DictionarySize)
    {
      if (lzmaState->Dictionary != 0)
        LZMAFree(lzmaState->Dictionary);
      lzmaState->Dictionary = LZMAAlloc(dictionarySize);
      lzmaState->DictionarySize = dictionarySize;
    }

    UINT32 res = lzmaDecodeder->Code(lzmaState);
    
    lzmaState->res = 1;
    if (res != 0)
      lzmaState->res = res;
  }

finished:

  lzmaState->finished = TRUE;

  LeaveCriticalSection(&lzmaState->cs);
  lzmaState->dt_locked = FALSE;
  return 0;
}

int __stdcall lzmaDecompress(CLZMAStateP lzmaState)
{
  if (!lzmaState->hThread)
  {
    DWORD dwThreadId;
    lzmaState->hThread = CreateThread(0, 0, lzmaDecompressThread, (LPVOID) lzmaState, 0, &dwThreadId);
    if (!lzmaState->hThread)
      return -4;
  }
  else
    LeaveCriticalSection(&lzmaState->cs);

  while (!lzmaState->dt_locked)
    Sleep(0);

  lzmaState->it_locked = FALSE;

  EnterCriticalSection(&lzmaState->cs);
  lzmaState->it_locked = TRUE;
  
  while (lzmaState->dt_locked)
  {
    if (lzmaState->finished)
    {
      LeaveCriticalSection(&lzmaState->cs);
      return lzmaState->res;
    }
    Sleep(0);
  }

  return 0;
}

} // extern "C"