#include <Windows.h>

#ifndef __LZMA_STATE__H___
#define __LZMA_STATE__H___

typedef struct
{
  void *lzmaDecoder;
  void *DynamicData;
  void *Dictionary;
  UINT32 DictionarySize;
  BYTE FirstProp;

  HANDLE   hThread;   /* decompression thread */

  BYTE     *next_in;  /* next input byte */
  UINT     avail_in;  /* number of bytes available at next_in */

  BYTE     *next_out; /* next output byte should be put there */
  UINT     avail_out; /* remaining free space at next_out */

  CRITICAL_SECTION cs;
  BOOL             cs_initialized;
  BOOL             it_locked; /* installer thread locked */
  BOOL             dt_locked; /* decompression thread locked */

  BOOL     finished;
  int      res;
} CLZMAState;

typedef CLZMAState 
#ifndef __cplusplus
FAR 
#endif
*CLZMAStateP;

void __stdcall LZMAGetIO(CLZMAStateP lzmaState);

#endif