// lzmaNSIS.h

#include "7zip/Compress/LZMA_SMALL/LZMAState.h"

#ifndef __LZMANSIS_H
#define __LZMANSIS_H

#ifdef __cplusplus
extern "C" {
#endif

void __stdcall lzmaInit(CLZMAStateP lzmaState);
int __stdcall lzmaDecompress(CLZMAStateP lzmaState);

#ifdef __cplusplus
}
#endif


#endif
