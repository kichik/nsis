#ifndef __LZMACONF_H
#define __LZMACONF_H

// define __LOC_OPT  for some speed optimization:
// It converts some class-member variables to local variables
// before some loops and it use inline code substitution 

#define __LOC_OPT 
// #define __UNROLL

#include <windows.h>

#ifdef __cplusplus
extern "C" {
#endif
#include "../../../../exehead/util.h"
#ifdef __cplusplus
}
#endif

#define LZMAAlloc my_GlobalAlloc
#define LZMAFree GlobalFree
#define LZMAMemCopy mini_memcpy

#endif