#ifndef _EXEDATA_H_
#define _EXEDATA_H_

#include <cstddef>

// TODO: these should live in a singleton
extern size_t zlib_exehead_size;
extern size_t bzip2_exehead_size;
extern size_t lzma_exehead_size;
extern size_t exeheader_size;
extern size_t icondata_size;
extern size_t unicondata_size;

extern unsigned char zlib_exehead[];
extern unsigned char bzip2_exehead[];
extern unsigned char lzma_exehead[];
extern unsigned char icon_data[];
extern unsigned char unicon_data[];
extern unsigned char bitmap1_data[630];

#endif //_EXEDATA_H_
