#ifndef _EXEDATA_H_
#define _EXEDATA_H_

extern int zlib_exeheader_size;
extern int bzip2_exeheader_size;
extern int lzma_exeheader_size;
extern int exeheader_size;
extern int icondata_size;
extern int unicondata_size;

extern unsigned char zlib_header_data[];
extern unsigned char bzip2_header_data[];
extern unsigned char lzma_header_data[];
extern unsigned char icon_data[];
extern unsigned char unicon_data[];
extern unsigned char bitmap1_data[630];

#endif //_EXEDATA_H_