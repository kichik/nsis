/*
 * BinInterop.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2017-2021 Anders Kjersem
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 */

#ifndef NSIS_BININTEROP_H
#define NSIS_BININTEROP_H

#include "Platform.h"
#include "tchar.h"
#include <stdio.h> // FILE*

FILE* MSTLB_fopen(const TCHAR*filepath, size_t*pResId = 0);
bool GetTLBVersion(const TCHAR *filepath, DWORD &high, DWORD &low);

bool GetDLLVersion(const TCHAR *filepath, DWORD &high, DWORD &low);

typedef struct GENERICIMAGEINFO {
  UINT32 Width, Height;
  INT32 RawHeight;
  WORD BPP, Planes;
  GENERICIMAGEINFO() : RawHeight(0) {}
  bool IsTopDownBitmap() const { return Height != (UINT32) RawHeight && RawHeight; }
} GENERICIMAGEINFO;

DWORD GetDIBHeaderInfo(const void*pData, size_t DataSize, GENERICIMAGEINFO&Info);
DWORD IsBMPFile(const void*pData, size_t DataSize, GENERICIMAGEINFO*pInfo = 0);
#define GetBMPFileHeaderSize IsBMPFile

inline WORD IsICOCURFile(const void*pData)
{
  WORD *p16 = (WORD*) pData, ico = 1, cur = 2, type, count;
  if (p16[0] == FIX_ENDIAN_INT16(0x0000))
    if ((type = FIX_ENDIAN_INT16(p16[1])) == ico || type == cur)
      return count = FIX_ENDIAN_INT16(p16[2]);
  return 0;
}
inline WORD IsICOCURFile(const void*pData, size_t DataSize)
{
  return DataSize > 6 ? IsICOCURFile(pData) : 0;
}

bool LoadImageCanLoadFile(const void*pData, size_t DataSize);
bool LoadImageCanLoadFile(const TCHAR *filepath);
#define LoadImageCanLoadFileFromResource LoadImageCanLoadFile

#endif //~ NSIS_BININTEROP_H
