//---------------------------------------------------------------------------
// ChunkedFile.h
//---------------------------------------------------------------------------
//                           -=* VPatch *=-
//---------------------------------------------------------------------------
// Copyright (C) 2001-2005 Koen van de Sande / Van de Sande Productions
//---------------------------------------------------------------------------
// Website: http://www.tibed.net/vpatch
//
// This software is provided 'as-is', without any express or implied
// warranty.  In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

#if !defined(ChunkedFile_H)
  #define ChunkedFile_H

  #include "GlobalTypes.h"
  #include "adler32.h"
  #include <iostream>
  #include <algorithm>

  // private data type: the data tree information
  typedef struct TChunkChecksum {
    Checksum::uLong adler32;
    CHECKSUM_BLOCK v;
  } TChunkChecksum;

  inline bool operator<(const TChunkChecksum& a, const TChunkChecksum& b) {
    return (a.adler32 < b.adler32) ? true : (
            (a.adler32 == b.adler32) ? (a.v < b.v) : false
                                            );
  }
  inline bool operator==(const TChunkChecksum& a, const TChunkChecksum& b) {
    return (a.v == b.v) && (a.adler32 == b.adler32);
  }

  typedef struct FileChunk {
    TFileOffset offset;
    TChunkChecksum checksum;
  } FileChunk;

  inline bool operator<(const FileChunk& a, const FileChunk& b) {
    return a.checksum < b.checksum;
  }

  class ChunkedFile {
    public:
      TFileOffset chunkCount;
      FileChunk* chunks;

      ChunkedFile(bistream& f, TFileOffset fSize, TFileOffset chunkSize);

      ~ChunkedFile() {
        if(chunks != NULL) delete[] chunks;
      }

      bool search(TChunkChecksum key, TFileOffset* start);

      inline void calculateChecksum(unsigned char* data, TFileOffset size, TChunkChecksum& K) {
        K.v = *reinterpret_cast<CHECKSUM_BLOCK*>(data);
        K.adler32 = Checksum::adler32(1L,data,size);
      }
  };

#endif // ChunkedFile_H
