//---------------------------------------------------------------------------
// ChunkedFile.cpp
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


#include "ChunkedFile.h"

using namespace std;

      ChunkedFile::ChunkedFile(bistream& f, TFileOffset fSize, TFileOffset chunkSize) :
        chunks(NULL) {

        chunkCount = fSize / chunkSize;
        cout << "[ChunkedFile] Filesize of " << static_cast<unsigned int>(fSize) << " gives " << static_cast<unsigned int>(chunkCount) << " chunks.\n";

        cout << "[ChunkedFile] Memory to be used by those chunks: " << sizeof(FileChunk) * chunkCount << " bytes...";
        if(chunkCount == 0) {
          chunks = NULL;
          return;
        }
        chunks = new FileChunk[chunkCount];
        cout << " allocated.\n";

        unsigned char* data = new unsigned char[chunkSize];
        for(TFileOffset i = 0; i < chunkCount; i++) {
          f.read(reinterpret_cast<char*>(data),chunkSize);
          chunks[i].offset = i * chunkSize;
          calculateChecksum(data,chunkSize,chunks[i].checksum);
        }
        delete[] data;

        cout << "[ChunkedFile] Sorting chunks... ";
        std::sort(chunks,chunks + chunkCount);
        cout << "done.\n";
      }


bool ChunkedFile::search(TChunkChecksum key, TFileOffset* start) {
   // function:
   //   Searches sortedArray[first]..sortedArray[last] for key.
   // returns: index of the matching element if it finds key,
   //         otherwise  -(index where it could be inserted)-1.
   // parameters:
   //   sortedArray in  array of sorted (ascending) values.
   //   first, last in  lower and upper subscript bounds
   //   key         in  value to search for.
   // returns:
   //   index of key, or -insertion_position -1 if key is not
   //                 in the array. This value can easily be
   //                 transformed into the position to insert it.
  if(chunkCount == 0) return false;
  int first = 0;
  int last = chunkCount - 1;
  while (first <= last) {
    int mid = (first + last) / 2;  // compute mid point.
    if(key == chunks[mid].checksum) {
      while(true) {
        if(mid == 0) break;
        mid--;
        if(!(key == chunks[mid].checksum)) {
          mid++;
          break;
        }
      }
      *start = mid;
      return true;     // found it. return position
    }
    if (key < chunks[mid].checksum)
      last = mid - 1; // repeat search in bottom half.
    else
      first = mid + 1;  // repeat search in top half.
  }
  return false;    // failed to find key
}
