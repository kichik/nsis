//---------------------------------------------------------------------------
// PatchGenerator.h
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

#if !defined(PatchGenerator_H)
  #define PatchGenerator_H

  #include <iostream>
  #include <vector>
  #include "GlobalTypes.h"
  #include "ChunkedFile.h"

  using namespace std;

  typedef struct SameBlock {
    TFileOffset sourceOffset;
    TFileOffset targetOffset;
    TFileOffset size;
  } SameBlock;

  class PatchGenerator {
  protected:
    bistream& source;
    TFileOffset sourceSize;
    bistream& target;
    TFileOffset targetSize;
    bostream& patch;

    // cache
    unsigned char* targetCData;
    TFileOffset targetCDataBaseOffset;
    TFileOffset targetCDataSize;

    SameBlock* findBlock(ChunkedFile* sourceTree,
                         TFileOffset targetFileStartOffset);
//    SameBlock* findBlock(ChunkedFile* sourceTree, unsigned char* targetData, TFileOffset targetStartOffset);
    void improveSameBlockMatch(SameBlock& match, TFileOffset currentBest = 0);
  public:
    TFileOffset blockSize;
    int maxMatches;
    bool beVerbose;

    // load in the source, target streams
    PatchGenerator(bistream& source, TFileOffset sourceSize, bistream& target, TFileOffset targetSize, bostream& patch);
    ~PatchGenerator();

    // construct the actual patch
    void execute(vector<SameBlock*>& sameBlocks);
  };

#endif // PatchGenerator_H
