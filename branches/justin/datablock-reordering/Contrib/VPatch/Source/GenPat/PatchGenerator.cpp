//---------------------------------------------------------------------------
// PatchGenerator.cpp
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

#include "PatchGenerator.h"
#include <algorithm>

#define TARGET_BUFFER_SIZE 65536
#define TARGET_LOOKAHEAD_SIZE 4096
#define MAX_BLOCK_SIZE 16384
#define DEFAULT_MAX_MATCHES 500

PatchGenerator::PatchGenerator(bistream& source, TFileOffset sourceSize, bistream& target, TFileOffset targetSize, bostream& patch) :
  source(source), sourceSize(sourceSize), target(target), targetSize(targetSize), patch(patch),
  targetCData(NULL), targetCDataBaseOffset(0), targetCDataSize(0), blockSize(64), maxMatches(DEFAULT_MAX_MATCHES), beVerbose(false) {
  targetCData = new unsigned char[TARGET_BUFFER_SIZE];
}

PatchGenerator::~PatchGenerator() {
  if(targetCData != NULL) delete[] targetCData;
}

void PatchGenerator::execute(vector<SameBlock*>& sameBlocks) {
  ChunkedFile* sourceTree = new ChunkedFile(source,sourceSize,blockSize);

  // sameBlocks: this vector will store blocks that have been found to be the same
  // between files

  // the vector needs an 'empty' first block so checking for overlap with the
  // 'previous' block never fails
  SameBlock* firstBlock = new SameBlock;
  firstBlock->sourceOffset = 0;
  firstBlock->targetOffset = 0;
  firstBlock->size = 0;
  sameBlocks.push_back(firstBlock);

  targetCDataBaseOffset = 0;
  targetCDataSize = 0;
  bool firstRun = true;

  // currentOffset is in the target file
  for(TFileOffset currentOffset = 0; currentOffset < targetSize; ) {
    bool reloadTargetCData = true;
    if((currentOffset >= targetCDataBaseOffset) &&
       (currentOffset + TARGET_LOOKAHEAD_SIZE < targetCDataBaseOffset + TARGET_BUFFER_SIZE)) {

      if(firstRun) {
        firstRun = false;
      } else {
        reloadTargetCData = false;
      }
    }
    if(reloadTargetCData) {
      // at least support looking back blockSize, if possible (findBlock relies on this!)
      targetCDataBaseOffset = currentOffset - blockSize;

      // handle start of file correctly
      if(currentOffset < blockSize) targetCDataBaseOffset = 0;

      targetCDataSize = TARGET_BUFFER_SIZE;
      // check if this does not extend beyond EOF
      if(targetCDataBaseOffset + targetCDataSize > targetSize) {
        targetCDataSize = targetSize - targetCDataBaseOffset;
      }

      // we need to update the memory cache of target
      cout << "[CacheReload] File position = " << static_cast<unsigned int>(targetCDataBaseOffset) << "\n";

      target.seekg(targetCDataBaseOffset,ios::beg);
      target.read(reinterpret_cast<char*>(targetCData),targetCDataSize);
    }
    //cout << currentOffset << " ";

    SameBlock* currentSameBlock = findBlock(sourceTree,currentOffset);

    if(currentSameBlock) {
      // we have a match
      SameBlock* previousBlock = sameBlocks.back();
      if(previousBlock->targetOffset + previousBlock->size > currentSameBlock->targetOffset) {
        // there is overlap, resolve it
        TFileOffset difference = previousBlock->targetOffset + previousBlock->size - currentSameBlock->targetOffset;
        currentSameBlock->sourceOffset += difference;
        currentSameBlock->targetOffset += difference;
        currentSameBlock->size -= difference;
      }
      sameBlocks.push_back(currentSameBlock);

      // debug info
      if(beVerbose) {
        cout << "Block found: " << static_cast<unsigned int>(currentSameBlock->targetOffset)
             << " " << static_cast<unsigned int>(currentSameBlock->size)
             << " (source offset=" << static_cast<unsigned int>(currentSameBlock->sourceOffset) << ")\n";
      }

      currentOffset = currentSameBlock->targetOffset + currentSameBlock->size;
    } else {
      // no match, advance one byte
      currentOffset++;
    }
  }

  // add a block at the end, again to prevent bounds checking hassle
  SameBlock* lastBlock = new SameBlock;
  lastBlock->sourceOffset = 0;
  lastBlock->targetOffset = targetSize;
  lastBlock->size = 0;
  sameBlocks.push_back(lastBlock);
  delete sourceTree;
}

SameBlock* PatchGenerator::findBlock(ChunkedFile* sourceTree,
                                     TFileOffset targetFileStartOffset) {
  if(targetSize - targetFileStartOffset < blockSize) return NULL;

  TFileOffset preDataSize = targetFileStartOffset - targetCDataBaseOffset;
  //unsigned char* p = &(targetCData[preDataSize]);
//  SameBlock* currentSameBlock = findBlock(sourceTree,p,preDataSize,
//                                          targetCDataSize - preDataSize,
//                                          currentOffset);
                                     //unsigned char* targetData,  // target data contains a memory part of target stream
                                     //TFileOffset targetDataPreSize, // this specifies how many bytes we can access in front (required to be at least blockSize)
                                     //TFileOffset targetDataSize,    // this specifies how many bytes we can acces in the pointer (must be at least blockSize)

  // read the current data part into memory
  TChunkChecksum checksum;
  sourceTree->calculateChecksum(&(targetCData[preDataSize]),blockSize,checksum);

  TFileOffset foundIndex;  // location into sourceTree chunks array of found chunk
  if(sourceTree->search(checksum,&foundIndex)) {
    // we found something
    SameBlock* bestMatch = new SameBlock;
    bestMatch->sourceOffset = sourceTree->chunks[foundIndex].offset;
    bestMatch->targetOffset = targetFileStartOffset;
    bestMatch->size = 0;      // default to 0, because they can all be mismatches as well

    // increase match size if possible, also check if it is a match at all
    int matchCount = 0;
    while((sourceTree->chunks[foundIndex].checksum == checksum) && ((maxMatches == 0) || (matchCount < maxMatches))) {
      // check if this one is better than the current match
      SameBlock match;
      match.sourceOffset = sourceTree->chunks[foundIndex].offset;
      match.targetOffset = targetFileStartOffset;
      match.size = 0;      // default to 0, could be a mismatch with the same key
      improveSameBlockMatch(match,bestMatch->size);
      if(match.size > bestMatch->size) {
        *bestMatch = match;
      }
      foundIndex++;
      matchCount++;
    }
    if(beVerbose) {
      if(maxMatches != 0) {
        if(matchCount == maxMatches) {
          cout << "[FindBlock] Abort due to >" << static_cast<unsigned int>(maxMatches)
               << " matches; file position = " << static_cast<unsigned int>(targetFileStartOffset) << "\n";
        }
      }
    }
    if(bestMatch->size == 0)
      return NULL;
    else
      return bestMatch;
  } else {
    return NULL;
  }
}

#define COMPARISON_SIZE 2048

void PatchGenerator::improveSameBlockMatch(SameBlock& match, TFileOffset currentBest) {
  // we should now try to make the match longer by reading big chunks of the
  // files to come
  source.seekg(match.sourceOffset + match.size,ios::beg);
  target.seekg(match.targetOffset + match.size,ios::beg);

  while(true) {
    unsigned char sourceData[COMPARISON_SIZE];
    unsigned char targetData[COMPARISON_SIZE];

    TFileOffset startTarget = match.targetOffset + match.size;
    TFileOffset startSource = match.sourceOffset + match.size;
    TFileOffset checkSize = COMPARISON_SIZE;
    if(checkSize > targetSize - startTarget) checkSize = targetSize - startTarget;
    if(checkSize > sourceSize - startSource) checkSize = sourceSize - startSource;
    if(checkSize == 0) break;
    source.read(reinterpret_cast<char*>(sourceData),checkSize);
    target.read(reinterpret_cast<char*>(targetData),checkSize);
    TFileOffset i = 0;
    while((sourceData[i] == targetData[i]) && (i < checkSize )) {
      match.size++;
      i++;
    }
    // check if we stopped because we had a mismatch
    if(i < checkSize) break;
  }
  if(match.size < blockSize) {
    match.size = 0;
  } else {
    // try to improve before match if this is useful
    if(match.size + blockSize <= currentBest) return;
    // do not do if there is no more data in the target...
    if(match.targetOffset == 0) return;

    // we know it is stored in the cache... so we just need the source one
    unsigned char sourceData[MAX_BLOCK_SIZE];

    TFileOffset startSource = match.sourceOffset - blockSize;
    TFileOffset checkSize = blockSize;
    if(checkSize > match.sourceOffset) {
      checkSize = match.sourceOffset;
      startSource = 0;
    }
    if(checkSize == 0) return;
    source.seekg(startSource,ios::beg);
    source.read(reinterpret_cast<char*>(sourceData),checkSize);
    checkSize--;
    while(sourceData[checkSize] == targetCData[match.targetOffset - targetCDataBaseOffset - 1]) {
      match.targetOffset--;
      match.sourceOffset--;
      match.size++;
      checkSize--;
      if(checkSize == 0) break;
      if(match.targetOffset == 0) break;
    }
  }
}
