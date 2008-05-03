//---------------------------------------------------------------------------
// FileFormat1.cpp
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

#include "FileFormat1.h"
#include "GlobalTypes.h"

#define MAGIC_VPAT 0x54415056

namespace FileFormat1 {
  void writeByte(bostream& patch, TFileOffset dw) {
    unsigned char b = dw & 0xFF;
    patch.write(reinterpret_cast<char*>(&b),sizeof(b));
  }
  void writeWord(bostream& patch, TFileOffset dw) {
    unsigned char b = dw & 0xFF;
    patch.write(reinterpret_cast<char*>(&b),sizeof(b));
    b = (dw & 0xFF00) >> 8;
    patch.write(reinterpret_cast<char*>(&b),sizeof(b));
  }
  void writeDword(bostream& patch, TFileOffset dw) {
    unsigned char b = dw & 0xFF;
    patch.write(reinterpret_cast<char*>(&b),sizeof(b));
    b = (dw & 0xFF00) >> 8;
    patch.write(reinterpret_cast<char*>(&b),sizeof(b));
    b = (dw & 0xFF0000) >> 16;
    patch.write(reinterpret_cast<char*>(&b),sizeof(b));
    b = (dw & 0xFF000000) >> 24;
    patch.write(reinterpret_cast<char*>(&b),sizeof(b));
  }

  void writeMD5(bostream& patch, md5_byte_t digest[16]) {
    for(int i = 0; i < 16; i++) {
      writeByte(patch, digest[i]);
    }
  }

  TFileOffset readDword(bistream& patch) {
    unsigned char b;
    patch.read(reinterpret_cast<char*>(&b),sizeof(b));
    TFileOffset dw = b;
    patch.read(reinterpret_cast<char*>(&b),sizeof(b));
    dw = dw | (b << 8);
    patch.read(reinterpret_cast<char*>(&b),sizeof(b));
    dw = dw | (b << 16);
    patch.read(reinterpret_cast<char*>(&b),sizeof(b));
    dw = dw | (b << 24);
    return dw;
  }

  void readMD5(bistream& patch, md5_byte_t digest[16]) {
    unsigned char b;
    for(int i = 0; i < 16; i++) {
      patch.read(reinterpret_cast<char*>(&b),sizeof(b));
      digest[i] = b;
    }
  }

  void writeFileCount(bostream& f, TFileOffset currentCount) {
    f.seekp(4,ios::beg);
    writeDword(f,currentCount);
  }

  TFileOffset removeExistingPatch(bistream& in, TFileOffset inSize, bostream& out, TChecksum* removeCRC, bool existanceIsError) {
    TFileOffset fileCount = 0x80000000;        // MD5 mode
    if(in.bad() || in.eof() || (inSize == 0)) {        // empty file/does not yet exist
      writeDword(out,MAGIC_VPAT);
      writeDword(out,fileCount);           // noFiles
      return fileCount;
    }
    // copy and do stuff
    if(readDword(in) != MAGIC_VPAT) {
      writeDword(out,MAGIC_VPAT);
      writeDword(out,fileCount);           // noFiles
      return fileCount;
    }
    fileCount = readDword(in);
    writeDword(out,MAGIC_VPAT);
    writeDword(out,fileCount);           // noFiles
    bool MD5Mode = (fileCount & 0x80000000) != 0;

    if(MD5Mode) removeCRC->mode = TChecksum::MD5;
    if(!MD5Mode) removeCRC->mode = TChecksum::CRC32;

    // top byte is reserved for extensions
    fileCount = fileCount & 0x00FFFFFF;

    TFileOffset tempCount = fileCount;
    for(TFileOffset i = 0; i < tempCount; i++) {
      TFileOffset startOffset = in.tellg();
      readDword(in);                           // noBlocks
      TChecksum sourceChecksum;
      if(!MD5Mode) {
        crc32_t sourceCRC = readDword(in);         // SourceCRC
        readDword(in);                             // TargetCRC
        sourceChecksum.loadCRC32(sourceCRC);
      } else {
        md5_byte_t digest[16];
        readMD5(in, digest);         // SourceCRC
        sourceChecksum.loadMD5(digest);
        readMD5(in, digest);         // TargetCRC
      }
      TFileOffset bodySize = readDword(in);    // bodySize
      in.seekg(bodySize,ios::cur);
      TFileOffset endOffset = in.tellg();
      if(sourceChecksum == *removeCRC) {
        if(existanceIsError) {
          throw "Source file with the exact same contents already exists in patch!\nUse /R option (replace) to replace it with this patch!";
        }
        fileCount--;
      } else {
        // copy this patch to out
        in.seekg(startOffset,ios::beg);
        TFileOffset size = endOffset-startOffset;
        char* buffer = new char[size];
        in.read(buffer,size);
        out.write(buffer,size);
        delete[] buffer;
      }
    }
    TFileOffset curPos = out.tellp();
    if(MD5Mode) fileCount = fileCount | 0x80000000;
    writeFileCount(out,fileCount);
    out.seekp(curPos,ios::beg);
    return fileCount;
  }

  void writePatch(bostream& patch, bistream& target, vector<SameBlock*>& sameBlocks, TChecksum* sourceCRC, TChecksum* targetCRC, TFileOffset currentFileCount, POSIX::ALT_FILETIME targetTime) {
    TFileOffset bodySize = 0;
    TFileOffset noBlocks = 0;
    TFileOffset noBlocksOffset = patch.tellp();
    writeDword(patch,noBlocks);
    if(sourceCRC->mode == TChecksum::MD5) {
      writeMD5(patch,sourceCRC->digest);           // sourceCRC
      writeMD5(patch,targetCRC->digest);           // targetCRC
    } else {
      writeDword(patch,sourceCRC->crc);           // sourceCRC
      writeDword(patch,targetCRC->crc);           // targetCRC
    }
    TFileOffset bodySizeOffset = patch.tellp();
    writeDword(patch,bodySize);

    for(vector<SameBlock*>::iterator iter = sameBlocks.begin(); iter != sameBlocks.end(); iter++) {
      SameBlock* current = *iter;

      // store current block
      if(current->size > 0) {
        // copy block from sourceFile
        if(current->size < 256) {
          writeByte(patch,1);
          writeByte(patch,current->size);
          bodySize += 2;
        } else if(current->size < 65536) {
          writeByte(patch,2);
          writeWord(patch,current->size);
          bodySize += 3;
        } else {
          writeByte(patch,3);
          writeDword(patch,current->size);
          bodySize += 5;
        }
        writeDword(patch,current->sourceOffset);
        bodySize += 4;
        noBlocks++;
      }
      iter++;
      if(iter == sameBlocks.end()) break;
      SameBlock* next = *iter;
      iter--;

      // calculate area inbetween this block and the next
      TFileOffset notFoundStart = current->targetOffset+current->size;
      if(notFoundStart > next->targetOffset) {
        throw "makeBinaryPatch input problem: there was overlap";
      }
      TFileOffset notFoundSize = next->targetOffset - notFoundStart;
      if(notFoundSize > 0) {
        // we need to include this area in the patch directly
        if(notFoundSize < 256) {
          writeByte(patch,5);
          writeByte(patch,notFoundSize);
          bodySize += 2;
        } else if(notFoundSize < 65536) {
          writeByte(patch,6);
          writeWord(patch,notFoundSize);
          bodySize += 3;
        } else {
          writeByte(patch,7);
          writeDword(patch,notFoundSize);
          bodySize += 5;
        }
        // copy from target...
        target.seekg(notFoundStart,ios::beg);
#define COPY_BUF_SIZE 4096
        char copyBuffer[COPY_BUF_SIZE];
        for(TFileOffset i = 0; i < notFoundSize; i += COPY_BUF_SIZE) {
          TFileOffset j = notFoundSize - i;
          if(j > COPY_BUF_SIZE) j = COPY_BUF_SIZE;
          target.read(copyBuffer,j);
          patch.write(copyBuffer,j);
        }
        bodySize += notFoundSize;
        noBlocks++;
      }
    }
    // we are done, now add just one extra block with the target file time
    writeByte(patch,255);
    writeDword(patch,targetTime.dwLowDateTime);
    writeDword(patch,targetTime.dwHighDateTime);
    noBlocks++;
    bodySize += 9;

    TFileOffset curPos = patch.tellp();
    patch.seekp(noBlocksOffset,ios::beg);
    writeDword(patch,noBlocks);
    patch.seekp(bodySizeOffset,ios::beg);
    writeDword(patch,bodySize);
    // do this at the end because it messes up file position
    writeFileCount(patch,++currentFileCount);
    patch.seekp(curPos,ios::beg);
  }
}
