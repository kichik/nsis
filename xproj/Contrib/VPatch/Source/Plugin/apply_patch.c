//---------------------------------------------------------------------------
// apply_patch.c
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

#include "apply_patch.h"
#include "checksum.h"

/* ------------------------ patch application ----------------- */

#define BLOCKSIZE    16384

int DoPatch(HANDLE hPatch, HANDLE hSource, HANDLE hDest) {

  static char block[BLOCKSIZE];
    
  unsigned long temp = 0;
  unsigned long read;
  unsigned long source_crc = 0;
  md5_byte_t source_md5[16];
  unsigned long patch_dest_crc = 0;
  md5_byte_t patch_dest_md5[16];
  int MD5Mode = 0;
  unsigned long patches = 0;
  int already_uptodate = 0;
  
  FILETIME targetModifiedTime;
  
  // special 'addition' for the dll: since the patch file is now
  // in a seperate file, the VPAT header might be right at the start
  // of the file, and a pointer at the end of the file is probably missing
  // (because all patch generator versions don't append it, the linker/gui
  //  does this).
  SetFilePointer(hPatch, 0, NULL, FILE_BEGIN);
  ReadFile(hPatch, &temp, 4, &read, NULL);
  // it's not at the start of file -> there must be a pointer at the end of
  // file then
  if (temp != 0x54415056) {
    SetFilePointer(hPatch, -4, NULL, FILE_END);
    ReadFile(hPatch, &temp, 4, &read, NULL);

    SetFilePointer(hPatch, temp, NULL, FILE_BEGIN);
    ReadFile(hPatch, &temp, 4, &read, NULL);
    if (temp != 0x54415056)
      return PATCH_CORRUPT;
  }

  // target file date is by default the current system time
  GetSystemTimeAsFileTime(&targetModifiedTime);
  
  // read the number of patches in the file
  ReadFile(hPatch, &patches, 4, &read, NULL);
  if(patches & 0x80000000) MD5Mode = 1;
  // MSB is now reserved for future extensions, anyone wanting more than
  // 16 million patches in a single file is nuts anyway
  patches = patches & 0x00FFFFFF;
  
  if(!MD5Mode) {
    if (!FileCRC(hSource, &source_crc))
      return PATCH_ERROR;
  } else {
    if (!FileMD5(hSource, source_md5))
      return PATCH_ERROR; 
  }

  while (patches--) {
    long patch_blocks = 0, patch_size = 0;
    
    // flag which needs to be set by one of the checksum checks
    int currentPatchMatchesChecksum = 0;

    // read the number of blocks this patch has
    if(!ReadFile(hPatch, &patch_blocks, 4, &read, NULL)) {
      return PATCH_CORRUPT;
    }

    // read checksums
    if(!MD5Mode) {
      unsigned long patch_source_crc = 0;
      if(!ReadFile(hPatch, &patch_source_crc, 4, &read, NULL)) {
        return PATCH_CORRUPT;
      }
      if(!ReadFile(hPatch, &patch_dest_crc, 4, &read, NULL)) {
        return PATCH_CORRUPT;
      }
      // check to see if it's already up-to-date for some patch
      if (source_crc == patch_dest_crc) {
        already_uptodate = 1;
      }
      if (source_crc == patch_source_crc) {
        currentPatchMatchesChecksum = 1;
      }
    } else {
      int md5index;
      md5_byte_t patch_source_md5[16];
      if(!ReadFile(hPatch, patch_source_md5, 16, &read, NULL)) {
        return PATCH_CORRUPT;
      }
      if(!ReadFile(hPatch, patch_dest_md5, 16, &read, NULL)) {
        return PATCH_CORRUPT;
      }
      // check to see if it's already up-to-date for some patch
      for(md5index = 0; md5index < 16; md5index++) {
        if(source_md5[md5index] != patch_dest_md5[md5index]) break;
        if(md5index == 15) already_uptodate = 1;
      }
      for(md5index = 0; md5index < 16; md5index++) {
        if(source_md5[md5index] != patch_source_md5[md5index]) break;
        if(md5index == 15) currentPatchMatchesChecksum = 1;
      }
    }
    // read the size of the patch, we can use this to skip over it
    if(!ReadFile(hPatch, &patch_size, 4, &read, NULL)) {
      return PATCH_CORRUPT;
    }
    
    if(currentPatchMatchesChecksum) {
      while (patch_blocks--) {
        unsigned char blocktype = 0;
        unsigned long blocksize = 0;
        if(!ReadFile(hPatch, &blocktype, 1, &read, NULL)) {
          return PATCH_CORRUPT;
        }
        switch (blocktype) {
        case 1:
        case 2:
        case 3:
          if (blocktype == 1)
            { unsigned char x; blocksize = ReadFile(hPatch,&x,1,&read,NULL)? x: 0; }
          else if (blocktype == 2)
            { unsigned short x; blocksize = ReadFile(hPatch,&x,2,&read,NULL)? x: 0; }
          else
            { unsigned long x; blocksize = ReadFile(hPatch,&x,4,&read,NULL)? x:0; }

          if (!blocksize || !ReadFile(hPatch, &temp, 4, &read, NULL) || read != 4)
            return PATCH_CORRUPT;
    
          SetFilePointer(hSource, temp, 0, FILE_BEGIN);

          do {
            if(!ReadFile(hSource, block, min(BLOCKSIZE, blocksize), &read, NULL)) {
              return PATCH_ERROR;
            }
            WriteFile(hDest, block, read, &temp, NULL);
            if (temp != min(BLOCKSIZE, blocksize))
              return PATCH_ERROR;
            blocksize -= temp;
          } while (temp);

          break;

        case 5:
        case 6:
        case 7:
          if (blocktype == 5)
            { unsigned char x; blocksize = ReadFile(hPatch,&x,1,&read,NULL)? x:0; }
          else if (blocktype == 6)
            { unsigned short x; blocksize = ReadFile(hPatch,&x,2,&read,NULL)? x:0; }
          else
            { unsigned long x; blocksize = ReadFile(hPatch,&x,4,&read,NULL)? x:0; }

          if (!blocksize)
            return PATCH_CORRUPT;
          
          do {
            if(!ReadFile(hPatch, block, min(BLOCKSIZE, blocksize), &read, NULL)) {
              return PATCH_CORRUPT;
            }
            WriteFile(hDest, block, read, &temp, NULL);
            if (temp != min(BLOCKSIZE, blocksize))
              return PATCH_ERROR;
            blocksize -= temp;
          } while (temp);

          break;

        case 255:   // read the file modified time from the patch
          if(!ReadFile(hPatch,&targetModifiedTime,sizeof(targetModifiedTime),&read,NULL)) {
            return PATCH_CORRUPT;
          }
          break;

        default:
          return PATCH_CORRUPT;
        }
      }
      if(!MD5Mode) {
        unsigned long dest_crc = 0;
        if(!FileCRC(hDest, &dest_crc)) {
          return PATCH_ERROR;
        }
        if (dest_crc != patch_dest_crc)
          return PATCH_ERROR;
      } else {
        int md5index;
        md5_byte_t dest_md5[16];
        if(!FileMD5(hDest, dest_md5)) {
          return PATCH_ERROR;
        }
        for(md5index = 0; md5index < 16; md5index++) {
          if(dest_md5[md5index] != patch_dest_md5[md5index]) return PATCH_ERROR;
        }
      }
      // set file time
      SetFileTime(hDest, NULL, NULL, &targetModifiedTime);

      return PATCH_SUCCESS;
    } else {
      SetFilePointer(hPatch, patch_size, NULL, FILE_CURRENT);
    }
  }
  
  // if already up to date, it doesn't matter that we didn't match
  if(already_uptodate) {
    return PATCH_UPTODATE;  
  } else {
    return PATCH_NOMATCH;
  }
}
