//---------------------------------------------------------------------------
// main.cpp
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

#ifdef __BORLANDC__
  #pragma argsused
#endif

#if !defined(__BORLANDC__) && !defined(_MSC_VER)
  #include <unistd.h>
#endif

#include "GlobalTypes.h"
#include "POSIXUtil.h"
#include "Checksums.h"
#include "PatchGenerator.h"
#include "FileFormat1.h"

#include <fstream>
#include <sstream>

int main( int argc, char * argv[] ) {
  cout << "GenPat v3.1\n";
  cout << "===========\n\n(c) 2001-2005 Van de Sande Productions\n";
  cout << "Website: http://www.tibed.net/vpatch\n\n";

  string sourceFileName;
  string targetFileName;
  string patchFileName;

  bool showHelp = true;

  int blockSize = 64;
  int maxMatches = 500;
  bool beVerbose = false;
  bool beOptimal = false;
  bool existanceIsError = true;     // flag error if patch already has source's MD5/CRC32?
  int fileNameArgument = 0;
  if(argc > 1) {
    for(int i = 1; i < argc; i++) {
      string s(argv[i]);
      if(s.size() > 0) {
        if(s[0] == '/') {
          if(s.size() > 1) {
            if((s[1] == 'v') || (s[1] == 'V')) {
              beVerbose = true;
            }
            if((s[1] == 'o') || (s[1] == 'O')) {
              beOptimal = true;
            }
            if((s[1] == 'r') || (s[1] == 'R')) {
              existanceIsError = false;
            }
          }
          if(s.size() > 2) {
            if((s[1] == 'b') || (s[1] == 'B')) {
              if(s[2] == '=') {
                istringstream ss(s.substr(3));
                ss >> blockSize;
              }
            }
            if((s[1] == 'a') || (s[1] == 'A')) {
              if(s[2] == '=') {
                istringstream ss(s.substr(3));
                ss >> maxMatches;
              }
            }
          }
        } else {
          switch (fileNameArgument) {
            case 0:
              sourceFileName = s;
              break;
            case 1:
              targetFileName = s;
              break;
            case 2:
              patchFileName = s;
              showHelp = false;
              break;
            default:
              cerr << "WARNING: extra filename argument not used: " << s << "\n";
          }
          fileNameArgument++;
        }
      }
    }
  }
  if(beOptimal) {
    maxMatches = 0;
  }
  if(showHelp) {
    cout << "This program will take (sourcefile) as input and create a (patchfile).\n";
    cout << "With this patchfile, you can convert a (sourcefile) into (targetfile).\n\n";
    cout << "Command line info:\n";
    cout << "  GENPAT (sourcefile) (targetfile) (patchfile)\n\n";

    cout << "Command line option (optional):\n";
    cout << "/R        Replace a patch with same contents as source silently if it\n          already exists.\n";
    cout << "/B=64     Set blocksize (default=64), multiple of 2 is required.\n";
    cout << "/V        More verbose information during patch creation.\n";
    cout << "/O        Deactivate match limit of the /A switch (sometimes smaller patches).\n";
    cout << "/A=500    Maximum number of block matches per block (improves performance).\n";
    cout << "          Default is 500, larger is slower. Use /V to see the cut-off aborts.\n\n";
    cout << "Note: filenames should never start with / character!\n\n";
    cout << "Possible exit codes:\n";
    cout << "  0  Success\n";
    cout << "  1  Arguments missing\n";
    cout << "  2  Other error\n";
    cout << "  3  Source file already has a patch in specified patch file (=error)\n";
    return 1;
  }

  cout << "[Source] " << sourceFileName.c_str() << "\n";
  cout << "[Target] " << targetFileName.c_str() << "\n[PatchFile] " << patchFileName.c_str() << "\n";

  // get the file sizes
  TFileOffset sourceSize = 0;
  try {
    sourceSize = POSIX::getFileSize(sourceFileName.c_str());
  }
  catch(char* s) {
    cerr << "Source file size reading failed: " << s << "\n";
    return 2;
  }
  TFileOffset targetSize;
  try {
    targetSize = POSIX::getFileSize(targetFileName.c_str());
  }
  catch(const char* s) {
    cerr << "Target file size reading failed: " << s << "\n";
    return 2;
  }

  // calculate CRCs
  TChecksum* sourceCRC = new TChecksum(sourceFileName);
  sourceCRC->mode = TChecksum::MD5;  // default
  TChecksum* targetCRC = new TChecksum(targetFileName);
  targetCRC->mode = TChecksum::MD5;  // default

  string tempFileName = POSIX::getTempFile();

  // open the files
  bifstream source;
  source.open(sourceFileName.c_str(), std::ios_base::binary | std::ios_base::in);
  bifstream target;
  target.open(targetFileName.c_str(), std::ios_base::binary | std::ios_base::in);
  bofstream patch;
  patch.open(tempFileName.c_str(), std::ios_base::binary | std::ios_base::out);

  // remove existing patch with sourceCRC
  TFileOffset fileCount = 0;
  while(true) {
    TFileOffset previousPatchSize;
    try {
      previousPatchSize = POSIX::getFileSize(patchFileName.c_str());
    } catch(const char* s) {
      cout << "Patch file does not yet exist: " << s << ", it will be created.\n";
      std::ofstream newfile;
      newfile.open(patchFileName.c_str(), std::ios_base::binary | std::ios_base::out);
      newfile.close();
    }
    bifstream previousPatch;
    previousPatch.open(patchFileName.c_str(), std::ios_base::binary | std::ios_base::in);

    try {
      // this will copy the contents of previousPatch to patch, but without sourceCRC
      fileCount = FileFormat1::removeExistingPatch(previousPatch,previousPatchSize,patch,sourceCRC,existanceIsError);
    } catch(const char* s) {
      cerr << "ERROR: " << s << "\n";
      patch.close();
      unlink(tempFileName.c_str());
      return 3;
    }

    // set them to the same checksum mode
    targetCRC->mode = sourceCRC->mode;
    cout << "[Checksum] Kind of checksums used: ";
    if(targetCRC->mode == TChecksum::MD5) cout  << "MD5\n";
    if(targetCRC->mode == TChecksum::CRC32) cout  << "CRC32\n";
    break;
  }

  if(source.good() && target.good() && patch.good()) {
    PatchGenerator* gen = new PatchGenerator(source,sourceSize,target,targetSize,patch);
    try {
      // clean up the blocksize to be a multiple of 2
      int orgBlockSize = blockSize;
      int bs_counter = 0;
      while(blockSize != 0) {
        bs_counter++;
        blockSize >>= 1;
      }
      blockSize = 1;
      while(bs_counter != 0) {
        blockSize <<= 1;
        bs_counter--;
      }
      if((blockSize >> 1) == orgBlockSize) blockSize = orgBlockSize;
      if(blockSize != orgBlockSize) {
        cout << "[BlockSizeFix] Your blocksize had to be fixed since it is not a multiple of 2\n";
      }
      if(blockSize < 16) {
        blockSize = 16;
        cout << "[BlockSizeFix] Your blocksize had to be fixed since it is smaller than 16\n";
      }

      gen->blockSize = blockSize;
      cout << "[BlockSize] " << static_cast<unsigned int>(gen->blockSize) << " bytes\n";

      gen->maxMatches = maxMatches;
      if(gen->maxMatches == 0) {
        cout << "[FindBlockMatchLimit] Unlimited matches\n";
      } else {
        cout << "[FindBlockMatchLimit] " << gen->maxMatches << " matches\n";
      }

      gen->beVerbose = beVerbose;
      if(beVerbose) {
        cout << "[Debug] Verbose output during patch generation activated.\n";
      }

      // create sameBlock storage
      vector<SameBlock*> sameBlocks;
      // run the patch generator to find similar blocks
      gen->execute(sameBlocks);
      // construct the actual patch in FileFormat1
      FileFormat1::writePatch(patch,target,sameBlocks,sourceCRC,targetCRC,fileCount,POSIX::getFileTime(targetFileName.c_str()));
      // cleanup sameblocks
      for(vector<SameBlock*>::iterator iter = sameBlocks.begin(); iter != sameBlocks.end(); iter++) {
        delete *iter;
        *iter = NULL;
      }

      patch.close();
      TFileOffset patchSize = POSIX::getFileSize(tempFileName.c_str());

      // finally: copy the temporary file to the actual patch
      bifstream tempF;
      tempF.open(tempFileName.c_str(), std::ios_base::binary | std::ios_base::in);
      bofstream patchF;
      patchF.open(patchFileName.c_str(), std::ios_base::binary | std::ios_base::out);
      char* buf = new char[patchSize];
      tempF.read(buf,patchSize);
      patchF.write(buf,patchSize);
      delete[] buf;
      tempF.close();

      // now empty the temporary file
      std::ofstream clearF;
      clearF.open(tempFileName.c_str(), std::ios_base::binary | std::ios_base::out);
    }
    catch(string s) {
      cerr << "Error thrown: " << s.c_str();
      return 2;
    }
    catch(const char* s) {
      cerr << "Error thrown: " << s;
      return 2;
    }
  } else {
    cerr << "There was a problem opening the files.\n";
    return 2;
  }
  if(*sourceCRC == *targetCRC)
    cerr << "WARNING: source and target file have equal CRCs!";
  delete sourceCRC;
  delete targetCRC;
  unlink(tempFileName.c_str());
  return 0;
}
