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
//
// Unicode support by Jim Park -- 08/29/2007

#ifdef __BORLANDC__
  #pragma argsused
#endif

#if !defined(__BORLANDC__) && !defined(_MSC_VER)
  #include <unistd.h>
#endif

#include "tchar.h"

#if defined(__WIN32__) || defined(_WIN32)
  #define OPT_CHAR _T('/')
#else
  #define OPT_CHAR _T('-')
#endif
const TCHAR OPT_CHARSTR[] = {OPT_CHAR, _T('\0')} ;
// Win32 now supports "/" AND "-" switches (like makensis) but the filename warning only makes sense for "-"
#define OPT_FSCONFLICTCHARSTR _T("-")

inline bool ISSINGLESWITCHCHAR(const TCHAR c) { return ( OPT_CHAR==(c) || (OPT_CHAR!=_T('-') && _T('-')==(c)) ); }
#undef OPT_CHAR

#include "GlobalTypes.h"
#include "POSIXUtil.h"
#include "Checksums.h"
#include "PatchGenerator.h"
#include "FileFormat1.h"

#include <fstream>
#include <sstream>

#ifdef _WIN32
#include "../../../../Source/Platform.h"
NSIS_ENTRYPOINT_TMAIN
#endif
int _tmain( int argc, TCHAR * argv[] ) {
  tout << _T("GenPat v3.1\n");
  tout << _T("===========\n\n(c) 2001-2005 Van de Sande Productions\n");
  tout << _T("Website: http://www.tibed.net/vpatch\n\n");

  tstring sourceFileName;
  tstring targetFileName;
  tstring patchFileName;

  bool showHelp = true;

  int blockSize = 64;
  int maxMatches = 500;
  bool beVerbose = false;
  bool beOptimal = false;
  bool existanceIsError = true;     // flag error if patch already has source's MD5/CRC32?
  int fileNameArgument = 0;
  if(argc > 1) {
    for(int i = 1; i < argc; i++) {
      tstring s(argv[i]);
      if(s.size() > 0) {
        if(ISSINGLESWITCHCHAR(s[0])) {
          if(s.size() > 1) {
            if((s[1] == _T('v')) || (s[1] == _T('V'))) {
              beVerbose = true;
            }
            if((s[1] == _T('o')) || (s[1] == _T('O'))) {
              beOptimal = true;
            }
            if((s[1] == _T('r')) || (s[1] == _T('R'))) {
              existanceIsError = false;
            }
          }
          if(s.size() > 2) {
            if((s[1] == _T('b')) || (s[1] == _T('B'))) {
              if(s[2] == _T('=')) {
                tistringstream ss(s.substr(3));
                ss >> blockSize;
              }
            }
            if((s[1] == _T('a')) || (s[1] == _T('A'))) {
              if(s[2] == _T('=')) {
                tistringstream ss(s.substr(3));
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
              terr << _T("WARNING: extra filename argument not used: ") << s << _T("\n");
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
    tout << _T("This program will take (sourcefile) as input and create a (patchfile).\n");
    tout << _T("With this patchfile, you can convert a (sourcefile) into (targetfile).\n\n");
    tout << _T("Command line info:\n");
    tout << _T("  GENPAT (sourcefile) (targetfile) (patchfile)\n\n");

    tout << _T("Command line option (optional):\n");
    tout << OPT_CHARSTR << _T("R        Replace a patch with same contents as source silently if it\n          already exists.\n");
    tout << OPT_CHARSTR << _T("B=64     Set blocksize (default=64), multiple of 2 is required.\n");
    tout << OPT_CHARSTR << _T("V        More verbose information during patch creation.\n");
    tout << OPT_CHARSTR << _T("O        Deactivate match limit of the ") << OPT_CHARSTR << _T("A switch (sometimes smaller patches).\n");
    tout << OPT_CHARSTR << _T("A=500    Maximum number of block matches per block (improves performance).\n");
    tout << _T("          Default is 500, larger is slower. Use ") << OPT_CHARSTR << _T("V to see the cut-off aborts.\n\n");
    tout << _T("Note: filenames should never start with ") << OPT_FSCONFLICTCHARSTR << _T(" character!\n\n");
    tout << _T("Possible exit codes:\n");
    tout << _T("  0  Success\n");
    tout << _T("  1  Arguments missing\n");
    tout << _T("  2  Other error\n");
    tout << _T("  3  Source file already has a patch in specified patch file (=error)\n");
    return 1;
  }

  tout << _T("[Source] ") << sourceFileName.c_str() << _T("\n");
  tout << _T("[Target] ") << targetFileName.c_str() << _T("\n[PatchFile] ") << patchFileName.c_str() << _T("\n");

  // get the file sizes
  TFileOffset sourceSize = 0;
  try {
    sourceSize = POSIX::getFileSize(sourceFileName.c_str());
  }
  catch(TCHAR* s) {
    terr << _T("Source file size reading failed: ") << s << _T("\n");
    return 2;
  }
  TFileOffset targetSize;
  try {
    targetSize = POSIX::getFileSize(targetFileName.c_str());
  }
  catch(const TCHAR* s) {
    terr << _T("Target file size reading failed: ") << s << _T("\n");
    return 2;
  }

  // calculate CRCs
  TChecksum* sourceCRC = new TChecksum(sourceFileName);
  sourceCRC->mode = TChecksum::MD5;  // default
  TChecksum* targetCRC = new TChecksum(targetFileName);
  targetCRC->mode = TChecksum::MD5;  // default

  tstring tempFileName = POSIX::getTempFile();
  if (tempFileName == _T(""))
    return 2;

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
    TFileOffset previousPatchSize = 0;
    try {
      previousPatchSize = POSIX::getFileSize(patchFileName.c_str());
    } catch(const TCHAR* s) {
      tout << _T("Patch file does not yet exist: ") << s << _T(", it will be created.\n");
      bofstream newfile;
      newfile.open(patchFileName.c_str(), std::ios_base::binary | std::ios_base::out);
      newfile.close();
    }
    bifstream previousPatch;
    previousPatch.open(patchFileName.c_str(), std::ios_base::binary | std::ios_base::in);

    try {
      // this will copy the contents of previousPatch to patch, but without sourceCRC
      fileCount = FileFormat1::removeExistingPatch(previousPatch,previousPatchSize,patch,sourceCRC,existanceIsError);
    } catch(const TCHAR* s) {
      terr << _T("ERROR: ") << s << _T("\n");
      patch.close();
      _tunlink(tempFileName.c_str());
      return 3;
    }

    // set them to the same checksum mode
    targetCRC->mode = sourceCRC->mode;
    tout << _T("[Checksum] Kind of checksums used: ");
    if(targetCRC->mode == TChecksum::MD5) tout  << _T("MD5\n");
    if(targetCRC->mode == TChecksum::CRC32) tout  << _T("CRC32\n");
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
        tout << _T("[BlockSizeFix] Your blocksize had to be fixed since it is not a multiple of 2\n");
      }
      if(blockSize < 16) {
        blockSize = 16;
        tout << _T("[BlockSizeFix] Your blocksize had to be fixed since it is smaller than 16\n");
      }

      gen->blockSize = blockSize;
      tout << _T("[BlockSize] ") << static_cast<unsigned int>(gen->blockSize) << _T(" bytes\n");

      gen->maxMatches = maxMatches;
      if(gen->maxMatches == 0) {
        tout << _T("[FindBlockMatchLimit] Unlimited matches\n");
      } else {
        tout << _T("[FindBlockMatchLimit] ") << gen->maxMatches << _T(" matches\n");
      }

      gen->beVerbose = beVerbose;
      if(beVerbose) {
        tout << _T("[Debug] Verbose output during patch generation activated.\n");
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
      bofstream clearF;
      clearF.open(tempFileName.c_str(), std::ios_base::binary | std::ios_base::out);
    }
    catch(tstring s) {
      terr << _T("Error thrown: ") << s.c_str();
      return 2;
    }
    catch(const TCHAR* s) {
      terr << _T("Error thrown: ") << s;
      return 2;
    }
  } else {
    terr << _T("There was a problem opening the files.\n");
    return 2;
  }
  if(*sourceCRC == *targetCRC)
    terr << _T("WARNING: source and target file have equal CRCs!");
  delete sourceCRC;
  delete targetCRC;
  _tunlink(tempFileName.c_str());
  return 0;
}
