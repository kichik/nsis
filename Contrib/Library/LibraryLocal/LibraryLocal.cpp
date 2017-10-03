/*

  LibraryLocal - used by the Library.nsh macros
  Get the version of local DLL and TLB files
  Written by Joost Verburg
  POSIX DLL version support by kichik -- 20070415
  Unicode support by Jim Park -- 20070727
  POSIX TLB version support by anders_k -- 20170929

*/

#include "../../../Source/Platform.h"
#include "../../../Source/tstring.h"

#include <stdio.h>
#include <iostream>
#include <fstream>

#include "../../../Source/BinInterop.h"
#include "../../../Source/util.h"
#include "../../../Source/winchar.h"

using namespace std;

int g_noconfig=0; // TODO: Not used?
NSISRT_DEFINEGLOBALS();

enum {
  EC_SUCCESS            =  0,
  EC_NO_VERSION_PRESENT =  1,
  EC_UNSUPPORTED_FORMAT = 10, // TODO: POSIX should return this for 16-bit NE files
  EC_FILE_NOT_FOUND     = 15,
  EC_INVALID_PARAMETER  = 20,
  EC_FILE_IO_ERROR      = 50,
  EC_UNKNOWN_ERROR      = 99
};

NSIS_ENTRYPOINT_TMAIN
int _tmain(int argc, TCHAR* argv[])
{
  if (!NSISRT_Initialize()) return EC_UNKNOWN_ERROR;

  tstring appmode;
  const TCHAR *filename;
  int filefound = 0, exitcode = EC_INVALID_PARAMETER;

  if (argc != 4)
    return EC_INVALID_PARAMETER;

  appmode = argv[1];
  filename = argv[2];

  // Validate filename
  FILE*fIn = FOPEN(filename, ("rb"));
  filefound = !!fIn;
  if (fIn)
    fclose(fIn);

  int versionfound = 0;
  DWORD low = 0, high = 0;

  if (filefound)
  {
    // DLL/EXE version
    if (appmode.compare(_T("D")) == 0)
    {
      versionfound = GetDLLVersion(filename, high, low);
    }

    // TLB version
    if (appmode.compare(_T("T")) == 0)
    {
      versionfound = GetTLBVersion(filename, high, low);
    }
  }

  // Write the version to a NSIS header file
  FILE*fHdr = FOPEN(argv[3], ("wt"));
  if (!fHdr) return EC_FILE_IO_ERROR;

  fputs("!warning \"LibraryLocal is deprecated, use !getdllversion /packed\"\n", fHdr);

  // File content is always ASCII so we don't use TCHAR
  if (!filefound)
  {
    fputs("!define LIBRARY_VERSION_FILENOTFOUND\n", fHdr);
    exitcode = EC_FILE_NOT_FOUND;
  }
  else if (!versionfound)
  {
    fputs("!define LIBRARY_VERSION_NONE\n", fHdr);
    exitcode = EC_NO_VERSION_PRESENT;
  }
  else
  {
    fprintf(fHdr, "!define LIBRARY_VERSION_HIGH %lu\n", static_cast<unsigned long>(high));
    fprintf(fHdr, "!define LIBRARY_VERSION_LOW %lu\n", static_cast<unsigned long>(low));
    exitcode = EC_SUCCESS;
  }

  fclose(fHdr);
  return exitcode;
}
