//---------------------------------------------------------------------------
// POSIXUtil.cpp
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


#include "POSIXUtil.h"
#include <iostream>
#include <fstream>

#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

/* My Borland/VC++ compiler do not have this file */
#if !defined(__BORLANDC__) && !defined(_MSC_VER)
  #include <unistd.h>
#endif

#ifdef __WIN32__     /* The Windows way of getting a temp file needs windows.h*/
  #include <windows.h>
#endif

using namespace std;

namespace POSIX {

  void TimeT_To_FILETIME(time_t t, ALT_FILETIME *pft ) {
      uint64_t ll = (((uint64_t)t) *10000000L) + (((uint64_t)116444736L) * 1000000000L);
      pft->dwLowDateTime = (uint32_t)(ll & 0xFFFFFFFF);
      pft->dwHighDateTime = (uint32_t)((ll>>32)&0xFFFFFFFF);
  }

#ifdef __WIN32__
  /* do it the old way on Win32, because POSIX does not get timezone stuff right */
  ALT_FILETIME getFileTime(const char* sFileName) {
    FILETIME temp;
    GetSystemTimeAsFileTime(&temp);
    HANDLE h = CreateFile(sFileName, GENERIC_READ, FILE_SHARE_READ, NULL,
                            OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (h == INVALID_HANDLE_VALUE) {
      cerr << "Cannot read file time of " << sFileName << "\n";
    } else {
      GetFileTime(h, NULL, NULL, &temp);
      CloseHandle(h);
    }
    ALT_FILETIME result;
    result.dwHighDateTime = temp.dwHighDateTime;
    result.dwLowDateTime = temp.dwLowDateTime;
    return result;
  }
#else
  ALT_FILETIME getFileTime(const char* sFileName) {
    struct stat buf;
    /* get current time first as a fall-back */
    time_t currentTime = time(NULL);

    if(stat(sFileName, &buf)) {
      cerr << "Cannot read file time of " << sFileName << "\n";
    } else {
      /* get the time from the file */
      currentTime = buf.st_mtime;
    }
    ALT_FILETIME result;
    TimeT_To_FILETIME(currentTime, &result);
    return result;
  }
#endif

  uint32_t getFileSize(const char* sFileName) {
    std::ifstream f;
    f.open(sFileName, std::ios_base::binary | std::ios_base::in);
    if (!f.good() || f.eof() || !f.is_open()) {
      throw "File could not be read (getFileSize)";
    }
    f.seekg(0, std::ios_base::beg);
    std::ifstream::pos_type begin_pos = f.tellg();
    f.seekg(0, std::ios_base::end);
    return static_cast<int>(f.tellg() - begin_pos);
  }

#ifdef __WIN32__
  string getTempFile() {
    char buffer[MAX_PATH];
    if(GetTempFileName(".","vpatch",0,buffer) == 0) {
      cerr << "Cannot create temporary filename";
    }
    return string(buffer);
  }
#else
//#ifdef POSIX
  // This is a POSIX version of the function, together with #include <stdio.h>
  // but I will not add it in a final release version.
  string getTempFile() {
    char filebuf [L_tmpnam];

    // create a temporary filename
    const char *fname = tmpnam (filebuf);

    if (!fname)
      cerr << "Cannot create temporary filename";

    return string(fname);
  }
//#endif
#endif

}
