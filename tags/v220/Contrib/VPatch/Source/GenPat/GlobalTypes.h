//---------------------------------------------------------------------------
// GlobalTypes.h
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

#if !defined(GlobalTypes_H)
  #define GlobalTypes_H

  #ifndef _MSC_VER
    #include <stdint.h>
  #endif
  #include <iostream>
  #include <fstream>
  #include <ios>
  #include <string>

  using namespace std;

  #ifdef _MSC_VER
    typedef unsigned char uint8_t;
    typedef unsigned __int32 uint32_t;
    typedef unsigned __int64 uint64_t;
    #define CHECKSUM_BLOCK unsigned __int64
    #define __WIN32__
  #else
    #define CHECKSUM_BLOCK unsigned long long
  #endif

  typedef uint32_t TFileOffset;
  typedef ifstream bifstream;
  typedef istream bistream;
  typedef ofstream bofstream;
  typedef ostream bostream;
#endif // GlobalTypes_H
