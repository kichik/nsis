//---------------------------------------------------------------------------
// checksum.h
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

#ifndef checksum_INCLUDED
#define checksum_INCLUDED

#include <windows.h>
#include "md5.h"

/* ------------------------ CRC32 checksum calculation ----------------- */

BOOL FileCRC(HANDLE hFile, DWORD *crc);
void CRC32ToString(char* string, DWORD crc);

/* ------------------------ MD5 checksum calculation ----------------- */

BOOL FileMD5(HANDLE hFile, md5_byte_t digest[16]);
void MD5ToString(char* string, md5_byte_t checksum[16]);

#endif