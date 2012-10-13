//---------------------------------------------------------------------------
// vpatchdll.c: NSIS plug-in version of the VPatch runtime
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

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <nsis/pluginapi.h> // nsis plugin
#include "apply_patch.h"
#include "checksum.h"

/* ------------------------ Plug-in code ------------------------- */

HINSTANCE g_hInstance;

HWND g_hwndParent;
  
void __declspec(dllexport) vpatchfile(HWND hwndParent, int string_size, 
                                      TCHAR *variables, stack_t **stacktop) {
  g_hwndParent=hwndParent;

  EXDLL_INIT();

  {
    TCHAR source[MAX_PATH];
    TCHAR dest[MAX_PATH];
    TCHAR exename[MAX_PATH];
    HANDLE hPatch, hSource, hDest;
    int result;

    popstring(exename);
    popstring(source);
    popstring(dest);

    hPatch = CreateFile(exename, GENERIC_READ, FILE_SHARE_READ, NULL,
                                        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (hPatch == INVALID_HANDLE_VALUE) {
      pushstring(_T("Unable to open patch file"));
      return;
    }

    hSource = CreateFile(source, GENERIC_READ, FILE_SHARE_READ, NULL,
                                        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (hSource == INVALID_HANDLE_VALUE) {
      CloseHandle(hPatch);
      pushstring(_T("Unable to open source file"));
      return;
    }
    
    hDest = CreateFile(dest, GENERIC_READ | GENERIC_WRITE, 0, NULL,
                                    CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if (hDest == INVALID_HANDLE_VALUE) {
      CloseHandle(hPatch);
      CloseHandle(hSource);
      pushstring(_T("Unable to open output file"));
      return;
    }
        
    result = DoPatch(hPatch, hSource, hDest);

    CloseHandle(hDest);
    CloseHandle(hSource);
    CloseHandle(hPatch);

    if ((result != PATCH_SUCCESS)) {
      if (result == PATCH_ERROR)
        pushstring(_T("An error occured while patching"));
      else if (result == PATCH_CORRUPT)
        pushstring(_T("Patch data is invalid or corrupt"));
      else if (result == PATCH_NOMATCH)
        pushstring(_T("No suitable patches were found"));
      else if (result == PATCH_UPTODATE)
        pushstring(_T("OK, new version already installed"));
      DeleteFile(dest);
    } else {
      pushstring(_T("OK"));
    }

    return;
  }
}

#ifdef DLL_CHECKSUMS
void __declspec(dllexport) GetFileCRC32(HWND hwndParent, int string_size, 
                                      TCHAR *variables, stack_t **stacktop) {
  g_hwndParent=hwndParent;

  EXDLL_INIT();

  {
    TCHAR filename[MAX_PATH];
    char crc_string[9];
    HANDLE hFile;
    unsigned long crc;

    popstring(filename);
    
    hFile = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ, NULL,
                                        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (hFile == INVALID_HANDLE_VALUE) {
      //pushstring("ERROR: Unable to open file for CRC32 calculation");
      pushstring(_T(""));
      return;
    }

    if (!FileCRC(hFile, &crc)) {
      //pushstring("ERROR: Unable to calculate CRC32");
      pushstring(_T(""));
    } else {
      crc_string[8] = '\0';
      CRC32ToString(crc_string,crc);
      PushStringA(crc_string);
    }

    CloseHandle(hFile);
  }
}

void __declspec(dllexport) GetFileMD5(HWND hwndParent, int string_size, 
                                      TCHAR *variables, stack_t **stacktop) {
  g_hwndParent=hwndParent;

  EXDLL_INIT();

  {
    TCHAR filename[MAX_PATH];
    char md5_string[33];
    HANDLE hFile;
    md5_byte_t digest[16];

    popstring(filename);
    
    hFile = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ, NULL,
                                        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (hFile == INVALID_HANDLE_VALUE) {
      //pushstring("ERROR: Unable to open file for MD5 calculation");
      pushstring(_T(""));
      return;
    }

    if (!FileMD5(hFile, digest)) {
      //pushstring("ERROR: Unable to calculate MD5");
      pushstring(_T(""));
    } else {
      md5_string[32] = '\0';
      MD5ToString(md5_string,digest);
      PushStringA(md5_string);
    }

    CloseHandle(hFile);
  }
}
#endif

BOOL WINAPI DllMain(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
  g_hInstance=hInst;
  return TRUE;
}
