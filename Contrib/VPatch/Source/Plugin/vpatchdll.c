#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "../../../ExDLL/exdll.h"

int DoPatch(HANDLE hPatch, HANDLE hSource, HANDLE hDest);
void strcopy(char *tgt, const char *src);
char* chop_arg(char **args);

#define PATCH_SUCCESS    0
#define PATCH_ERROR      1
#define PATCH_CORRUPT    2
#define PATCH_NOMATCH    3
#define PATCH_UPTODATE   4
#define FILE_ERR_PATCH   5
#define FILE_ERR_SOURCE  6
#define FILE_ERR_DEST    7

HINSTANCE g_hInstance;

HWND g_hwndParent;

void __declspec(dllexport) vpatchfile(HWND hwndParent, int string_size, 
                                      char *variables, stack_t **stacktop)
{
  g_hwndParent=hwndParent;

  EXDLL_INIT();


  // note if you want parameters from the stack, pop them off in order.
  // i.e. if you are called via exdll::myFunction file.dat poop.dat
  // calling popstring() the first time would give you file.dat,
  // and the second time would give you poop.dat. 
  // you should empty the stack of your parameters, and ONLY your
  // parameters.

  // do your stuff here
  {
    static char source[1024];
    static char dest[1024];
    static char exename[1024];
    HANDLE hPatch, hSource, hDest;
    int result;

    popstring(exename);
    popstring(source);
    popstring(dest);

    hPatch = CreateFile(exename, GENERIC_READ, FILE_SHARE_READ, NULL,
                                        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (hPatch == INVALID_HANDLE_VALUE) {
      pushstring("Unable to open patch file");
      return;
    }

    hSource = CreateFile(source, GENERIC_READ, FILE_SHARE_READ, NULL,
                                        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (hSource == INVALID_HANDLE_VALUE) {
      CloseHandle(hPatch);
      pushstring("Unable to open source file");
      return;
    }
    
    hDest = CreateFile(dest, GENERIC_READ | GENERIC_WRITE, 0, NULL,
                                    CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if (hDest == INVALID_HANDLE_VALUE) {
      CloseHandle(hPatch);
      CloseHandle(hSource);
      pushstring("Unable to open output file");
      return;
    }
        
    result = DoPatch(hPatch, hSource, hDest);

    CloseHandle(hDest);
    CloseHandle(hSource);
    CloseHandle(hPatch);

    if ((result != PATCH_SUCCESS)) {
      if (result == PATCH_ERROR)
        pushstring("An error occurred while patching");
      else if (result == PATCH_CORRUPT)
        pushstring("Patch data is invalid or corrupt");
      else if (result == PATCH_NOMATCH)
        pushstring("No suitable patches were found");
      else if (result == PATCH_UPTODATE)
        pushstring("OK, new version already installed");
      DeleteFile(dest);
    } else {
      pushstring("OK");
    }

    return;
  }
}



BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  g_hInstance=hInst;
  return TRUE;
}



UINT CRCTable[256];
BOOL bInitCRC = FALSE;

void InitCRC() {
  int i, j; unsigned long c;
  for (c = i = 0; i < 256; c = ++i) {
    for (j = 0; j < 8; j++) {
      if (c & 1) c = (c>>1) ^ 0xEDB88320;
      else       c >>= 1;
    }
    CRCTable[i] = c;
  }
  bInitCRC = TRUE;
}

#define CRCBLOCKSIZE    4096

BOOL FileCRC(HANDLE hFile, DWORD *crc) {
  static BYTE crcblock[CRCBLOCKSIZE];
  DWORD read;
  BYTE *p;

  UINT c = 0xFFFFFFFF;
  if (bInitCRC == FALSE)
    InitCRC();
  
  SetFilePointer(hFile, 0, NULL, FILE_BEGIN);
  do {
    if (ReadFile(hFile, crcblock, CRCBLOCKSIZE, &read, NULL) == FALSE)
      return FALSE;
    for (p = crcblock; p < crcblock + read; p++)
      c = CRCTable[(c & 0xFF) ^ *p] ^ (c >> 8);
  } while (read);

  *crc = (c ^ 0xFFFFFFFF);

  return TRUE;
}

#define BLOCKSIZE    16384

int DoPatch(HANDLE hPatch, HANDLE hSource, HANDLE hDest) {

  static char block[BLOCKSIZE];
    
  unsigned long temp = 0;
  unsigned long read;
  unsigned long source_crc = 0;
  unsigned long patch_dest_crc = 0;
  long patches = 0;
  int already_uptodate = 0;
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

  if (!FileCRC(hSource, &source_crc))
    return PATCH_ERROR;

    
  ReadFile(hPatch, &patches, 4, &read, NULL);

  while (patches--) {
    long patch_blocks = 0, patch_size = 0;
    unsigned long patch_source_crc = 0;

    ReadFile(hPatch, &patch_blocks, 4, &read, NULL);
    ReadFile(hPatch, &patch_source_crc, 4, &read, NULL);
    ReadFile(hPatch, &patch_dest_crc, 4, &read, NULL);
    ReadFile(hPatch, &patch_size, 4, &read, NULL);
    
    //added by Koen - check to see if it's already up-to-date for some patch (so
    //we can tell NSIS this isn't an error, but we already have the latest version)
    if (source_crc == patch_dest_crc) {
      already_uptodate = 1;
    }

    if (source_crc == patch_source_crc) {
      while (patch_blocks--) {
        unsigned char blocktype = 0;
        unsigned long blocksize = 0;
        ReadFile(hPatch, &blocktype, 1, &read, NULL);

        switch (blocktype) {
        case 1:
        case 2:
        case 3:
          if (blocktype == 1)
            { unsigned char x; blocksize = ReadFile(hPatch,&x,1,&read,NULL)? x:0; }
          else if (blocktype == 2)
            { unsigned short x; blocksize = ReadFile(hPatch,&x,2,&read,NULL)? x:0; }
          else
            { unsigned long x; blocksize = ReadFile(hPatch,&x,4,&read,NULL)? x:0; }

          if (!blocksize || !ReadFile(hPatch, &temp, 4, &read, NULL) || read != 4)
            return PATCH_CORRUPT;
    
          SetFilePointer(hSource, temp, 0, FILE_BEGIN);

          do {
            ReadFile(hSource, block, min(BLOCKSIZE, blocksize), &read, NULL);
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
            ReadFile(hPatch, block, min(BLOCKSIZE, blocksize), &read, NULL);
            WriteFile(hDest, block, read, &temp, NULL);
            if (temp != min(BLOCKSIZE, blocksize))
              return PATCH_ERROR;
            blocksize -= temp;
          } while (temp);

          break;

        default:
          return PATCH_CORRUPT;
        }
      }
      {
        unsigned long dest_crc = 0;
        FileCRC(hDest, &dest_crc);
        if (dest_crc != patch_dest_crc)
          return PATCH_ERROR;

        return PATCH_SUCCESS;
      }
    } else {
      SetFilePointer(hPatch, patch_size, NULL, FILE_CURRENT);
    }
  }
  
  //added by Koen - if already up to date, it doesn't matter that we didn't match
  if(already_uptodate) {
    return PATCH_UPTODATE;  
  } else {
    return PATCH_NOMATCH;
  }
}
