/*
 * BinInterop.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2017-2021 Anders Kjersem
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 */

#include "BinInterop.h"
#include "ResourceEditor.h"
#include "util.h"
#include <string.h> // strlen
#include <wchar.h> // _tcstoul
#include <stdexcept>

#define MKPTR(cast, base, offset) ( (cast) ( ((char*)(base)) + (offset) ) )
#define LE2HE16 FIX_ENDIAN_INT16 // Little-endian 2 Host-endian
#define LE2HE32 FIX_ENDIAN_INT32
#define HE2LE16 LE2HE16
#define HE2BE32 BE2HE32
const size_t invalid_res_id = ~(size_t)0;

FILE* MSTLB_fopen(const TCHAR*filepath, size_t*pResId)
{
  size_t resid = invalid_res_id; // MSDN:"By default, the type library is extracted from the first resource of type ITypeLib"
  FILE*f = FOPEN(filepath, ("rb"));
  if (!f)
  {
    // Retry with the "filename.exe\1234" syntax supported by LoadTypeLib
    tstring name = get_file_name(filepath), parent = get_dir_name(filepath);
    const TCHAR *pStart;
    TCHAR *pEnd;
    resid = _tcstoul(pStart = name.c_str(), &pEnd, 10);
    if (pEnd != pStart && !*pEnd && (f = FOPEN(parent.c_str(), ("rb"))))
    {
      USHORT mz, valid = false;
      valid = fread(&mz, 1, sizeof(mz), f) == 2 && (mz == 0x4d5a || mz == 0x5a4d);
      if (!valid) fclose(f), f = 0; // Only allow the special syntax on executable files
    }
  }
  if (pResId) *pResId = resid;
  return f;
}

#if !defined(_WIN32) || defined(NSIS_GETTLBVERSION_FORCEINTERNAL)

#if 0
// midl /DOLDTLB=1 /oldtlb /tlb SLTG.tlb test.idl && midl /DNEWTLB=1 /newtlb /tlb MSFT.tlb test.idl
#ifdef NEWTLB
import "unknwn.idl";
[ object, uuid(42424242-1111-1111-0001-424242424242) ] interface IInTeRfAcE1 : IUnknown { HRESULT I1_MeThOd1(); };
[ object, uuid(42424242-1111-1111-0002-424242424242) ] interface IInTeRfAcE2 : IUnknown { [idempotent] HRESULT I2_MeThOd1(); };
#endif

[ //msdn.microsoft.com/en-us/library/windows/desktop/aa367069
  uuid(42424242-1234-1234-1234-424242424242), lcid(0x0809), version(666.1337),
  helpstring("HeLpStRiNg"), // ICreateTypeInfo::SetDocString?
  helpfile("HeLpFiLe"), helpcontext(0xBABEFACE),
#ifdef NEWTLB
  helpstringdll("HeLpStRiNgDlL"), helpstringcontext(0xF00DBABE),
#endif
  control, /* LIBFLAG_FCONTROL */ hidden, /* LIBFLAG_FHIDDEN */ restricted, /* LIBFLAG_FRESTRICTED */
] 
library LiBnAmE
{
#ifdef NEWTLB
  //importlib("stdole2.tlb");
#else
  importlib("stdole32.tlb");
#endif
  [ uuid(42424242-0000-0000-0000-424242424242), helpstring("CoClAsSHeLpStRiNg") ]
  coclass CoClAsS
  {
#ifdef NEWTLB
    [default] interface IInTeRfAcE1; interface IInTeRfAcE2;
#else
    [default] interface IUnknown;
#endif
  };
};
#endif //~ .IDL

#pragma pack(1)
typedef struct {
  UINT32 Sig;            // 'MSFT'
  USHORT FmtVerMaj, FmtVerMin; // This is just a guess, always seems to be 2.1?
  UINT32 Unknown;
  UINT32 UnkLocale;      // idl:library:lcid
  UINT32 Locale;         // idl:library:lcid & ICreateTypeLib::SetLcid? This is the LCID returned in TLIBATTR.
  UINT32 FlagsAndSK;     // 0x03=tagSYSKIND mask. 0x0010=idl:library:helpfile 0x0100=idl:library:helpstringdll
  USHORT VerMaj, VerMin; // ICreateTypeLib::SetVersion?
  USHORT LibFlags;       // tagLIBFLAGS (TLIBATTR.wLibFlags)
  USHORT Unknown2;
  UINT32 Unknown3;       // Count of? This changes when the number of interfaces in the .idl changes.
  UINT32 HelpString;
  UINT32 HelpStringContext;
  UINT32 HelpContext;
  // ...?
} MSTLB_MSFT_MINIHEADER;

typedef struct {
  UINT32 Sig;        // 'SLTG'
  USHORT Count;      // Count of stream descriptors
  USHORT CompObjFooterSize, CompObjHeaderSize;
  USHORT First;      // Index of the first stream (Streams must be enumerated in the correct order!)
  char   Guid[16];   // DEFINE_OLEGUID(CLSID_?, 0x000204ff, 0, 0)
} MSTLB_SLTG_HEADER;
typedef struct {
  BYTE Sig[1+7+1+3+1]; // "\001CompObj\0dir" (Is "dir" part of the signature or is it the offset in one of the stream descriptors?)
} MSTLB_SLTG_COMPOBJ;
typedef struct {
  UINT32 Size;    // Size of stream
  USHORT Unknown; // Offset to something in the COMPOBJ header?
  USHORT Next;    // Next stream index
} MSTLB_SLTG_SD;
typedef struct {
  USHORT Len; // 0xffff if there is no string.
} MSTLB_SLTG_CSHDR; // Counted narrow string without \0 terminator.
typedef struct {
  enum { SIG = 0x51cc, CSCOUNT = 3 };
  USHORT Sig;         // 'CC51'
  USHORT Unknown[2];
  MSTLB_SLTG_CSHDR Strings[CSCOUNT]; // (Unknown, idl:library:helpstring and idl:library:helpfile)
} MSTLB_SLTG_BLOCK_LIBATTR_HEADER;
typedef struct {
  UINT32 HelpContext; // idl:library:helpcontext
  USHORT SysKind;     // tagSYSKIND (All 16-bits returned in TLIBATTR.syskind)
  UINT32 Locale;
  USHORT BadLocale;   // If this is non-zero the returned TLIBATTR LCID is 0.
  USHORT LibFlags;    // tagLIBFLAGS (TLIBATTR.wLibFlags)
  USHORT VerMaj, VerMin;
  char   LibGuid[16]; // idl:library:uuid
  //...?
} MSTLB_SLTG_BLOCK_LIBATTR_FOOTER;
#pragma pack()

static bool MSTLB_GetVersion_MSFT(const void*pData, size_t cbData, DWORD &high, DWORD &low)
{
  if (cbData >= sizeof(MSTLB_MSFT_MINIHEADER))
  {
    const MSTLB_MSFT_MINIHEADER &h = *(MSTLB_MSFT_MINIHEADER*) pData;
    if (h.Sig == FIX_ENDIAN_INT32(0x5446534D))
    {
      if (FIX_ENDIAN_INT16(h.FmtVerMaj) == 2 && FIX_ENDIAN_INT16(h.FmtVerMin) == 1) // Is this always 2.1?
      {
        // lcid = FIX_ENDIAN_INT32(h.Locale);
        // sysk = FIX_ENDIAN_INT32(h.FlagsAndSK) & 0x03;
        // libf = FIX_ENDIAN_INT16(h.LibFlags) | 0x08; // 0x08 for LIBFLAG_FHASDISKIMAGE
        high = FIX_ENDIAN_INT16(h.VerMaj), low = FIX_ENDIAN_INT16(h.VerMin);
        return true;
      }
    }
  }
  return false;
}

static USHORT GetLenLEToHE(const MSTLB_SLTG_CSHDR &s)
{
  USHORT len = FIX_ENDIAN_INT16(s.Len);
  return len != 0xffff ? len : 0;
}

static bool MSTLB_IsSerializedOleGuid(const void*pData, UINT32 Bits1 = 0, UINT32 Mask1 = 0)
{
  bool failed = false;
  failed |= (FIX_ENDIAN_INT32(*MKPTR(UINT32*, pData, 4*0)) & Mask1) != Bits1;
  failed |= (FIX_ENDIAN_INT32(*MKPTR(UINT32*, pData, 4*1))) != 0x00000000;
  failed |= (FIX_ENDIAN_INT32(*MKPTR(UINT32*, pData, 4*2))) != 0x000000C0;
  failed |= (FIX_ENDIAN_INT32(*MKPTR(UINT32*, pData, 4*3))) != 0x46000000;
  return !failed; // Does it look like a DEFINE_OLEGUID() GUID?
}

static bool MSTLB_GetVersion_SLTG(const void*pData, size_t cbData, DWORD &high, DWORD &low)
{
  if (cbData >= sizeof(MSTLB_SLTG_HEADER))
  {
    size_t eofPtr = MKPTR(size_t, pData, cbData);
    const MSTLB_SLTG_HEADER &h = *(MSTLB_SLTG_HEADER*) pData;
    if (h.Sig == FIX_ENDIAN_INT32(0x047544C53) && MSTLB_IsSerializedOleGuid(h.Guid, 0x00020400, 0xffffff00)) // 0x000204xx for IID_ITypeLib and friends
    {
      MSTLB_SLTG_SD *pSD = MKPTR(MSTLB_SLTG_SD*, &h, sizeof(MSTLB_SLTG_HEADER));
      UINT32 streamCount = FIX_ENDIAN_INT16(h.Count);

      // Check the data in each stream until we find the LIBATTR block
      void *pFirstStreamData = MKPTR(void*, pSD, (sizeof(MSTLB_SLTG_SD) * streamCount) + FIX_ENDIAN_INT16(h.CompObjHeaderSize) + FIX_ENDIAN_INT16(h.CompObjFooterSize));
      for (UINT32 tries = 0, i = FIX_ENDIAN_INT16(h.First), c = streamCount, o = 0; tries < c && i < c; ++tries)
      {
        MSTLB_SLTG_BLOCK_LIBATTR_HEADER *pBH = MKPTR(MSTLB_SLTG_BLOCK_LIBATTR_HEADER*, pFirstStreamData, o), *pD1 = pBH;
        if (eofPtr < MKPTR(size_t, pBH, sizeof(USHORT))) break; // The stream must at least have a signature

        if (FIX_ENDIAN_INT16(pBH->Sig) == pD1->SIG && eofPtr > MKPTR(size_t, pD1, sizeof(*pD1)))
        {
          unsigned long o2 = sizeof(USHORT) * 3; // Skip past the initial members
          for (UINT32 strIdx = 0; strIdx < pD1->CSCOUNT; ++strIdx)
          {
            MSTLB_SLTG_CSHDR *pS = MKPTR(MSTLB_SLTG_CSHDR*, pD1, o2);
            o2 += sizeof(MSTLB_SLTG_CSHDR) + GetLenLEToHE(*pS); // Skip past the embedded counted string
          }

          MSTLB_SLTG_BLOCK_LIBATTR_FOOTER *pD2 = MKPTR(MSTLB_SLTG_BLOCK_LIBATTR_FOOTER*, pD1, o2);
          if (eofPtr < MKPTR(size_t, pD2, sizeof(*pD2))) break;

          // lcid = FIX_ENDIAN_INT32(pD2->Locale);
          // sysk = FIX_ENDIAN_INT16(pD2->SysKind);
          // libf = FIX_ENDIAN_INT16(pD2->LibFlags);
          high = FIX_ENDIAN_INT16(pD2->VerMaj), low = FIX_ENDIAN_INT16(pD2->VerMin);
          return true;
        }
        o += FIX_ENDIAN_INT32(pSD[i].Size), i = FIX_ENDIAN_INT16(pSD[i].Next);
      }
    }
  }
  return false;
}

static bool MSTLB_GetVersion(const void*pData, size_t cbData, DWORD &high, DWORD &low)
{
  bool rv     = MSTLB_GetVersion_MSFT(pData, cbData, high, low);
  if (!rv) rv = MSTLB_GetVersion_SLTG(pData, cbData, high, low);
  return rv;
}

static BYTE* GetResource(CResourceEditor&RE, const TCHAR*RT, int RN, int RL, size_t&cbData)
{
  BYTE *pResData = RE.GetResource(RT, RN, RL);
  if (pResData) cbData = RE.GetResourceSize(RT, RN, RL);
  return pResData;
}

static bool GetTLBVersionUsingRE(const void*pPEFile, size_t cbData, size_t resid, DWORD &high, DWORD &low)
{
  bool result = false;
  try
  {
    const TCHAR* rt = _T("TYPELIB");
    int rn = (int) resid, rl = CResourceEditor::ANYLANGID;
    CResourceEditor re((void*) pPEFile, (int) cbData);
    BYTE *pResData = resid == invalid_res_id ? re.GetFirstResource(rt, cbData) : GetResource(re, rt, rn, rl, cbData);
    if (pResData)
    {
      result = MSTLB_GetVersion(pResData, cbData, high, low);
      re.FreeResource(pResData);
    }
  }
  catch (std::exception&)
  {
  }
  return result;
}

static bool GetTLBVersionInterop(const TCHAR *filepath, DWORD &high, DWORD &low)
{
  unsigned long size;
  size_t resid;
  FILE *f = MSTLB_fopen(filepath, &resid);
  bool result = false, resonly = invalid_res_id != resid;
  void *pFileData = alloc_and_read_file(f, size);
  if (f) fclose(f);
  if (pFileData)
  {
    if (!result && !resonly) result = MSTLB_GetVersion(pFileData, size, high, low); // A raw TLB file?
    if (!result) result = GetTLBVersionUsingRE(pFileData, size, resid, high, low);  // A resource in a PE file?
    // TODO: if (!result) result = GetTLBVersion16(pFileData, size, resid, high, low); // A resource in a 16-bit executable?
    free(pFileData);
  }
  // Not supported: if (!result) result = GetTLBVersionFromMoniker(filepath, high, low);
  return result;
}
#else // !_WIN32
static bool GetTLBVersionUsingAPI(const TCHAR *filepath, DWORD &high, DWORD &low)
{
  bool found = false;
  HRESULT hr;
  ITypeLib *pTL;
  TCHAR fullpath[1024], *p;
  if (!GetFullPathName(filepath, COUNTOF(fullpath), fullpath, &p)) return false;

#ifdef _UNICODE
  hr = LoadTypeLib(fullpath, &pTL);
#else
  WCHAR *olepath = (WCHAR*) WinWStrDupFromTChar(fullpath);
  if (!olepath) return false;
  hr = LoadTypeLib(olepath, &pTL);
  free(olepath);
#endif //~ _UNICODE
  if (SUCCEEDED(hr))
  {
    TLIBATTR *tlatt;
    hr = pTL->GetLibAttr(&tlatt);
    if (SUCCEEDED(hr))
    {
      high = tlatt->wMajorVerNum, low = tlatt->wMinorVerNum;
      found = true;
    }
    pTL->Release();
  }
  return found;
}
#endif //~ !_WIN32

bool GetTLBVersion(const TCHAR *filepath, DWORD &high, DWORD &low)
{
  bool found = false;
#if defined(_WIN32) && !defined(NSIS_GETTLBVERSION_FORCEINTERNAL)
  found = GetTLBVersionUsingAPI(filepath, high, low);
#else //! _WIN32
  found = GetTLBVersionInterop(filepath, high, low);
#endif //~ _WIN32
  return found;
}

static bool GetDLLVersionUsingRE(const TCHAR *filepath, DWORD &high, DWORD &low)
{
  bool found = false;
  LANGID anylangid = CResourceEditor::ANYLANGID;
  unsigned long fileSize;
  void*pFileData = alloc_and_read_file(filepath, fileSize);
  if (!pFileData) return false;
  try
  {
    CResourceEditor re(pFileData, fileSize);
    LPBYTE resdata = re.GetResource(VS_FILE_INFO, VS_VERSION_INFO, anylangid);
    if (resdata)
    {
      size_t ressize = re.GetResourceSize(VS_FILE_INFO, VS_VERSION_INFO, anylangid);
      size_t vsvhdrsize = sizeof(WORD) * 3;
      if (ressize > vsvhdrsize)
      {
        // Locate VS_FIXEDFILEINFO inside VS_VERSIONINFO
        WINWCHAR *szKey = (WINWCHAR*)(resdata + vsvhdrsize);
        size_t len = vsvhdrsize + (WinWStrLen(szKey) + 1) * sizeof(WINWCHAR);
        len = (len + 3) & ~3; // Align on DWORD boundary
        VS_FIXEDFILEINFO *verinfo = (VS_FIXEDFILEINFO*)(resdata + len);
        if (ressize >= len + sizeof(VS_FIXEDFILEINFO) && verinfo->dwSignature == FIX_ENDIAN_INT32(VS_FFI_SIGNATURE))
        {
          high = FIX_ENDIAN_INT32(verinfo->dwFileVersionMS), low = FIX_ENDIAN_INT32(verinfo->dwFileVersionLS);
          found = true;
        }
      }
      re.FreeResource(resdata);
    }
  }
  catch (std::exception&)
  {
  }
  free(pFileData);
  return found;
}

static bool GetDLLVersionUsingAPI(const TCHAR *filepath, DWORD &high, DWORD &low)
{
  bool found = false;
#ifdef _WIN32
  TCHAR path[1024], *name;
  path[0] = 0;
  GetFullPathName(filepath, 1024, path, &name);

  DWORD ignore, verSize = GetFileVersionInfoSize(path, &ignore);
  if (verSize)
  {
    void *buf = malloc(verSize);
    if (buf)
    {
      UINT valSize;
      VS_FIXEDFILEINFO *pvsf;
      if (GetFileVersionInfo(path, 0, verSize, buf) && VerQueryValue(buf, _T("\\"), (void**) &pvsf, &valSize))
      {
        high = pvsf->dwFileVersionMS, low = pvsf->dwFileVersionLS;
        found = true;
      }
      free(buf);
    }
  }
#endif
  return found;
}

#pragma pack(push, 1)
typedef struct tagMINI_IMAGE_VXD_HEADER {
  WORD   e32_magic, endian;
  BYTE   data[180];
  DWORD  e32_winresoff, e32_winreslen;
  WORD   e32_devid, e32_ddkver;
} MINI_IMAGE_VXD_HEADER, *PMINI_IMAGE_VXD_HEADER;
#pragma pack(pop)

static bool GetDLLVersionFromVXD(const TCHAR *filepath, DWORD &high, DWORD &low)
{
  bool found = false;
  FILEVIEW map;
  char *filedata = create_file_view_readonly(filepath, map);
  if (filedata)
  {
    PIMAGE_DOS_HEADER pDosHdr = (PIMAGE_DOS_HEADER) filedata;
    if ((pDosHdr->e_magic == 0x5A4D) | (pDosHdr->e_magic == 0x4D5A))
    {
      PMINI_IMAGE_VXD_HEADER pVxdHdr = MKPTR(PMINI_IMAGE_VXD_HEADER, pDosHdr, LE2HE32(pDosHdr->e_lfanew));
      if (pVxdHdr->e32_magic == HE2LE16(0x454C) && pVxdHdr->endian == 0) // Is it a little-endian VXD?
      {
        UINT minvsvi16 = 2 + 2 + 16, minvsffi = 52, minressecsize = ((1 + 2) + (1 + 2) + 2 + 4) + minvsvi16 + minvsffi;
        PMINI_IMAGE_VXD_HEADER pLEHdr = pVxdHdr;
        UINT ressecsize = LE2HE32(pLEHdr->e32_winreslen);
        if (ressecsize >= minressecsize && pLEHdr->e32_winresoff != 0)
        {
          // MSKB201685 just assumes that the first item is the version and we do the same.
          char *pRes = MKPTR(char*, pDosHdr, LE2HE32(pLEHdr->e32_winresoff));
          UINT ressize, ofs = 3, succ = *MKPTR(BYTE*, pRes, 0) == 0xff && *MKPTR(WORD*, pRes, 1) == HE2LE16(16); // RT_VERSION
          if (succ) succ = *MKPTR(BYTE*, pRes, ofs) == 0xff ? (ofs += (1 + 2) + 2) : (ofs += ((UINT)strlen(MKPTR(char*, pRes, ofs)) + !0) + 2); // Ordinal or string name
          if (succ) succ = ofs + 4 < ressecsize;
          if (succ) ressize = LE2HE32(*MKPTR(DWORD*, pRes, ofs)), ofs += 4;
          if (succ && ressize >= minvsvi16 + minvsffi && ressize < ressecsize)
          {
            WORD *pVSVI = MKPTR(WORD*, pRes, ofs); // VS_VERSIONINFO (16-bit/ASCII version)
            if (LE2HE16(pVSVI[0]) >= minvsvi16 + minvsffi && LE2HE16(pVSVI[1]) >= minvsffi && !memcmp(&pVSVI[2], "VS_VERSION_INFO", 16))
            {
              VS_FIXEDFILEINFO *pFFI = MKPTR(VS_FIXEDFILEINFO*, pVSVI, 2 + 2 + 16);
              if (LE2HE32(pFFI->dwSignature) == 0xFEEF04BD)
              {
                high = LE2HE32(pFFI->dwFileVersionMS), low = LE2HE32(pFFI->dwFileVersionLS);
                found = true;
              }
            }
          }
        }
      }
    }
    close_file_view(map);
  }
  return found;
}

bool GetDLLVersion(const TCHAR *filepath, DWORD &high, DWORD &low)
{
  bool result         = GetDLLVersionUsingAPI(filepath, high, low);
  if (!result) result = GetDLLVersionUsingRE(filepath, high, low);
  if (!result) result = GetDLLVersionFromVXD(filepath, high, low);
  return result;
}

DWORD GetDIBHeaderInfo(const void*pData, size_t DataSize, GENERICIMAGEINFO&Info)
{
  DWORD *p32 = (DWORD*) pData;
  WORD *p16 = (WORD*) pData;
  if (DataSize >= 12)
  {
    DWORD size = LE2HE32(p32[0]);
    if (size == 12) // BITMAPCOREHEADER
    {
      Info.Width = LE2HE16(p16[2]), Info.RawHeight = (INT32) (SHORT) LE2HE16(p16[3]);
      Info.Height = Info.RawHeight; // BITMAPCOREHEADER bitmaps cannot be top-down bitmaps (docs.microsoft.com/en-us/windows/win32/gdi/bitmap-header-types)
      Info.Planes = LE2HE16(p16[4]), Info.BPP = LE2HE16(p16[5]);
      return size;
    }
    if (size >= 16) // OS22XBITMAPHEADER/BITMAPINFOHEADER/BITMAPV*HEADER
    {
      Info.Width = LE2HE32(p32[1]), Info.RawHeight = (INT32) LE2HE32(p32[2]);
      Info.Height = abs(Info.RawHeight);
      Info.Planes = LE2HE16(p16[6+0]), Info.BPP = LE2HE16(p16[6+1]);
      return size;
    }
  }
  return 0;
}

DWORD IsBMPFile(const void*pData, size_t DataSize, GENERICIMAGEINFO*pInfo)
{
  BYTE *p8 = (BYTE*) pData;
  if (DataSize >= 14+12 && p8[0] == 'B' && p8[1] == 'M')
  {
    DWORD *p32 = (DWORD*) (&p8[2]), fsize = LE2HE32(p32[0]), bitsoffs = LE2HE32(p32[2]);
    if ((!fsize || fsize > 14+12) && (!bitsoffs || bitsoffs >= 14+12))
    {
      GENERICIMAGEINFO info;
      return GetDIBHeaderInfo(p8 + 14, DataSize - 14, pInfo ? *pInfo : info);
    }
  }
  return 0;
}

bool LoadImageCanLoadFile(const void*pData, size_t DataSize)
{
  bool valid = IsICOCURFile(pData, DataSize) != 0;
  if (!valid)
  {
    GENERICIMAGEINFO info;
    UINT headersize = GetBMPFileHeaderSize(pData, DataSize, &info);
    valid = headersize == 12 || headersize == 40; // Only supports BITMAPCOREHEADER and BITMAPINFOHEADER (Bug #681 & FR #559)
    valid = valid && !info.IsTopDownBitmap(); // TopDown bitmaps are only valid if they are loaded with LR_CREATEDIBSECTION, and if loaded from a resource, and if running on Vista+? and therefore we deny!
  }
  return valid;
}

bool LoadImageCanLoadFile(const TCHAR *filepath)
{
  bool valid = false;
  unsigned char header[14+124];
  FILE *f = my_fopen(filepath, "rb");
  if (f)
  {
    valid = LoadImageCanLoadFile(header, fread(header, 1, sizeof(header), f));
    fclose(f);
  }
  return valid;
}
