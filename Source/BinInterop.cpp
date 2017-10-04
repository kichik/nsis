/*
 * BinInterop.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2017 Anders Kjersem
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
#include <wchar.h> // _tcstoul
#include <stdexcept>

#define MKPTR(cast, base, offset) ( (cast) ( ((char*)(base)) + (offset) ) )
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

unsigned long get_file_size32(FILE*f)
{
  unsigned long error = ~(0UL), result = error, restoreseek = false;
  long cb;
  fpos_t orgpos;
  if (restoreseek ? 0 == fgetpos(f, &orgpos) : true)
    if (0 == fseek(f, 0, SEEK_END))
      if ((cb = ftell(f)) != -1L)
        if (restoreseek ? 0 == fsetpos(f, &orgpos) : true)
          result = cb;
  return result;
}

void* alloc_and_read_file(FILE*f, unsigned long&size)
{
  void *result = 0, *mem = 0;
  if (!f) return result;
  size = get_file_size32(f);
  mem = (~(0UL) != size) ? malloc(size) : 0;
  if (mem)
    if (0 == fseek(f, 0, SEEK_SET))
      if (fread(mem, 1, size, f) == size)
        result = mem, mem = 0;
  free(mem);
  return result;
}

/*void* alloc_and_read_file(const TCHAR*filepath, unsigned long&size)
{
  void *result = 0;
  FILE*f = FOPEN(filepath, ("rb"));
  if (f)
    result = alloc_and_read_file(f, size), fclose(f);
  return result;
}*/

#if 0
// midl /DOLDTLB=1 /oldtlb /tlb SLTG.tlb test.idl && midl /DNEWTLB=1 /newtlb /tlb MSFT.tlb test.idl
#ifdef NEWTLB
import "unknwn.idl";
[
  object, uuid(42424242-1111-1111-0001-424242424242),
  //oleautomation, //TYPEFLAG_FOLEAUTOMATION?
  //dual, //"Specifying dual on an interface implies that the interface is compatible with Automation, and therefore causes both the TYPEFLAG_FDUAL and TYPEFLAG_FOLEAUTOMATION flags to be set"
]
interface IInTeRfAcE1 : IUnknown { HRESULT I1_MeThOd1(); };

[ object, uuid(42424242-1111-1111-0002-424242424242) ]
interface IInTeRfAcE2 : IUnknown { [idempotent] HRESULT I2_MeThOd1(); };
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
    if (FIX_ENDIAN_INT32(h.Sig) == 0x5446534D)
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
    if (FIX_ENDIAN_INT32(h.Sig) == 0x047544C53 && MSTLB_IsSerializedOleGuid(h.Guid, 0x00020400, 0xffffff00)) // 0x000204xx for IID_ITypeLib and friends
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
    CResourceEditor re((BYTE*) pPEFile, (int) cbData);
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
  void *filedata = alloc_and_read_file(f, size);
  if (f) fclose(f);
  if (filedata)
  {
    if (!result && !resonly) result = MSTLB_GetVersion(filedata, size, high, low); // A raw TLB file?
    if (!result) result = GetTLBVersionUsingRE(filedata, size, resid, high, low);  // A resource in a PE file?
    // TODO: if (!result) result = GetTLBVersion16(filedata, size, resid, high, low); // A resouce in a 16-bit executable?
    free(filedata);
  }
  // Not supported: if (!result) result = GetTLBVersionFromMoniker(filepath, high, low);
  return result;
}
#else //! !_WIN32
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
