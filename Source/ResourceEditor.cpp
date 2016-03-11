/*
 * ResourceEditor.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2002-2016 Amir Szekely <kichik@users.sourceforge.net>
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Reviewed for Unicode support by Jim Park -- 08/21/2007
 */

#include "ResourceEditor.h"
#include "util.h"
#include "winchar.h"
#include <queue>
#include "tchar.h"
#include "utf.h"
using namespace std;

//////////////////////////////////////////////////////////////////////
// Utilities
//////////////////////////////////////////////////////////////////////

#define WCHARPTR2WINWCHARPTR(s) ( (WINWCHAR*) (s) ) // Only for WinSDK structs like IMAGE_RESOURCE_DIR_STRING_U where we cannot change the WCHAR type!
#define ALIGN(dwToAlign, dwAlignOn) dwToAlign = (dwToAlign%dwAlignOn == 0) ? dwToAlign : dwToAlign - (dwToAlign%dwAlignOn) + dwAlignOn
#define RALIGN(dwToAlign, dwAlignOn) ((dwToAlign%dwAlignOn == 0) ? dwToAlign : dwToAlign - (dwToAlign%dwAlignOn) + dwAlignOn)

static inline DWORD ConvertEndianness(DWORD d) {
  return FIX_ENDIAN_INT32(d);
}

static inline WORD ConvertEndianness(WORD w) {
  return FIX_ENDIAN_INT16(w);
}

PIMAGE_NT_HEADERS CResourceEditor::GetNTHeaders(BYTE* pbPE) {
  // Get dos header
  PIMAGE_DOS_HEADER dosHeader = (PIMAGE_DOS_HEADER) pbPE;
  if (dosHeader->e_magic != IMAGE_DOS_SIGNATURE)
    throw runtime_error("PE file contains invalid DOS header");

  // Get NT headers
  PIMAGE_NT_HEADERS ntHeaders = (PIMAGE_NT_HEADERS)(pbPE + ConvertEndianness((DWORD)dosHeader->e_lfanew));
  if (ntHeaders->Signature != IMAGE_NT_SIGNATURE)
    throw runtime_error("PE file missing NT signature");

  // Make sure this is a supported PE format
  const WORD ohm = *GetCommonMemberFromPEOptHdr(ntHeaders->OptionalHeader, Magic);
  if (ohm != IMAGE_NT_OPTIONAL_HDR32_MAGIC && ohm != IMAGE_NT_OPTIONAL_HDR64_MAGIC)
    throw runtime_error("Unsupported PE format");

  return ntHeaders;
}

PRESOURCE_DIRECTORY CResourceEditor::GetResourceDirectory(
  BYTE* pbPE,
  DWORD dwSize,
  PIMAGE_NT_HEADERS ntHeaders,
  DWORD *pdwResSecVA /*=NULL*/,
  DWORD *pdwSectionIndex /*=NULL*/
) {
  PIMAGE_DATA_DIRECTORY dataDirectory = *GetMemberFromPEOptHdr(ntHeaders->OptionalHeader, DataDirectory);
  DWORD dwNumberOfRvaAndSizes = *GetMemberFromPEOptHdr(ntHeaders->OptionalHeader, NumberOfRvaAndSizes);
  
  if (ConvertEndianness(dwNumberOfRvaAndSizes) <= IMAGE_DIRECTORY_ENTRY_RESOURCE)
    throw runtime_error("No resource section found");
  // Get resource section virtual address
  DWORD dwResSecVA = ConvertEndianness(dataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress);
  // Pointer to the sections headers array
  PIMAGE_SECTION_HEADER sectionHeadersArray = IMAGE_FIRST_SECTION(ntHeaders);

  DWORD dwSectionIndex = (DWORD) -1;

  // Find resource section index in the array
  for (int i = 0; i < ConvertEndianness(ntHeaders->FileHeader.NumberOfSections); i++) {
    if (dwResSecVA == ConvertEndianness(sectionHeadersArray[i].VirtualAddress)) {
      // Remember resource section index
      dwSectionIndex = i;
      // Check for invalid resource section pointer
      if (!sectionHeadersArray[i].PointerToRawData)
        throw runtime_error("Invalid resource section pointer");

      break;
    }

    // Invalid section pointer (goes beyond the PE image)
    if (ConvertEndianness(sectionHeadersArray[i].PointerToRawData) > dwSize)
      throw runtime_error("Invalid section pointer");
  }

  // No resource section...
  if (dwSectionIndex == (DWORD) -1)
    throw runtime_error("PE file doesn't contain any resource section");

  // Return extra parameters
  if (pdwSectionIndex)
    *pdwSectionIndex = dwSectionIndex;
  if (pdwResSecVA)
    *pdwResSecVA = dwResSecVA;

  // Pointer to section data, the first resource directory
  DWORD dwResSecPtr = ConvertEndianness(sectionHeadersArray[dwSectionIndex].PointerToRawData);
  return PRESOURCE_DIRECTORY(pbPE + dwResSecPtr);
}

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
// CResourceEditor
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CResourceEditor::CResourceEditor(BYTE* pbPE, int iSize, bool bKeepData /*=true*/) {
  // Copy the data pointer
  m_pbPE = pbPE;
  m_iSize = iSize;
  m_bKeepData = bKeepData;

  // Get NT headers
  m_ntHeaders = GetNTHeaders(m_pbPE);

  // No check sum support yet...
  DWORD* pdwCheckSum = GetCommonMemberFromPEOptHdr(m_ntHeaders->OptionalHeader, CheckSum);
  if (*pdwCheckSum)
  {
    // clear checksum (should be [re]calculated after all changes done)
    pdwCheckSum = 0;
    //throw runtime_error("CResourceEditor doesn't yet support check sum");
  }

  // Get resource section virtual address, resource section index and pointer to resource directory
  PRESOURCE_DIRECTORY rdRoot = GetResourceDirectory(m_pbPE, iSize, m_ntHeaders, &m_dwResourceSectionVA, &m_dwResourceSectionIndex);

  // Scan the resource directory
  m_cResDir = ScanDirectory(rdRoot, rdRoot);
}

CResourceEditor::~CResourceEditor() {
  delete m_cResDir;
}

//////////////////////////////////////////////////////////////////////
// Methods
//////////////////////////////////////////////////////////////////////

// Adds/Replaces/Removes a resource.
// If lpData is 0 UpdateResource removes the resource.
bool CResourceEditor::UpdateResourceW(const WINWCHAR* szType, WINWCHAR* szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize) {
  CResourceDirectory* nameDir = 0;
  CResourceDirectory* langDir = 0;
  CResourceDataEntry* data = 0;
  IMAGE_RESOURCE_DIRECTORY rd = {0, /*time(0),*/};
  int iTypeIdx = -1, iNameIdx = -1, iLangIdx = -1;

  iTypeIdx = m_cResDir->Find(szType);
  if (iTypeIdx > -1) {
    nameDir = m_cResDir->GetEntry(iTypeIdx)->GetSubDirectory();
    iNameIdx = nameDir->Find(szName);
    if (iNameIdx > -1) {
      langDir = nameDir->GetEntry(iNameIdx)->GetSubDirectory();
      iLangIdx = langDir->Find(wLanguage);
      if (iLangIdx > -1) {
        data = langDir->GetEntry(iLangIdx)->GetDataEntry();
      }
    }
  }
  
  if (lpData) {
    // Replace/Add the resource
    if (data) {
      data->SetData(lpData, dwSize);
      return true;
    }

    if (!nameDir) {
      // Type doesn't yet exist
      nameDir = new CResourceDirectory(&rd);
      CResourceDirectoryEntry *pRDE = new CResourceDirectoryEntry(szType, nameDir);
      if (!m_cResDir->AddEntry(pRDE)) delete pRDE;
    }
    if (!langDir) {
      // Name doesn't yet exist
      langDir = new CResourceDirectory(&rd);
      CResourceDirectoryEntry *pRDE = new CResourceDirectoryEntry(szName, langDir);
      if (!nameDir->AddEntry(pRDE)) delete pRDE;
    }
    if (!data) {
      // Language doesn't yet exist, hence data nither
      data = new CResourceDataEntry(lpData, dwSize);
      CResourceDirectoryEntry *pRDE = new CResourceDirectoryEntry(MAKEINTRESOURCEWINW(wLanguage), data);
      if (!langDir->AddEntry(pRDE)) delete pRDE;
    }
  }
  else if (data) {
    // Delete the resource
    delete data;
    langDir->RemoveEntry(iLangIdx);
    // Delete directories holding the resource if empty
    if (!langDir->CountEntries()) {
      delete langDir;
      nameDir->RemoveEntry(iNameIdx);
      if (!nameDir->CountEntries()) {
        delete nameDir;
        m_cResDir->RemoveEntry(iTypeIdx);
      }
    }
  }
  else return false;
  return true;
}

#ifndef _UNICODE
static WINWCHAR* ResStringToUnicode(const char *szString) {
  if (IS_INTRESOURCE(szString)) return MAKEINTRESOURCEWINW((ULONG_PTR)szString);
  WINWCHAR *s = WinWStrDupFromTChar(szString);
  if (!s) throw std::bad_alloc();
  return s;
}
static void FreeUnicodeResString(WINWCHAR* szwString) {
  if (!IS_INTRESOURCE(szwString)) free(szwString);
}
#else
#ifndef _WIN32
static WINWCHAR* ResStringToUnicode(const TCHAR *s) {
  if (IS_INTRESOURCE(s)) return MAKEINTRESOURCEWINW((ULONG_PTR)s);
  WCToUTF16LEHlpr cnv;
  if (!cnv.Create(s)) throw std::runtime_error("Unicode conversion failed");
  return (WINWCHAR*) cnv.Detach();
}
static void FreeUnicodeResString(WINWCHAR* s) {
  if (!IS_INTRESOURCE(s)) free(s);
}
#endif // ~_WIN32
#endif // ~_UNICODE


bool CResourceEditor::UpdateResourceT(const TCHAR* szType, WORD szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize) {
#if defined(_WIN32) && defined(_UNICODE)
  return UpdateResourceW((WINWCHAR*)szType, MAKEINTRESOURCEWINW(szName), wLanguage, lpData, dwSize);
#else
  WINWCHAR* szwType = ResStringToUnicode(szType); 
  bool result = UpdateResourceW(szwType, MAKEINTRESOURCEWINW(szName), wLanguage, lpData, dwSize);
  FreeUnicodeResString(szwType);
  return result;
#endif
}

// Returns a copy of the requested resource
// Returns 0 if the requested resource can't be found
BYTE* CResourceEditor::GetResourceW(const WINWCHAR* szType, WINWCHAR* szName, LANGID wLanguage) {
  if (!m_bKeepData)
    throw runtime_error("Can't GetResource() when bKeepData is false");

  CResourceDirectory* nameDir = 0;
  CResourceDirectory* langDir = 0;
  CResourceDataEntry* data = 0;

  int i = m_cResDir->Find(szType);
  if (i > -1) {
    nameDir = m_cResDir->GetEntry(i)->GetSubDirectory();
    i = nameDir->Find(szName);
    if (i > -1) {
      langDir = nameDir->GetEntry(i)->GetSubDirectory();
      i = wLanguage ? langDir->Find(wLanguage) : 0;
      if (i > -1) {
        data = langDir->GetEntry(i)->GetDataEntry();
      }
    }
  }

  if (data) {
    BYTE* toReturn = new BYTE[data->GetSize()];
    CopyMemory(toReturn, data->GetData(), data->GetSize());
    return toReturn;
  }
  else
    return NULL;
}

BYTE* CResourceEditor::GetResourceT(const TCHAR* szType, WORD szName, LANGID wLanguage) {
#if defined(_WIN32) && defined(_UNICODE)
  return GetResourceW((WINWCHAR*)szType, MAKEINTRESOURCEWINW(szName), wLanguage);
#else
  WINWCHAR* szwType = ResStringToUnicode(szType);
  BYTE* result = GetResourceW(szwType, MAKEINTRESOURCEWINW(szName), wLanguage);
  FreeUnicodeResString(szwType);
  return result;
#endif
}

// Returns the size of the requested resource
// Returns -1 if the requested resource can't be found
int CResourceEditor::GetResourceSizeW(const WINWCHAR* szType, WINWCHAR* szName, LANGID wLanguage) {
  CResourceDirectory* nameDir = 0;
  CResourceDirectory* langDir = 0;
  CResourceDataEntry* data = 0;

  int i = m_cResDir->Find(szType);
  if (i > -1) {
    nameDir = m_cResDir->GetEntry(i)->GetSubDirectory();
    i = nameDir->Find(szName);
    if (i > -1) {
      langDir = nameDir->GetEntry(i)->GetSubDirectory();
      i = wLanguage ? langDir->Find(wLanguage) : 0;
      if (i > -1) {
        data = langDir->GetEntry(i)->GetDataEntry();
      }
    }
  }

  if (data)
    return (int) data->GetSize();
  else
    return -1;
}

int CResourceEditor::GetResourceSizeT(const TCHAR* szType, WORD szName, LANGID wLanguage) {
#if defined(_WIN32) && defined(_UNICODE)
  return GetResourceSizeW((WINWCHAR*)szType, MAKEINTRESOURCEWINW(szName), wLanguage);
#else
  WINWCHAR* szwType = ResStringToUnicode(szType);
  int result = GetResourceSizeW(szwType, MAKEINTRESOURCEWINW(szName), wLanguage);
  FreeUnicodeResString(szwType);
  return result;
#endif
}

// Returns the offset of the requested resource in the original PE
// Returns -1 if the requested resource can't be found
DWORD CResourceEditor::GetResourceOffsetW(const WINWCHAR* szType, WINWCHAR* szName, LANGID wLanguage) {
  CResourceDirectory* nameDir = 0;
  CResourceDirectory* langDir = 0;
  CResourceDataEntry* data = 0;

  int i = m_cResDir->Find(szType);
  if (i > -1) {
    nameDir = m_cResDir->GetEntry(i)->GetSubDirectory();
    i = nameDir->Find(szName);
    if (i > -1) {
      langDir = nameDir->GetEntry(i)->GetSubDirectory();
      i = wLanguage ? langDir->Find(wLanguage) : 0;
      if (i > -1) {
        data = langDir->GetEntry(i)->GetDataEntry();
      }
    }
  }

  if (data)
    return data->GetOffset();
  else
    return DWORD(-1);
}

DWORD CResourceEditor::GetResourceOffsetT(const TCHAR* szType, WORD szName, LANGID wLanguage) {
#if defined(_WIN32) && defined(_UNICODE)
  return GetResourceOffsetW((WINWCHAR*)szType, MAKEINTRESOURCEWINW(szName), wLanguage);
#else
  WINWCHAR* szwType = ResStringToUnicode(szType);
  DWORD result = GetResourceOffsetW(szwType, MAKEINTRESOURCEWINW(szName), wLanguage);
  FreeUnicodeResString(szwType);
  return result;
#endif
}

void CResourceEditor::FreeResource(BYTE* pbResource)
{
  if (pbResource)
    delete [] pbResource;
}

// Saves the edited PE into a buffer and returns it.
DWORD CResourceEditor::Save(BYTE* pbBuf, DWORD &dwSize) {
  if (!m_bKeepData)
    throw runtime_error("Can't Save() when bKeepData is false");

  unsigned int i;
  DWORD dwReqSize, temp32;

  temp32 = *GetCommonMemberFromPEOptHdr(m_ntHeaders->OptionalHeader, FileAlignment);
  const DWORD dwFileAlign = ConvertEndianness(temp32);
  temp32 = *GetCommonMemberFromPEOptHdr(m_ntHeaders->OptionalHeader, SectionAlignment);
  const DWORD dwSecAlign = ConvertEndianness(temp32);

  DWORD dwRsrcSize = m_cResDir->GetSize(); // Size of new resource section
  DWORD dwRsrcSizeAligned = RALIGN(dwRsrcSize, dwFileAlign); // Align it to FileAlignment

  // Calculate the total new PE size
  DWORD dwOldRsrcSize = ConvertEndianness(IMAGE_FIRST_SECTION(m_ntHeaders)[m_dwResourceSectionIndex].SizeOfRawData);
  dwReqSize = m_iSize - dwOldRsrcSize + dwRsrcSizeAligned;

  if (!pbBuf || dwSize < dwReqSize)
    return dwReqSize;

  // Use buffer
  BYTE* pbNewPE = pbBuf;
  dwSize = dwReqSize;
  // Fill buffer with zeros
  ZeroMemory(pbNewPE, dwSize);

  BYTE* seeker = pbNewPE;
  BYTE* oldSeeker = m_pbPE;

  PIMAGE_SECTION_HEADER old_sectionHeadersArray = IMAGE_FIRST_SECTION(m_ntHeaders);

  DWORD dwHeaderSize = ConvertEndianness(old_sectionHeadersArray[m_dwResourceSectionIndex].PointerToRawData);
  WORD wNumberOfSections = ConvertEndianness(m_ntHeaders->FileHeader.NumberOfSections);

  // Copy everything until the resource section (including headers and everything that might come after them)
  // We don't use SizeOfHeaders because sometimes (using VC6) it can extend beyond the first section
  // or (Borland) there could be some more information between the headers and the first section.
  CopyMemory(seeker, oldSeeker, dwHeaderSize);

  // Skip the headers and whatever comes after them
  seeker += dwHeaderSize;
  oldSeeker += dwHeaderSize;

  // Get new nt headers pointer
  PIMAGE_NT_HEADERS ntHeaders = GetNTHeaders(pbNewPE);
  // Get a pointer to the new section headers
  PIMAGE_SECTION_HEADER sectionHeadersArray = IMAGE_FIRST_SECTION(ntHeaders);

  // Skip the resource section in the old PE seeker.
  oldSeeker += dwOldRsrcSize;

  // Save the old virtual size of the resource section
  DWORD dwNewVirtualSize = RALIGN(dwRsrcSize, dwSecAlign);
  DWORD dwOldVirtualSize = ConvertEndianness(sectionHeadersArray[m_dwResourceSectionIndex].Misc.VirtualSize);
  ALIGN(dwOldVirtualSize, dwSecAlign);
  DWORD dwVAAdjustment = dwNewVirtualSize - dwOldVirtualSize;

  // Set the new size of the resource section (size aligned to FileAlignment)
  sectionHeadersArray[m_dwResourceSectionIndex].SizeOfRawData = ConvertEndianness(dwRsrcSizeAligned);
  // Set the virtual size as well (in memory)
  sectionHeadersArray[m_dwResourceSectionIndex].Misc.VirtualSize = ConvertEndianness(dwRsrcSize);
  (*GetMemberFromPEOptHdr(ntHeaders->OptionalHeader, DataDirectory))[IMAGE_DIRECTORY_ENTRY_RESOURCE].Size = ConvertEndianness(dwRsrcSize);

  // Set the new virtual size of the image
  DWORD* pdwSizeOfImage = GetCommonMemberFromPEOptHdr(ntHeaders->OptionalHeader, SizeOfImage);
  *pdwSizeOfImage = AlignVA(*GetCommonMemberFromPEOptHdr(ntHeaders->OptionalHeader, SizeOfHeaders));
  for (i = 0; i < wNumberOfSections; i++) {
    DWORD dwSecSize = ConvertEndianness(sectionHeadersArray[i].Misc.VirtualSize);
    *pdwSizeOfImage = AlignVA(AdjustVA(*pdwSizeOfImage, dwSecSize));
  }

  // Set the new AddressOfEntryPoint if needed
  DWORD* pdwAddressOfEntryPoint = GetCommonMemberFromPEOptHdr(ntHeaders->OptionalHeader, AddressOfEntryPoint);
  if (ConvertEndianness(*pdwAddressOfEntryPoint) > m_dwResourceSectionVA)
    *pdwAddressOfEntryPoint = AdjustVA(*pdwAddressOfEntryPoint, dwVAAdjustment);

  // Set the new BaseOfCode if needed
  DWORD* pdwBaseOfCode = GetCommonMemberFromPEOptHdr(ntHeaders->OptionalHeader, BaseOfCode);
  if (ConvertEndianness(*pdwBaseOfCode) > m_dwResourceSectionVA)
    *pdwBaseOfCode = AdjustVA(*pdwBaseOfCode, dwVAAdjustment);

  // Set the new BaseOfData if needed
  if (*GetCommonMemberFromPEOptHdr(ntHeaders->OptionalHeader, Magic) == IMAGE_NT_OPTIONAL_HDR32_MAGIC) {
    DWORD* pdwBaseOfData = &((PIMAGE_OPTIONAL_HEADER32)&ntHeaders->OptionalHeader)->BaseOfData;
    if (ConvertEndianness(*pdwBaseOfData) > m_dwResourceSectionVA)
      *pdwBaseOfData = AdjustVA(*pdwBaseOfData, dwVAAdjustment);
  }

  // Refresh the headers of the sections that come after the resource section, and the data directory
  DWORD dwNumberOfRvaAndSizes = *GetMemberFromPEOptHdr(ntHeaders->OptionalHeader, NumberOfRvaAndSizes);
  PIMAGE_DATA_DIRECTORY pDataDirectory = *GetMemberFromPEOptHdr(ntHeaders->OptionalHeader, DataDirectory);
  for (i = m_dwResourceSectionIndex + 1; i < wNumberOfSections; i++) {
    if (sectionHeadersArray[i].PointerToRawData) {
      AdjustVA(sectionHeadersArray[i].PointerToRawData, dwRsrcSizeAligned - dwOldRsrcSize);
    }

    // We must find the right data directory entry before we change the virtual address
    unsigned int uDataDirIdx = 0;
    for (unsigned int j = 0; j < ConvertEndianness(dwNumberOfRvaAndSizes); j++)
      if (pDataDirectory[j].VirtualAddress == sectionHeadersArray[i].VirtualAddress)
        uDataDirIdx = j;

    sectionHeadersArray[i].VirtualAddress = AdjustVA(sectionHeadersArray[i].VirtualAddress, dwVAAdjustment);

    // Change the virtual address in the data directory too
    if (uDataDirIdx)
      pDataDirectory[uDataDirIdx].VirtualAddress = sectionHeadersArray[i].VirtualAddress;
  }

  // Write the resource section
  WriteRsrcSec(seeker);
  // Advance the pointer
  seeker += dwRsrcSizeAligned;

  // Copy everything that comes after the resource section (other sections and tacked data)
  size_t cbLeft = m_iSize - (oldSeeker - m_pbPE);
  if (cbLeft)
    CopyMemory(seeker, oldSeeker, cbLeft);

  seeker += cbLeft;
  oldSeeker += cbLeft;

  /**********************************************************
   * To add checksum to the header use MapFileAndCheckSum
   **********************************************************/

  // From now on, we are working on the new PE
  // Freeing the old PE memory is up to the user
  m_pbPE = pbNewPE;
  m_iSize = dwSize;
  m_ntHeaders = ntHeaders;
  // We just wrote the resource section according to m_cResDir, so we don't need to rescan
  // m_dwResourceSectionIndex and m_dwResourceSectionVA have also been left unchanged as
  // we didn't move the resources section

  return 0;
}

// This function scans exe sections and after find a match with given name
// increments it's virtual size (auto fixes image size based on section alignment, etc)
// Jim Park: The section name must be ASCII code.  Do not TCHAR this stuff.
bool CResourceEditor::SetPESectionVirtualSize(const char* pszSectionName, DWORD newsize)
{
  PIMAGE_SECTION_HEADER sectionHeadersArray = IMAGE_FIRST_SECTION(m_ntHeaders);

  // Refresh the headers of the sections that come after the resource section, and the data directory
  for (int i = 0; i < ConvertEndianness(m_ntHeaders->FileHeader.NumberOfSections); i++) {
    if (!strcmp((LPCSTR)sectionHeadersArray[i].Name, pszSectionName)) {
      sectionHeadersArray[i].Misc.VirtualSize = AlignVA(ConvertEndianness(newsize));

      sectionHeadersArray[i].Characteristics &= ConvertEndianness((DWORD) ~IMAGE_SCN_MEM_DISCARDABLE);

      // now fix any section after
      for (int k = i + 1; k < ConvertEndianness(m_ntHeaders->FileHeader.NumberOfSections); k++, i++) {
        DWORD dwLastSecVA = ConvertEndianness(sectionHeadersArray[i].VirtualAddress);
        DWORD dwLastSecSize = ConvertEndianness(sectionHeadersArray[i].Misc.VirtualSize);
        DWORD dwSecVA = AlignVA(ConvertEndianness(dwLastSecVA + dwLastSecSize));

        sectionHeadersArray[k].VirtualAddress = dwSecVA;

        if (m_dwResourceSectionIndex == (DWORD) k)
        {
          // fix the resources virtual address if it changed
          PIMAGE_DATA_DIRECTORY pDataDirectory = *GetMemberFromPEOptHdr(m_ntHeaders->OptionalHeader, DataDirectory);
          pDataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress = dwSecVA;
          m_dwResourceSectionVA = ConvertEndianness(dwSecVA);
        }
      }

      return true;
    }
  }

  return false;
}

//////////////////////////////////////////////////////////////////////
// Private Methods
//////////////////////////////////////////////////////////////////////

// This function scans a given resource directory and returns a CResourceDirectory object
// rdRoot must point to the root directory of the resource section
CResourceDirectory* CResourceEditor::ScanDirectory(PRESOURCE_DIRECTORY rdRoot, PRESOURCE_DIRECTORY rdToScan) {
  // Create CResourceDirectory from rdToScan
  CResourceDirectory* rdc = new CResourceDirectory(PIMAGE_RESOURCE_DIRECTORY(rdToScan));
  WINWCHAR* szName;
  PIMAGE_RESOURCE_DATA_ENTRY rde = NULL;

  // Go through all entries of this resource directory
  int entries = ConvertEndianness(rdToScan->Header.NumberOfNamedEntries);
  entries += ConvertEndianness(rdToScan->Header.NumberOfIdEntries);

  for (int i = 0; i < entries; i++) {
    MY_IMAGE_RESOURCE_DIRECTORY_ENTRY rd = rdToScan->Entries[i];
    rd.UOffset.OffsetToData = ConvertEndianness(rd.UOffset.OffsetToData);
    rd.UName.Name = ConvertEndianness(rd.UName.Name);

    // If this entry points to data entry get a pointer to it
    if (!rd.UOffset.DirectoryOffset.DataIsDirectory)
      rde = PIMAGE_RESOURCE_DATA_ENTRY(rd.UOffset.OffsetToData + (BYTE*)rdRoot);

    // If this entry has a name, translate it from Unicode
    if (rd.UName.NameString.NameIsString) {
      PIMAGE_RESOURCE_DIR_STRING_U rds = PIMAGE_RESOURCE_DIR_STRING_U(rd.UName.NameString.NameOffset + (char*)rdRoot);

      size_t nameSize = ConvertEndianness(rds->Length);
      szName = new WINWCHAR[nameSize+1];
      WinWStrNCpy(szName, WCHARPTR2WINWCHARPTR(rds->NameString), nameSize);
      szName[nameSize] = 0;
    }
    // Else, set the name to this entry's id
    else
      szName = MAKEINTRESOURCEWINW(ConvertEndianness(rdToScan->Entries[i].UName.Id));

    if (rd.UOffset.DirectoryOffset.DataIsDirectory)
    {
      CResourceDirectoryEntry *pRDE = new CResourceDirectoryEntry(
        szName,
        ScanDirectory(
          rdRoot,
          PRESOURCE_DIRECTORY(rd.UOffset.DirectoryOffset.OffsetToDirectory + (LPBYTE)rdRoot)
        )
      );
      if (!rdc->AddEntry(pRDE)) delete pRDE;
    }
    else
    {
      LPBYTE pbData = (LPBYTE)rdRoot + ConvertEndianness(rde->OffsetToData) - m_dwResourceSectionVA;
      DWORD dwOffset = DWORD(pbData - m_pbPE);

      if (m_bKeepData)
      {
        if (dwOffset > DWORD(m_iSize))
        {
          throw runtime_error("Invalid resource entry data pointer, possibly compressed resources");
        }
      }
      else
      {
        pbData = m_pbPE; // dummy pointer to "nothing"
      }
      CResourceDirectoryEntry *pRDE = new CResourceDirectoryEntry(
        szName,
        new CResourceDataEntry(
          pbData,
          ConvertEndianness(rde->Size),
          ConvertEndianness(rde->CodePage),
          dwOffset
        )
      );
      if (!rdc->AddEntry(pRDE)) delete pRDE;
    }

    // Delete the dynamicly allocated name if it is a name and not an id
    if (!IS_INTRESOURCE(szName))
      delete [] szName;
  }

  return rdc;
}

// This function writes into a given place in memory (pbRsrcSec) the edited resource section
void CResourceEditor::WriteRsrcSec(BYTE* pbRsrcSec) {
  BYTE* seeker = pbRsrcSec;

  queue<CResourceDirectory*> qDirs; // Used to scan the tree by level
  queue<CResourceDataEntry*> qDataEntries; // Used for writing the data entries
  queue<CResourceDataEntry*> qDataEntries2; // Used for writing raw resources data
  queue<CResourceDirectoryEntry*> qStrings; // Used for writing resources' names

  qDirs.push(m_cResDir);

  while (!qDirs.empty()) {
    CResourceDirectory* crd = qDirs.front();

    IMAGE_RESOURCE_DIRECTORY rdDir = crd->GetInfo();

    rdDir.NumberOfNamedEntries = ConvertEndianness(rdDir.NumberOfNamedEntries);
    rdDir.NumberOfIdEntries = ConvertEndianness(rdDir.NumberOfIdEntries);

    CopyMemory(seeker, &rdDir, sizeof(IMAGE_RESOURCE_DIRECTORY));
    crd->m_ulWrittenAt = (ULONG_PTR)(seeker);
    seeker += sizeof(IMAGE_RESOURCE_DIRECTORY);

    for (unsigned int i = 0; i < crd->CountEntries(); i++) {
      if (crd->GetEntry(i)->HasName())
        qStrings.push(crd->GetEntry(i));
      if (crd->GetEntry(i)->IsDataDirectory())
        qDirs.push(crd->GetEntry(i)->GetSubDirectory());
      else {
        qDataEntries.push(crd->GetEntry(i)->GetDataEntry());
        qDataEntries2.push(crd->GetEntry(i)->GetDataEntry());
      }

      MY_IMAGE_RESOURCE_DIRECTORY_ENTRY rDirE;
      ZeroMemory(&rDirE, sizeof(rDirE));
      rDirE.UOffset.DirectoryOffset.DataIsDirectory = crd->GetEntry(i)->IsDataDirectory();
      rDirE.UName.Id = crd->GetEntry(i)->HasName() ? 0 : crd->GetEntry(i)->GetId();
      rDirE.UName.Id = ConvertEndianness(rDirE.UName.Id);
      rDirE.UName.NameString.NameIsString = (crd->GetEntry(i)->HasName()) ? 1 : 0;

      CopyMemory(seeker, &rDirE, sizeof(MY_IMAGE_RESOURCE_DIRECTORY_ENTRY));
      crd->GetEntry(i)->m_ulWrittenAt = (ULONG_PTR)(seeker);
      seeker += sizeof(MY_IMAGE_RESOURCE_DIRECTORY_ENTRY);
    }
    qDirs.pop();
  }

  /*
   * Write IMAGE_RESOURCE_DATA_ENTRYs.  
   */
  while (!qDataEntries.empty()) {
    CResourceDataEntry* cRDataE = qDataEntries.front();
    IMAGE_RESOURCE_DATA_ENTRY rDataE = {0,};
    rDataE.CodePage = ConvertEndianness(cRDataE->GetCodePage());
    rDataE.Size = ConvertEndianness(cRDataE->GetSize());

    CopyMemory(seeker, &rDataE, sizeof(IMAGE_RESOURCE_DATA_ENTRY));
    cRDataE->m_ulWrittenAt = (ULONG_PTR)(seeker);
    seeker += sizeof(IMAGE_RESOURCE_DATA_ENTRY);

    qDataEntries.pop();
  }

  /*
   * Write strings
   */
  while (!qStrings.empty()) {
    CResourceDirectoryEntry* cRDirE = qStrings.front();

    PMY_IMAGE_RESOURCE_DIRECTORY_ENTRY(cRDirE->m_ulWrittenAt)->UName.NameString.NameOffset = ConvertEndianness((DWORD) (seeker - pbRsrcSec));

    const WINWCHAR* szName = cRDirE->GetName();
    WORD iLen = (WORD) WinWStrLen(szName) + 1;

    *(WORD*)seeker = ConvertEndianness(iLen);
    CopyMemory(seeker + sizeof(WORD), szName, iLen*sizeof(WINWCHAR));

    seeker += RALIGN(iLen * sizeof(WINWCHAR) + sizeof(WORD), 4);

    qStrings.pop();
  }

  /*
   * Write raw resource data and set offsets in IMAGE_RESOURCE_DATA_ENTRYs.
   */
  while (!qDataEntries2.empty()) {
    CResourceDataEntry* cRDataE = qDataEntries2.front();
    CopyMemory(seeker, cRDataE->GetData(), cRDataE->GetSize());
    PIMAGE_RESOURCE_DATA_ENTRY(cRDataE->m_ulWrittenAt)->OffsetToData = ConvertEndianness((DWORD)(seeker - pbRsrcSec) + m_dwResourceSectionVA);

    seeker += RALIGN(cRDataE->GetSize(), 8);

    qDataEntries2.pop();
  }

  /*
   * Set all of the directory entries offsets.
   */
  SetOffsets(m_cResDir, (ULONG_PTR)(pbRsrcSec));
}

// Sets the offsets in directory entries
void CResourceEditor::SetOffsets(CResourceDirectory* resDir, ULONG_PTR newResDirAt) {
  for (unsigned int i = 0; i < resDir->CountEntries(); i++) {
    PMY_IMAGE_RESOURCE_DIRECTORY_ENTRY rde = PMY_IMAGE_RESOURCE_DIRECTORY_ENTRY(resDir->GetEntry(i)->m_ulWrittenAt);
    if (resDir->GetEntry(i)->IsDataDirectory()) {
      rde->UOffset.DirectoryOffset.DataIsDirectory = 1;
      rde->UOffset.DirectoryOffset.OffsetToDirectory = resDir->GetEntry(i)->GetSubDirectory()->m_ulWrittenAt - newResDirAt;
      rde->UOffset.OffsetToData = ConvertEndianness(rde->UOffset.OffsetToData);
      SetOffsets(resDir->GetEntry(i)->GetSubDirectory(), newResDirAt);
    }
    else {
      rde->UOffset.OffsetToData = ConvertEndianness((DWORD)(resDir->GetEntry(i)->GetDataEntry()->m_ulWrittenAt - newResDirAt));
    }
  }
}

// Adjusts a virtual address by a specific amount
DWORD CResourceEditor::AdjustVA(DWORD dwVirtualAddress, DWORD dwAdjustment) {
  dwVirtualAddress = ConvertEndianness(dwVirtualAddress);
  dwVirtualAddress += dwAdjustment;
  dwVirtualAddress = ConvertEndianness(dwVirtualAddress);

  return dwVirtualAddress;
}

// Aligns a virtual address to the section alignment
DWORD CResourceEditor::AlignVA(DWORD dwVirtualAddress) {
  DWORD temp32 = *GetCommonMemberFromPEOptHdr(m_ntHeaders->OptionalHeader, SectionAlignment);
  DWORD dwAlignment = ConvertEndianness(temp32);

  dwVirtualAddress = ConvertEndianness(dwVirtualAddress);
  dwVirtualAddress = RALIGN(dwVirtualAddress, dwAlignment);
  dwVirtualAddress = ConvertEndianness(dwVirtualAddress);

  return dwVirtualAddress;
}

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
// CResourceDirectory
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CResourceDirectory::CResourceDirectory(PIMAGE_RESOURCE_DIRECTORY prd) {
  m_rdDir = *prd;
  m_rdDir.NumberOfIdEntries = 0;
  m_rdDir.NumberOfNamedEntries = 0;
}

CResourceDirectory::~CResourceDirectory() {
  Destroy();
}

//////////////////////////////////////////////////////////////////////
// Methods
//////////////////////////////////////////////////////////////////////

IMAGE_RESOURCE_DIRECTORY CResourceDirectory::GetInfo() {
  return m_rdDir;
}

CResourceDirectoryEntry* CResourceDirectory::GetEntry(unsigned int i) {
  if (m_vEntries.size() < i)
    return 0;
  return m_vEntries[i];
}

// This function inserts a new directory entry
// It also keeps the directory entries sorted
bool CResourceDirectory::AddEntry(CResourceDirectoryEntry* entry) {
  int i = 0;
  if (entry->HasName()) {
    const WINWCHAR* szEntName = entry->GetName();
    for (i = 0; i < m_rdDir.NumberOfNamedEntries; i++) {
      const WINWCHAR* szName = m_vEntries[i]->GetName();
      int cmp = WinWStrCmp(szName, szEntName);
      if (cmp == 0)
        return false;
      if (cmp > 0)
        break;
    }
    m_rdDir.NumberOfNamedEntries++;
  }
  else {
    for (i = m_rdDir.NumberOfNamedEntries; i < m_rdDir.NumberOfNamedEntries+m_rdDir.NumberOfIdEntries; i++) {
      if (m_vEntries[i]->GetId() == entry->GetId()) return false;
      if (m_vEntries[i]->GetId() > entry->GetId()) break;
    }
    m_rdDir.NumberOfIdEntries++;
  }
  m_vEntries.insert(m_vEntries.begin() + i, entry);
  return true;
}

void CResourceDirectory::RemoveEntry(int i) {
  if (m_vEntries[i]->HasName())
    m_rdDir.NumberOfNamedEntries--;
  else
    m_rdDir.NumberOfIdEntries--;
  delete m_vEntries[i];
  m_vEntries.erase(m_vEntries.begin() + i);
}

unsigned int CResourceDirectory::CountEntries() {
  return truncate_cast(unsigned int,m_vEntries.size());
}

// Returns the index of a directory entry with the specified name
// Name can be a string or an id
// Returns -1 if can not be found
int CResourceDirectory::Find(const WINWCHAR* szName) {
  if (IS_INTRESOURCE(szName))
    return Find((WORD) (ULONG_PTR) szName);
  else
    if (szName[0] == L'#')
      return Find(WORD(WinWStrToInt(szName + 1)));

  for (unsigned int i = 0; i < m_vEntries.size(); i++) {
    if (!m_vEntries[i]->HasName())
      continue;

    const WINWCHAR* szEntName = m_vEntries[i]->GetName();
    int cmp = WinWStrCmp(szName, szEntName);
    if (!cmp)
      return i;
  }
  return -1;
}

// Returns the index of a directory entry with the specified id
// Returns -1 if can not be found
int CResourceDirectory::Find(WORD wId) {
  for (unsigned int i = 0; i < m_vEntries.size(); i++) {
    if (m_vEntries[i]->HasName()) continue;
    if (wId == m_vEntries[i]->GetId()) return i;
  }
  return -1;
}

// Get the size of this resource directory (including all of its children)
DWORD CResourceDirectory::GetSize() {
  DWORD dwSize = sizeof(IMAGE_RESOURCE_DIRECTORY);
  for (unsigned int i = 0; i < m_vEntries.size(); i++) {
    dwSize += sizeof(MY_IMAGE_RESOURCE_DIRECTORY_ENTRY);
    if (m_vEntries[i]->HasName())
      dwSize += sizeof(IMAGE_RESOURCE_DIR_STRING_U) + (m_vEntries[i]->GetNameLength()+1)*sizeof(WINWCHAR);
    if (m_vEntries[i]->IsDataDirectory())
      dwSize += m_vEntries[i]->GetSubDirectory()->GetSize();
    else {
      DWORD dwAligned = m_vEntries[i]->GetDataEntry()->GetSize();
      ALIGN(dwAligned, 8);
      dwSize += sizeof(IMAGE_RESOURCE_DATA_ENTRY) + dwAligned;
    }
  }
  return dwSize;
}

// Destroys this directory and all of its children
void CResourceDirectory::Destroy() {
  for (unsigned int i = 0; i < m_vEntries.size(); i++) {
    if (m_vEntries[i]->IsDataDirectory()) {
      m_vEntries[i]->GetSubDirectory()->Destroy();
      delete m_vEntries[i]->GetSubDirectory();
    }
    else
      delete m_vEntries[i]->GetDataEntry();
    delete m_vEntries[i];
  }
  m_vEntries.clear();
}

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
// CResourceDirectoryEntry
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CResourceDirectoryEntry::CResourceDirectoryEntry(const WINWCHAR* szName, CResourceDirectory* rdSubDir) {
  if (IS_INTRESOURCE(szName)) {
    m_bHasName = false;
    m_szName = 0;
    m_wId = (WORD) (ULONG_PTR) szName;
  }
  else {
    m_bHasName = true;
    m_szName = WinWStrDupFromWinWStr(szName);
  }
  m_bIsDataDirectory = true;
  m_rdSubDir = rdSubDir;
}

CResourceDirectoryEntry::CResourceDirectoryEntry(const WINWCHAR* szName, CResourceDataEntry* rdeData) {
  if (IS_INTRESOURCE(szName)) {
    m_bHasName = false;
    m_szName = 0;
    m_wId = (WORD) (ULONG_PTR) szName;
  }
  else {
    m_bHasName = true;
    m_szName = WinWStrDupFromWinWStr(szName);
  }
  m_bIsDataDirectory = false;
  m_rdeData = rdeData;
}

CResourceDirectoryEntry::~CResourceDirectoryEntry() {
  if (m_bHasName)
    free(m_szName);
}

//////////////////////////////////////////////////////////////////////
// Methods
//////////////////////////////////////////////////////////////////////

bool CResourceDirectoryEntry::HasName() const {
  return m_bHasName;
}

// Don't forget to free the memory used by the string after usage!
const WINWCHAR* CResourceDirectoryEntry::GetName() const {
  return m_bHasName ? m_szName : 0;
}

int CResourceDirectoryEntry::GetNameLength() const {
  return (int) WinWStrLen(m_szName);
}

WORD CResourceDirectoryEntry::GetId() const {
  return m_bHasName ? 0 : m_wId;
}

bool CResourceDirectoryEntry::IsDataDirectory() const {
  return m_bIsDataDirectory;
}

CResourceDirectory* CResourceDirectoryEntry::GetSubDirectory() const {
  if (!m_bIsDataDirectory)
    return NULL;
  return m_rdSubDir;
}

CResourceDataEntry* CResourceDirectoryEntry::GetDataEntry() const {
  if (m_bIsDataDirectory)
    return NULL;
  return m_rdeData;
}

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
// CResourceDataEntry
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CResourceDataEntry::CResourceDataEntry(BYTE* pbData, DWORD dwSize, DWORD dwCodePage, DWORD dwOffset) {
  m_pbData = 0;
  SetData(pbData, dwSize, dwCodePage);
  m_dwOffset = dwOffset;
}

CResourceDataEntry::~CResourceDataEntry() {
  if (m_pbData)
    delete [] m_pbData;
}

//////////////////////////////////////////////////////////////////////
// Methods
//////////////////////////////////////////////////////////////////////

// To save memory this function doesn't give you a copy of the data
// Don't mess with the data returned from this function!
BYTE* CResourceDataEntry::GetData() {
  return m_pbData;
}

void CResourceDataEntry::SetData(BYTE* pbData, DWORD dwSize) {
  SetData(pbData, dwSize, m_dwCodePage);
}

void CResourceDataEntry::SetData(BYTE* pbData, DWORD dwSize, DWORD dwCodePage) {
  if (m_pbData) delete [] m_pbData;
  m_pbData = new BYTE[dwSize];
  CopyMemory(m_pbData, pbData, dwSize);
  m_dwSize = dwSize;
  m_dwCodePage = dwCodePage;
  m_dwOffset = DWORD(-1); // unset
}

DWORD CResourceDataEntry::GetSize() {
  return m_dwSize;
}

DWORD CResourceDataEntry::GetCodePage() {
  return m_dwCodePage;
}

DWORD CResourceDataEntry::GetOffset() {
  return m_dwOffset;
}
