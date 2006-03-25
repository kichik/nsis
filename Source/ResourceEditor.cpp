/*
  Copyright (C) 2002-2006 Amir Szekely <kichik@users.sourceforge.net>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
  claim that you wrote the original software. If you use this software
  in a product, an acknowledgment in the product documentation would be
  appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must not be
  misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.
*/

#include "ResourceEditor.h"
#include "util.h"
#include <time.h>
#include <queue>
using namespace std;

//////////////////////////////////////////////////////////////////////
// Utilities
//////////////////////////////////////////////////////////////////////

#define ALIGN(dwToAlign, dwAlignOn) dwToAlign = (dwToAlign%dwAlignOn == 0) ? dwToAlign : dwToAlign - (dwToAlign%dwAlignOn) + dwAlignOn
#define RALIGN(dwToAlign, dwAlignOn) ((dwToAlign%dwAlignOn == 0) ? dwToAlign : dwToAlign - (dwToAlign%dwAlignOn) + dwAlignOn)

static inline DWORD ConvertEndianness(DWORD d) {
  return FIX_ENDIAN_INT32(d);
}

static inline LONG ConvertEndianness(LONG l) {
  return FIX_ENDIAN_INT32(l);
}

static inline WORD ConvertEndianness(WORD w) {
  return FIX_ENDIAN_INT16(w);
}

static PIMAGE_NT_HEADERS GetNTHeaders(BYTE* pbPE) {
  // Get dos header
  PIMAGE_DOS_HEADER dosHeader = (PIMAGE_DOS_HEADER) pbPE;
  if (dosHeader->e_magic != IMAGE_DOS_SIGNATURE)
    throw runtime_error("PE file contains invalid DOS header");

  // Get NT headers
  PIMAGE_NT_HEADERS ntHeaders = (PIMAGE_NT_HEADERS)(pbPE + ConvertEndianness(dosHeader->e_lfanew));
  if (ntHeaders->Signature != IMAGE_NT_SIGNATURE)
    throw runtime_error("PE file missing NT signature");

  return ntHeaders;
}

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
// CResourceEditor
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CResourceEditor::CResourceEditor(BYTE* pbPE, int iSize) {
  // Copy the data pointer
  m_pbPE = pbPE;
  m_iSize = iSize;

  // Get NT headers
  m_ntHeaders = GetNTHeaders(m_pbPE);

  // No check sum support yet...
  if (m_ntHeaders->OptionalHeader.CheckSum)
  {
    // clear checksum (should be [re]calculated after all changes done)
    m_ntHeaders->OptionalHeader.CheckSum = 0;
    //throw runtime_error("CResourceEditor doesn't yet support check sum");
  }

  // Get resource section virtual address
  m_dwResourceSectionVA = ConvertEndianness(m_ntHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress);
  // Pointer to the sections headers array
  PIMAGE_SECTION_HEADER sectionHeadersArray = IMAGE_FIRST_SECTION(m_ntHeaders);

  m_dwResourceSectionIndex = (DWORD) -1;

  // Find resource section index in the array
  for (int i = 0; i < ConvertEndianness(m_ntHeaders->FileHeader.NumberOfSections); i++) {
    if (m_dwResourceSectionVA == ConvertEndianness(sectionHeadersArray[i].VirtualAddress)) {
      // Remember resource section index
      m_dwResourceSectionIndex = i;
      // Check for invalid resource section pointer
      if (!sectionHeadersArray[i].PointerToRawData)
        throw runtime_error("Invalid resource section pointer");

      break;
    }

    // Invalid section pointer (goes beyond the PE image)
    if (ConvertEndianness(sectionHeadersArray[i].PointerToRawData) > (unsigned int)m_iSize)
      throw runtime_error("Invalid section pointer");
  }

  // No resource section...
  if (m_dwResourceSectionIndex == (DWORD) -1)
    throw runtime_error("PE file doesn't contain any resource section");

  // Pointer to section data, the first resource directory
  DWORD dwResSecPtr = ConvertEndianness(sectionHeadersArray[m_dwResourceSectionIndex].PointerToRawData);
  PRESOURCE_DIRECTORY rdRoot = PRESOURCE_DIRECTORY(m_pbPE + dwResSecPtr);

  // Scan the resource directory
  m_cResDir = ScanDirectory(rdRoot, rdRoot);
}

CResourceEditor::~CResourceEditor() {
  if (m_cResDir) {
    m_cResDir->Destroy();
    delete m_cResDir;
  }
}

//////////////////////////////////////////////////////////////////////
// Methods
//////////////////////////////////////////////////////////////////////

// Adds/Replaces/Removes a resource.
// If lpData is 0 UpdateResource removes the resource.
bool CResourceEditor::UpdateResource(char* szType, char* szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize) {
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
      m_cResDir->AddEntry(new CResourceDirectoryEntry(szType, nameDir));
    }
    if (!langDir) {
      // Name doesn't yet exist
      langDir = new CResourceDirectory(&rd);
      nameDir->AddEntry(new CResourceDirectoryEntry(szName, langDir));
    }
    if (!data) {
      // Language doesn't yet exist, hence data nither
      data = new CResourceDataEntry(lpData, dwSize);
      langDir->AddEntry(new CResourceDirectoryEntry(MAKEINTRESOURCE(wLanguage), data));
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

bool CResourceEditor::UpdateResource(WORD szType, char* szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize) {
  return UpdateResource(MAKEINTRESOURCE(szType), szName, wLanguage, lpData, dwSize);
}

bool CResourceEditor::UpdateResource(char* szType, WORD szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize) {
  return UpdateResource(szType, MAKEINTRESOURCE(szName), wLanguage, lpData, dwSize);
}

bool CResourceEditor::UpdateResource(WORD szType, WORD szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize) {
  return UpdateResource(MAKEINTRESOURCE(szType), MAKEINTRESOURCE(szName), wLanguage, lpData, dwSize);
}

// Returns a copy of the requested resource
// Returns 0 if the requested resource can't be found
BYTE* CResourceEditor::GetResource(char* szType, char* szName, LANGID wLanguage) {
  CResourceDirectory* nameDir = 0;
  CResourceDirectory* langDir = 0;
  CResourceDataEntry* data = 0;

  int i = m_cResDir->Find(szType);
  if (i > -1) {
    nameDir = m_cResDir->GetEntry(i)->GetSubDirectory();
    i = nameDir->Find(szName);
    if (i > -1) {
      langDir = nameDir->GetEntry(i)->GetSubDirectory();
      i = 0;
      if (wLanguage)
        i = langDir->Find(wLanguage);
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

// Returns the size of the requested resource
// Returns -1 if the requested resource can't be found
int CResourceEditor::GetResourceSize(char* szType, char* szName, LANGID wLanguage) {
  CResourceDirectory* nameDir = 0;
  CResourceDirectory* langDir = 0;
  CResourceDataEntry* data = 0;

  int i = m_cResDir->Find(szType);
  if (i > -1) {
    nameDir = m_cResDir->GetEntry(i)->GetSubDirectory();
    i = nameDir->Find(szName);
    if (i > -1) {
      langDir = nameDir->GetEntry(i)->GetSubDirectory();
      i = 0;
      if (wLanguage)
        i = langDir->Find(wLanguage);
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

void CResourceEditor::FreeResource(BYTE* pbResource)
{
  if (pbResource)
    delete [] pbResource;
}

// Saves the edited PE into a buffer and returns it.
DWORD CResourceEditor::Save(BYTE* pbBuf, DWORD &dwSize) {
  unsigned int i;
  DWORD dwReqSize;

  DWORD dwFileAlign = ConvertEndianness(m_ntHeaders->OptionalHeader.FileAlignment);
  DWORD dwSecAlign = ConvertEndianness(m_ntHeaders->OptionalHeader.SectionAlignment);

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
  DWORD dwVAAdjustment = dwNewVirtualSize - dwOldVirtualSize;

  // Set the new size of the resource section (size aligned to FileAlignment)
  sectionHeadersArray[m_dwResourceSectionIndex].SizeOfRawData = ConvertEndianness(dwRsrcSizeAligned);
  // Set the virtual size as well (in memory)
  sectionHeadersArray[m_dwResourceSectionIndex].Misc.VirtualSize = ConvertEndianness(dwRsrcSize);
  ntHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].Size = ConvertEndianness(dwRsrcSize);

  // Set the new virtual size of the image
  ntHeaders->OptionalHeader.SizeOfImage = AlignVA(ntHeaders->OptionalHeader.SizeOfHeaders);
  for (i = 0; i < wNumberOfSections; i++) {
    DWORD dwSecSize = ConvertEndianness(sectionHeadersArray[i].Misc.VirtualSize);
    ntHeaders->OptionalHeader.SizeOfImage = AlignVA(AdjustVA(ntHeaders->OptionalHeader.SizeOfImage, dwSecSize));
  }

  // Set the new AddressOfEntryPoint if needed
  if (ConvertEndianness(ntHeaders->OptionalHeader.AddressOfEntryPoint) > m_dwResourceSectionVA)
    ntHeaders->OptionalHeader.AddressOfEntryPoint = AdjustVA(ntHeaders->OptionalHeader.AddressOfEntryPoint, dwVAAdjustment);

  // Set the new BaseOfCode if needed
  if (ConvertEndianness(ntHeaders->OptionalHeader.BaseOfCode) > m_dwResourceSectionVA)
    ntHeaders->OptionalHeader.BaseOfCode = AdjustVA(ntHeaders->OptionalHeader.BaseOfCode, dwVAAdjustment);

  // Set the new BaseOfData if needed
  if (ConvertEndianness(ntHeaders->OptionalHeader.BaseOfData) > m_dwResourceSectionVA)
    ntHeaders->OptionalHeader.BaseOfData = AdjustVA(ntHeaders->OptionalHeader.BaseOfData, dwVAAdjustment);

  // Refresh the headers of the sections that come after the resource section, and the data directory
  for (i = m_dwResourceSectionIndex + 1; i < wNumberOfSections; i++) {
    if (sectionHeadersArray[i].PointerToRawData) {
      AdjustVA(sectionHeadersArray[i].PointerToRawData, dwRsrcSizeAligned - dwOldRsrcSize);
    }

    // We must find the right data directory entry before we change the virtual address
    unsigned int uDataDirIdx = 0;
    for (unsigned int j = 0; j < ConvertEndianness(ntHeaders->OptionalHeader.NumberOfRvaAndSizes); j++)
      if (ntHeaders->OptionalHeader.DataDirectory[j].VirtualAddress == sectionHeadersArray[i].VirtualAddress)
        uDataDirIdx = j;

    sectionHeadersArray[i].VirtualAddress = AdjustVA(sectionHeadersArray[i].VirtualAddress, dwVAAdjustment);

    // Change the virtual address in the data directory too
    if (uDataDirIdx)
      ntHeaders->OptionalHeader.DataDirectory[uDataDirIdx].VirtualAddress = sectionHeadersArray[i].VirtualAddress;
  }

  // Write the resource section
  WriteRsrcSec(seeker);
  // Advance the pointer
  seeker += dwRsrcSizeAligned;

  // Copy everything that comes after the resource section (other sections and tacked data)
  DWORD dwLeft = m_iSize - (oldSeeker - m_pbPE);
  if (dwLeft)
    CopyMemory(seeker, oldSeeker, dwLeft);

  seeker += dwLeft;
  oldSeeker += dwLeft;

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
bool CResourceEditor::AddExtraVirtualSize2PESection(const char* pszSectionName, int addsize)
{
  PIMAGE_SECTION_HEADER sectionHeadersArray = IMAGE_FIRST_SECTION(m_ntHeaders);

  // Refresh the headers of the sections that come after the resource section, and the data directory
  for (int i = 0; i < ConvertEndianness(m_ntHeaders->FileHeader.NumberOfSections); i++) {
    if (!strcmp((LPCSTR)sectionHeadersArray[i].Name, pszSectionName)) {
      sectionHeadersArray[i].Misc.VirtualSize = AlignVA(AdjustVA(sectionHeadersArray[i].Misc.VirtualSize, addsize));

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
          m_ntHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress = dwSecVA;
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

// This function scans a give resource directory and return a CResourceDirectory object
// rdRoot must point to the root directory of the resource section
CResourceDirectory* CResourceEditor::ScanDirectory(PRESOURCE_DIRECTORY rdRoot, PRESOURCE_DIRECTORY rdToScan) {
  // Create CResourceDirectory from rdToScan
  CResourceDirectory* rdc = new CResourceDirectory(PIMAGE_RESOURCE_DIRECTORY(rdToScan));
  char* szName;
  PIMAGE_RESOURCE_DATA_ENTRY rde = NULL;

  // Go through all entries of this resource directory
  int entries = ConvertEndianness(rdToScan->Header.NumberOfNamedEntries);
  entries += ConvertEndianness(rdToScan->Header.NumberOfIdEntries);

  for (int i = 0; i < entries; i++) {
    MY_IMAGE_RESOURCE_DIRECTORY_ENTRY rd = rdToScan->Entries[i];
    rd.OffsetToData = ConvertEndianness(rd.OffsetToData);
    rd.Name = ConvertEndianness(rd.Name);

    // If this entry points to data entry get a pointer to it
    if (!rd.DirectoryOffset.DataIsDirectory)
      rde = PIMAGE_RESOURCE_DATA_ENTRY(rd.OffsetToData + (BYTE*)rdRoot);

    // If this entry has a name, translate it from Unicode
    if (rd.NameString.NameIsString) {
      PIMAGE_RESOURCE_DIR_STRING_U rds = PIMAGE_RESOURCE_DIR_STRING_U(rd.NameString.NameOffset + (char*)rdRoot);

      int mbsSize = WideCharToMultiByte(CP_ACP, 0, rds->NameString, ConvertEndianness(rds->Length), 0, 0, 0, 0);
      szName = new char[mbsSize+1];
      if (!WideCharToMultiByte(CP_ACP, 0, rds->NameString, ConvertEndianness(rds->Length), szName, mbsSize, 0, 0)) {
        throw runtime_error("Unicode conversion failed");
      }
      szName[mbsSize] = 0;
    }
    // Else, set the name to this entry's id
    else
      szName = MAKEINTRESOURCE(ConvertEndianness(rdToScan->Entries[i].Id));

    if (rd.DirectoryOffset.DataIsDirectory)
      rdc->AddEntry(
        new CResourceDirectoryEntry(
          szName,
          ScanDirectory(
            rdRoot,
            PRESOURCE_DIRECTORY(rd.DirectoryOffset.OffsetToDirectory + (BYTE*)rdRoot)
          )
        )
      );
    else
      rdc->AddEntry(
        new CResourceDirectoryEntry(
          szName,
          new CResourceDataEntry(
            (BYTE*)rdRoot + ConvertEndianness(rde->OffsetToData) - m_dwResourceSectionVA,
            ConvertEndianness(rde->Size),
            ConvertEndianness(rde->CodePage)
          )
        )
      );

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
    crd->m_dwWrittenAt = DWORD(seeker);
    seeker += sizeof(IMAGE_RESOURCE_DIRECTORY);

    for (int i = 0; i < crd->CountEntries(); i++) {
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
      rDirE.DirectoryOffset.DataIsDirectory = crd->GetEntry(i)->IsDataDirectory();
      rDirE.Id = crd->GetEntry(i)->HasName() ? 0 : crd->GetEntry(i)->GetId();
      rDirE.Id = ConvertEndianness(rDirE.Id);
      rDirE.NameString.NameIsString = (crd->GetEntry(i)->HasName()) ? 1 : 0;

      CopyMemory(seeker, &rDirE, sizeof(MY_IMAGE_RESOURCE_DIRECTORY_ENTRY));
      crd->GetEntry(i)->m_dwWrittenAt = DWORD(seeker);
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
    cRDataE->m_dwWrittenAt = DWORD(seeker);
    seeker += sizeof(IMAGE_RESOURCE_DATA_ENTRY);

    qDataEntries.pop();
  }

  /*
   * Write strings
   */
  while (!qStrings.empty()) {
    CResourceDirectoryEntry* cRDirE = qStrings.front();

    PMY_IMAGE_RESOURCE_DIRECTORY_ENTRY(cRDirE->m_dwWrittenAt)->NameString.NameOffset = ConvertEndianness(DWORD(seeker) - DWORD(pbRsrcSec));

    char* szName = cRDirE->GetName();
    WORD iLen = strlen(szName) + 1;
    WCHAR *szwName = new WCHAR[iLen];

    // MultiByteToWideChar return value includes the null char, so -1
    iLen = MultiByteToWideChar(CP_ACP, 0, szName, iLen, szwName, iLen) - 1;
    if (iLen == (WORD) -1)
      throw runtime_error("Unicode conversion failed");

    *(WORD*)seeker = ConvertEndianness(iLen);
    seeker += sizeof(WORD);
    CopyMemory(seeker, szwName, iLen*sizeof(WCHAR));
    seeker += iLen*sizeof(WCHAR);

    // Even though the number of chars is predefined a null termination is required
    *(WORD*)seeker = 0;
    seeker += sizeof(WORD);

    delete [] szName;
    delete [] szwName;

    qStrings.pop();
  }

  /*
   * Write raw resource data and set offsets in IMAGE_RESOURCE_DATA_ENTRYs.
   */
  while (!qDataEntries2.empty()) {
    CResourceDataEntry* cRDataE = qDataEntries2.front();
    CopyMemory(seeker, cRDataE->GetData(), cRDataE->GetSize());
    PIMAGE_RESOURCE_DATA_ENTRY(cRDataE->m_dwWrittenAt)->OffsetToData = ConvertEndianness(seeker - pbRsrcSec + m_dwResourceSectionVA);

    seeker += RALIGN(cRDataE->GetSize(), 8);

    qDataEntries2.pop();
  }

  /*
   * Set all of the directory entries offsets.
   */
  SetOffsets(m_cResDir, DWORD(pbRsrcSec));
}

// Sets the offsets in directory entries
void CResourceEditor::SetOffsets(CResourceDirectory* resDir, DWORD newResDirAt) {
  for (int i = 0; i < resDir->CountEntries(); i++) {
    PMY_IMAGE_RESOURCE_DIRECTORY_ENTRY rde = PMY_IMAGE_RESOURCE_DIRECTORY_ENTRY(resDir->GetEntry(i)->m_dwWrittenAt);
    if (resDir->GetEntry(i)->IsDataDirectory()) {
      rde->DirectoryOffset.DataIsDirectory = 1;
      rde->DirectoryOffset.OffsetToDirectory = resDir->GetEntry(i)->GetSubDirectory()->m_dwWrittenAt - newResDirAt;
      rde->OffsetToData = ConvertEndianness(rde->OffsetToData);
      SetOffsets(resDir->GetEntry(i)->GetSubDirectory(), newResDirAt);
    }
    else {
      rde->OffsetToData = ConvertEndianness(resDir->GetEntry(i)->GetDataEntry()->m_dwWrittenAt - newResDirAt);
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
  DWORD dwAlignment = ConvertEndianness(m_ntHeaders->OptionalHeader.SectionAlignment);

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
void CResourceDirectory::AddEntry(CResourceDirectoryEntry* entry) {
  int i = 0;
  if (entry->HasName()) {
    char* szEntName = entry->GetName();
    for (i = 0; i < m_rdDir.NumberOfIdEntries; i++) {
      char* szName = m_vEntries[i]->GetName();
      int cmp = strcmp(szName, szEntName);
      delete [] szName;
      if (cmp == 0) {
        delete [] szEntName;
        return;
      }
      if (cmp > 0)
        break;
    }
    delete [] szEntName;
    m_rdDir.NumberOfNamedEntries++;
  }
  else {
    for (i = m_rdDir.NumberOfNamedEntries; i < m_rdDir.NumberOfNamedEntries+m_rdDir.NumberOfIdEntries; i++) {
      if (m_vEntries[i]->GetId() == entry->GetId()) 
        return;
      if (m_vEntries[i]->GetId() > entry->GetId())
        break;
    }
    m_rdDir.NumberOfIdEntries++;
  }
  m_vEntries.insert(m_vEntries.begin() + i, entry);
}

void CResourceDirectory::RemoveEntry(int i) {
  if (m_vEntries[i]->HasName())
    m_rdDir.NumberOfNamedEntries--;
  else
    m_rdDir.NumberOfIdEntries--;
  delete m_vEntries[i];
  m_vEntries.erase(m_vEntries.begin() + i);
}

int CResourceDirectory::CountEntries() {
  return m_vEntries.size();
}

// Returns the index of a directory entry with the specified name
// Name can be a string or an id
// Returns -1 if can not be found
int CResourceDirectory::Find(char* szName) {
  if (IS_INTRESOURCE(szName))
    return Find((WORD) (DWORD) szName);
  else
    if (szName[0] == '#')
      return Find(WORD(atoi(szName + 1)));

  for (unsigned int i = 0; i < m_vEntries.size(); i++) {
    if (!m_vEntries[i]->HasName())
      continue;

    char* szEntName = m_vEntries[i]->GetName();
    int cmp = strcmp(szName, szEntName);
    delete [] szEntName;

    if (!cmp)
      return i;
  }

  return -1;
}

// Returns the index of a directory entry with the specified id
// Returns -1 if can not be found
int CResourceDirectory::Find(WORD wId) {
  for (unsigned int i = 0; i < m_vEntries.size(); i++) {
    if (m_vEntries[i]->HasName())
      continue;

    if (wId == m_vEntries[i]->GetId())
      return i;
  }

  return -1;
}

// Get the size of this resource directory (including all of its children)
DWORD CResourceDirectory::GetSize() {
  DWORD dwSize = sizeof(IMAGE_RESOURCE_DIRECTORY);
  for (unsigned int i = 0; i < m_vEntries.size(); i++) {
    dwSize += sizeof(MY_IMAGE_RESOURCE_DIRECTORY_ENTRY);
    if (m_vEntries[i]->HasName())
      dwSize += sizeof(IMAGE_RESOURCE_DIR_STRING_U) + (m_vEntries[i]->GetNameLength()+1)*sizeof(WCHAR);
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
  }
}

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
// CResourceDirectoryEntry
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CResourceDirectoryEntry::CResourceDirectoryEntry(char* szName, CResourceDirectory* rdSubDir) {
  if (IS_INTRESOURCE(szName)) {
    m_bHasName = false;
    m_szName = 0;
    m_wId = (WORD) (DWORD) szName;
  }
  else {
    m_bHasName = true;
    m_szName = new char[strlen(szName)+1];
    strcpy(m_szName, szName);
  }
  m_bIsDataDirectory = true;
  m_rdSubDir = rdSubDir;
}

CResourceDirectoryEntry::CResourceDirectoryEntry(char* szName, CResourceDataEntry* rdeData) {
  if (IS_INTRESOURCE(szName)) {
    m_bHasName = false;
    m_szName = 0;
    m_wId = (WORD) (DWORD) szName;
  }
  else {
    m_bHasName = true;
    m_szName = new char[strlen(szName)+1];
    strcpy(m_szName, szName);
  }
  m_bIsDataDirectory = false;
  m_rdeData = rdeData;
}

CResourceDirectoryEntry::~CResourceDirectoryEntry() {
  if (m_szName && m_bHasName)
    delete [] m_szName;
}

//////////////////////////////////////////////////////////////////////
// Methods
//////////////////////////////////////////////////////////////////////

bool CResourceDirectoryEntry::HasName() {
  return m_bHasName;
}

// Don't forget to free the memory used by the string after usage!
char* CResourceDirectoryEntry::GetName() {
  if (!m_bHasName)
    return 0;
  char* szName = 0;
  szName = new char[strlen(m_szName)+1];
  strcpy(szName, m_szName);
  return szName;
}

int CResourceDirectoryEntry::GetNameLength() {
  return strlen(m_szName);
}

WORD CResourceDirectoryEntry::GetId() {
  if (m_bHasName)
    return 0;
  return m_wId;
}

bool CResourceDirectoryEntry::IsDataDirectory() {
  return m_bIsDataDirectory;
}

CResourceDirectory* CResourceDirectoryEntry::GetSubDirectory() {
  if (!m_bIsDataDirectory)
    return NULL;
  return m_rdSubDir;
}

CResourceDataEntry* CResourceDirectoryEntry::GetDataEntry() {
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

CResourceDataEntry::CResourceDataEntry(BYTE* pbData, DWORD dwSize, DWORD dwCodePage) {
  m_pbData = 0;
  SetData(pbData, dwSize, dwCodePage);
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
}

DWORD CResourceDataEntry::GetSize() {
  return m_dwSize;
}

DWORD CResourceDataEntry::GetCodePage() {
  return m_dwCodePage;
}
