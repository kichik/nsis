/*
  Copyright (C) 2002-2005 Amir Szekely <kichik@netvision.net.il>

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

#if !defined(AFX_RESOURCEEDITOR_H__683BF710_E805_4093_975B_D5729186A89A__INCLUDED_)
#define AFX_RESOURCEEDITOR_H__683BF710_E805_4093_975B_D5729186A89A__INCLUDED_


#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


#include <vector>

#include "Platform.h"
#ifdef _WIN32
#  include <WinNT.h>
#else
// all definitions for non Win32 platforms were taken from MinGW's free Win32 library
#  define IMAGE_DIRECTORY_ENTRY_RESOURCE  2
#  define IMAGE_SCN_MEM_DISCARDABLE 0x2000000
#  pragma pack(4)
typedef struct _IMAGE_RESOURCE_DIRECTORY {
  DWORD Characteristics;
  DWORD TimeDateStamp;
  WORD MajorVersion;
  WORD MinorVersion;
  WORD NumberOfNamedEntries;
  WORD NumberOfIdEntries;
} IMAGE_RESOURCE_DIRECTORY,*PIMAGE_RESOURCE_DIRECTORY;
typedef struct _IMAGE_RESOURCE_DATA_ENTRY {
  DWORD OffsetToData;
  DWORD Size;
  DWORD CodePage;
  DWORD Reserved;
} IMAGE_RESOURCE_DATA_ENTRY,*PIMAGE_RESOURCE_DATA_ENTRY;
typedef struct _IMAGE_RESOURCE_DIRECTORY_STRING {
  WORD Length;
  CHAR NameString[1];
} IMAGE_RESOURCE_DIRECTORY_STRING,*PIMAGE_RESOURCE_DIRECTORY_STRING;
typedef struct _IMAGE_RESOURCE_DIR_STRING_U {
  WORD Length;
  WCHAR NameString[1];
} IMAGE_RESOURCE_DIR_STRING_U,*PIMAGE_RESOURCE_DIR_STRING_U;
#endif

#pragma pack(4)
typedef struct _MY_IMAGE_RESOURCE_DIRECTORY_ENTRY {
  union {
    struct {
      DWORD NameOffset:31;
      DWORD NameIsString:1;
    } NameString;
    DWORD Name;
    WORD Id;
  };
  union {
    DWORD OffsetToData;
    struct {
      DWORD OffsetToDirectory:31;
      DWORD DataIsDirectory:1;
    } DirectoryOffset;
  };
} MY_IMAGE_RESOURCE_DIRECTORY_ENTRY,*PMY_IMAGE_RESOURCE_DIRECTORY_ENTRY;

#pragma pack()

#include <stdexcept>

class CResourceDirectory;
class CResourceDirectoryEntry;
class CResourceDataEntry;

// Resource directory with entries
typedef struct RESOURCE_DIRECTORY {
  IMAGE_RESOURCE_DIRECTORY Header;
  MY_IMAGE_RESOURCE_DIRECTORY_ENTRY Entries[1];
} *PRESOURCE_DIRECTORY;

class CResourceEditor {
public:
  CResourceEditor(BYTE* pbPE, int iSize);
  virtual ~CResourceEditor();

  bool  UpdateResource(char* szType, char* szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize);
  bool  UpdateResource(WORD szType, char* szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize);
  bool  UpdateResource(char* szType, WORD szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize);
  bool  UpdateResource(WORD szType, WORD szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize);
  BYTE* GetResource(char* szType, char* szName, LANGID wLanguage);
  int   GetResourceSize(char* szType, char* szName, LANGID wLanguage);
  void  FreeResource(BYTE* pbResource);

  bool  AddExtraVirtualSize2PESection(const char* pszSectionName, int addsize);
  DWORD Save(BYTE* pbBuf, DWORD &dwSize);

private:
  BYTE* m_pbPE;
  int   m_iSize;

  PIMAGE_DOS_HEADER m_dosHeader;
  PIMAGE_NT_HEADERS m_ntHeaders;

  DWORD m_dwResourceSectionIndex;
  DWORD m_dwResourceSectionVA;

  CResourceDirectory* m_cResDir;

  CResourceDirectory* ScanDirectory(PRESOURCE_DIRECTORY rdRoot, PRESOURCE_DIRECTORY rdToScan);

  void WriteRsrcSec(BYTE* pbRsrcSec);
  void SetOffsets(CResourceDirectory* resDir, DWORD newResDirAt);
};

class CResourceDirectory {
public:
  CResourceDirectory(PIMAGE_RESOURCE_DIRECTORY prd);
  virtual ~CResourceDirectory();

  IMAGE_RESOURCE_DIRECTORY GetInfo();

  CResourceDirectoryEntry* GetEntry(unsigned int i);
  void AddEntry(CResourceDirectoryEntry* entry);
  void RemoveEntry(int i);
  int  CountEntries();
  int  Find(char* szName);
  int  Find(WORD wId);

  DWORD GetSize();

  void Destroy();

  DWORD m_dwWrittenAt;

private:
  IMAGE_RESOURCE_DIRECTORY m_rdDir;
  std::vector<CResourceDirectoryEntry*> m_vEntries;
};

class CResourceDirectoryEntry {
public:
  CResourceDirectoryEntry(char* szName, CResourceDirectory* rdSubDir);
  CResourceDirectoryEntry(char* szName, CResourceDataEntry* rdeData);
  virtual ~CResourceDirectoryEntry();

  bool HasName();
  char* GetName();
  int GetNameLength();

  WORD GetId();

  bool IsDataDirectory();
  CResourceDirectory* GetSubDirectory();

  CResourceDataEntry* GetDataEntry();

  DWORD m_dwWrittenAt;

private:
  bool m_bHasName;
  union {
    char* m_szName;
    WORD m_wId;
  };

  bool m_bIsDataDirectory;
  union {
    CResourceDirectory* m_rdSubDir;
    CResourceDataEntry* m_rdeData;
  };
};

class CResourceDataEntry {
public:
  CResourceDataEntry(BYTE* pbData, DWORD dwSize, DWORD dwCodePage = 0);
  ~CResourceDataEntry();

  BYTE* GetData();

  void SetData(BYTE* pbData, DWORD dwSize);
  void SetData(BYTE* pbData, DWORD dwSize, DWORD dwCodePage);

  DWORD GetSize();
  DWORD GetCodePage();

  DWORD m_dwWrittenAt;

private:
  BYTE* m_pbData;
  DWORD m_dwSize;
  DWORD m_dwCodePage;
};

#endif // !defined(AFX_RESOURCEEDITOR_H__683BF710_E805_4093_975B_D5729186A89A__INCLUDED_)
