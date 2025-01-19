/*
 * ResourceEditor.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2002-2025 Amir Szekely <kichik@users.sourceforge.net>
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

#if !defined(AFX_RESOURCEEDITOR_H__683BF710_E805_4093_975B_D5729186A89A__INCLUDED_)
#define AFX_RESOURCEEDITOR_H__683BF710_E805_4093_975B_D5729186A89A__INCLUDED_


#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Platform.h"
#include "winchar.h"
#include <vector>
#include <cassert>

#define MAIN_ICON_LAST_IMAGE 99 // Main icon is special, we must reserve space for installer/uninstaller images

#ifdef _WIN32
#include <winnt.h>
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
#  pragma pack()
#endif

#pragma pack(4)
typedef struct _MY_IMAGE_RESOURCE_DIRECTORY_ENTRY {
  union {
    struct {
#ifndef __BIG_ENDIAN__
      DWORD NameOffset:31;
      DWORD NameIsString:1;
#else
      DWORD NameIsString:1;
      DWORD NameOffset:31;
#endif
    } NameString;
    DWORD Name;
    WORD Id;
  } UName;
  union {
    DWORD OffsetToData;
    struct {
#ifndef __BIG_ENDIAN__
      DWORD OffsetToDirectory:31;
      DWORD DataIsDirectory:1;
#else
      DWORD DataIsDirectory:1;
      DWORD OffsetToDirectory:31;
#endif
    } DirectoryOffset;
  } UOffset;
} MY_IMAGE_RESOURCE_DIRECTORY_ENTRY,*PMY_IMAGE_RESOURCE_DIRECTORY_ENTRY;

#pragma pack()

#include <stdexcept>

// classes
class CResourceDirectory;
class CResourceDirectoryEntry;
class CResourceDataEntry;

// Resource directory with entries
typedef struct RESOURCE_DIRECTORY {
  IMAGE_RESOURCE_DIRECTORY Header;
  MY_IMAGE_RESOURCE_DIRECTORY_ENTRY Entries[1];
} *PRESOURCE_DIRECTORY;

#define GetCommonStructField(ref, s1, s2, fld) \
  ( (&((ref).fld))[(1 / ( 0 + !!(FIELD_OFFSET(s1, fld) == FIELD_OFFSET(s2, fld) && sizeof(((s1*)0)->fld) == sizeof(((s2*)0)->fld)) )) - 1] ) // Try to fail at compile-time if the field is not at the same offset in both structs or does not have the same size
#define GetCommonMemberFromPEOptHdr(OptHdr, Member) \
  ( &GetCommonStructField(OptHdr, IMAGE_OPTIONAL_HEADER32, IMAGE_OPTIONAL_HEADER64, Member) )
#define GetMemberFromPEOptHdrEx(OptHdr, Member, Sixtyfour) \
  ( (Sixtyfour) ? \
    &((PIMAGE_OPTIONAL_HEADER64)&(OptHdr))->Member : \
    &((PIMAGE_OPTIONAL_HEADER32)&(OptHdr))->Member \
  )
#define GetMemberFromPEOptHdr(OptHdr, Member) \
  ( GetMemberFromPEOptHdrEx(OptHdr, Member, ((OptHdr).Magic == IMAGE_NT_OPTIONAL_HDR64_MAGIC)) )

class CResourceEditor {
public:
  CResourceEditor(void* pbPE, int iSize, bool bKeepData = true);
  virtual ~CResourceEditor();
  enum { ANYLANGID = 0xffff, INVALIDLANGID = 0xffff-1, ALLLANGID = 0xffff-2 };
  typedef enum { TM_RAW = 0, TM_ICONFILE = 0x01, TM_ICONRSRC = 0x02, TM_ICON = (TM_ICONFILE|TM_ICONRSRC), TM_AUTO = 0x04 } TYPEMANIPULATION;

  // On POSIX+Unicode GetResource(RT_VERSION,..) is not TCHAR nor WINWCHAR, it is WCHAR/UINT16 (MAKEINTRESOURCEW).
  // If it passes IS_INTRESOURCE we must allow it.
  // Use TCHAR* for real strings. If you need to pass in a WINWCHAR*, make GetResourceW public...
  template<class T> bool UpdateResourceFromExternal(const T*Type, WORD Name, LANGID Lang, const TCHAR*File, TYPEMANIPULATION Manip = TM_AUTO)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return false; }
    return UpdateResourceFromExternalT((const TCHAR*) Type, Name, Lang, File, Manip);
  }
  template<class T> bool UpdateResource(const T*Type, WORD Name, LANGID Lang, BYTE*Data, DWORD Size, TYPEMANIPULATION Manip = TM_RAW)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return false; }
    return UpdateResourceT((const TCHAR*) Type, Name, Lang, Data, Size, Manip);
  }
  template<class T> bool UpdateResource(const T*Type, WORD Name, LANGID Lang, FILE*Data, TYPEMANIPULATION Manip = TM_AUTO)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return false; }
    return UpdateResourceT((const TCHAR*) Type, Name, Lang, Data, Manip);
  }
  template<class T> bool DeleteResource(const T*Type, WORD Name, LANGID Lang, TYPEMANIPULATION Manip = TM_RAW)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return false; }
    return DeleteResourceT((const TCHAR*) Type, Name, Lang, Manip);
  }
  template<class T> BYTE* GetResource(const T*Type, WORD Name, LANGID Lang)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return NULL; }
    return GetResourceT((const TCHAR*) Type, Name, Lang);
  }
  template<class T> int GetResourceSize(const T*Type, WORD Name, LANGID Lang)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return -1; }
    return GetResourceSizeT((const TCHAR*) Type, Name, Lang);
  }
  template<class T> DWORD GetResourceOffset(const T*Type, WORD Name, LANGID Lang)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return -1; }
    return GetResourceOffsetT((const TCHAR*) Type, Name, Lang);
  }
  template<class T> bool ResourceExists(const T*Type, WORD Name, LANGID Lang, LANGID*pFoundLanguage = 0)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return false; }
    return ResourceExistsT((const TCHAR*) Type, Name, Lang, pFoundLanguage);
  }
  template<class T> BYTE* GetFirstResource(const T*Type, size_t&cbData)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return NULL; }
    return GetFirstResourceT((const TCHAR*) Type, cbData);
  }
  template<class T> BYTE* ExtractIcoCur(const T*Type, WORD Name, LANGID Lang, size_t&cbData)
  {
    if (sizeof(T) != sizeof(TCHAR) && !IS_INTRESOURCE(Type)) { assert(IS_INTRESOURCE(Type)); return NULL; }
    return ExtractIcoCurT((const TCHAR*) Type, Name, Lang, cbData);
  }

  CResourceDataEntry* FindResourceT(const TCHAR*RT, const TCHAR*RN, LANGID RL, CResourceDirectoryEntry**pTE, CResourceDirectoryEntry**pNE, CResourceDirectoryEntry**pLE) const;
  bool  UpdateResourceFromExternalT(const TCHAR* Type, WORD Name, LANGID Lang, const TCHAR*File, TYPEMANIPULATION Manip = TM_AUTO);
  bool  UpdateResourceT   (const TCHAR* szType, WORD szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize, TYPEMANIPULATION Manip = TM_RAW);
  bool  UpdateResourceT   (const TCHAR* szType, WORD szName, LANGID wLanguage, FILE*Data, TYPEMANIPULATION Manip = TM_AUTO);
  bool  DeleteResourceT   (const TCHAR* szType, WORD szName, LANGID wLanguage, TYPEMANIPULATION Manip = TM_RAW);
  BYTE* GetResourceT      (const TCHAR* szType, WORD szName, LANGID wLanguage);
  int   GetResourceSizeT  (const TCHAR* szType, WORD szName, LANGID wLanguage);
  DWORD GetResourceOffsetT(const TCHAR* szType, WORD szName, LANGID wLanguage);
  bool  ResourceExistsT   (const TCHAR* szType, WORD szName, LANGID wLanguage, LANGID*pFoundLanguage = 0);
  BYTE* GetFirstResourceT (const TCHAR* szType, size_t&cbData);
  BYTE* ExtractIcoCurT    (const TCHAR* szType, WORD szName, LANGID wLanguage, size_t&cbData) const;
  BYTE* ExtractIcoCur(const CResourceDataEntry&rde, LANGID ChildLang, size_t&cbData) const;
  void  FreeResource(BYTE* pbResource);

  // The section name must be in ASCII.
  bool  SetPESectionVirtualSize(const char* pszSectionName, DWORD newsize);
  DWORD Save(BYTE* pbBuf, DWORD &dwSize);

  // utility functions
  static PIMAGE_NT_HEADERS GetNTHeaders(BYTE* pbPE);

  static PRESOURCE_DIRECTORY GetResourceDirectory(
    BYTE* pbPE,
    DWORD dwSize,
    PIMAGE_NT_HEADERS ntHeaders,
    DWORD *pdwResSecVA = NULL,
    DWORD *pdwSectionIndex = NULL
  );

  static const TCHAR* ParseResourceTypeString(const TCHAR*String);
  static const TCHAR* ParseResourceNameString(const TCHAR*String, bool AllowFirst = false);
  static LANGID ParseResourceLangString(const TCHAR*String);
  static LANGID ParseResourceTypeNameLangString(const TCHAR**Type, const TCHAR**Name, const TCHAR*Lang, bool AllowFirst = false);
  static bool CanOpen(const void*Data, size_t Size);
  static UINT IsResProtocol(const TCHAR*Url);
  typedef struct { BYTE*Data; size_t cbData, Map[2]; void*FreeThis; const TCHAR*RT, *RN; LANGID RL; } EXTERNAL;
  static void FreeExternal(EXTERNAL&External);
  static const TCHAR* MapExternal(const TCHAR*File, TYPEMANIPULATION Manip, EXTERNAL&External);
  static bool EditorSupportsStringNames() { return false; } // UpdateResource/GetResource do not support string names (yet)
  static bool EditorSupportsCursorPng() { return false; }

private:
  bool  UpdateResourceW   (const WINWCHAR* szType, WINWCHAR* szName, LANGID wLanguage, BYTE* lpData, DWORD dwSize, TYPEMANIPULATION Manip = TM_RAW);
  BYTE* GetResourceW      (const WINWCHAR* szType, WINWCHAR* szName, LANGID wLanguage);
  int   GetResourceSizeW  (const WINWCHAR* szType, WINWCHAR* szName, LANGID wLanguage);
  DWORD GetResourceOffsetW(const WINWCHAR* szType, WINWCHAR* szName, LANGID wLanguage);
  BYTE* GetFirstResourceW (const WINWCHAR* szType, size_t&cbData);
  BYTE* ExtractIcoCurW    (const WINWCHAR* szType, const WINWCHAR* szName, LANGID wLanguage, size_t&cbData) const;
  CResourceDataEntry* FindResource(const WINWCHAR*RT, const WINWCHAR*RN, LANGID RL) const;
  CResourceDataEntry* FindResourceW(const WINWCHAR*RT, const WINWCHAR*RN, LANGID RL, CResourceDirectoryEntry**ppTE, CResourceDirectoryEntry**ppNE, CResourceDirectoryEntry**ppLE) const;
  CResourceDirectoryEntry* FindResourceLanguageDirEntryW(const WINWCHAR* RT, const WINWCHAR* RN, LANGID RL) const;
  CResourceDirectoryEntry* FindResourceLanguageDirEntryT(const TCHAR* RT, const TCHAR* RN, LANGID RL) const;
  bool DeleteIconImages(const CResourceDirectoryEntry& LangDir);
  bool DeleteIconImagesW(const WINWCHAR* OwnerType, WINWCHAR* Name, LANGID LangId);
  bool AddExtraIconFromFile(const WINWCHAR* Type, WINWCHAR* Name, LANGID LangId, BYTE* Data, DWORD Size);
  CResourceDataEntry* FindIcoCurDataEntry(WORD Type, WORD Id, LANGID PrefLang) const;

  BYTE* DupData(CResourceDataEntry*pDE); // Free with FreeResource
  CResourceDirectory* ScanDirectory(PRESOURCE_DIRECTORY rdRoot, PRESOURCE_DIRECTORY rdToScan);
  void WriteRsrcSec(BYTE* pbRsrcSec);
  void SetOffsets(CResourceDirectory* resDir, ULONG_PTR newResDirAt);
  DWORD AdjustVA(DWORD dwVirtualAddress, DWORD dwAdjustment);
  DWORD AlignVA(DWORD dwVirtualAddress);

private:
  BYTE* m_pbPE;
  int   m_iSize;
  bool  m_bKeepData;

  PIMAGE_NT_HEADERS m_ntHeaders;

  DWORD m_dwResourceSectionIndex;
  DWORD m_dwResourceSectionVA;
  CResourceDirectory* m_cResDir;
};

class CResourceDirectory {
public:
  CResourceDirectory(PIMAGE_RESOURCE_DIRECTORY prd);
  virtual ~CResourceDirectory();

  IMAGE_RESOURCE_DIRECTORY GetInfo();

  CResourceDirectoryEntry* GetEntry(unsigned int i);
  bool AddEntry(CResourceDirectoryEntry* entry);
  void RemoveEntry(int i);
  unsigned int  CountEntries();
  int  Find(const WINWCHAR* szName);
  int  Find(WORD wId);

  DWORD GetSize();

  void Destroy();

  ULONG_PTR m_ulWrittenAt;

private:
  IMAGE_RESOURCE_DIRECTORY m_rdDir;
  std::vector<CResourceDirectoryEntry*> m_vEntries;
};

class CResourceDirectoryEntry {
public:
  CResourceDirectoryEntry(const WINWCHAR* szName, CResourceDirectory* rdSubDir);
  CResourceDirectoryEntry(const WINWCHAR* szName, CResourceDataEntry* rdeData);
  virtual ~CResourceDirectoryEntry();

  const WINWCHAR* GetNameOrId() const { return HasName() ? GetName() : (WINWCHAR*)(size_t) GetId(); }
  bool HasName() const;
  const WINWCHAR* GetName() const;
  int GetNameLength() const;
  WORD GetId() const;
  bool IsDataDirectory() const;
  CResourceDirectory* GetSubDirectory() const;
  CResourceDataEntry* GetDataEntry() const;

  ULONG_PTR m_ulWrittenAt;

private:
  bool m_bHasName;
  WINWCHAR* m_szName;
  WORD m_wId;

  bool m_bIsDataDirectory;
  union {
    CResourceDirectory* m_rdSubDir;
    CResourceDataEntry* m_rdeData;
  };
};

class CResourceDataEntry {
public:
  CResourceDataEntry(BYTE* pbData, DWORD dwSize, DWORD dwCodePage = 0, DWORD dwOffset = DWORD(-1));
  ~CResourceDataEntry();

  BYTE* GetData() const;
  void SetData(BYTE* pbData, DWORD dwSize);
  void SetData(BYTE* pbData, DWORD dwSize, DWORD dwCodePage);

  DWORD GetSize() const;
  DWORD GetCodePage() const;
  DWORD GetOffset() const;

  ULONG_PTR m_ulWrittenAt;

private:
  BYTE* m_pbData;
  DWORD m_dwSize;
  DWORD m_dwCodePage;
  DWORD m_dwOffset;
};

#endif // !defined(AFX_RESOURCEEDITOR_H__683BF710_E805_4093_975B_D5729186A89A__INCLUDED_)
