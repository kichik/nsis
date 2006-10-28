/*
 * ResourceVersionInfo.h: interface for the CResourceVersionInfo class.
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2006 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#if !defined(AFX_RESOURCEVERSIONINFO_H__80439ADA_49DA_4623_8DA9_1663FF356E76__INCLUDED_)
#define AFX_RESOURCEVERSIONINFO_H__80439ADA_49DA_4623_8DA9_1663FF356E76__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "exehead/config.h"
#ifdef NSIS_SUPPORT_VERSION_INFO

#include "Platform.h"
#include "strlist.h"
#ifndef _WIN32
// all definitions for non Win32 platforms were taken from MinGW's free Win32 library
typedef struct tagVS_FIXEDFILEINFO {
  DWORD dwSignature;
  DWORD dwStrucVersion;
  DWORD dwFileVersionMS;
  DWORD dwFileVersionLS;
  DWORD dwProductVersionMS;
  DWORD dwProductVersionLS;
  DWORD dwFileFlagsMask;
  DWORD dwFileFlags;
  DWORD dwFileOS;
  DWORD dwFileType;
  DWORD dwFileSubtype;
  DWORD dwFileDateMS;
  DWORD dwFileDateLS;
} VS_FIXEDFILEINFO;
#endif

struct version_string_list;

class CVersionStrigList : public SortedStringListND<struct version_string_list>
{
public:
  ~CVersionStrigList();
  int add(LANGID langid, int codepage);
  LANGID get_lang(int idx);
  int get_codepage(int idx);
  DefineList* get_strings(int idx);
  int find(LANGID lang_id, int codepage);
  int getlen();
  int getnum();
};

/////////////////////////////////////////////////////////////////////////////////////////////
class CResourceVersionInfo 
{
  VS_FIXEDFILEINFO m_FixedInfo;
  CVersionStrigList m_ChildStringLists;
    
public:
  CResourceVersionInfo();
  virtual ~CResourceVersionInfo();
  int SetKeyValue(LANGID lang_id, int codepage, char* AKeyName, char* AValue);
  void SetFileFlags(int Value);
  void SetFileVersion(int HighPart, int LowPart);
  void SetProductVersion(int HighPart, int LowPart);
  void ExportToStream(GrowBuf &strm, int Index);
  int GetStringTablesCount();
  LANGID GetLangID(int Index);
  int GetCodePage(int Index);
  char *FindKey(LANGID LangID, int codepage, char *pKeyName);
};

#endif

#endif // !defined(AFX_RESOURCEVERSIONINFO_H__80439ADA_49DA_4623_8DA9_1663FF356E76__INCLUDED_)
