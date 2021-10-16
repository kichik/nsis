/*
 * ResourceVersionInfo.h: interface for the CResourceVersionInfo class.
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Unicode support and Doxygen comments by Jim Park -- 07/26/2007
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

struct version_string_list;

class CVersionStrigList : public SortedStringListND<struct version_string_list>
{
public:
  ~CVersionStrigList();

  /**
   * Add a version_string_list struct referred to by langid.  Then add the
   * codepage value to the structure.
   *
   * @param langid The language ID (LANGID)
   * @param codepage The code page value to set.
   * @return The position to the inserted structure, false (0) if failed.
   */
  int add(LANGID langid, int codepage);

  /**
   * Get the language ID given the positional index idx.
   */
  LANGID get_lang(int idx);

  /**
   * Get the codepage value given the positional index idx.
   */
  int get_codepage(int idx);

  /**
   * Get the string pair mappings given the positional index idx.
   */
  DefineList* get_strings(int idx);

  /**
   * Given a language ID return the positional index that holds the
   * version_string_list struct.  Actually, the codepage value is ignored.
   */
  int find(LANGID lang_id, int codepage);

  /**
   * Get the number of version_string_list objects stored in this list.
   */
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
  int SetKeyValue(LANGID lang_id, int codepage, TCHAR* AKeyName, TCHAR* AValue);

  /**
   * Set the file version.
   */
  void SetFileVersion(int HighPart, int LowPart);

  /**
   * Set the product version.
   */
  void SetProductVersion(int HighPart, int LowPart);

  /**
   * Write the data out to the flat buffer 'strm'.  Not sure where and how
   * it gets read back in though.
   */
  void ExportToStream(GrowBuf &strm, int Index);

  /**
   * How many string tables are we storing in the m_ChildStringLists?
   */
  int GetStringTablesCount();

  /**
   * Given a positional index, get the Language ID associated with it.
   */
  LANGID GetLangID(int Index);

  /**
   * Given a positional index, get the CodePage associated with it.
   */
  int GetCodePage(int Index);

  /**
   * Given the language ID, codepage, and the 'keyname', return the
   * TCHAR* pointer to the value portion of the key-value pair.
   *
   * @param LangID The language ID.
   * @param codepage The codepage.  (Not used.)
   * @param pKeyName The key name in the key-value pair of strings.
   * @return The value string associated with the key string.  NULL
   * if not found.
   */
  TCHAR *FindKey(LANGID LangID, int codepage, const TCHAR *pKeyName);
};

#endif

#endif // !defined(AFX_RESOURCEVERSIONINFO_H__80439ADA_49DA_4623_8DA9_1663FF356E76__INCLUDED_)
