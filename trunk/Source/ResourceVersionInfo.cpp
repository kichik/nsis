/*
 * ResourceVersionInfo.cpp: implementation of the CResourceVersionInfo class.
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2025 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 * 
 * Modified for Unicode support by Jim Park -- 08/21/2007
 */

#include "ResourceVersionInfo.h"

#include "Platform.h"
#include "util.h"
#include "winchar.h"
#include "utf.h"

#ifdef NSIS_SUPPORT_VERSION_INFO

#ifndef VOS__WINDOWS32
#  define VOS__WINDOWS32 4
#endif
#ifndef VFT_APP
#  define VFT_APP 1
#endif

#ifndef _WIN32
#  include <iconv.h>
#endif

struct version_string_list
{
  int codepage;
  LANGID lang_id;
  int name;
  DefineList *pChildStrings;
};

CVersionStrigList::~CVersionStrigList()
{
  struct version_string_list *itr = (struct version_string_list *) m_gr.get();
  int i = m_gr.getlen() / sizeof(struct version_string_list);

  while (i--)
  {
    delete itr[i].pChildStrings;
  }
}

int CVersionStrigList::add(LANGID langid, int codepage)
{
  TCHAR Buff[10];
  _stprintf(Buff, _T("%04x"), langid);
  int pos = SortedStringListND<struct version_string_list>::add(Buff);
  if (pos == -1) return false;
  
  version_string_list *data = ((version_string_list *)m_gr.get())+ pos;
  data->pChildStrings = new DefineList;
  data->codepage      = codepage;
  data->lang_id       = langid;
  return pos;
}

LANGID CVersionStrigList::get_lang(int idx)
{
  version_string_list *data=(version_string_list *)m_gr.get();
  return data[idx].lang_id;
}

int CVersionStrigList::get_codepage(int idx)
{
  version_string_list *data=(version_string_list *)m_gr.get();
  return data[idx].codepage;
}

DefineList* CVersionStrigList::get_strings(int idx)
{
  version_string_list *data=(version_string_list *)m_gr.get();
  return data[idx].pChildStrings;
}

int CVersionStrigList::find(LANGID lang_id, int codepage)
{
  TCHAR Buff[10];
  _stprintf(Buff, _T("%04x"), lang_id);
  return SortedStringListND<struct version_string_list>::find(Buff);
}

int CVersionStrigList::getnum()
{
  return m_gr.getlen()/sizeof(struct version_string_list);
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CResourceVersionInfo::CResourceVersionInfo()
{
    memset(&m_FixedInfo, 0, sizeof(VS_FIXEDFILEINFO));
    m_FixedInfo.dwSignature = 0xFEEF04BD;
    m_FixedInfo.dwFileOS = VOS__WINDOWS32;
    m_FixedInfo.dwFileType = VFT_APP;
}

CResourceVersionInfo::~CResourceVersionInfo()
{
    
}

void CResourceVersionInfo::SetFileVersion(int HighPart, int LowPart)
{
    m_FixedInfo.dwFileVersionLS = LowPart;
    m_FixedInfo.dwFileVersionMS = HighPart;
}

void CResourceVersionInfo::SetProductVersion(int HighPart, int LowPart)
{
    m_FixedInfo.dwProductVersionLS = LowPart;
    m_FixedInfo.dwProductVersionMS = HighPart;
}

// Jim Park: Not sure where this is used.
int GetVersionHeader (LPSTR &p, WORD &wLength, WORD &wValueLength, WORD &wType)
{
    WINWCHAR *szKey;
    char *baseP = p;

    wLength = *(WORD*)p;
    p += sizeof(WORD);
    wValueLength = *(WORD*)p;
    p += sizeof(WORD);
    wType = *(WORD*)p;
    p += sizeof(WORD);
    szKey = (WINWCHAR*)p;
    p += (WinWStrLen(szKey) + 1) * sizeof (WINWCHAR);
    while ( ((ULONG_PTR)p % 4) != 0 ) p++;

    return (int)(p - baseP);
}

DWORD ZEROS = 0;

void PadStream (GrowBuf &strm)
{
    if ( (strm.getlen() % 4) != 0 )
        strm.add (&ZEROS, 4 - (strm.getlen() % 4));
}

// Helper function only used by CResourceVersionInfo::ExportToStream
// Cannot handle anything longer than 65K objects.
//
// @param wLength Size in bytes of the entire object we are storing.
// @param wValueLength The value length in bytes.
// @param wType If type is 1, it's a wchar_t string, so save value length appropriately.
// @param key The string key
// @param value The value mapped to string key.
static void SaveVersionHeaderUTF16LE(GrowBuf &strm, WORD wLength, WORD wValueLength, WORD wType, const unsigned short *key, void *value)
{
    WORD valueLen;
    WORD keyLen;
    
    strm.add (&wLength, sizeof(wLength));
    
    strm.add (&wValueLength, sizeof(wValueLength));
    strm.add (&wType, sizeof (wType));
    keyLen = WORD((StrLenUTF16(key) + 1) * sizeof(WINWCHAR));
    strm.add ((void*)key, keyLen);
    
    PadStream(strm);
    
    if ( wValueLength > 0 )
    {
        valueLen = wValueLength;
        if ( wType == 1 )
            valueLen = valueLen * WORD(sizeof(WINWCHAR));
        strm.add (value, valueLen);
    }
}
static void SaveVersionHeader(GrowBuf &strm, WORD wLength, WORD wValueLength, WORD wType, const wchar_t *key, void *value)
{
  WCToUTF16LEHlpr cnv;
  if (!cnv.Create(key)) throw std::runtime_error("Unicode conversion failed");
  SaveVersionHeaderUTF16LE(strm, wLength, wValueLength, wType, cnv.Get(), value);
  cnv.Destroy();
}

void CResourceVersionInfo::ExportToStream(GrowBuf &strm, int Index)
{
    DWORD v;
    WORD wSize;  
    int p, p1;

    strm.resize(0);
    SaveVersionHeader(strm, 0, sizeof (VS_FIXEDFILEINFO), 0, L"VS_VERSION_INFO", &m_FixedInfo);
    
    DefineList *pChildStrings = m_ChildStringLists.get_strings(Index);
    if ( pChildStrings->getnum() > 0 )
    {
      GrowBuf stringInfoStream;
      int codepage = m_ChildStringLists.get_codepage(Index);
      LANGID langid = m_ChildStringLists.get_lang(Index);
      wchar_t Buff[16];
      _snwprintf(Buff, COUNTOF(Buff), L"%04x%04x", langid, codepage);
      SaveVersionHeader(stringInfoStream, 0, 0, 0, Buff, &ZEROS);
      
      for ( int i = 0; i < pChildStrings->getnum(); i++ )
      {
        PadStream (stringInfoStream);
        WCToUTF16LEHlpr cnvName, cnvValue;
        if (!cnvName.Create(pChildStrings->getname(i), codepage)) throw std::runtime_error("Unicode conversion failed");
        if (!cnvValue.Create(pChildStrings->getvalue(i), codepage)) throw std::runtime_error("Unicode conversion failed");
        p = stringInfoStream.getlen();
        SaveVersionHeaderUTF16LE(stringInfoStream, 0, WORD(StrLenUTF16(cnvValue.Get()) + 1), 1, cnvName.Get(), (void*)cnvValue.Get());
        cnvName.Destroy(), cnvValue.Destroy();
        wSize = WORD(stringInfoStream.getlen() - p);
        
        *(WORD*)((PBYTE)stringInfoStream.get()+p)=wSize;
      }
      
      wSize = WORD(stringInfoStream.getlen());
      *(WORD*)((PBYTE)stringInfoStream.get())=wSize;
      
      PadStream (strm);
      p = strm.getlen();
      SaveVersionHeader(strm, 0, 0, 0, L"StringFileInfo", &ZEROS);
      strm.add (stringInfoStream.get(), stringInfoStream.getlen());
      wSize = WORD(strm.getlen() - p);
      
      *(WORD*)((PBYTE)strm.get()+p)=wSize;
    }

    // Show all languages available using Var-Translations
    if ( m_ChildStringLists.getnum() > 0 )
    {
      PadStream (strm);
      p = strm.getlen();
      SaveVersionHeader(strm, 0, 0, 0, L"VarFileInfo", &ZEROS);
      PadStream (strm);
      
      p1 = strm.getlen();
      SaveVersionHeader(strm, 0, 0, 0, L"Translation", &ZEROS);
      
      // First add selected code language translation
      v = MAKELONG(m_ChildStringLists.get_lang(Index), m_ChildStringLists.get_codepage(Index));
      strm.add (&v, sizeof (v));

      for ( int k =0; k < m_ChildStringLists.getnum(); k++ )
      {
        if ( k != Index )
        {
          v = MAKELONG(m_ChildStringLists.get_lang(k), m_ChildStringLists.get_codepage(k));
          strm.add (&v, sizeof (v));
        }
      }
      
      wSize = WORD(strm.getlen() - p1);
      *(WORD*)((PBYTE)strm.get()+p1)=wSize;
      wSize = WORD(sizeof (int) * m_ChildStringLists.getnum());
      p1+=sizeof(WORD);
      *(WORD*)((PBYTE)strm.get()+p1)=wSize;
      
      wSize = WORD(strm.getlen() - p);
      *(WORD*)((PBYTE)strm.get()+p)=wSize;
    }
    
    wSize = WORD(strm.getlen());
    *(WORD*)((PBYTE)strm.get())=wSize;
}

// Returns 0 if success, 1 if already defined
int CResourceVersionInfo::SetKeyValue(LANGID lang_id, int codepage, TCHAR* AKeyName, TCHAR* AValue)
{
  int pos = m_ChildStringLists.find(lang_id, codepage);
  if ( pos == -1 )
  {
    pos = m_ChildStringLists.add(lang_id, codepage);
  }
  DefineList *pStrings = m_ChildStringLists.get_strings(pos);
  return pStrings->add(AKeyName, AValue);
}

int CResourceVersionInfo::GetStringTablesCount()
{
  return m_ChildStringLists.getnum();
}

LANGID CResourceVersionInfo::GetLangID(int Index)
{
  return m_ChildStringLists.get_lang(Index);
}

int CResourceVersionInfo::GetCodePage(int Index)
{
  return m_ChildStringLists.get_codepage(Index);
}

TCHAR *CResourceVersionInfo::FindKey(LANGID LangID, int codepage, const TCHAR *pKeyName)
{
  int pos = m_ChildStringLists.find(LangID, codepage);
  if ( pos == -1 )
  {
    return NULL;
  }
  DefineList *pStrings = m_ChildStringLists.get_strings(pos);
  return pStrings->find(pKeyName);
}

#endif
