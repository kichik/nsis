// ResourceVersionInfo.cpp: implementation of the CResourceVersionInfo class.
//
//////////////////////////////////////////////////////////////////////

#include "ResourceVersionInfo.h"

#include "Platform.h"
#include "util.h"

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
  struct version_string_list *itr = (struct version_string_list *) gr.get();
  int i = gr.getlen() / sizeof(struct version_string_list);

  while (i--)
  {
    delete itr[i].pChildStrings;
  }
}

int CVersionStrigList::add(LANGID langid, int codepage)
{
  char Buff[10];
  sprintf(Buff, "%04x", langid);
  int pos = SortedStringListND<struct version_string_list>::add(Buff);
  if (pos == -1) return false;
  ((struct version_string_list*)gr.get())[pos].pChildStrings = new DefineList;
  ((struct version_string_list*)gr.get())[pos].codepage = codepage;
  ((struct version_string_list*)gr.get())[pos].lang_id = langid;
  return pos;
}

LANGID CVersionStrigList::get_lang(int idx)
{
  version_string_list *data=(version_string_list *)gr.get();
  return data[idx].lang_id;
}

int CVersionStrigList::get_codepage(int idx)
{
  version_string_list *data=(version_string_list *)gr.get();
  return data[idx].codepage;
}

DefineList* CVersionStrigList::get_strings(int idx)
{
  version_string_list *data=(version_string_list *)gr.get();
  return data[idx].pChildStrings;
}

int CVersionStrigList::find(LANGID lang_id, int codepage)
{
  char Buff[10];
  sprintf(Buff, "%04x", lang_id);
  return SortedStringListND<struct version_string_list>::find(Buff);
}

int CVersionStrigList::getlen()
{
  return strings.getlen();
}

int CVersionStrigList::getnum()
{
  return gr.getlen()/sizeof(struct version_string_list);
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

void CResourceVersionInfo::SetFileFlags(int Value)
{
    m_FixedInfo.dwFileFlags = (m_FixedInfo.dwFileFlags & ~(m_FixedInfo.dwFileFlagsMask)) || Value;    
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

// Util function - must be freeded
WCHAR* StrToWstrAlloc(const char* istr, int codepage)
{
#ifdef _WIN32
  int strSize = MultiByteToWideChar(codepage, 0, istr, -1, 0, 0);
  WCHAR* wstr = new WCHAR[strSize];
  MultiByteToWideChar(codepage, 0, istr, -1, wstr, strSize);
  return wstr;
#else
  WCHAR *wstr = NULL;
  char cp[128] = "";
  if (codepage != CP_ACP)
    snprintf(cp, 128, "CP%d", codepage);
  iconv_t cd = iconv_open("UCS-2", cp);
  if (cd != (iconv_t) -1)
  {
    int len = strlen(istr);
    char *in = (char *) istr;
    wstr = new WCHAR[len + 1];
    char *out = (char *) wstr;
    size_t insize = len + 1;
    size_t outsize = (len + 1) * sizeof(WCHAR);
    if (__iconv_adaptor(iconv, cd, &in, &insize, &out, &outsize) == (size_t) -1)
    {
      delete [] wstr;
      wstr = NULL;
    }
    iconv_close(cd);
  }
  return wstr;
#endif
}

int GetVersionHeader (LPSTR &p, WORD &wLength, WORD &wValueLength, WORD &wType)
{
    WCHAR *szKey;
    char * baseP;
    
    baseP = p;
    wLength = *(WORD*)p;
    p += sizeof(WORD);
    wValueLength = *(WORD*)p;
    p += sizeof(WORD);
    wType = *(WORD*)p;
    p += sizeof(WORD);
    szKey = (WCHAR*)p;
    p += (WCStrLen(szKey)) * sizeof (WCHAR);
    while ( ((long)p % 4) != 0 )
        p++;
    return p - baseP;    
}

DWORD ZEROS = 0;

void PadStream (GrowBuf &strm)
{
    if ( (strm.getlen() % 4) != 0 )
        strm.add (&ZEROS, 4 - (strm.getlen() % 4));
}

void SaveVersionHeader (GrowBuf &strm, WORD wLength, WORD wValueLength, WORD wType, const WCHAR *key, void *value)
{
    WORD valueLen;
    WORD keyLen;
    
    strm.add (&wLength, sizeof (wLength));
    
    strm.add (&wValueLength, sizeof (wValueLength));
    strm.add (&wType, sizeof (wType));
    keyLen = (WCStrLen(key)) * sizeof (WCHAR);
    strm.add ((void*)key, keyLen);
    
    PadStream(strm);
    
    if ( wValueLength > 0 )
    {
        valueLen = wValueLength;
        if ( wType == 1 )
            valueLen = valueLen * sizeof (WCHAR);
        strm.add (value, valueLen);
    }
}

void CResourceVersionInfo::ExportToStream(GrowBuf &strm, int Index)
{
    DWORD v;
    WORD wSize;  
    int p, p1;
    WCHAR *KeyName, *KeyValue;

    strm.resize(0);
    KeyName = StrToWstrAlloc("VS_VERSION_INFO", CP_ACP);
    SaveVersionHeader (strm, 0, sizeof (VS_FIXEDFILEINFO), 0, KeyName, &m_FixedInfo);
    delete [] KeyName;
    
    DefineList *pChildStrings = m_ChildStringLists.get_strings(Index);
    if ( pChildStrings->getnum() > 0 )
    {
      GrowBuf stringInfoStream;
      int codepage = m_ChildStringLists.get_codepage(Index);
      LANGID langid = m_ChildStringLists.get_lang(Index);
      char Buff[16];
      sprintf(Buff, "%04x%04x", langid, codepage);
      KeyName = StrToWstrAlloc(Buff, CP_ACP);
      SaveVersionHeader (stringInfoStream, 0, 0, 0, KeyName, &ZEROS);
      delete [] KeyName;
      
      for ( int i = 0; i < pChildStrings->getnum(); i++ )
      {
        PadStream (stringInfoStream);
        
        p = stringInfoStream.getlen();
        KeyName = StrToWstrAlloc(pChildStrings->getname(i), codepage);
        KeyValue = StrToWstrAlloc(pChildStrings->getvalue(i), codepage);
        SaveVersionHeader (stringInfoStream, 0, WCStrLen(KeyValue), 1, KeyName, (void*)KeyValue);
        delete [] KeyName;
        delete [] KeyValue;
        wSize = stringInfoStream.getlen() - p;
        
        *(WORD*)((PBYTE)stringInfoStream.get()+p)=wSize;
      }
      
      wSize = stringInfoStream.getlen();
      *(WORD*)((PBYTE)stringInfoStream.get())=wSize;
      
      PadStream (strm);
      p = strm.getlen();
      KeyName = StrToWstrAlloc("StringFileInfo", CP_ACP);
      SaveVersionHeader (strm, 0, 0, 0, KeyName, &ZEROS);
      delete [] KeyName;
      strm.add (stringInfoStream.get(), stringInfoStream.getlen());
      wSize = strm.getlen() - p;
      
      *(WORD*)((PBYTE)strm.get()+p)=wSize;
    }

    // Show all languages avaiable using Var-Translations
    if ( m_ChildStringLists.getnum() > 0 )
    {
      PadStream (strm);
      p = strm.getlen();
      KeyName = StrToWstrAlloc("VarFileInfo", CP_ACP);
      SaveVersionHeader (strm, 0, 0, 0, KeyName, &ZEROS);
      delete [] KeyName;
      PadStream (strm);
      
      p1 = strm.getlen();
      KeyName = StrToWstrAlloc("Translation", CP_ACP);
      SaveVersionHeader (strm, 0, 0, 0, KeyName, &ZEROS);
      delete [] KeyName;
      
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
      
      wSize = strm.getlen() - p1;
      *(WORD*)((PBYTE)strm.get()+p1)=wSize;
      wSize = sizeof (int) * m_ChildStringLists.getnum();
      p1+=sizeof(WORD);
      *(WORD*)((PBYTE)strm.get()+p1)=wSize;
      
      wSize = strm.getlen() - p;
      *(WORD*)((PBYTE)strm.get()+p)=wSize;
    }
    
    wSize = strm.getlen();
    *(WORD*)((PBYTE)strm.get())=wSize;
}

// Returns 0 if success, 1 if already defined
int CResourceVersionInfo::SetKeyValue(LANGID lang_id, int codepage, char* AKeyName, char* AValue)
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

char *CResourceVersionInfo::FindKey(LANGID LangID, int codepage, char *pKeyName)
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
