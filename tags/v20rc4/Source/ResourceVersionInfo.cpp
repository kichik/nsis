// ResourceVersionInfo.cpp: implementation of the CResourceVersionInfo class.
//
//////////////////////////////////////////////////////////////////////

#include "Platform.h"
#include "build.h"

#include "ResourceVersionInfo.h"
#ifdef NSIS_SUPPORT_VERSION_INFO
/*
int ValidCodePages[] = {
437, 708, 709, 710, 720, 737, 775, 850, 852, 855, 85, 86, 86, 86, 86, 864,
865, 866, 869, 874, 932, 936, 949, 950, 1200, 1250, 1251, 1252, 1253, 1254,
1255, 1256, 1257, 1258, 20000, 20001, 20002, 20003, 20004, 20005, 20127, 20261,
20269, 20866, 21027, 21866, 28591, 28592, 28593, 28594, 28595, 28596, 28597, 28598,
28599, 29001, 1361, 0 };
*/
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CResourceVersionInfo::CResourceVersionInfo()
{
    memset(&m_FixedInfo, 0, sizeof(VS_FIXEDFILEINFO));
    m_FixedInfo.dwSignature = 0xFEEF04BD;
    m_FixedInfo.dwFileOS = VOS__WINDOWS32;
    m_FixedInfo.dwFileType = VFT_APP;

    // Detect local codepage and language
    WORD Lang = GetSystemDefaultLangID();
    WORD CodePage = GetACP();
/*
    SetKeyValue(Lang, CodePage, "Comments", "Portuguese");
    SetKeyValue(Lang, CodePage, "FileVersion", "1.2");
    SetKeyValue(Lang, CodePage, "FileDescription", "Soft");
    SetKeyValue(Lang, CodePage, "LegalCopyright", "My");
    SetKeyValue(Lang, CodePage, "InternalName", "My");
    SetKeyValue(Lang, CodePage, "CompanyName", "rats");
    SetKeyValue(Lang, CodePage, "ProductVersion", "ProductVersion");

    Lang = MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US);
    SetKeyValue(Lang, CodePage, "Comments", "English");
    SetKeyValue(Lang, CodePage, "FileVersion", "1.2");
    SetKeyValue(Lang, CodePage, "FileDescription", "Soft");
    SetKeyValue(Lang, CodePage, "LegalCopyright", "My");
    SetKeyValue(Lang, CodePage, "InternalName", "My");
    SetKeyValue(Lang, CodePage, "CompanyName", "rats");
    SetKeyValue(Lang, CodePage, "ProductVersion", "ProductVersion");
*/
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
  int strSize = MultiByteToWideChar(codepage, 0, istr, -1, 0, 0);
  WCHAR* wstr = new WCHAR[strSize];
  MultiByteToWideChar(codepage, 0, istr, -1, wstr, strSize);
  return wstr;
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
    p += (wcslen(szKey) + 1) * sizeof (WCHAR);
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
    keyLen = (wcslen(key) + 1) * sizeof (WCHAR);
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
    SaveVersionHeader (strm, 0, sizeof (VS_FIXEDFILEINFO), 0, L"VS_VERSION_INFO", &m_FixedInfo);
    
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
        SaveVersionHeader (stringInfoStream, 0, wcslen(KeyValue) + 1, 1, KeyName, (void*)KeyValue);
        delete [] KeyName;
        delete [] KeyValue;
        wSize = stringInfoStream.getlen() - p;
        
        *(WORD*)((PBYTE)stringInfoStream.get()+p)=wSize;
      }
      
      wSize = stringInfoStream.getlen();
      *(WORD*)((PBYTE)stringInfoStream.get())=wSize;
      
      PadStream (strm);
      p = strm.getlen();
      SaveVersionHeader (strm, 0, 0, 0, L"StringFileInfo", &ZEROS);
      strm.add (stringInfoStream.get(), stringInfoStream.getlen());
      wSize = strm.getlen() - p;
      
      *(WORD*)((PBYTE)strm.get()+p)=wSize;
    }

    // Show all languages avaiable using Var-Translations
    if ( m_ChildStringLists.getnum() > 0 )
    {
      PadStream (strm);
      p = strm.getlen();
      SaveVersionHeader (strm, 0, 0, 0, L"VarFileInfo", &ZEROS);
      PadStream (strm);
      
      p1 = strm.getlen();
      SaveVersionHeader (strm, 0, 0, 0, L"Translation", &ZEROS);
      
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
/*
bool CResourceVersionInfo::IsValidCodePage(WORD codePage )
{
  int *pCP = ValidCodePages;
  if ( !codePage )
    return false;
  while ( *pCP++ )
  {
    if ( *pCP == codePage )
      return true;  
  }
  return false;
}
*/
#endif