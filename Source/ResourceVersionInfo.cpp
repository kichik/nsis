// ResourceVersionInfo.cpp: implementation of the CResourceVersionInfo class.
//
//////////////////////////////////////////////////////////////////////

#include <windows.h>
#include "build.h"

#include "ResourceVersionInfo.h"
#ifdef NSIS_SUPPORT_VERSION_INFO
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

// Util function
wstring StrToWstr(const string& istr)
{
    wstring wstr;
    for(string::const_iterator it = istr.begin(); it != istr.end(); ++it)
    {
        wstr += *it;
    } return wstr;
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

void SaveVersionHeader (GrowBuf &strm, WORD wLength, WORD wValueLength, WORD wType, const wstring &key, void *value)
{
    WORD valueLen;
    WORD keyLen;
    
    strm.add (&wLength, sizeof (wLength));
    
    strm.add (&wValueLength, sizeof (wValueLength));
    strm.add (&wType, sizeof (wType));
    keyLen = (key.length() + 1) * sizeof (WCHAR);
    strm.add ((void*)key.c_str(), keyLen);
    
    PadStream(strm);
    
    if ( wValueLength > 0 )
    {
        valueLen = wValueLength;
        if ( wType == 1 )
            valueLen = valueLen * sizeof (WCHAR);
        strm.add (value, valueLen);
    }
}

void CResourceVersionInfo::ExportToStream(GrowBuf &strm)
{
    DWORD v;
    WORD wSize;  
    int p, p1;
    wstring KeyName, KeyValue;
    
    SaveVersionHeader (strm, 0, sizeof (VS_FIXEDFILEINFO), 0, L"VS_VERSION_INFO", &m_FixedInfo);
    
    if ( m_ChildStrings.getnum() > 0 )
    {
        GrowBuf stringInfoStream;
        
        SaveVersionHeader (stringInfoStream, 0, 0, 0, VERINFO_LANGUAGE, &ZEROS);
        
        for ( int i = 0; i < m_ChildStrings.getnum(); i++ )
        {
            PadStream (stringInfoStream);
            
            p = stringInfoStream.getlen();
            KeyName = StrToWstr(m_ChildStrings.getname(i));
            KeyValue = StrToWstr(m_ChildStrings.getvalue(i));
            SaveVersionHeader (stringInfoStream, 0, KeyValue.length() + 1, 1, KeyName.c_str(), (void*)KeyValue.c_str());
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

    if ( m_Translations.size() > 0 )
    {
        PadStream (strm);
        p = strm.getlen();
        SaveVersionHeader (strm, 0, 0, 0, L"VarFileInfo", &ZEROS);
        PadStream (strm);
        
        p1 = strm.getlen();
        SaveVersionHeader (strm, 0, 0, 0, L"Translation", &ZEROS);
        
        for ( int i = 0; i < m_Translations.size(); i++ )
        {
            v = m_Translations[i];
            strm.add (&v, sizeof (v));
        }
        
        wSize = strm.getlen() - p1;
        *(WORD*)((PBYTE)strm.get()+p1)=wSize;
        wSize = sizeof (int) * m_Translations.size();
        p1+=sizeof(WORD);
        *(WORD*)((PBYTE)strm.get()+p1)=wSize;
        
        wSize = strm.getlen() - p;
        *(WORD*)((PBYTE)strm.get()+p)=wSize;
    }
    
    wSize = strm.getlen();
    *(WORD*)((PBYTE)strm.get())=wSize;
}

void CResourceVersionInfo::SetKeyValue(char* AKeyName, char* AValue)
{
    m_ChildStrings.add(AKeyName, AValue);
}

void CResourceVersionInfo::AddTranslation(WORD CodePage, WORD LangID )
{
    DWORD dwTrans = MAKELONG(LangID, CodePage);
    if ( find(m_Translations.begin(), m_Translations.end(), dwTrans) == m_Translations.end() )
        m_Translations.push_back(dwTrans);
}
#endif