// ResourceVersionInfo.h: interface for the CResourceVersionInfo class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_RESOURCEVERSIONINFO_H__80439ADA_49DA_4623_8DA9_1663FF356E76__INCLUDED_)
#define AFX_RESOURCEVERSIONINFO_H__80439ADA_49DA_4623_8DA9_1663FF356E76__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifdef NSIS_SUPPORT_VERSION_INFO

#include <algorithm>
using namespace std;

#define VERINFO_LANGUAGE     L"040904b0"  // English language and codepage
#define VERINFO_TRANSLATION  0x04B00409   // English language and codepage
/////////////////////////////////////////////////////////////////////////////////////////////
class CResourceVersionInfo 
{
    VS_FIXEDFILEINFO m_FixedInfo;
    DefineList m_ChildStrings;
    vector< DWORD > m_Translations;
    
public:
    CResourceVersionInfo();
    virtual ~CResourceVersionInfo();
    void SetKeyValue (char *AKeyName, char *AValue);
    void AddTranslation(WORD CodePage, WORD LangID );
    void SetFileFlags(int Value);
    void SetFileVersion(int HighPart, int LowPart);
    void SetProductVersion(int HighPart, int LowPart);
    void ExportToStream(GrowBuf &strm);
};

#endif

#endif // !defined(AFX_RESOURCEVERSIONINFO_H__80439ADA_49DA_4623_8DA9_1663FF356E76__INCLUDED_)
