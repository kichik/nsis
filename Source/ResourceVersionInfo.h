// ResourceVersionInfo.h: interface for the CResourceVersionInfo class.
//
//////////////////////////////////////////////////////////////////////

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

struct version_string_list {
  int codepage;
  LANGID lang_id;
  int name;
  DefineList *pChildStrings;
};

class CVersionStrigList : public SortedStringListND<struct version_string_list>
{
public:
    ~CVersionStrigList()
    {
      struct version_string_list *itr = (struct version_string_list *) gr.get();
      int i = gr.getlen() / sizeof(struct version_string_list);

      while (i--)
      {
        delete itr[i].pChildStrings;
      }
    }

    int add(LANGID langid, int codepage)
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

    LANGID get_lang(int idx)
    {
      version_string_list *data=(version_string_list *)gr.get();
      return data[idx].lang_id;
    }

    int get_codepage(int idx)
    {
      version_string_list *data=(version_string_list *)gr.get();
      return data[idx].codepage;
    }

    DefineList* get_strings(int idx)
    {
      version_string_list *data=(version_string_list *)gr.get();
      return data[idx].pChildStrings;
    }

    int find(LANGID lang_id, int codepage)
    {
      char Buff[10];
      sprintf(Buff, "%04x", lang_id);
      return SortedStringListND<struct version_string_list>::find(Buff);
    }

    int getlen()
    {
      return strings.getlen();
    }

    int getnum()
    {
      return gr.getlen()/sizeof(struct version_string_list);
    }
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
    //bool IsValidCodePage(WORD codePage );
};

#endif

#endif // !defined(AFX_RESOURCEVERSIONINFO_H__80439ADA_49DA_4623_8DA9_1663FF356E76__INCLUDED_)
