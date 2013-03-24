// tstring.cpp
//
// This file is a part of Unicode NSIS.
//
// Copyright (C) 2007-2009 Jim Park
//
// Licensed under the zlib/libpng license (the "License");
// you may not use this file except in compliance with the License.
//
// This software is provided 'as-is', without any expressed or implied
// warranty.
//
// Provides TSTRING support.

#ifdef _UNICODE

#include "tstring.h"
#include "validateunicode.h"
#include "util.h"
#include <vector>
#include <stdio.h>

FILE* FileOpenUnicodeText(const TCHAR* file, const TCHAR* mode, BOOL* unicode)
{
	extern FILE *g_output;
	CValidateUnicode::FILE_TYPE ftype = CValidateUnicode::UTF_8; // default file format is UTF-8
	if (unicode) *unicode = TRUE;

	// If we are reading an existing file, check to see what type of file it
	// is first.
	if (_tcsstr(mode, _T("w+")) ||
	    _tcsstr(mode, _T("r")))
	{
		FILE* fp = _tfopen(file, _T("rb"));

		if (fp)
		{
			MANAGE_WITH(fp, fclose);
			fseek(fp, 0, SEEK_END);
			size_t fileSize = ftell(fp);
			if (fileSize == 0)
			{
				// Empty files are treated as UTF-8.
				ftype = CValidateUnicode::UTF_8;
			}
			else
			{
				std::vector<unsigned char> buffer(fileSize);
				fseek(fp, 0, SEEK_SET);
				fread(&buffer[0], sizeof(unsigned char), fileSize, fp);

				ftype = CValidateUnicode::CheckBOM(&buffer[0], buffer.size());

				switch (ftype)
				{
				case CValidateUnicode::UTF_8:
				case CValidateUnicode::UTF_16LE:
				case CValidateUnicode::UTF_16BE:
					break;
				case CValidateUnicode::UTF_32LE:
				case CValidateUnicode::UTF_32BE:
					PrintColorFmtMsg_ERR(_T("File '%s' has a BOM marked as %s which is not supported at this time.\n"),
					   file, CValidateUnicode::TypeToName(ftype));
					exit(-1);
					break;
				case CValidateUnicode::UNKNOWN:
					// If unknown, let's see if it's not just UTF_8 without a BOM.
					if (CValidateUnicode::ValidateUTF8(&buffer[0], buffer.size()) == 2)
					{
						// contains UTF-8 characters sequences
						_ftprintf(g_output, _T("File '%s' has no BOM but seems to be UTF-8.\n"), file);
						ftype = CValidateUnicode::UTF_8;
					}
					break;
				default:
					PrintColorFmtMsg_ERR(_T("CValidateUnicode::CheckBOM() for file '%s' returned an unknown return value: %d\n"),
					   file, ftype);
					exit(-1);
					break;
				}
			}
		}
	}

	tstring strMode(mode);

	switch (ftype)
	{
		case CValidateUnicode::UTF_8:
			strMode.append(_T(", ccs=UTF-8"));
			break;
		case CValidateUnicode::UTF_16LE:
			strMode.append(_T(", ccs=UTF-16LE"));
			break;
		default:
			// Looks like fopen() doesn't support other encodings of Unicode.
			if (unicode) *unicode = FALSE;
			break;
	}

	return _tfopen(file, strMode.c_str());
}

CtoTString::CtoTString(const char* str)
{
	int len = MultiByteToWideChar(CP_ACP, 0, str, -1, NULL, 0);
	m_wStr = (wchar_t*) malloc(len*sizeof(wchar_t));
	MultiByteToWideChar(CP_ACP, 0, str, -1, m_wStr, len);
}

CtoTString::CtoTString(const char* str, UINT cp)
{
	int len = MultiByteToWideChar(cp, 0, str, -1, NULL, 0);
	m_wStr = (wchar_t*) malloc(len*sizeof(wchar_t));
	MultiByteToWideChar(cp, 0, str, -1, m_wStr, len);
}

CtoTString::CtoTString(const std::string& str)
{
	int len = MultiByteToWideChar(CP_ACP, 0, str.c_str(), str.length()+1, NULL, 0);
	m_wStr = (wchar_t*) malloc(len*sizeof(wchar_t));
	MultiByteToWideChar(CP_ACP, 0, str.c_str(), str.length()+1, m_wStr, len);
}

CtoTString::~CtoTString() { free(m_wStr); m_wStr = 0; }

operator const CtoTString::wchar_t*() const { return GetTStr(); }
inline const wchar_t* CtoTString::GetTStr() const { return m_wStr; }



TtoCString::TtoCString(const wchar_t* wStr)
{
	int len = WideCharToMultiByte(CP_ACP, 0, wStr, -1, NULL, 0, 0, 0);
	m_cStr = (char*) malloc(len);
	WideCharToMultiByte(CP_ACP, 0, wStr, -1, m_cStr, len, 0, 0);
}

TtoCString::TtoCString(const tstring& wStr)
{
	int len = WideCharToMultiByte(CP_ACP, 0, wStr.c_str(), wStr.length()+1, NULL, 0, 0, 0);
	m_cStr = (char*) malloc(len);
	WideCharToMultiByte(CP_ACP, 0, wStr.c_str(), wStr.length()+1, m_cStr, len, 0, 0);
}

TtoCString::~TtoCString() { free(m_cStr); m_cStr = 0; }

operator const TtoCString::char*() const { return m_cStr; }

#endif
