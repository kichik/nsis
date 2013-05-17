// tstring.cpp
//
// This file is a part of Unicode NSIS.
//
// Copyright (C) 2007-2013 Jim Park
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
#include "util.h"
#include <vector>
#include <stdio.h>


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

CtoTString::operator const wchar_t*() const { return GetTStr(); }
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

TtoCString::operator const char*() const { return m_cStr; }

#endif
