/*
 * utf.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2011 Anders Kjersem
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 */

#ifndef NSIS_UTF_H
#define NSIS_UTF_H

#include "Platform.h"
#include <stdlib.h>
#include <stdio.h>

typedef unsigned short EXEHEADWCHAR_T;


#ifdef _UNICODE
typedef EXEHEADWCHAR_T EXEHEADTCHAR_T;

void RawTStrToASCII(const TCHAR*in,char*out,UINT maxcch);
#else // !_UNICODE
typedef char EXEHEADTCHAR_T;


inline EXEHEADTCHAR_T* ExeHeadTStrAlloc(UINT cb) 
{
  // NOTE: UTF8ToExeHeadTStrDup calls this so we are required to use malloc
  return (EXEHEADTCHAR_T*) malloc(cb);
}
extern EXEHEADTCHAR_T* UTF8ToExeHeadTStrDup(LPCSTR StrU8,UINT Codepage);

inline void RawTStrToASCII(const TCHAR*in,char*out,UINT maxcch) { lstrcpyn(out,in,maxcch); }
#endif // ?_UNICODE

template<typename T> T S7ChLwr(T c) { return c>='A' && c<='Z' ? (T)(c|32) : c; }
template<typename T> T S7ChUpr(T c) { return c>='a' && c<='z' ? (T)(c-'a'+'A') : c; }
template<typename T> bool S7IsChEqualI(char ch,T cmp)
{
  return cmp==(T)S7ChLwr(ch) || cmp==(T)S7ChUpr(ch);
}


/**
 * Tries to peek at the first few bytes in the stream to determine if it is a UTF-8 BOM.
 * If it is a UTF-8 BOM it will eat the BOM, 
 * if it is not it tries its best to restore the data.
 */
extern bool IsUTF8BOM(FILE*fstrm);

#endif // NSIS_UTF_H
