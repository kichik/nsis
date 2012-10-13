/*
 * utf.cpp
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

#include "utf.h"

// BUGBUG: We might want to use MB_ERR_INVALID_CHARS but it is not supported
// on < WinXP or in our current POSIX implementation.
static const int UTF8MBTWCFLAGS  = 0;


#define ExeHeadWStrFree free
static EXEHEADWCHAR_T* ExeHeadWStrAlloc(UINT cch) 
{
  EXEHEADWCHAR_T* s = (EXEHEADWCHAR_T*) malloc(cch*sizeof(EXEHEADWCHAR_T));
#if 0
  // TODO: We should add POSIX versions of  G/SetLastError
  // if we want to tell _why_ UTF8ToExeHeadTStr failed...
  if (!s) SetLastError(ERROR_OUTOFMEMORY);
#endif
  return s;
}

#ifdef _UNICODE

void RawTStrToASCII(const TCHAR*in,char*out,UINT maxcch)
{
  const bool empty = !maxcch;
  for(; maxcch && *in; --maxcch) *out++ = (char) *in++;
  if (!empty) *out = 0;
}

#else // !_UNICODE

EXEHEADTCHAR_T* UTF8ToExeHeadTStrDup(LPCSTR StrU8,UINT Codepage) 
{
  int cchW = MultiByteToWideChar(CP_UTF8,UTF8MBTWCFLAGS,StrU8,-1,NULL,0);
  if (!cchW) return NULL;
  WCHAR *bufWStr = (WCHAR*) ExeHeadWStrAlloc(cchW);
  if (!bufWStr) return NULL;
  EXEHEADTCHAR_T *outstr = NULL;
  if (MultiByteToWideChar(CP_UTF8,UTF8MBTWCFLAGS,StrU8,-1,bufWStr,cchW))
  {
    int cbA = WideCharToMultiByte(Codepage,0,bufWStr,cchW,NULL,0,NULL,NULL);
    if (cbA && (outstr = ExeHeadTStrAlloc(cbA)))
    {
      if (!WideCharToMultiByte(Codepage,0,bufWStr,cchW,outstr,cbA,NULL,NULL))
      {
        free(outstr);
        outstr = NULL;
      }
    }
  }
  ExeHeadWStrFree(bufWStr);
  return outstr;
}

#endif // ?_UNICODE


bool IsUTF8BOM(FILE*fstrm) 
{
  // ungetc is only guaranteed to support 1 pushback, 
  // lets hope no ASCII file starts with 0xEF and is not a BOM!
  const int c = fgetc(fstrm);
  if (EOF == c) return false;
  if (0xef == c)
  {
    const int c2 = fgetc(fstrm);
    if (0xbb == c2)
    {
      const int c3 = fgetc(fstrm);
      if (0xbf == c3) return true;
      ungetc(c3,fstrm);
    }
    ungetc(c2,fstrm);
  }
  ungetc(c,fstrm);
  return false;
}
