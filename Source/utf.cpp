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

#define FIX_ENDIAN_INT16LETOHOST_INPLACE FIX_ENDIAN_INT16_INPLACE

void RawTStrToASCII(const TCHAR*in,char*out,UINT maxcch)
{
  const bool empty = !maxcch;
  for(; maxcch && *in; --maxcch) *out++ = (char) *in++;
  if (!empty) *out = 0;
}

UINT StrLenUTF16LE(const void*str)
{
  unsigned short *p = (unsigned short *) str;
  for(;*p;) ++p;
  UINT cch = 0;
  if ((size_t)p > (size_t)str) cch = ((size_t)p - (size_t)str) - 1;
  return cch;
}

bool StrSetUTF16LE(tstring&dest, const void*src)
{
#ifdef _WIN32
  dest = (unsigned short *) src;
#else
#error TODO: UTF16LE to wchar_t
#endif
  return true;
}

inline UINT UTF8ToWC_Convert(LPCSTR StrU8,UINT cbU8,wchar_t*Buffer,UINT cchBuf)
{
#ifndef MB_ERR_INVALID_CHARS
  const UINT MB_ERR_INVALID_CHARS = 8; // MSDN says this flag is OK for CP_UTF8
#endif
  return (UINT) MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,StrU8,cbU8,Buffer,cchBuf);
}
inline UINT UTF8ToWC_Prepare(LPCSTR StrU8,UINT cbU8)
{
  return UTF8ToWC_Convert(StrU8,cbU8,0,0);
}

wchar_t* DupWCFromBytes(void*Buffer,UINT cbBuffer,WORD SrcCP)
{
  /*\
  Converts a buffer encoded with SrcCP to a \0 terminated wchar_t malloc'ed buffer.
  Returns 0 if malloc failed or -1 if conversion to wchar_t failed.
  \*/
  NStreamEncoding srcenc(SrcCP);
  wchar_t*pwc = 0;
#ifdef _WIN32 
  if (srcenc.IsUTF16LE())
  {
    // Assuming wchar_t==UTF16LE
    pwc = (wchar_t*) malloc(cbBuffer + 2);
    if (!pwc) return pwc;
    memcpy(pwc, Buffer, cbBuffer);
    *((wchar_t*)(((char*)pwc)+cbBuffer)) = L'\0';
    return pwc;
  }
  // TODO: MBTWC on Windows is lame, we are going to fail if SrcCP is UTF16BE or UTF32
#endif
  UINT cchW = MultiByteToWideChar(SrcCP,0,(char*)Buffer,cbBuffer,0,0);
  if (!cchW && NStreamEncoding::GetCodeUnitSize(SrcCP) <= cbBuffer)
  {
    return (wchar_t*)-1;
  }
  pwc = (wchar_t*) malloc((cchW+1)*sizeof(wchar_t));
  if (!pwc) return pwc;
  MultiByteToWideChar(SrcCP,0,(char*)Buffer,cbBuffer,pwc,cchW);
  pwc[cchW] = L'\0';
  return pwc;
}

UINT DetectUTFBOM(FILE*strm)
{
  /*\
  Tries to detect a BOM at the start of a stream. If a BOM is found it is eaten.
  NOTE: ungetc is only guaranteed to support 1 pushback, 
  lets hope no MBCS file starts with parts of a BOM.
  \*/
  const int b1 = fgetc(strm);
  if (EOF == b1) return 0;
  if (0xef == b1)
  {
    const int b2 = fgetc(strm);
    if (0xbb == b2)
    {
      const int b3 = fgetc(strm);
      if (0xbf == b3) return NStreamEncoding::UTF8;
      ungetc(b3,strm);
    }
    ungetc(b2,strm);
  }
  if (0xfe == b1 || 0xff == b1 || 0x00 == b1)
  {
    const int b2 = fgetc(strm), b3 = fgetc(strm);
    if (b1 && (b1^b2) == (0xfe^0xff))
    {
      if (0xff == b1 && 0 == b3)
      {
        const int b4 = fgetc(strm);
        if (0 == b4) return NStreamEncoding::UTF32LE;
        ungetc(b4,strm);
      }
      ungetc(b3,strm);
      return 0xff == b1 ? NStreamEncoding::UTF16LE : NStreamEncoding::UTF16BE;
    }
    if (0 == b1 && 0 == b2)
    {
      if (0xfe == b3)
      {
        const int b4 = fgetc(strm);
        if (0xff == b4) return NStreamEncoding::UTF32BE;
        ungetc(b4,strm);
      }
    }
    ungetc(b3,strm);
    ungetc(b2,strm);
  }
  ungetc(b1,strm);
  return 0;
}

WORD GetEncodingFromString(const TCHAR*s)
{
  if (!_tcsicmp(s,_T("ACP"))) return NStreamEncoding::ACP;
  if (!_tcsicmp(s,_T("OEM"))) return NStreamEncoding::OEMCP;
  if (!_tcsicmp(s,_T("UTF8"))) return NStreamEncoding::UTF8;
  if (!_tcsicmp(s,_T("UTF16LE"))) return NStreamEncoding::UTF16LE;
  if (!_tcsicmp(s,_T("UTF16BE"))) return NStreamEncoding::UTF16BE;
  if (S7IsChEqualI('C',*s++) && S7IsChEqualI('P',*s++))
  {
    int cp = _tstoi(s);
    if (cp > 0 && cp < NStreamEncoding::CPCOUNT) return (WORD) cp;
  }
  return NStreamEncoding::UNKNOWN;
}

void NStreamEncoding::GetCPDisplayName(WORD CP, TCHAR*Buf)
{
  TCHAR mybuf[10], *p = mybuf;
  switch(CP)
  {
  case ACP: p = _T("ACP"); break;
  case OEMCP: p = _T("OEM"); break;
  case UTF16LE: p = _T("UTF16LE"); break;
  case UTF16BE: p = _T("UTF16BE"); break;
  case UTF32LE: p = _T("UTF32LE"); break;
  case UTF32BE: p = _T("UTF32BE"); break;
  case UTF8: p = _T("UTF8"); break;
  default: 
    _stprintf(mybuf,_T("CP%u"),CP);
    if (CP >= NStreamEncoding::CPCOUNT) p = _T("?");
  }
  _tcscpy(Buf,p);
}

tstring NStreamLineReader::GetErrorMessage(UINT Error, const TCHAR*Filename, UINT Line)
{
  tstring msg;
  TCHAR buf[40];
  switch(Error)
  {
  case NStream::ERR_BUFFEROVERFLOW:
    msg = _T("Line too long: ");
    break;
  case NStream::ERR_IOERROR:
    msg = _T("I/O  error"), Filename = 0;
    break;
  case NStream::ERR_UNSUPPORTEDENCODING:
    StreamEncoding().GetCPDisplayName(buf);
    msg = tstring(buf) + _T(" is not supported"), Filename = 0;
    break;
  default:
    msg = _T("Bad text encoding: ");
    break;
  }
  if (Filename)
  {
    _stprintf(buf,_T("%u"),Line);
    msg = msg + Filename + _T(":") + buf;
  }
  return msg + _T("\n");
}

UINT NStreamLineReader::ReadLine(wchar_t*Buffer, UINT cchBuf)
{
  /*\
  Reads from the associated stream until it finds a new-line or 
  the read fails (I/O error or EOF). It fails with ERR_BUFFEROVERFLOW if 
  cchBuf-1 wchar_t's are read without finding the end of the line.
  Buffer MUST be a valid pointer, it will be \0 terminated as long as cchBuf > 0.
  \*/
  if (!cchBuf) return NStream::ERR_BUFFEROVERFLOW;
#ifndef MB_ERR_INVALID_CHARS
  const UINT MB_ERR_INVALID_CHARS = 8;
#endif
  const UINT cchFullBuf = cchBuf;
  NIStream&strm = GetStream();

l_restart:
  // Only supports MBCS and UTF-8 for now...
  if (StreamEncoding().IsUTF8())
  {
    for(;;)
    {
      BYTE cb = 0; // bytes in chU8 -1
      BYTE chU8[6];
      if (!strm.ReadOctet(&chU8[0])) goto l_ioerror;
      UINT cchWC;
#if defined(WIN32) // TODO: Is wchar_t==UTF16LE under cygwin?
      // Fast path if wchar_t == UTF16 and in ASCII range
      if (chU8[0] <= 127 && sizeof(wchar_t) == 2)
      {
        cchWC = ++cb;
        if (cchBuf <= cchWC) goto l_lineoverflow;
        *Buffer = (wchar_t) chU8[0];
      }
      else
#endif
      {
        if (0xC0 == (0xC0 & chU8[0]))
        {
          ++cb;
          if (0xE0 == (0xE0 & chU8[0]))
          {
            ++cb;
            if (0xF0 == (0xF0 & chU8[0]))
            {
              ++cb;
              if (0xF8 == (0xF8 & chU8[0]))
              {
                ++cb;
                if (0xFC == (0xFE & chU8[0]))
                  ++cb; 
                else 
                  goto l_badutf;
              }
            }
          }
        }
        for(BYTE moreU8 = 0; moreU8 < cb;) 
        {
          BYTE b;
          if (!strm.ReadOctet(&b)) goto l_ioerror;
          if (0x80 != (0xC0 & b)) goto l_badutf; // chU8[1..n] must be 0b10xxxxxx
          chU8[++moreU8] = b;
        }
        ++cb;
        cchWC = UTF8ToWC_Prepare((LPCSTR)chU8,cb);
        if (!cchWC) goto l_badutf;
        if (cchBuf <= cchWC) goto l_lineoverflow;
        cchWC = UTF8ToWC_Convert((LPCSTR)chU8,cb,Buffer,cchWC);
      }
      if (CompleteLine(Buffer,cchWC,cchBuf,true)) goto l_success;
    }
  }
#ifdef _WIN32
  else if (StreamEncoding().IsUTF16LE())
  {
    unsigned short lead, trail, cchWC;
    for(;;)
    {
      if (!strm.ReadInt16(&lead)) goto l_ioerror;
      FIX_ENDIAN_INT16LETOHOST_INPLACE(lead);
      if (IsTrailSurrogateUTF16(lead)) goto l_badutf;
      UINT32 codpt = lead;
      Buffer[0] = lead, cchWC = 0;
      if (IsLeadSurrogateUTF16(lead))
      {
        if (!strm.ReadInt16(&trail)) goto l_ioerror;
        FIX_ENDIAN_INT16LETOHOST_INPLACE(trail);
        if (!IsTrailSurrogateUTF16(trail)) goto l_badutf;
        codpt = CodePointFromUTF16SurrogatePair(lead,trail);
        Buffer[1] = trail, ++cchWC;
      }
      if (!IsValidUnicodeCodePoint(codpt)) goto l_badutf;
      if (CompleteLine(Buffer,++cchWC,cchBuf,true)) goto l_success;
    }
  }
#endif
  else if (StreamEncoding().IsUnicode())
  {
    goto l_unsupportedencoding; 
  }
  else
  {
    const UINT cp = StreamEncoding().GetCodepage();
    UINT mbtowcflags = 0;
    if (cp < 50220 && cp != 42) mbtowcflags = MB_ERR_INVALID_CHARS;
    for(;;) 
    {
      BYTE bufMB[2];
      BYTE mb = 0;
      if (!strm.ReadOctet(&bufMB[0])) goto l_ioerror;
      if (IsDBCSLeadByteEx(cp,bufMB[0]))
      {
        if (!strm.ReadOctet(&bufMB[++mb])) goto l_ioerror;
      }
      ++mb;
      UINT cchWC = MultiByteToWideChar(cp,mbtowcflags,(LPCSTR)bufMB,mb,0,0);
      if (!cchWC) goto l_badencoding;
      if (cchBuf <= cchWC) goto l_lineoverflow;
      cchWC = MultiByteToWideChar(cp,mbtowcflags,(LPCSTR)bufMB,mb,Buffer,cchWC);
      if (CompleteLine(Buffer,cchWC,cchBuf,false)) goto l_success;
    }
  }
l_ioerror:
  *Buffer = 0;
  return NStream::ERR_IOERROR;
l_lineoverflow:
  *Buffer = 0;
  return NStream::ERR_BUFFEROVERFLOW;
l_badutf:
l_badencoding:
  *Buffer = 0;
  return NStream::ERR_INVALIDENCODING;
l_unsupportedencoding:
  *Buffer = 0;
  return NStream::ERR_UNSUPPORTEDENCODING;
l_success:
  *Buffer = 0;
  // "Foo\r\nBar" is 2 and not 3 lines
  const wchar_t chThisNL = *--Buffer, chPrevNL = m_PrevNL;
  const bool onlyNL = ++cchBuf == cchFullBuf;
  m_PrevNL = chThisNL;
  if (onlyNL && (chPrevNL^chThisNL) == ('\r'^'\n'))
  {
    m_PrevNL = 0;
    goto l_restart; // Previous line was "Foo\r". This line was "\n", ignore it.
  }
  return NStream::OK;
}
