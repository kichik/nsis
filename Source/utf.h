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
#include "util.h" // For my_fopen

#define TSTR_INPUTCHARSET _T("ACP|OEM|CP#|UTF8|UTF16LE")


void RawTStrToASCII(const TCHAR*in,char*out,UINT maxcch);

template<typename T> T S7ChLwr(T c) { return c>='A' && c<='Z' ? (T)(c|32) : c; }
template<typename T> T S7ChUpr(T c) { return c>='a' && c<='z' ? (T)(c-'a'+'A') : c; }
template<typename T> bool S7IsChEqualI(char ch,T cmp)
{
  return S7ChLwr((T)ch) == S7ChLwr(cmp);
}

inline bool IsValidUnicodeCodePoint(UINT32 c,bool StrictUTF32=false)
{
  // Unicode 6.1: 16.7 Noncharacters
  if ((c&0xfffe) == 0xfffe) return false; // ..FFFE & ..FFFF is reserved in each plane
  if (c >= 0xfdd0 && c <= 0xfdef) return false; // Reserved in BMP
  if (StrictUTF32 && c > 0x10ffff) return false;
  return true;
}
inline bool IsLeadSurrogateUTF16(unsigned short c) { return c >= 0xd800 && c <= 0xdbff; }
inline bool IsTrailSurrogateUTF16(unsigned short c) { return c >= 0xdc00 && c <= 0xdfff; }
inline UINT32 CodePointFromUTF16SurrogatePair(unsigned short lea,unsigned short tra)
{
  const UINT32 surrogate_offset = 0x10000 - (0xD800 << 10) - 0xDC00;
  return ((UINT32)lea << 10) + tra + surrogate_offset;
}

UINT StrLenUTF16LE(const void*str);
bool StrSetUTF16LE(tstring&dest, const void*src);

wchar_t* DupWCFromBytes(void*Buffer,UINT cbBuffer,WORD SrcCP);
UINT DetectUTFBOM(FILE*strm);
WORD GetEncodingFromString(const TCHAR*s);

class WCToUTF16LEHlpr {
  unsigned short* m_s;
public:
  WCToUTF16LEHlpr() : m_s(0) {}

  bool Create(const TCHAR*in)
  {
#if defined(_WIN32) && defined(_UNICODE)
    m_s = (unsigned short*) in;
#else
#error TODO: wchar_t to UTF16LE
#endif
    return true;
  }
  void Destroy()
  {
#if !defined(_WIN32) && !defined(_UNICODE)
    delete[] m_s;
#endif
  }
  const unsigned short* Get() const { return m_s; }
  UINT GetLen() const { return StrLenUTF16LE(m_s); }
  UINT GetSize() const { return (GetLen()+1) * 2; }
};

class NStreamEncoding {
protected:
  WORD m_cp;
public:
  enum {
    ACP = CP_ACP,
    OEMCP = 1,
    UTF16LE = 1200,
    UTF16BE = 1201,
    UTF32LE = 12000,
    UTF32BE = 12001,
    UTF8 = CP_UTF8,
    UNKNOWN = (0xffff-0),
    AUTO = (0xffff-1),
    CPCOUNT = (0xffff-2) // Must be less than our other magic numbers
  };

  NStreamEncoding() { Reset(); }
  NStreamEncoding(WORD cp) { Reset();SetCodepage(cp); }
  WORD GetCodepage() const { return m_cp; }
  void SetCodepage(WORD cp) { m_cp = cp; }
  void SafeSetCodepage(WORD cp)
  {
    if (NStreamEncoding::AUTO==cp) cp = GetPlatformDefaultCodepage();
    if (NStreamEncoding::UNKNOWN==cp) cp = GetPlatformDefaultCodepage();
    SetCodepage(cp);
  }
  void Reset() { SetCodepage(GetPlatformDefaultCodepage()); }
  WORD GetPlatformDefaultCodepage() const
  {
#ifdef _WIN32
    return ACP;
#else
    return UTF8;
#endif
  }
  bool IsUTF8() const { return UTF8==GetCodepage(); }
  bool IsUTF16LE() const { return UTF16LE==GetCodepage(); }
  bool IsUnicode() const { return IsUnicodeCodepage(GetCodepage()); }
  void GetCPDisplayName(TCHAR*Buf) { GetCPDisplayName(m_cp, Buf); }

  static UINT GetCodeUnitSize(WORD cp)
  {
    if ((UTF16LE|1)==(cp|1)) return 2;
    if ((UTF32LE|1)==(cp|1)) return 4;
    return 1;
  }
  static bool IsUnicodeCodepage(WORD cp)
  {
    return UTF8==cp || (UTF16LE|1)==(cp|1) || (UTF32LE|1)==(cp|1); 
  }
  static void GetCPDisplayName(WORD CP, TCHAR*Buf);
};

class NStream {
public:
  enum {
    OK = 0,
    ERR_BUFFEROVERFLOW,
    ERR_IOERROR,
    ERR_INVALIDENCODING,
    ERR_UNSUPPORTEDENCODING,
  };
  static bool IsNewline(wchar_t chW, bool HandleUnicodeNL)
  {
    if (L'\n'==chW || L'\r'==chW) return true;
    if (HandleUnicodeNL)
    {
      // www.unicode.org/standard/reports/tr13/tr13-5.html#UNICODE NEWLINE GUIDELINES
      if (L'\f'==chW) return true; // FF/Form Feed
      if (L'\v'==chW) return true; // VT/Vertical Tab
      // NOTIMPLEMENTED: NEL/Next Line/U+0085
      // NOTIMPLEMENTED: LS/Line Separator/U+2028
      // NOTIMPLEMENTED: PS/Paragraph Separator/U+2029
    }
    return false;
  }
};

class NIStream {
protected:
  FILE* m_hFile;
  NStreamEncoding m_Enc;

public:
  NIStream() : m_hFile(0) {}
  ~NIStream() { Close(); }
  FILE* GetHandle() const { return m_hFile; }
  NStreamEncoding& StreamEncoding() { return m_Enc; }
  bool IsEOF() const { return feof(m_hFile) != 0; }
  bool IsError() const { return ferror(m_hFile) != 0; }
  bool IsUnicode() const { return m_Enc.IsUnicode(); }

  void Close()
  {
    FILE*hF = Detach();
    if (hF) fclose(hF);
  }
  
  bool OpenFileForReading(const TCHAR* Path, WORD enc = NStreamEncoding::AUTO)
  {
    FILE *hFile = my_fopen(Path, "rb");
    return Attach(hFile, enc);
  }
  bool OpenFileForReading(const TCHAR* Path, NStreamEncoding&Enc)
  {
    return OpenFileForReading(Path, Enc.GetCodepage());
  }
  bool OpenStdIn(WORD enc = NStreamEncoding::AUTO)
  {
    return Attach(stdin, enc);
  }
  bool OpenStdIn(NStreamEncoding&Enc)
  {
    return OpenStdIn(Enc.GetCodepage());
  }

  FILE* Detach() 
  {
    FILE *hFile = m_hFile;
    m_hFile = 0;
    return hFile;
  }
  bool Attach(FILE*hFile, WORD enc)
  {
    Close();
    m_hFile = hFile;
    if (m_hFile)
    {
      WORD cp = DetectUTFBOM(m_hFile);
      if (!cp) cp = enc;
      m_Enc.SafeSetCodepage(cp);
    }
    return 0 != m_hFile;
  }

  UINT ReadOctets(void*Buffer, UINT cbBuf)
  {
    size_t cb = fread(Buffer, 1, cbBuf, m_hFile);
    return (UINT) cb;
  }
  bool ReadOctets(void*Buffer, UINT*pcbBuf)
  {
    UINT cbReq = *pcbBuf, cb = ReadOctets(Buffer, cbReq);
    *pcbBuf = cb;
    return cbReq == cb;
  }
  bool ReadOctet(void*Buffer) { return 1 == ReadOctets(Buffer, 1); }
  bool ReadInt16(void*Buffer) { return 2 == ReadOctets(Buffer, 2); }
};

class NStreamLineReader {
protected:
  NIStream &m_Strm;
  wchar_t m_PrevNL;

public:
  NStreamLineReader(NIStream &Strm) : m_Strm(Strm), m_PrevNL(0) {}

  NIStream& GetStream() { return m_Strm; }
  NStreamEncoding& StreamEncoding() { return m_Strm.StreamEncoding(); }
  bool IsEOF() const { return m_Strm.IsEOF(); }
  bool IsUnicode() const { return m_Strm.IsUnicode(); }

  UINT ReadLine(wchar_t*Buffer, UINT cchBuf);
  tstring GetErrorMessage(UINT Error, const TCHAR*Filename=0, UINT Line=0);

protected:
  bool CompleteLine(wchar_t*&BufWC, UINT cchWC, UINT&cchRemain, bool HandleUnicodeNL)
  {
    const wchar_t chW = *BufWC;
    BufWC += cchWC, cchRemain -= cchWC;
    if (0 == --cchWC) // We only care about code points that fit in a single wchar_t
      return NStream::IsNewline(chW, HandleUnicodeNL);
    return false;
  }
};

#endif // NSIS_UTF_H
