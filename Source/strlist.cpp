/*
 * strlist.cpp: Implementation of the StringList class.
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Unicode support and Doxygen comments by Jim Park -- 08/01/2007
 */

#include "strlist.h"
#include "utf.h"
#include "util.h" // For PrintColorFmtMsg_ERR

#ifdef _UNICODE
char* convert_processed_string_to_ansi(char *out, const TCHAR *in, WORD codepage); // defined in build.cpp
#endif

static inline bool byte_rev_match(const void*ptr1, const void*ptr2, size_t cb)
{
  char *p1 = (char*) ptr1, *p2 = (char*) ptr2;
  for(; cb--;) if (p1[cb] != p2[cb]) return false;
  return true;
}

unsigned int ExeHeadStringList::getnum() const
{
  char *p = (char*) m_gr.get();
  if (!p) return 1; // The empty string always exists
  unsigned int num = 1;
  size_t cbList = gettotalsize(), cb = 0, pos;
  pos = 1 + !!m_wide, p += pos; // Skip empty string
  if (m_wide)
  {
    for(;;)
    {
      if ((pos+=cb) >= cbList) break;
      cb = (StrLenUTF16(p+=cb) + 1) * 2, ++num;
    }
  }
  else
  {
    for(;;)
    {
      if ((pos+=cb) >= cbList) break;
      cb = strlen(p+=cb) + 1, ++num;
    }
  }
  return num;
}

bool ExeHeadStringList::get(unsigned int offset, tstring&outstr) const
{
  if (0 == offset)
  {
    outstr.assign(_T(""));
    return true;
  }
  char *p = (char*) m_gr.get();
  unsigned int cbList = gettotalsize();
  if (p && cbList < offset)
  {
    if (m_wide)
      StrSetUTF16LE(outstr,&p[offset*WIDEDIV]);
    else
      // BUGBUG: There is no way for us to know the correct codepage
      outstr = CtoTString(&p[offset]);
    return true;
  }
  return false;
}

/*
 * find() finds the offset where the string is stored, returns -1 if not found.
 * It only compares raw byte values, there is no Unicode normalization handling.
 * If ppBufMB is non-null you must delete[] it (Only valid when m_wide is false)!
*/
unsigned int ExeHeadStringList::find(const TCHAR *str, WORD codepage, bool processed, char**ppBufMB) const
{
  if (m_wide && *str)
  {
    WCToUTF16LEHlpr cnv;
    if (!cnv.Create(str)) return -1;
    unsigned int pos = find(cnv.Get(),StrLenUTF16(cnv.Get()),codepage,processed,ppBufMB);
    cnv.Destroy();
    return pos;
  }
  else
  {
    return find(str,(unsigned int)_tcslen(str),codepage,processed,ppBufMB);
  }
}
unsigned int ExeHeadStringList::find(const void *ptr, unsigned int cchF, WORD codepage, bool processed, char**ppBufMB) const
{
  const wchar_t *find = (const wchar_t*) ptr; // Data is: m_wide ? UTF16LE : wchar_t
  if (!*find) return 0; // The empty string is always first (ExeHead uses string block offset 0 to indicate no parameter present in some places).

  char *p = (char*) m_gr.get();
  if (!p) return -1;

  unsigned int cbF = ++cchF * 2; // Include \0 as part of cchF, * 2 for UTF16 & DBCS.
  char *bufMB = 0;
  if (!m_wide)
  {
    unsigned int cbMB;
    bufMB = new char[cbF];
    if (processed)
    {
      char *pTmp = convert_processed_string_to_ansi(bufMB,find,codepage);
      cbMB = (int)(pTmp ? pTmp - bufMB : 0);
    }
    else
    {
      cbMB = WideCharToMultiByte(codepage,0,find,cchF,bufMB,cbF,0,0);
    }
#ifndef NDEBUG
    if (!cbMB)
    {
      const TCHAR *fmt = _T("Unable to convert%")NPRIns _T(" string \"%")NPRIs _T("\" to codepage %u\n");
      PrintColorFmtMsg_ERR(fmt,(processed ? " processed" : ""),find,codepage);
    }
#endif
    assert(cbMB);
    cbF = cbMB, find = (const wchar_t*) bufMB;
  }

  size_t cbList = gettotalsize(), cb = 0, retval = -1, pos;
  pos = 1 + !!m_wide, p += pos; // Skip empty string
  if (m_wide)
  {
    for(;;)
    {
      if ((pos+=cb) >= cbList) break;
      cb = (StrLenUTF16(p+=cb) + 1) * 2;
      if (cb < cbF) continue;
      size_t cbOfs = cb - cbF;
      if (byte_rev_match(p + cbOfs,find,cbF)) { retval = (pos + cbOfs) / WIDEDIV; break; }
    }
  }
  else
  {
    for(;;)
    {
      if ((pos+=cb) >= cbList) break;
      cb = (unsigned int) strlen(p+=cb) + 1;
      if (cb < cbF) continue;
      size_t cbOfs = cb - cbF;
      if (byte_rev_match(p + cbOfs,find,cbF)) { retval = (pos + cbOfs); break; }
    }
    if (ppBufMB) 
      *ppBufMB = bufMB;
    else 
      delete[] bufMB;
  }
  // -1 is a valid magic return value but we must avoid the truncation check in truncate_cast
  return retval != (size_t)(-1) ? truncate_cast(unsigned int,retval) : (unsigned int) retval;
}

int ExeHeadStringList::add(const TCHAR *str, WORD codepage, bool processed)
{
  char *p = (char*) m_gr.get();
  if (!p)
  {
    if (!*str) return 0; // Delay allocating the empty string
    char *&zero = p, cb = 1 + !!m_wide;
    unsigned int pos = m_gr.add(&zero,cb);
    assert(0 == pos);
  }

  char *bufMB = 0;
  unsigned int pos = find(str,codepage,processed,m_wide ? 0 : &bufMB);
  if ((unsigned int)-1 != pos)
  {
    delete[] bufMB;
    return pos;
  }

  if (m_wide)
  {
    WCToUTF16LEHlpr cnv;
    if (!cnv.Create(str)) throw std::bad_alloc();
    pos = m_gr.add(cnv.Get(),cnv.GetSize()) / WIDEDIV;
    cnv.Destroy();
  }
  else
  {
    unsigned int cbMB = (unsigned int) strlen(bufMB) + 1;
    pos = m_gr.add(bufMB,cbMB);
    delete[] bufMB;
  }
  return pos;
}


int StringList::add(const TCHAR *str, int case_sensitive)
{
  int a=find(str,case_sensitive);
  if (a >= 0 && case_sensitive!=-1) return a;
  return m_gr.add(str,truncate_cast(int,(_tcslen(str)+1)*sizeof(TCHAR)))/sizeof(TCHAR);
}

// use 2 for case sensitive end-of-string matches too
int StringList::find(const TCHAR *str, int case_sensitive, int *idx/*=NULL*/) const // returns -1 if not found
{
  const TCHAR *s=get();
  int ml=getcount();
  int offs=0;

  size_t str_slen = _tcslen(str);
  size_t offs_slen;

  if (idx) *idx=0;
  while (offs < ml)
  {
    // Check if the whole string matches str.
    if ((case_sensitive && !_tcscmp(s+offs,str)) ||
        (!case_sensitive && !_tcsicmp(s+offs,str)))
    {
      return offs;
    }

    offs_slen = _tcslen(s+offs);

    // Check if just the end of the string matches str.
    if (case_sensitive==2 &&
        str_slen < offs_slen &&  // check for end of string
        !_tcscmp(s + offs + offs_slen - str_slen,str))
    {
      return truncate_cast(int,offs + offs_slen - str_slen);
    }
    offs += truncate_cast(int,offs_slen + 1);

    if (idx) (*idx)++;
  }
  return -1;
}

// pos is the position in TCHARs, not bytes.
void StringList::delbypos(int pos)
{
  TCHAR *s=(TCHAR*) m_gr.get();
  int len=(int)_tcslen(s+pos)+1;

  if (pos+len < getcount()) 
  {
    // Move everything after the string position to the current position.
    memcpy(s+pos,s+pos+len, (getcount()-pos+len)*sizeof(TCHAR));
  }
  m_gr.resize(m_gr.getlen()-len*sizeof(TCHAR));
}

// idx corresponds to the nth string in the list.
int StringList::idx2pos(int idx) const
{
  TCHAR *s=(TCHAR*) m_gr.get();
  int offs=0;
  int cnt=0;
  if (idx>=0) while (offs < getcount())
  {
    if (cnt++ == idx) return offs;
    offs+=(int)_tcslen(s+offs)+1;
  }
  return -1;
}

int StringList::getnum() const
{
  TCHAR *s=(TCHAR*) m_gr.get();
  int ml=getcount();
  int offs=0;
  int idx=0;
  while (offs < ml)
  {
    offs+=(int)_tcslen(s+offs)+1;
    idx++;
  }
  return idx;
}


// ==========
// DefineList
// ==========

/** 
 * Since the SortedStringList base class handles the memory for .name values,
 * this destructor handles all the .value values in struct define.
 */
DefineList::~DefineList()
{
  struct define *s=(struct define*) m_gr.get();
  int num=m_gr.getlen()/sizeof(struct define);

  for (int i=0; i<num; i++) free(s[i].value);
}

int DefineList::addn(const TCHAR *name, size_t maxvallen, const TCHAR *value)
{
  int pos=SortedStringList<struct define>::add(name);
  if (pos == -1) return 1;

  size_t cbVal = ++maxvallen * sizeof(TCHAR);
  TCHAR **newvalue=&(((struct define*) m_gr.get())[pos].value);

  *newvalue = (TCHAR*)malloc(cbVal);
  if (!(*newvalue))
  {
    extern int g_display_errors;
    extern void quit();
    if (g_display_errors)
    {
      PrintColorFmtMsg_ERR(_T("\nInternal compiler error #12345: DefineList malloc(%lu) failed.\n"), truncate_cast(unsigned long,cbVal));
    }
    quit();
  }
  my_strncpy(*newvalue, value, maxvallen);
  return 0;
}

int DefineList::add(const TCHAR *name, const TCHAR *value/*=_T("")*/)
{
  return addn(name, _tcslen(value), value);
}

int DefineList::set(const TCHAR *name, const TCHAR *value/*=_T("")*/)
{
  del(name);
  return add(name, value);
}

int DefineList::set_si32(const TCHAR *name, long value)
{
  TCHAR buf[50];
  _stprintf(buf, _T("%ld"), value);
  return set(name, buf);
}

int DefineList::set_ui32(const TCHAR *name, unsigned long value)
{
  TCHAR buf[50];
  _stprintf(buf, _T("%lu"), value);
  return set(name, buf);
}

TCHAR *DefineList::find(const TCHAR *name)
{
  int v=SortedStringList<struct define>::find(name);
  if (v==-1)
  {
    return NULL;
  }
  return ((struct define*) m_gr.get())[v].value;
}

// returns 0 on success, 1 otherwise
int DefineList::del(const TCHAR *str)
{
  int pos=SortedStringList<struct define>::find(str);
  if (pos==-1) return 1;

  struct define *db=(struct define *) m_gr.get();
  free(db[pos].value);
  delbypos(pos);

  return 0;
}

int DefineList::getnum()
{
  return m_gr.getlen()/sizeof(define);
}

TCHAR *DefineList::getname(int num)
{
  if ((unsigned int)getnum() <= (unsigned int)num)
    return 0;
  return ((struct define*) m_gr.get())[num].name;
}

TCHAR *DefineList::getvalue(int num)
{
  if ((unsigned int)getnum() <= (unsigned int)num)
    return 0;
  return ((struct define*) m_gr.get())[num].value;
}

// ==============
// FastStringList
// ==============

int FastStringList::add(const TCHAR *name, int case_sensitive/*=0*/)
{
  int pos = SortedStringListND<struct string_t>::add(name, case_sensitive);
  if (pos == -1) return -1;
  return ((struct string_t*) m_gr.get())[pos].name;
}

TCHAR *FastStringList::get() const
{
  return (TCHAR*)m_strings.get();
}

int FastStringList::getcount() const
{
  return m_strings.getlen()/sizeof(TCHAR);
}

int FastStringList::getnum() const
{
  return m_gr.getlen()/sizeof(struct string_t);
}

