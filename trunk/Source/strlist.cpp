/*
 * strlist.cpp: Implementation of the StringList class.
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2009 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Doxygen comments by Jim Park -- 08/01/2007
 */

#include "strlist.h"

int StringList::add(const TCHAR *str, int case_sensitive)
{
  int a=find(str,case_sensitive);
  if (a >= 0 && case_sensitive!=-1) return a;
  return m_gr.add(str,(_tcsclen(str)+1)*sizeof(TCHAR))/sizeof(TCHAR);
}

// use 2 for case sensitive end-of-string matches too
int StringList::find(const TCHAR *str, int case_sensitive, int *idx/*=NULL*/) const // returns -1 if not found
{
  const TCHAR *s=get();
  int ml=getlen();
  int offs=0;

  if (idx) *idx=0;
  while (offs < ml)
  {
    // Check if the whole string matches str.
    if ((case_sensitive && !_tcscmp(s+offs,str)) ||
        (!case_sensitive && !_tcsicmp(s+offs,str)))
    {
      return offs;
    }

    // Check if just the end of the string matches str.
    if (case_sensitive==2 &&
        _tcslen(str) < _tcslen(s+offs) &&  // check for end of string
        !_tcscmp(s+offs+_tcslen(s+offs)-_tcslen(str),str))
    {
      return offs+_tcslen(s+offs)-_tcslen(str);
    }
    offs+=_tcslen(s+offs)+1;

    if (idx) (*idx)++;
  }
  return -1;
}

// pos is the position in TCHARs, not bytes.
void StringList::delbypos(int pos)
{
  TCHAR *s=(TCHAR*) m_gr.get();
  int len=_tcslen(s+pos)+1;

  if (pos+len < m_gr.getlen()) 
  {
	  // Move everything after the string position to the current position.
      memcpy(s+pos,s+pos+len,m_gr.getlen()-(pos+len));
  }
  m_gr.resize(m_gr.getlen()-len);
}

// idx corresponds to the nth string in the list.
int StringList::idx2pos(int idx) const
{
  TCHAR *s=(TCHAR*) m_gr.get();
  int offs=0;
  int cnt=0;
  if (idx>=0) while (offs < m_gr.getlen())
  {
    if (cnt++ == idx) return offs;
    offs+=_tcslen(s+offs)+1;
  }
  return -1;
}

int StringList::getnum() const
{
  TCHAR *s=(TCHAR*) m_gr.get();
  int ml=m_gr.getlen();
  int offs=0;
  int idx=0;
  while (offs < ml)
  {
    offs+=_tcslen(s+offs)+1;
    idx++;
  }
  return idx;
}

const TCHAR *StringList::get() const
{
  return (const TCHAR*) m_gr.get();
}

int StringList::getlen() const
{
  return m_gr.getlen();
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

  for (int i=0; i<num; i++) {
    free(s[i].value);
  }
}

int DefineList::add(const TCHAR *name, const TCHAR *value/*=_T("")*/)
{
  int pos=SortedStringList<struct define>::add(name);
  if (pos == -1)
  {
    return 1;
  }

  TCHAR **newvalue=&(((struct define*) m_gr.get())[pos].value);
  size_t size_in_bytes = (_tcslen(value) + 1) * sizeof(TCHAR);

  *newvalue=(TCHAR*)malloc(size_in_bytes);

  if (!(*newvalue))
  {
    extern FILE *g_output;
    extern int g_display_errors;
    extern void quit();
    if (g_display_errors)
    {
      _ftprintf(g_output,_T("\nInternal compiler error #12345: GrowBuf realloc/malloc(%lu) failed.\n"), (unsigned long) size_in_bytes);
      fflush(g_output);
    }
    quit();
  }
  _tcscpy(*newvalue,value);
  return 0;
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

int FastStringList::getlen() const
{
  return m_strings.getlen();
}

int FastStringList::getnum() const
{
  return m_gr.getlen()/sizeof(struct string_t);
}

