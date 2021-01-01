/*
 * ShConstants.cpp
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
 * Unicode support by Jim Park -- 08/24/2007
 */

#include "ShConstants.h"

ConstantsStringList::ConstantsStringList()
{
  m_index = 0;
}

int ConstantsStringList::add(const TCHAR *name, int value1, int value2)
{
  int pos=SortedStringListND<struct constantstring>::add(name);
  if (pos == -1) return -1;

  constantstring *ptr = ((constantstring*) m_gr.get()) + pos;
  ptr->index = m_index;
  ptr->pos = pos;
  ptr->value1 = value1;
  ptr->value2 = value2;

  int temp = m_index;
  m_index++;

  return temp;
}

int ConstantsStringList::get(const TCHAR *name, int n_chars /*= -1*/)
{
  int v=SortedStringListND<struct constantstring>::find(name, n_chars);
  if (v==-1) return -1;
  return (((struct constantstring*) m_gr.get())[v].index);
}

int ConstantsStringList::getnum()
{
  return m_index;
}

int ConstantsStringList::get_value1(int idx)
{
  int pos=get_internal_idx(idx);
  if (pos==-1) return -1;
  return (((struct constantstring*) m_gr.get())[pos].value1);
}

int ConstantsStringList::get_value2(int idx)
{
  int pos=get_internal_idx(idx);
  if (pos==-1) return -1;
  return (((struct constantstring*) m_gr.get())[pos].value2);
}

TCHAR* ConstantsStringList::idx2name(int idx)
{
  int pos=get_internal_idx(idx);
  if (pos==-1) return NULL;
  struct constantstring *data=(struct constantstring *) m_gr.get();      
  return ((TCHAR*) m_strings.get() + data[pos].name);
}

int ConstantsStringList::get_internal_idx(int idx)
{
  struct constantstring *data=(struct constantstring *) m_gr.get();      

  // We do a linear search because the strings are sorted.
  for (int i = 0; i < m_index; i++)
  {
    if (data[i].index == idx)
    {
      return i;
    }
  }
  return -1;
}

bool ConstantsStringList::set_values(const TCHAR *name, int val1, int val2)
{
  int v = SortedStringListND<struct constantstring>::find(name, -1);
  if (-1 == v) return false;

  struct constantstring & cs = ((struct constantstring*) m_gr.get())[v];
  cs.value1 = val1;
  cs.value2 = val2;
  return true;
}
