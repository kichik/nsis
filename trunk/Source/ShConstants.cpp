/*
 * ShConstants.cpp
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
 * Unicode support by Jim Park -- 08/24/2007
 */

#include "ShConstants.h"

ConstantsStringList::ConstantsStringList()
{
  index = 0;
}

int ConstantsStringList::add(const TCHAR *name, int value1, int value2)
{
  int pos=SortedStringListND<struct constantstring>::add(name);
  if (pos == -1) return -1;

  ((struct constantstring*)gr.get())[pos].index = index;
  ((struct constantstring*)gr.get())[pos].pos = pos;
  ((struct constantstring*)gr.get())[pos].value1 = value1;
  ((struct constantstring*)gr.get())[pos].value2 = value2;

  int temp = index;
  index++;

  return temp;
}

int ConstantsStringList::get(TCHAR *name, int n_chars /*= -1*/)
{
  int v=SortedStringListND<struct constantstring>::find(name, n_chars);
  if (v==-1) return -1;
  return (((struct constantstring*)gr.get())[v].index);
}

int ConstantsStringList::getnum()
{
  return index;
}

int ConstantsStringList::get_value1(int idx)
{
  int pos=get_internal_idx(idx);
  if (pos==-1) return -1;
  return (((struct constantstring*)gr.get())[pos].value1);
}

int ConstantsStringList::get_value2(int idx)
{
  int pos=get_internal_idx(idx);
  if (pos==-1) return -1;
  return (((struct constantstring*)gr.get())[pos].value2);
}

TCHAR* ConstantsStringList::idx2name(int idx)
{
  int pos=get_internal_idx(idx);
  if (pos==-1) return NULL;
  struct constantstring *data=(struct constantstring *)gr.get();      
  return ((TCHAR*)strings.get() + data[pos].name);
}

int ConstantsStringList::get_internal_idx(int idx)
{
  struct constantstring *data=(struct constantstring *)gr.get();      
  for (int i = 0; i < index; i++)
  {
    if (data[i].index == idx)
    {
      return i;
    }
  }
  return -1;
}
