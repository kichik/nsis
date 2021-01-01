/*
 * ShConstants.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2003 Ramon
 * Copyright (C) 2003-2021 NSIS Contributors
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

#ifndef ___CONSTANTS___H_____
#define ___CONSTANTS___H_____

#include "strlist.h"

struct constantstring {
  int name;
  int index;
  int pos;
  int value1;
  int value2;
};

class ConstantsStringList : public SortedStringListND<struct constantstring>
{
  public:
    ConstantsStringList();

    int add(const TCHAR *name, int value1, int value2);
    int get(const TCHAR *name, int n_chars = -1);
    int getnum();
    int get_value1(int idx);
    int get_value2(int idx);
    TCHAR *idx2name(int idx);
    bool set_values(const TCHAR *name, int val1, int val2);

  private:
    int m_index;
    int get_internal_idx(int idx);
};

#endif
