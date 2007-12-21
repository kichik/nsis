/*
 * nobjs.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "nobj.h"
#include "Platform.h"

#include <stdexcept>

using std::string;
using std::length_error;

/**
 * nobj
 */

const nobjs nobj::dependencies() const
{
  return m_dependencies;
}

void nobj::add_dependency(const nobj& obj)
{
  m_dependencies.push_back(obj);
}

/**
 * nobj_entry
 */

nobj_entry::nobj_entry(const int which, const nobjs& parms)
  : m_which(which)
{
  nobjs_const_iterator i = parms.begin();

  while (i != parms.end())
  {
    add_dependency(*i);
    i++;
  }
}

const int nobj_entry::which() const
{
  return m_which;
}

/**
 * nobj_string
 */

nobj_string::nobj_string(const string& str)
  : m_string(str)
{}

nobj_string::nobj_string(const char* str)
  : m_string(str)
{}

nobj_string::nobj_string(char* str)
  : m_string(str)
{}

const string nobj_string::get_string()
{
  return m_string;
}

/**
 * nobj_int
 */

nobj_int::nobj_int(const int i)
  : m_int(i)
{}

const int nobj_int::get_int()
{
  return m_int;
}
