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

nobj::~nobj()
{
  nobjs_const_iterator i = m_dependencies.begin();

  while (i != m_dependencies.end())
  {
    delete *i;
    i++;
  }
}

const nobjs nobj::dependencies() const
{
  return m_dependencies;
}

void nobj::add_dependency(const nobj* obj)
{
  m_dependencies.push_back(obj);
}

void nobj::set_dependency(int offset, const nobj* obj)
{
  if (m_dependencies.size() <= offset)
  {
    m_dependencies.resize(offset + 1);
  }

  m_dependencies[offset] = obj;
}

/**
 * nobj_entry
 */

nobj_entry::nobj_entry(const int which)
  : m_which(which)
{}

nobj_entry::nobj_entry(const int which, const nobjs& parms)
  : m_which(which)
{
  nobjs_const_iterator i = parms.begin();

  while (i != parms.end())
  {
    nobj::add_dependency(*i);
    i++;
  }
}

void nobj_entry::set_parm(int offset, const nobj* parm)
{
  nobj::set_dependency(offset, parm);
}

void nobj_entry::set_parm(int offset, const int parm)
{
  nobj::set_dependency(offset, new nobj_int(parm));
}

void nobj_entry::set_parm(int offset, const char* parm)
{
  nobj::set_dependency(offset, new nobj_string(parm));
}

void nobj_entry::set_parm(int offset, string& parm)
{
  nobj::set_dependency(offset, new nobj_string(parm));
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

const string nobj_string::get_string() const
{
  return m_string;
}

/**
 * nobj_int
 */

nobj_int::nobj_int(const int i)
  : m_int(i)
{}

const int nobj_int::get_int() const
{
  return m_int;
}

/**
 * nobj_jump
 */

nobj_jump::nobj_jump(const string& str)
  : m_jump(str)
{}

nobj_jump::nobj_jump(const char* str)
  : m_jump(str)
{}

nobj_jump::nobj_jump(char* str)
  : m_jump(str)
{}

const string nobj_jump::get_jump() const
{
  return m_jump;
}
