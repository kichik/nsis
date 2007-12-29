/*
 * entry.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2008 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "entry.h"
#include "Platform.h"

#include "int.h"
#include "jump.h"
#include "str.h"
#include "var.h"

#include <stdexcept>

using std::string;
using std::length_error;

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

void nobj_entry::set_parm_jump(int offset, const char* jump)
{
  nobj::set_dependency(offset, new nobj_jump(jump));
}

void nobj_entry::set_parm_jump(int offset, string& jump)
{
  nobj::set_dependency(offset, new nobj_jump(jump));
}

void nobj_entry::set_parm_var(int offset, const char* var)
{
  nobj::set_dependency(offset, new nobj_var(var));
}

void nobj_entry::set_parm_var(int offset, string& var)
{
  nobj::set_dependency(offset, new nobj_var(var));
}

const int nobj_entry::which() const
{
  return m_which;
}
