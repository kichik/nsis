/*
 * section.cpp
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

#include "section.h"

using std::string;

/**
 * nobj_section
 */

nobj_section::nobj_section(const string& name, const int inst_types, const int flags)
  : m_name(name), m_inst_types(inst_types), m_flags(flags)
{
  m_size = 0;
}

void nobj_section::add_flags(int flags)
{
  m_flags |= flags;
}

void nobj_section::remove_flags(int flags)
{
  m_flags &= ~flags;
}

void nobj_section::add_inst_type(int inst_type)
{
  // TODO keep this is a flag "untouched section"...
  if (m_inst_types == ~0)
    m_inst_types = 0;

  m_inst_types |= inst_type;
}

void nobj_section::add_size(int size)
{
  m_size += size;
}

const std::string& nobj_section::get_name()
{
  return m_name;
}

const int nobj_section::get_inst_types()
{
  return m_inst_types;
}

const int nobj_section::get_flags()
{
  return m_flags;
}

const int nobj_section::get_size()
{
  return m_size;
}
