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
#include "Platform.h"

using std::string;

/**
 * nobj_section
 */

nobj_section::nobj_section(const int name_addr, const int code_addr, const int inst_types, const int flags)
{
  m_section.name_ptr      = name_addr;
  m_section.code          = code_addr;
  m_section.code_size     = 0;
  m_section.install_types = inst_types;
  m_section.flags         = flags;
  m_section.size_kb       = 0;

  memset(m_section.name, 0, sizeof(m_section.name));
}

section* nobj_section::get_section()
{
  return &m_section;
}

void nobj_section::add_entry(const nobj_entry& entry)
{
  nobj_entry* entryp = new nobj_entry(entry.which(), entry.dependencies());
  nobj::add_dependency(entryp);
}

void nobj_section::add_flags(int flags)
{
  m_section.flags |= flags;
}

void nobj_section::remove_flags(int flags)
{
  m_section.flags &= ~flags;
}

void nobj_section::add_inst_type(int inst_type)
{
  // TODO keep this is a flag "untouched section"...
  if (m_section.install_types == ~0)
    m_section.install_types = 0;

  m_section.install_types |= inst_type;
}

void nobj_section::add_size(int size)
{
  m_section.size_kb += size;
}
