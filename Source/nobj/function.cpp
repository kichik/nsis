/*
 * function.cpp
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

#include "function.h"
#include "Platform.h"

using std::string;

/**
 * nobj_function
 */

nobj_function::nobj_function(const int name_addr, const int code_addr)
{
  m_section.name_ptr      = name_addr;
  m_section.code          = code_addr;
  m_section.code_size     = 0;
  m_section.install_types = 0;
  m_section.flags         = 0;
  m_section.size_kb       = 0;

  memset(m_section.name, 0, sizeof(m_section.name));
}

section* nobj_function::get_function()
{
  return &m_section;
}

void nobj_function::add_entry(const nobj_entry& entry)
{
  nobj_entry* entryp = new nobj_entry(entry.which(), entry.dependencies());
  nobj::add_dependency(entryp);
}

void nobj_function::add_label(const nobj_label& label)
{
  nobj_label* labelp = new nobj_label(label.get_label());
  nobj::add_dependency(labelp);
}
