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

nobj_function::nobj_function()
{
  m_section.name_ptr      = 0;
  m_section.code          = 0;
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
