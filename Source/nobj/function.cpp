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

using std::string;

/**
 * nobj_function
 */

nobj_function::nobj_function()
  : m_used(false), m_offset(0)
{
}

bool nobj_function::is_used()
{
  return m_used;
}

void nobj_function::set_used()
{
  m_used = true;
}

int nobj_function::get_offset()
{
  return m_offset;
}

void nobj_function::set_offset(int offset)
{
  m_offset = offset;
}
