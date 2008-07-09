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

bool nobj_function::is_used()
{
  return m_used;
}

void nobj_function::set_used()
{
  m_used = true;
}
