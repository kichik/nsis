/*
 * var.cpp
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

#include "var.h"
#include "Platform.h"

#include <stdexcept>

using std::string;
using std::length_error;

/**
 * nobj_var
 */

nobj_var::nobj_var(const string& str)
  : m_var(str)
{}

nobj_var::nobj_var(const char* str)
  : m_var(str)
{}

nobj_var::nobj_var(char* str)
  : m_var(str)
{}

const string nobj_var::get_var() const
{
  return m_var;
}
