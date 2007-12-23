/*
 * str.cpp
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

#include "str.h"
#include "Platform.h"

#include <stdexcept>

using std::string;
using std::length_error;

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
