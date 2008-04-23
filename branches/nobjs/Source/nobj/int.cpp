/*
 * int.cpp
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

#include "int.h"

#include <stdexcept>

using std::string;
using std::length_error;

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
