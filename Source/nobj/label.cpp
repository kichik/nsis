/*
 * label.cpp
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

#include "label.h"

#include <stdexcept>

using std::string;
using std::length_error;

/**
 * nobj_label
 */

nobj_label::nobj_label(const string& str)
  : m_label(str)
{}

nobj_label::nobj_label(const char* str)
  : m_label(str)
{}

nobj_label::nobj_label(char* str)
  : m_label(str)
{}

const string nobj_label::get_label() const
{
  return m_label;
}
