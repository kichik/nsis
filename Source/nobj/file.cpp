/*
 * file.cpp
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

#include "file.h"

#include <stdexcept>

using std::string;
using std::length_error;

/**
 * nobj_file
 */

nobj_file::nobj_file(const string& path)
  : m_path(path)
{}

nobj_file::nobj_file(const char* str)
  : m_path(path)
{}

const string nobj_file::get_path() const
{
  return m_path;
}
