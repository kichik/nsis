/*
 * jump.cpp
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

#include "jump.h"
#include "Platform.h"

#include <stdexcept>

using std::string;
using std::length_error;

/**
 * nobj_jump
 */

nobj_jump::nobj_jump(const string& str)
  : m_jump(str)
{}

nobj_jump::nobj_jump(const char* str)
  : m_jump(str)
{}

nobj_jump::nobj_jump(char* str)
  : m_jump(str)
{}

const string nobj_jump::get_jump() const
{
  return m_jump;
}
