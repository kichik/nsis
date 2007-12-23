/*
 * nobj.cpp
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

#include "nobj.h"
#include "Platform.h"

#include <stdexcept>

using std::string;
using std::length_error;

/**
 * nobj
 */

nobj::~nobj()
{
  nobjs_const_iterator i = m_dependencies.begin();

  while (i != m_dependencies.end())
  {
    delete *i;
    i++;
  }
}

const nobjs nobj::dependencies() const
{
  return m_dependencies;
}

void nobj::add_dependency(const nobj* obj)
{
  m_dependencies.push_back(obj);
}

void nobj::set_dependency(int offset, const nobj* obj)
{
  if (m_dependencies.size() <= offset)
  {
    m_dependencies.resize(offset + 1);
  }

  m_dependencies[offset] = obj;
}
