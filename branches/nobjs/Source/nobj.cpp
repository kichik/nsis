/*
 * nobjs.cpp
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

const nobjs nobj::dependencies()
{
  return m_dependencies;
}

void nobj::add_dependency(nobj& obj)
{
  m_dependencies.push_back(obj);
}
