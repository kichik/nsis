/*
 * container.cpp
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

#include "container.h"

/**
 * nobj_container
 */

void nobj_container::add(nobj* obj)
{
  m_nobjs.push_back(obj);
}

void nobj_container::add_recursively(nobj* obj)
{
  m_nobjs.push_back(obj);
}
