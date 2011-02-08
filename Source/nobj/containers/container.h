/*
 * container.h
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

#ifndef ___MAKENSIS_NOBJ_CONTAINERS_CONTAINER_H___
#define ___MAKENSIS_NOBJ_CONTAINERS_CONTAINER_H___

#include "../nobj.h"

#include <vector>

/**
 * nobj_container
 */

class nobj_container
{

public:

  void add(nobj* obj);
  void add_recursively(nobj* obj);

  nobjs_iterator get();

private:

  nobjs m_nobjs;

};

#endif//!___MAKENSIS_NOBJ_CONTAINERS_CONTAINER_H___
