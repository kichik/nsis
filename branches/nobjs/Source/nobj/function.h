/*
 * function.h
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

#ifndef ___MAKENSIS_NOBJ_FUNCTION_H___
#define ___MAKENSIS_NOBJ_FUNCTION_H___

#include "nobj.h"
#include "entry.h"

#include "../exehead/fileform.h" // TODO skip section structure

/**
 * nobj_function
 */

class nobj_function : public nobj
{

public:

  nobj_function(const int name_addr, const int code_addr);

  const section* get_function() const; // XXX pointer?

  void add_entry(nobj_entry& entry);

private:

  section m_section;

};

#endif//!___MAKENSIS_NOBJ_FUNCTION_H___
