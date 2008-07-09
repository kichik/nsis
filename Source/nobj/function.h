/*
 * function.h
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

#ifndef ___MAKENSIS_NOBJ_FUNCTION_H___
#define ___MAKENSIS_NOBJ_FUNCTION_H___

#include "nobj.h"
#include "code.h"

/**
 * nobj_function
 */

class nobj_function : public nobj_code
{

public:

  nobj_function();

  bool is_used();
  void set_used();

  int get_offset();
  void set_offset(int offset);

private:

  bool m_used;
  int m_offset;

};

#endif//!___MAKENSIS_NOBJ_FUNCTION_H___
