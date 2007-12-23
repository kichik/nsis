/*
 * var.h
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

#ifndef ___MAKENSIS_NOBJ_VAR_H___
#define ___MAKENSIS_NOBJ_VAR_H___

#include "nobj.h"

/**
 * nobj_var
 */

class nobj_var : public nobj
{

public:

  nobj_var(const std::string& str);
  nobj_var(const char* str);
  nobj_var(char* str);

  const std::string get_var() const;

private:

  std::string m_var;

};

#endif//!___MAKENSIS_NOBJ_VAR_H___
