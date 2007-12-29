/*
 * str.h
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

#ifndef ___MAKENSIS_NOBJ_STR_H___
#define ___MAKENSIS_NOBJ_STR_H___

#include "nobj.h"

/**
 * nobj_string
 */

class nobj_string : public nobj
{

public:

  nobj_string(const std::string& str);
  nobj_string(const char* str);
  nobj_string(char* str);

  const std::string get_string() const;

private:

  std::string m_string;

};

#endif//!___MAKENSIS_NOBJ_STR_H___
