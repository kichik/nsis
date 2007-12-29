/*
 * jump.h
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

#ifndef ___MAKENSIS_NOBJ_JUMP_H___
#define ___MAKENSIS_NOBJ_JUMP_H___

#include "nobj.h"

/**
 * nobj_jump
 */

class nobj_jump : public nobj
{

public:

  nobj_jump(const std::string& str);
  nobj_jump(const char* str);
  nobj_jump(char* str);

  const std::string get_jump() const;

private:

  std::string m_jump;

};

#endif//!___MAKENSIS_NOBJ_JUMP_H___
