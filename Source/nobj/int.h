/*
 * int.h
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

#ifndef ___MAKENSIS_NOBJ_INT_H___
#define ___MAKENSIS_NOBJ_INT_H___

#include "nobj.h"

/**
 * nobj_int
 */

class nobj_int : public nobj
{

public:

  nobj_int(const int i);

  const int get_int() const;

private:

  int m_int;

};

#endif//!___MAKENSIS_NOBJ_INT_H___
