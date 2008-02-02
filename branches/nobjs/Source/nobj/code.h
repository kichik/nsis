/*
 * code.h
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

#ifndef ___MAKENSIS_NOBJ_CODE_H___
#define ___MAKENSIS_NOBJ_CODE_H___

#include "nobj.h"
#include "entry.h"
#include "label.h"

/**
 * nobj_code
 */

class nobj_code : public nobj
{

public:

  void add_entry(const nobj_entry& entry);
  void add_label(const nobj_label& label);

};

#endif//!___MAKENSIS_NOBJ_CODE_H___
