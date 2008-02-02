/*
 * code.cpp
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

#include "code.h"
#include "Platform.h"

using std::string;

/**
 * nobj_code
 */

void nobj_code::add_entry(const nobj_entry& entry)
{
  nobj_entry* entryp = new nobj_entry(entry.which(), entry.dependencies());
  nobj::add_dependency(entryp);
}

void nobj_code::add_label(const nobj_label& label)
{
  nobj_label* labelp = new nobj_label(label.get_label());
  nobj::add_dependency(labelp);
}
