/*
 * factory.h
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

#ifndef ___MAKENSIS_NOBJ_FACTORY_H___
#define ___MAKENSIS_NOBJ_FACTORY_H___

#include "nobj.h"
#include "entry.h"
#include "file.h"
#include "function.h"
#include "int.h"
#include "jump.h"
#include "label.h"
#include "section.h"
#include "str.h"
#include "var.h"

#include <set>

/**
 * nobj_factory
 */

class nobj_factory
{

public:

  nobj_entry* create_entry(const int which);
  nobj_entry* create_entry(const int which, const nobjs& parms);

  nobj_file* create_file(const std::string& path); // XXX TODO

  nobj_function* create_function(const std::string& name);
  bool function_exists(const std::string& name);
  nobj_function* get_function(const std::string& name);

  nobj_int* create_int();

  nobj_jump* create_jump();

  nobj_label* create_label();

  nobj_section* create_section();
  nobjs_iterator get_sections();
  nobjs_const_iterator get_sections() const;

  nobj_string* create_string();

  nobj_var* create_var();

private:

  nobjs     m_entries;
  nobjs     m_ints;
  nobjs     m_jumps;
  nobjs     m_labels;
  nobjs     m_sections;

  std::set<nobj_string*>   m_strings;
  //std::set<nobj_vars*>     m_vars;
  //std::set<nobj_file*>     m_files;
  std::set<nobj_function*> m_functions;

};

#endif//!___MAKENSIS_NOBJ_VAR_H___
