/*
 * factory.cpp
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

#include "factory.h"

#include <stdexcept>

using std::string;
using std::length_error;

/**
 * nobj_factory
 */

// entries

nobj_entry* nobj_factory::create_entry(const int which)
{
  nobj_entry* entry = new nobj_entry(which);
  m_entries.push_back(entry);
  return entry;
}

nobj_entry* nobj_factory::create_entry(const int which, const nobjs& parms)
{
  nobj_entry* entry = new nobj_entry(which, parms);
  m_entries.push_back(entry);
  return entry;
}

// files

/*
nobj_file* nobj_factory::create_file(const std::string& path)
{
  nobj_file* file = new nobj_file(path);
  m_files.insert(file);
  return file;
}
*/

// functions

nobj_function* nobj_factory::create_function(const std::string& name)
{
  nobj_function* function = new nobj_function(name);
  m_functions.insert(function);
  return function;
}

bool nobj_factory::function_exists(const std::string& name)
{
  nobj_function search_function(name);
  return m_functions.find(&search_function) != m_functions.end();
}

nobj_function* nobj_factory::get_function(const std::string& name)
{
  nobj_function search_function(name);
  return m_functions.find(&search_function) != m_functions.end();
}

// ints

nobj_int* nobj_factory::create_int();

// jumps

nobj_jump* nobj_factory::create_jump();

// labels

nobj_label* nobj_factory::create_label();

// sections

nobj_section* nobj_factory::create_section();

nobjs_iterator nobj_factory::get_sections()
{
  return m_sections.begin();
}

const_nobjs_iterator nobj_factory::get_sections()
{
  return m_sections.begin();
}

// strings

nobj_string* nobj_factory::create_string()
{
}

// vars

nobj_var* nobj_factory::create_var()
{
}
*/