/*
 * nobjs.h
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

#ifndef ___MAKENSIS_NOBJS_H___
#define ___MAKENSIS_NOBJS_H___

#include <vector>
#include <string>

/**
 * nobj and nobj list declaration
 */

class nobj;
typedef std::vector<nobj> nobjs;

class nobj
{

public:

  /**
   * Returns an ordered list of nobjs this nobj depends on.
   */
  virtual const nobjs dependencies();

protected:

  /**
   * Adds a nobj dependency.
   */
  virtual void add_dependency(nobj& obj);

private:

  nobjs m_dependencies;

};

/**
 * nobj_string
 */

class nobj_string : public nobj
{

public:

  nobj_string(const std::string& str);
  nobj_string(const char* str);
  nobj_string(char* str);

  const std::string get_string();

private:

  std::string m_string;

};

/**
 * nobj_int
 */

class nobj_int : public nobj
{

public:

  nobj_int(const int i);

  const int get_int();

private:

  int m_int;

};

#endif//!___MAKENSIS_NOBJS_H___
