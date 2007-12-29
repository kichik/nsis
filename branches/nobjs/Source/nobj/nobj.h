/*
 * nobj.h
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

#ifndef ___MAKENSIS_NOBJ_H___
#define ___MAKENSIS_NOBJ_H___

#include <vector>
#include <string>

/**
 * nobj and nobj list declaration
 */

class nobj;
typedef std::vector<const nobj*> nobjs;
typedef nobjs::iterator nobjs_iterator;
typedef nobjs::const_iterator nobjs_const_iterator;

class nobj
{

public:

  /**
   * Deletes everyt dependency.
   * XXX fix with reference or something better when really used.
   */
  virtual ~nobj();

  /**
   * Returns an ordered list of nobjs this nobj depends on.
   */
  virtual const nobjs dependencies() const;

protected:

  /**
   * Adds a nobj dependency.
   */
  virtual void add_dependency(const nobj* obj);

  /**
   * Sets a nobj dependency in a specific place.
   */
  virtual void set_dependency(int offset, const nobj* obj);

private:

  nobjs m_dependencies;

};

#endif//!___MAKENSIS_NOBJ_H___
