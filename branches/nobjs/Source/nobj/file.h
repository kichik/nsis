/*
 * file.h
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

#ifndef ___MAKENSIS_NOBJ_FILE_H___
#define ___MAKENSIS_NOBJ_FILE_H___

#include "nobj.h"

/**
 * nobj_file
 */

class nobj_file : public nobj
{

public:

  nobj_file(const std::string& path);
  nobj_file(const char* path);

  const std::string get_path() const;

private:

  std::string m_path;

};

#endif//!___MAKENSIS_NOBJ_FILE_H___
