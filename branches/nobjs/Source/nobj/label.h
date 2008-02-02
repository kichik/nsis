/*
 * label.h
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

#ifndef ___MAKENSIS_NOBJ_LABEL_H___
#define ___MAKENSIS_NOBJ_LABEL_H___

#include "nobj.h"

/**
 * nobj_label
 */

class nobj_label : public nobj
{

public:

  nobj_label(const std::string& str);
  nobj_label(const char* str);
  nobj_label(char* str);

  const std::string get_label() const;

private:

  std::string m_label;

};

#endif//!___MAKENSIS_NOBJ_LABEL_H___
