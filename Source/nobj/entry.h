/*
 * entry.h
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

#ifndef ___MAKENSIS_NOBJ_ENTRY_H___
#define ___MAKENSIS_NOBJ_ENTRY_H___

#include "nobj.h"

/**
 * nobj_entry
 */

class nobj_entry : public nobj
{

public:

  nobj_entry(const int which);
  nobj_entry(const int which, const nobjs& parms);

  void set_parm(int offset, const nobj* parm);
  void set_parm(int offset, const int parm);
  void set_parm(int offset, const char* parm);
  void set_parm(int offset, std::string& parm);
  void set_parm_string(int offset, int i);
  void set_parm_jump(int offset, const char* jump);
  void set_parm_jump(int offset, std::string& jump);
  void set_parm_var(int offset, const char* var);
  void set_parm_var(int offset, std::string& var);

  const int which() const;

private:

  int m_which;

};

#endif//!___MAKENSIS_NOBJ_ENTRY_H___
