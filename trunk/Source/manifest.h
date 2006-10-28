/*
 * manifest.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2006 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef ___MANIFEST_H___
#define ___MANIFEST_H___

#include <string>

namespace manifest
{
  enum comctl
  {
    comctl_old,
    comctl_xp
  };

  enum exec_level
  {
    exec_level_none,
    exec_level_user,
    exec_level_admin
  };

  std::string generate(comctl, exec_level);
};

#endif//!___MANIFEST_H___
