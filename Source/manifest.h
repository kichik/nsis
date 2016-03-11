/*
 * manifest.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2016 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Reviewed for Unicode support by Jim Park -- 08/22/2007
 */

#ifndef ___MANIFEST_H___
#define ___MANIFEST_H___

#include "tstring.h"
#include "strlist.h"

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
    exec_level_highest,
    exec_level_admin
  };

  enum dpiaware
  {
    dpiaware_notset,
    dpiaware_false,
    dpiaware_true,
  };

  class SupportedOSList
  {
    StringList m_list;
    bool m_isdefaultlist;
  public:
    SupportedOSList() : m_isdefaultlist(false) {}

    bool append(const TCHAR* osid);
    int getcount() const { return m_list.getnum(); }
    bool isdefaultlist() const { return m_isdefaultlist; }
    const TCHAR* get(int idx)
    {
      int pos = m_list.idx2pos(idx);
      if (-1 == pos) return 0;
      return m_list.get() + pos;
    }
    void addall();
    void deleteall() 
    { 
      m_list.deleteall();
      m_isdefaultlist = false;
    }
    void setdefault()
    {
      m_list.deleteall();
      append(_T("Win7"));
      append(_T("Win8"));
      append(_T("Win8.1")); // In the default list because GetVersion[Ex] lies if this is not set in the manifest
      append(_T("Win10"));
      m_isdefaultlist = true;
    }
  };

  std::string generate(comctl, exec_level, dpiaware, SupportedOSList&);

};

#endif//!___MANIFEST_H___
