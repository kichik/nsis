/*
 * manifest.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2025 Nullsoft and Contributors
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
  enum flags
  {
    disablewindowfiltering = 0x01, // Win8+ (Allow EnumWindows() to return Windows 8 immersive apps)
    gdiscaling = 0x02, // Win10FU1703+ blogs.windows.com/windowsdeveloper/2017/05/19/improving-high-dpi-experience-gdi-based-desktop-apps/
    flags_default = 0
  };

  enum comctl // WinXP+
  {
    comctl_old,
    comctl_xp
  };

  enum exec_level // WinVista+
  {
    exec_level_none,
    exec_level_user,
    exec_level_highest,
    exec_level_admin
  };

  enum dpiaware // WinVista+
  {
    dpiaware_notset,
    dpiaware_false,
    dpiaware_true, // System DPI on Vista+
    dpiaware_permonitor, // System DPI on Vista/7/8, PerMonitor on 8.1+ (Undocumented because we don't handle WM_DPICHANGED)
    dpiaware_explorer // Win8.1+? Undocumented?
  };

  enum longpathaware
  {
    lpaware_notset,
    lpaware_false,
    lpaware_true // Win10.0.14352+
  };

  class SupportedOSList // Win7+
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
    bool addall();
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

  typedef struct {
    flags Flags;
    dpiaware DPIA;
    const TCHAR *DPIA2; // Win10FU1607+
    longpathaware lpaware;
    SupportedOSList*pSOSL;
    const TCHAR *MaxVersionTested; // Win10FU1903+ github.com/microsoft/AppConsult-WinAppsModernizationWorkshop/tree/master/Exercise2
  } SPECIFICATION;

  std::string generate(comctl, exec_level, const SPECIFICATION&);
  bool addappendstring(const TCHAR*path, const TCHAR*data);

};

#endif//!___MANIFEST_H___
