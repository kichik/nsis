/*
 * Plugins.h
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
 */

#ifndef NSIS_EXEHEADPLUGINS_H
#define NSIS_EXEHEADPLUGINS_H

#include <map>
#include <set>
#include "tstring.h"

namespace STL 
{
  template <class Arg1, class Arg2, class Result>
  struct binary_function
  {
    typedef Arg1 first_argument_type;
    typedef Arg2 second_argument_type;
    typedef Result result_type;
  };
  template<class S, class C>
  struct string_nocasecmpless : binary_function<S, S, bool> 
  {
    struct cmp : public binary_function<C, C, bool> 
    {
      bool operator() (const C&a, const C&b) const 
      {
        return tolower(a) < tolower(b); 
      }
    };
    bool operator() (const S&a,const S&b) const
    {
      return std::lexicographical_compare(a.begin(), a.end(), b.begin(), b.end(), cmp());
    }
  };
}

class Plugins
{
  public:
    typedef STL::string_nocasecmpless<tstring, tstring::value_type> strnocasecmp;

    Plugins() : m_initialized(false) {}

    bool Initialize(const TCHAR*arcsubdir, bool pe64, bool displayInfo);
    void AddPluginsDir(const tstring& path, bool pe64, bool displayInfo);
    bool FindDllPath(const tstring filename, tstring&dllPath);
    bool IsPluginCommand(const tstring& command) const;
    bool IsKnownPlugin(const tstring& token) const;
    bool GetCommandInfo(const tstring&command, tstring&canoniccmd, tstring&dllPath);
    int GetDllDataHandle(bool uninst, const tstring& command) const;
    void SetDllDataHandle(bool uninst, tstring&canoniccmd, int dataHandle);
    static bool IsPluginCallSyntax(const tstring& token);
    void PrintPluginDirs();

  private: // methods
    void GetExports(const tstring &pathToDll, bool pe64, bool displayInfo);
    bool DllHasDataHandle(const tstring& dllnamelowercase) const;

  private: // data members
    std::set<tstring, strnocasecmp> m_commands;
    std::map<tstring, tstring, strnocasecmp> m_dllname_to_path;
    std::map<tstring, int, strnocasecmp> m_dllname_to_inst_datahandle;
    std::map<tstring, int, strnocasecmp> m_dllname_to_unst_datahandle;
    std::set<tstring, strnocasecmp> m_dllname_conflicts;
    bool m_initialized;
};

#endif
