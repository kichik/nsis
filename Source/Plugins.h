/*
 * Plugins.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2009 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Unicode support by Jim Park -- 08/21/2007
 */

#ifndef __X18_PLUGINS_H
#define __X18_PLUGINS_H

#include <map>
#include "tstring.h"

class Plugins
{
  public:
    void FindCommands(const tstring& path, bool displayInfo);
    bool IsPluginCommand(const tstring& command) const;
    tstring NormalizedCommand(const tstring& command) const;
    int GetPluginHandle(bool uninst, const tstring& command) const;
    tstring GetPluginPath(const tstring& command) const;
    void SetDllDataHandle(bool uninst, const tstring& command, int dataHandle);

  private: // methods
    void GetExports(const tstring &pathToDll, bool displayInfo);

  private: // data members
    std::map<tstring, tstring> m_command_lowercase_to_command;
    std::map<tstring, tstring> m_command_to_path;
    std::map<tstring, int> m_command_to_data_handle;
    std::map<tstring, int> m_command_to_uninstall_data_handle;
};

#endif
