/*
 * Plugins.h
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

#ifndef __X18_PLUGINS_H
#define __X18_PLUGINS_H

#include <map>
#include <string>

class Plugins
{
  public:
    void FindCommands(const std::string& path, bool displayInfo);
    bool IsPluginCommand(const std::string& command) const;
    std::string NormalizedCommand(const std::string& command) const;
    int GetPluginHandle(bool uninst, const std::string& command) const;
    std::string GetPluginPath(const std::string& command) const;
    void SetDllDataHandle(bool uninst, const std::string& command, int dataHandle);

  private: // methods
    void GetExports(const std::string &pathToDll, bool displayInfo);

  private: // data members
    std::map<std::string, std::string> m_command_lowercase_to_command;
    std::map<std::string, std::string> m_command_to_path;
    std::map<std::string, int> m_command_to_data_handle;
    std::map<std::string, int> m_command_to_uninstall_data_handle;
};

#endif
