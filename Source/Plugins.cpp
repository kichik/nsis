/*
 * Plugins.cpp
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
 */

#include "exehead/config.h"
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT

#include <map>
#include "tstring.h"
#include <fstream>

#include "Plugins.h"
#include "Platform.h"
#include "util.h"
#include "ResourceEditor.h"

#include "dirreader.h"

#ifdef _WIN32
#  include <winnt.h>
#else
#  include <sys/stat.h>
#endif

#include "boost/scoped_ptr.hpp"

using namespace std;

extern FILE *g_output;

void Plugins::FindCommands(const tstring &path, bool displayInfo)
{
  boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
  dr->read(path);

  for (dir_reader::iterator files_itr = dr->files().begin();
       files_itr != dr->files().end();
       files_itr++)
  {
    if (!dir_reader::matches(*files_itr, _T("*.dll")))
      continue;

    const tstring plugin = path + PLATFORM_PATH_SEPARATOR_C + *files_itr;
    GetExports(plugin, displayInfo);
  }
}

struct NSISException : public std::runtime_error
{
    NSISException(const tstring& msg) : std::runtime_error(string(TtoCString(msg))) {}
};

namespace {
size_t file_size(ifstream& file) {
  const ifstream::pos_type pos = file.tellg();

  file.seekg(0, ios::end);

  ifstream::pos_type result = file.tellg();
  assert(result >= (ifstream::pos_type)0);

  file.seekg(pos);

  return (size_t)result;
}

// This function slurps the whole file into the vector.
// Modified so the huge vector isn't returned by value.
void read_file(const tstring& filename, vector<unsigned char>& data) {
  ifstream file(filename.c_str(), ios::binary);

  if (!file) {
    throw NSISException(_T("Can't open file '") + filename + _T("'"));
  }

  // get the file size
  size_t filesize = file_size(file);

  data.resize(filesize);

  file.read(reinterpret_cast<char*>(&data[0]), filesize);

  if (size_t(file.tellg()) != filesize) { // ifstream::eof doesn't return true here
    throw NSISException(_T("Couldn't read entire file '") + filename + _T("'"));
  }
}
}

void Plugins::GetExports(const tstring &pathToDll, bool displayInfo)
{
  vector<unsigned char> dlldata;
  PIMAGE_NT_HEADERS NTHeaders;
  try {
    read_file(pathToDll, dlldata);
    NTHeaders = CResourceEditor::GetNTHeaders(&dlldata[0]);
  } catch (std::runtime_error&) {
    return;
  }

  const tstring dllName = remove_file_extension(get_file_name(pathToDll));

  FIX_ENDIAN_INT16_INPLACE(NTHeaders->FileHeader.Characteristics);
  if (NTHeaders->FileHeader.Characteristics & IMAGE_FILE_DLL)
  {
    FIX_ENDIAN_INT32_INPLACE(NTHeaders->OptionalHeader.NumberOfRvaAndSizes);
    if (NTHeaders->OptionalHeader.NumberOfRvaAndSizes <= IMAGE_DIRECTORY_ENTRY_EXPORT) return;

    DWORD ExportDirVA = NTHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
    DWORD ExportDirSize = NTHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
    PIMAGE_SECTION_HEADER sections = IMAGE_FIRST_SECTION(NTHeaders);

    FIX_ENDIAN_INT32_INPLACE(ExportDirVA);
    FIX_ENDIAN_INT32_INPLACE(ExportDirSize);

    WORD num_sections = FIX_ENDIAN_INT16(NTHeaders->FileHeader.NumberOfSections);

    for (DWORD i = 0; i < num_sections; i++)
    {
      DWORD va = FIX_ENDIAN_INT32(sections[i].VirtualAddress);
      if (va <= ExportDirVA
          && va + FIX_ENDIAN_INT32(sections[i].Misc.VirtualSize) >= ExportDirVA + ExportDirSize)
      {
        DWORD prd = FIX_ENDIAN_INT32(sections[i].PointerToRawData);
        PIMAGE_EXPORT_DIRECTORY exports = PIMAGE_EXPORT_DIRECTORY(&dlldata[0] + prd + ExportDirVA - va);
        DWORD na = FIX_ENDIAN_INT32(exports->AddressOfNames);
        LPDWORD names = (LPDWORD)((ULONG_PTR)exports + na - ExportDirVA);
        for (DWORD j = 0; j < FIX_ENDIAN_INT32(exports->NumberOfNames); j++)
        {
          const string name = string((char*)exports + FIX_ENDIAN_INT32(names[j]) - ExportDirVA);
          const tstring signature = dllName + _T("::") + tstring(CtoTString(name));
          const tstring lcsig = lowercase(signature);
          m_command_to_path[lcsig] = pathToDll;
          m_command_lowercase_to_command[lcsig] = signature;
          if (displayInfo)
            _ftprintf(g_output, _T(" - %s\n"), signature.c_str());
        }
        break;
      }
    }
  }
}

bool Plugins::IsPluginCommand(const tstring& token) const {
  return m_command_to_path.find(lowercase(token)) != m_command_to_path.end();
}

namespace {
template <class Key, class Value>
Value get_value(const map<Key, Value>& the_map,
                const Key& key)
{
  assert(the_map.find(key) != the_map.end());
  return the_map.find(key)->second;
}

template <class Key, class Value>
Value get_value(const map<Key, Value>& the_map,
                const Key& key,
                const Value& defaultValue)
{
  if (the_map.find(key) == the_map.end())
    return defaultValue;
  return the_map.find(key)->second;
}
}

tstring Plugins::NormalizedCommand(const tstring& command) const {
  return get_value(m_command_lowercase_to_command, lowercase(command));
}

int Plugins::GetPluginHandle(bool uninst, const tstring& command) const {
  if (uninst) {
    return get_value(m_command_to_uninstall_data_handle, command, -1);
  }
  else {
    return get_value(m_command_to_data_handle, command, -1);
  }
}

tstring Plugins::GetPluginPath(const tstring& command) const {
  return get_value(m_command_to_path, lowercase(command));
}

void Plugins::SetDllDataHandle(bool uninst, const tstring& command, int dataHandle)
{
  if (uninst) {
    m_command_to_uninstall_data_handle[command] = dataHandle;
  }
  else {
    m_command_to_data_handle[command] = dataHandle;
  }
}

#endif
