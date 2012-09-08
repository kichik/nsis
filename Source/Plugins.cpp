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
       files_itr++) // note: files are listed alphabetically, so plugin.dll will be listed before pluginW.dll
  {
    if (!dir_reader::matches(*files_itr, _T("*.dll")))
      continue;

    const tstring plugin = path + PLATFORM_PATH_SEPARATOR_C + *files_itr;
    GetExports(plugin, displayInfo);
  }
}

// VC6 cannot handle NSISException(const tstring& msg) : std::runtime_error(string(TtoCString(msg))) {}
struct NSISExceptionInner : public std::runtime_error
{
  NSISExceptionInner(const char* msg) : std::runtime_error(string(msg)) {} // Unicode
  NSISExceptionInner(const string&msg) : std::runtime_error(msg) {} // Ansi
};
struct NSISException : public NSISExceptionInner
{
  NSISException(const tstring& msg) : NSISExceptionInner(TtoCString(msg)) {}
};

namespace {
// This function slurps the whole file into the vector.
// Modified so the huge vector isn't returned by value.
void read_file(const tstring& filename, vector<unsigned char>& data) {
  FILE*file = FOPEN(filename.c_str(), ("rb"));

  if (!file) throw NSISException(_T("Can't open file '") + filename + _T("'"));

  MANAGE_WITH(file, fclose);
  bool succ = false;

  if (!fseek(file, 0, SEEK_END))
  {
    const long filesize = ftell(file);
    rewind(file);
    data.resize(filesize);
    size_t cbio = fread(reinterpret_cast<char*>(&data[0]), 1, filesize, file);
    succ = cbio == (unsigned)filesize;
  }
  if (!succ) throw NSISException(_T("Couldn't read entire file '") + filename + _T("'"));
}
}

void Plugins::GetExports(const tstring &pathToDll, bool displayInfo)
{
  vector<unsigned char> dlldata;
  PIMAGE_NT_HEADERS NTHeaders;
  try {
    read_file(pathToDll, dlldata);
    if (dlldata.empty()) return;
    NTHeaders = CResourceEditor::GetNTHeaders(&dlldata[0]);
  } catch (std::runtime_error&) {
    return;
  }

  tstring dllName = remove_file_extension(get_file_name(pathToDll));
#ifdef _UNICODE
  bool unicodeDll = dllName[dllName.size()-1] == 'W';
#endif

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
#ifdef _UNICODE
          const tstring lcsigA = lowercase(dllName.substr(0,dllName.size()-1) + _T("::") + tstring(CtoTString(name)));
          if (unicodeDll && m_command_to_path.find(lcsigA) != m_command_to_path.end())
            m_unicode_variant[lcsigA] = signature;
          else
#endif
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

tstring Plugins::UseUnicodeVariant(const tstring& command) const {
  return get_value(m_unicode_variant, lowercase(command), command);
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
