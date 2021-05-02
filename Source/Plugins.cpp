/*
 * Plugins.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
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

namespace {

template <class C, class K>
bool contains(const C& cntnr, const K& key)
{
  return cntnr.find(key) != cntnr.end();
}

template <class C, class K>
const typename C::const_iterator get_iterator(const C& cntnr, const K& key)
{
  const typename C::const_iterator it = cntnr.find(key);
  assert(cntnr.end() != it);
  return it;
}

template <class C, class K> 
typename C::const_iterator::value_type get_value(const C& cntnr, const K& key)
{
  return *get_iterator(cntnr,key);
}

template <class C, class K>
typename C::value_type::second_type get_paired_value(const C& cntnr, const K& key)
{
  return get_iterator(cntnr,key)->second;
}
template <class V, class C, class K>
V get_paired_value(const C& cntnr, const K& key, const V& defval)
{
   typename C::const_iterator it = cntnr.find(key);
   return cntnr.end() == it ? defval : it->second;
}

}

static tstring GetDllPath(const tstring&fullpath)
{
  tstring::size_type platsep = fullpath.rfind(PLATFORM_PATH_SEPARATOR_STR);
  tstring::size_type unixsep = platsep;
  if (PLATFORM_PATH_SEPARATOR_C != _T('/')) unixsep = fullpath.rfind(_T("/")); // Ideally get_dir_name would do this for us
  tstring::size_type lastsep = platsep != string::npos && unixsep != string::npos ? STD_MAX(platsep, unixsep) : STD_MIN(platsep, unixsep);
  if (lastsep == string::npos) return _T("");
  return fullpath.substr(0, lastsep);
}

static inline tstring GetDllName(const tstring&command)
{
  return get_string_prefix(command, _T("::"));
}

static inline void PrintCommandSig(const tstring sig)
{
  _ftprintf(g_output, _T(" + %") NPRIs _T("\n"), sig.c_str());
}

void Plugins::AddPluginsDir(const tstring &path, bool pe64, bool displayInfo)
{
  boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
  dr->read(path);

  for (dir_reader::iterator files_itr = dr->files().begin();
       files_itr != dr->files().end();
       files_itr++) // note: files are listed alphabetically, so plugin.dll will be listed before pluginW.dll
  {
    if (!dir_reader::matches(*files_itr, _T("*.dll")))
      continue;

    const tstring plugin = get_full_path(path + PLATFORM_PATH_SEPARATOR_C + *files_itr);
    GetExports(plugin, pe64, displayInfo);
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

void Plugins::GetExports(const tstring &pathToDll, bool pe64, bool displayInfo)
{
  PIMAGE_NT_HEADERS pNTHdrs;
  unsigned long dllsize;
  BYTE *dlldata = alloc_and_read_file(pathToDll.c_str(), dllsize);
  MANAGE_WITH(dlldata, free);

  try {
    if (!dllsize) return ;
    pNTHdrs = CResourceEditor::GetNTHeaders(&dlldata[0]); // This might throw
  } catch (std::runtime_error&) {
    return;
  }

  const WORD reqohm = pe64 ? IMAGE_NT_OPTIONAL_HDR64_MAGIC : IMAGE_NT_OPTIONAL_HDR32_MAGIC;
  if (*GetCommonMemberFromPEOptHdr(pNTHdrs->OptionalHeader, Magic) != reqohm)
  {
    // Ignore DLLs that don't match our target
    return;
  }

  tstring dllName = remove_file_extension(get_file_name(pathToDll));
  if (DllHasDataHandle(dllName))
  {
    m_dllname_conflicts.insert(dllName);
  }

  FIX_ENDIAN_INT16_INPLACE(pNTHdrs->FileHeader.Characteristics);
  if (pNTHdrs->FileHeader.Characteristics & IMAGE_FILE_DLL)
  {
    DWORD numrvaandsiz = *GetMemberFromPEOptHdr(pNTHdrs->OptionalHeader, NumberOfRvaAndSizes);
    FIX_ENDIAN_INT32_INPLACE(numrvaandsiz);
    if (numrvaandsiz <= IMAGE_DIRECTORY_ENTRY_EXPORT) return;

    const IMAGE_DATA_DIRECTORY *pExportDir;
    pExportDir = &(*GetMemberFromPEOptHdr(pNTHdrs->OptionalHeader, DataDirectory))[IMAGE_DIRECTORY_ENTRY_EXPORT];
    const DWORD ExportDirVA = FIX_ENDIAN_INT32(pExportDir->VirtualAddress);
    const DWORD ExportDirSize = FIX_ENDIAN_INT32(pExportDir->Size);

    PIMAGE_SECTION_HEADER sections = IMAGE_FIRST_SECTION(pNTHdrs);
    const WORD num_sections = FIX_ENDIAN_INT16(pNTHdrs->FileHeader.NumberOfSections);

    for (DWORD i = 0; i < num_sections; i++)
    {
      DWORD va = FIX_ENDIAN_INT32(sections[i].VirtualAddress);
      if (va <= ExportDirVA
          && va + FIX_ENDIAN_INT32(sections[i].SizeOfRawData) >= ExportDirVA + ExportDirSize)
      {
        DWORD prd = FIX_ENDIAN_INT32(sections[i].PointerToRawData);
        PIMAGE_EXPORT_DIRECTORY exports = PIMAGE_EXPORT_DIRECTORY(&dlldata[0] + prd + ExportDirVA - va);
        DWORD na = FIX_ENDIAN_INT32(exports->AddressOfNames);
        LPDWORD names = (LPDWORD)((ULONG_PTR)exports + na - ExportDirVA);
        for (DWORD j = 0; j < FIX_ENDIAN_INT32(exports->NumberOfNames); j++)
        {
          if (0 == j) m_dllname_to_path[dllName] = pathToDll;

          const string name = string((char*)exports + FIX_ENDIAN_INT32(names[j]) - ExportDirVA);
          const tstring canoniccmd = dllName + _T("::") + tstring(CtoTString(name));
          if (displayInfo)
          {
            bool hadCmd = contains(m_commands, canoniccmd);
            if (!hadCmd) PrintCommandSig(canoniccmd);
          }
          m_commands.insert(canoniccmd);
        }
        break;
      }
    }
  }
}

int Plugins::GetDllDataHandle(bool uninst, const tstring& command) const
{
  const tstring dllname = GetDllName(command);
  if (uninst)
    return get_paired_value(m_dllname_to_unst_datahandle, dllname, -1);
  else
    return get_paired_value(m_dllname_to_inst_datahandle, dllname, -1);
}

void Plugins::SetDllDataHandle(bool uninst, tstring&canoniccmd, int dataHandle)
{
  const tstring dllname = GetDllName(canoniccmd);
  if (uninst)
    m_dllname_to_unst_datahandle[dllname] = dataHandle;
  else
    m_dllname_to_inst_datahandle[dllname] = dataHandle;
}

bool Plugins::DllHasDataHandle(const tstring& dllnamelowercase) const
{
  int h = GetDllDataHandle(false, dllnamelowercase);
  if (-1 == h) h = GetDllDataHandle(true, dllnamelowercase);
  return -1 != h;
}

bool Plugins::Initialize(const TCHAR*arcsubdir, bool pe64, bool displayInfo)
{
  if (m_initialized) return true;
  m_initialized = true;

  AddPluginsDir(tstring(arcsubdir), pe64, displayInfo);

  return true;
}

bool Plugins::FindDllPath(const tstring filename, tstring&dllPath)
{
  tstring dllName = remove_file_extension(filename);
  if (!contains(m_dllname_to_path, dllName)) return false;
  dllPath = get_paired_value(m_dllname_to_path, dllName);
  return true;
}

bool Plugins::GetCommandInfo(const tstring&command, tstring&canoniccmd, tstring&dllPath)
{
  const tstring dllname = GetDllName(command);
  dllPath = get_paired_value(m_dllname_to_path, dllname);
  canoniccmd = get_value(m_commands, command);
  return !contains(m_dllname_conflicts, dllname);
}

bool Plugins::IsPluginCommand(const tstring& token) const
{
  return contains(m_commands, token);
}

bool Plugins::IsKnownPlugin(const tstring& token) const
{
  const tstring dllname = GetDllName(token);
  return contains(m_dllname_to_path, dllname);
}

bool Plugins::IsPluginCallSyntax(const tstring& token)
{
  const tstring dllname = GetDllName(token);
  return dllname.length() + 2 < token.length();
}

struct PrintPluginDirsHelper {
  template<class C> static void print(const C&c, const char*indent = "")
  {
    std::/*unordered_*/set<NSIS_CXX_TYPENAME STL::mapped_type<C>::type
#ifdef _WIN32
      , Plugins::strnocasecmp
#endif
    > seen;
    for (NSIS_CXX_TYPENAME C::const_iterator it = c.begin(); it != c.end(); ++it)
    {
      const tstring path = GetDllPath(it->second);
      if (contains(seen, path)) continue;
      seen.insert(path);
      _ftprintf(g_output, _T("%") NPRIns _T("%") NPRIs _T("\n"), indent, path.c_str());
    }
  }
};

void Plugins::PrintPluginDirs()
{
  _ftprintf(g_output, _T("Plugin directories:\n"));
  PrintPluginDirsHelper::print(m_dllname_to_path, "  ");
}

#endif
