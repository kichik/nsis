#include "exehead/config.h"
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT

#include <map>
#include <string>
#include <fstream>

#include "Plugins.h"
#include "Platform.h"
#include "util.h"
#include "ResourceEditor.h"

#include "dirreader.h"

#ifdef _WIN32
#  include <WinNT.h>
#else
#  include <sys/stat.h>
#endif

#include "boost/scoped_ptr.hpp"

using namespace std;

extern FILE *g_output;

void Plugins::FindCommands(const string &path, bool displayInfo)
{
  boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
  dr->read(path);

  for (dir_reader::iterator files_itr = dr->files().begin();
       files_itr != dr->files().end();
       files_itr++)
  {
    if (!dir_reader::matches(*files_itr, "*.dll"))
      continue;

    const string plugin = path + PLATFORM_PATH_SEPARATOR_C + *files_itr;
    GetExports(plugin, displayInfo);
  }
}

struct NSISException : public std::runtime_error
{
  NSISException(const string& msg) : std::runtime_error(msg) {}
};

namespace {
size_t file_size(ifstream& file) {
  const ifstream::pos_type pos = file.tellg();

  file.seekg(0, ios::end);

  ifstream::pos_type result = file.tellg();
  assert(result >= 0);

  file.seekg(pos);

  return (size_t)result;
}

vector<unsigned char> read_file(const string& filename) {
  ifstream file(filename.c_str(), ios::binary);

  if (!file) {
    throw NSISException("Can't open file '" + filename + "'");
  }

  // get the file size
  size_t filesize = file_size(file);

  vector<unsigned char> result;
  result.resize(filesize);

  file.read(reinterpret_cast<char*>(&result[0]), filesize);

  if (size_t(file.tellg()) != filesize) { // ifstream::eof doesn't return true here
    throw NSISException("Couldn't read entire file '" + filename + "'");
  }

  return result;
}
}

void Plugins::GetExports(const string &pathToDll, bool displayInfo)
{
  vector<unsigned char> dlldata;
  PIMAGE_NT_HEADERS NTHeaders;
  try {
    dlldata = read_file(pathToDll);
    NTHeaders = CResourceEditor::GetNTHeaders(&dlldata[0]);
  } catch (std::runtime_error&) {
    return;
  }

  const string dllName = remove_file_extension(get_file_name(pathToDll));

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
        unsigned long *names = (unsigned long*)((unsigned long) exports + (char *) na - ExportDirVA);
        for (unsigned long j = 0; j < FIX_ENDIAN_INT32(exports->NumberOfNames); j++)
        {
          const string name = string((char*)exports + FIX_ENDIAN_INT32(names[j]) - ExportDirVA);
          const string signature = dllName + "::" + name;
          const string lcsig = lowercase(signature);
          m_command_to_path[lcsig] = pathToDll;
          m_command_lowercase_to_command[lcsig] = signature;
          if (displayInfo)
            fprintf(g_output, " - %s\n", signature.c_str());
        }
        break;
      }
    }
  }
}

bool Plugins::IsPluginCommand(const string& token) const {
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

string Plugins::NormalizedCommand(const string& command) const {
  return get_value(m_command_lowercase_to_command, lowercase(command));
}

int Plugins::GetPluginHandle(bool uninst, const string& command) const {
  if (uninst) {
    return get_value(m_command_to_uninstall_data_handle, command, -1);
  }
  else {
    return get_value(m_command_to_data_handle, command, -1);
  }
}

string Plugins::GetPluginPath(const string& command) const {
  return get_value(m_command_to_path, lowercase(command));
}

void Plugins::SetDllDataHandle(bool uninst, const string& command, int dataHandle)
{
  if (uninst) {
    m_command_to_uninstall_data_handle[command] = dataHandle;
  }
  else {
    m_command_to_data_handle[command] = dataHandle;
  }
}

#endif
