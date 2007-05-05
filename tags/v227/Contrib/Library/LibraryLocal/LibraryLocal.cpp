/*

  LibraryLocal - used by the Library.nsh macros
  Get the version of local DLL and TLB files
  Written by Joost Verburg

*/

#include "../../../Source/Platform.h"

#include <stdio.h>
#include <iostream>
#include <fstream>

#include "../../../Source/util.h"
#include "../../../Source/winchar.h"

using namespace std;

int g_noconfig=0;
int g_display_errors=1;
FILE *g_output=stdout;

int GetTLBVersion(string& filepath, DWORD& high, DWORD & low)
{
#ifdef _WIN32

  int found = 0;

  char fullpath[1024];
  char *p;
  if (!GetFullPathName(filepath.c_str(), sizeof(fullpath), fullpath, &p))
    return 0;

  wchar_t ole_filename[1024];
  MultiByteToWideChar(CP_ACP, 0, fullpath, lstrlen(fullpath) + 1, ole_filename, 1024);
  
  ITypeLib* typeLib;
  HRESULT hr;
  
  hr = LoadTypeLib(ole_filename, &typeLib);
  
  if (SUCCEEDED(hr)) {

    TLIBATTR* typelibAttr;
    
    hr = typeLib->GetLibAttr(&typelibAttr);

    if (SUCCEEDED(hr)) {
      
      high = typelibAttr->wMajorVerNum;
      low = typelibAttr->wMinorVerNum;
      
      found = 1;

    }

    typeLib->Release();

  }

  return found;

#else

  return 0;

#endif
}

int main(int argc, char* argv[])
{

  // Parse the command line

  string cmdline;

  string mode;
  string filename;
  string filepath;

  int filefound = 0;

  if (argc != 4)
    return 1;

  // Get the full path of the local file

  mode = argv[1];
  filename = argv[2];

  // Validate filename

  ifstream fs(filename.c_str());
  
  if (fs.is_open())
  {
    filefound = 1;
    fs.close();
  }

  // Work
  
  int versionfound = 0;
  DWORD low = 0, high = 0;

  if (filefound)
  {

    // Get version
    
    // DLL
    
    if (mode.compare("D") == 0)
    {
      
      versionfound = GetDLLVersion(filename, high, low);

    }

    // TLB
    
    if (mode.compare("T") == 0)
    {
      
      versionfound = GetTLBVersion(filename, high, low);

    }

  }

  // Write the version to an NSIS header file

  ofstream header(argv[3], ofstream::out);
  
  if (header)
  {

    if (!filefound)
    {
      header << "!define LIBRARY_VERSION_FILENOTFOUND" << endl;
    }
    else if (!versionfound)
    {
      header << "!define LIBRARY_VERSION_NONE" << endl;
    }
    else
    {
      header << "!define LIBRARY_VERSION_HIGH " << high << endl;
      header << "!define LIBRARY_VERSION_LOW " << low << endl;
    }
    
    header.close();

  }

  return 0;

}
