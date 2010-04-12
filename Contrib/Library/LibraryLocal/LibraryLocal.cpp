/*

  LibraryLocal - used by the Library.nsh macros
  Get the version of local DLL and TLB files
  Written by Joost Verburg
  Unicode support by Jim Park -- 07/27/2007

*/

#include "../../../Source/Platform.h"
#include "../../../Source/tstring.h"

#include <stdio.h>
#include <iostream>
#include <fstream>

#include "../../../Source/util.h"
#include "../../../Source/winchar.h"

using namespace std;

int g_noconfig=0;
int g_display_errors=1;
FILE *g_output=stdout;

int GetTLBVersion(tstring& filepath, DWORD& high, DWORD & low)
{
#ifdef _WIN32

#ifdef _countof
#define COUNTOF _countof
#else
#define COUNTOF(a) (sizeof(a)/sizeof(a[0]))
#endif

  int found = 0;

  TCHAR fullpath[1024];
  TCHAR *p;
  if (!GetFullPathName(filepath.c_str(), COUNTOF(fullpath), fullpath, &p))
    return 0;

  ITypeLib* typeLib;
  HRESULT hr;

#ifdef _UNICODE
  hr = LoadTypeLib(fullpath, &typeLib);
#else
  // If built without UNICODE, we still need to convert this string to a Unicode string.
  WCHAR *ole_filename = winchar_fromTchar(fullpath);
  hr = LoadTypeLib(ole_filename, &typeLib);
#endif
  
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

int _tmain(int argc, TCHAR* argv[])
{

  // Parse the command line

  tstring cmdline;

  tstring mode;
  tstring filename;
  tstring filepath;

  int filefound = 0;

  if (argc != 4)
    return 1;

  // Get the full path of the local file

  mode = argv[1];
  filename = argv[2];

  // Validate filename

  tifstream fs(filename.c_str());
  
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
    
    // DLL / EXE
    
    if (mode.compare(_T("D")) == 0)
    {
      
      versionfound = GetDLLVersion(filename, high, low);

    }

    // TLB
    
    if (mode.compare(_T("T")) == 0)
    {
      
      versionfound = GetTLBVersion(filename, high, low);

    }

  }

  // Write the version to an NSIS header file

  tofstream header(argv[3], tofstream::out);
  
  if (header)
  {

    if (!filefound)
    {
      header << _T("!define LIBRARY_VERSION_FILENOTFOUND") << endl;
    }
    else if (!versionfound)
    {
      header << _T("!define LIBRARY_VERSION_NONE") << endl;
    }
    else
    {
      header << _T("!define LIBRARY_VERSION_HIGH ") << high << endl;
      header << _T("!define LIBRARY_VERSION_LOW ") << low << endl;
    }
    
    header.close();

  }

  return 0;

}
