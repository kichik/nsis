// Added for Unicode support by Jim Park -- 08/29/2007
#pragma once
#include <string>
#include <sstream>

#ifdef _UNICODE
#  define tout        wcout
#  define terr        wcerr
#  define __T(x)      L ## x
#  define _T(x)       __T(x)
#  define _tmain      wmain
#  define _tunlink    _wunlink
#  define FOPEN _wfopen

   typedef std::wstring        tstring;
   typedef std::wistringstream tistringstream;

   typedef wchar_t      TCHAR;
#else
#  define tout        cout
#  define terr        cerr
#  define _T(x)       x
#  define _tmain      main
#  ifdef _WIN32
#  define _tunlink    _unlink
#  else
#  define _tunlink    unlink
#  endif
#  define FOPEN fopen

   typedef std::string         tstring;
   typedef std::istringstream  tistringstream;
   typedef char        TCHAR;
#endif
