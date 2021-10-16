/*
 * dirreader.cpp
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

#include "Platform.h"
#include "dirreader.h"
#include "tstring.h"
#include "util.h"
#include <set>

#include <string.h> // for stricmp()
#include <ctype.h> // for tolower()
#ifdef _UNICODE
#  include <wctype.h> // towlower()
#endif

using namespace std;

dir_reader::dir_reader() {
  exclude(_T("."));
  exclude(_T(".."));
}

const set<tstring>& dir_reader::files() {
  return m_files;
}

const set<tstring>& dir_reader::dirs() {
  return m_dirs;
}

void dir_reader::exclude(const tstring& spec) {
  if (spec.find_first_of(_T("?*")) != tstring::npos) {
    m_wildcard_excluded.insert(spec);
  } else {
    m_excluded.insert(spec);
  }
}

void dir_reader::exclude(const set<tstring>& specs) {
  iterator i = specs.begin();
  iterator e = specs.end();

  for (; i != e; i++) {
    exclude(*i);
  }
}

bool dir_reader::matches(const tstring& name, const tstring& spec) {
  tstring::const_iterator name_itr = name.begin();
  tstring::const_iterator name_end = name.end();

  tstring::const_iterator spec_itr = spec.begin();
  tstring::const_iterator spec_end = spec.end();

  tstring::const_iterator last_good_spec = spec_end;
  tstring::const_iterator last_good_name = name_end;

  while (name_itr != name_end && spec_itr != spec_end) {
    switch (*spec_itr) {
      case _T('?'):
        // question mark mathes one char
        name_itr++;
        spec_itr++;
        break;

      case _T('*'):
        // double asterisk is the same as a single asterisk
        while (*spec_itr == _T('*')) {
          spec_itr++;
          // asterisk at the end of the spec matches the end of the name
          if (spec_itr == spec_end)
            return true;
        }

        // remember last good name and spec for prematurely stopped asterisk
        last_good_spec = spec_itr;
        last_good_name = name_itr;

        break;

      default:
        // Jim Park: This should work since tolower is templated with Chartype.
        if (::tolower(*name_itr) != ::tolower(*spec_itr)) {
          if (last_good_spec != spec_end) {
            // matched wrong part of the name, try again
            spec_itr = last_good_spec;
            name_itr = ++last_good_name;
          } else {
            // no match and no asterisk to use
            return false;
          }
        } else {
          // remember last good name for prematurely stopped asterisk
          last_good_name = name_itr;

          spec_itr++;
          name_itr++;

          if (spec_itr == spec_end && name_itr != name_end && last_good_spec != spec_end) {
            // asterisk hasn't matched enough, keep matching
            spec_itr = last_good_spec;
          }
        }
        break;
    }
  }

  // skip any redundant asterisks and periods at the end of the name
  while (spec_itr != spec_end) {
    if (*spec_itr != _T('.') && *spec_itr != _T('*')) {
      break;
    }
    spec_itr++;
  }

  // return true only if managed to match everything
  return name_itr == name_end && spec_itr == spec_end;
}

void dir_reader::add_file(const tstring& file) {
  if (!is_excluded(file)) {
    m_files.insert(file);
  }
}

void dir_reader::add_dir(const tstring& dir) {
  if (!is_excluded(dir)) {
    m_dirs.insert(dir);
  }
}

bool dir_reader::is_excluded(const tstring& name) const {
  iterator i = m_excluded.begin();
  iterator e = m_excluded.end();

  for (; i != e; i++) {
    if (!::_tcsicmp(name.c_str(), i->c_str())) {
      return true;
    }
  }

  i = m_wildcard_excluded.begin();
  e = m_wildcard_excluded.end();

  for (; i != e; i++) {
    if (matches(name, *i)) {
      return true;
    }
  }

  return false;
}

#ifdef _WIN32

class win32_dir_reader : public dir_reader {
public:

  virtual void read(const tstring& dir) {
    WIN32_FIND_DATA fd;

    tstring spec = dir + PLATFORM_PATH_SEPARATOR_STR + _T("*.*");

    HANDLE h = ::FindFirstFile(spec.c_str(), &fd);
    if (h != INVALID_HANDLE_VALUE) {
      do {
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
          dir_reader::add_dir(fd.cFileName);
        else
          dir_reader::add_file(fd.cFileName);
      } while (::FindNextFile(h, &fd));
      ::FindClose(h);
    }
  }

};

#else

#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

class posix_dir_reader : public dir_reader {
public:

  virtual void read(const tstring& dir) {

    static const char platformpathsep[2] = {(char)PLATFORM_PATH_SEPARATOR_C, '\0'};

    char *nativedir = NSISRT_ttombpath(dir.c_str());
    if (!nativedir) return ;

    DIR *dip = ::opendir(nativedir);
    if (dip) {
      dirent *dit;
      while ((dit = ::readdir(dip))) {
        struct stat st;
        string file = nativedir;
        file += platformpathsep, file += dit->d_name;

        if (!stat(file.c_str(), &st)) {
          tstring name;
          name = PosixBug_CtoTString(dit->d_name);
          if (S_ISDIR(st.st_mode))
            dir_reader::add_dir(name);
          else
            dir_reader::add_file(name);
        }
      }
      ::closedir(dip);
    }
    NSISRT_free(nativedir);
  }
};

#endif

dir_reader* new_dir_reader() {
#ifdef _WIN32
  return new win32_dir_reader();
#else
  return new posix_dir_reader();
#endif
}
