#include "Platform.h"
#include "dirreader.h"
#include <string>
#include <set>

#include <ctype.h> // for tolower()

using namespace std;

dir_reader::dir_reader() {
  exclude(".");
  exclude("..");
}

const set<string>& dir_reader::files() {
  return m_files;
}

const set<string>& dir_reader::dirs() {
  return m_dirs;
}

void dir_reader::exclude(const string& spec) {
  if (spec.find_first_of("?*") != string::npos) {
    m_wildcard_excluded.insert(spec);
  } else {
    m_excluded.insert(spec);
  }
}

void dir_reader::exclude(const set<string>& specs) {
  iterator i = specs.begin();
  iterator e = specs.end();

  for (; i != e; i++) {
    exclude(*i);
  }
}

bool dir_reader::matches(const string& name, const string& spec) {
  string::const_iterator name_itr = name.begin();
  string::const_iterator name_end = name.end();

  string::const_iterator spec_itr = spec.begin();
  string::const_iterator spec_end = spec.end();

  string::const_iterator last_good_spec = spec_end;
  string::const_iterator last_good_name = name_end;

  while (name_itr != name_end && spec_itr != spec_end) {
    switch (*spec_itr) {
      case '?':
        // question mark mathes one char
        name_itr++;
        spec_itr++;
        break;

      case '*':
        // double asterisk is the same as a simgle asterisk
        while (*spec_itr == '*' && spec_itr != spec_end)
          spec_itr++;

        // asterisk at the end of the spec matches the end of the name
        if (spec_itr == spec_end)
          return true;

        // remember last good name and spec for prematurely stopped asterisk
        last_good_spec = spec_itr;
        last_good_name = name_itr;

        break;

      default:
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
    if (*spec_itr != '.' && *spec_itr != '*') {
      break;
    }
    spec_itr++;
  }

  // return true only if managed to match everything
  return name_itr == name_end && spec_itr == spec_end;
}

void dir_reader::add_file(const string& file) {
  if (!is_excluded(file)) {
    m_files.insert(file);
  }
}

void dir_reader::add_dir(const string& dir) {
  if (!is_excluded(dir)) {
    m_dirs.insert(dir);
  }
}

bool dir_reader::is_excluded(const string& name) const {
  iterator i = m_excluded.begin();
  iterator e = m_excluded.end();

  for (; i != e; i++) {
    if (name == *i) {
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

  virtual void read(const string& dir) {
    WIN32_FIND_DATA fd;

    string spec = dir + PLATFORM_PATH_SEPARATOR_STR + "*.*";

    HANDLE h = ::FindFirstFile(spec.c_str(), &fd);
    if (h != INVALID_HANDLE_VALUE) {
      do {
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
          dir_reader::add_dir(fd.cFileName);
        } else {
          dir_reader::add_file(fd.cFileName);
        }
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

  virtual void read(const string& dir) {
    //convert(dir);

    DIR *dip = ::opendir(dir.c_str());
    if (dip) {
      dirent *dit;
      while ((dit = ::readdir(dip))) {
        struct stat st;
        string file = dir + PLATFORM_PATH_SEPARATOR_STR + dit->d_name;

        if (!stat(file.c_str(), &st)) {
          if (S_ISDIR(st.st_mode)) {
            dir_reader::add_dir(dit->d_name);
          } else {
            dir_reader::add_file(dit->d_name);
          }
        }
      }
      ::closedir(dip);
    }
  }

private:

  void convert(string& path) {
    string::size_type pos = path.find('\\');
    while (pos != string::npos) {
      path[pos] = '/';
      pos = path.find('\\');
    }

    /* Replace drive letter X: by /x */
    if (path[1] == ':') {
      path[1] = ::tolower(path[0]);
      path[0] = '/';
    }
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
