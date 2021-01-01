/*
 * dirreader.h
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
 *
 * Unicode support by Jim Park -- 08/23/2007
 */

#include "Platform.h"
#include "tstring.h"
#include <set>

class dir_reader {
public:

  typedef std::set<tstring>::const_iterator iterator;

  dir_reader();
  virtual ~dir_reader() {}

  virtual void read(const tstring& dir) = 0;

  virtual const std::set<tstring>& files();
  virtual const std::set<tstring>& dirs();

  // dir_reader always excludes . and .. AND the exclude list is private,
  // use this backdoor if you need to match "."
  virtual std::set<tstring>& hack_simpleexcluded() {return m_excluded;}

  virtual void exclude(const tstring& spec);
  virtual void exclude(const std::set<tstring>& specs);

  static bool matches(const tstring& name, const tstring& spec);

protected:

  virtual void add_file(const tstring& file);
  virtual void add_dir(const tstring& dir);

  virtual bool is_excluded(const tstring& name) const;

private:

  std::set<tstring> m_excluded;
  std::set<tstring> m_wildcard_excluded;

  std::set<tstring> m_files;
  std::set<tstring> m_dirs;

};

dir_reader* new_dir_reader();
