/*
 * scriptpp.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2025 Nullsoft and Contributors
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
#include <stdio.h>
#include <ctype.h>
#include "tstring.h"
#include "lineparse.h"
#include <nsis-version.h>
#include "tokens.h"
#include "build.h"
#include "utf.h"
#include "util.h"
#include "BinInterop.h"
#include "dirreader.h"
#include <cassert> // for assert(3)
#include <time.h>
#include <algorithm>
#include "boost/scoped_ptr.hpp"

using namespace std;

#ifdef _WIN32
#  include <direct.h> // for chdir
#else
#  include <sys/stat.h> // for stat and umask
#  include <sys/types.h> // for mode_t
#  include <fcntl.h> // for O_RDONLY
#  include <unistd.h>
#  include <stdlib.h> // for mkstemp
#endif

#define MAX_INCLUDEDEPTH 10

#define PRINTHELPEX(cmdname) { print_help((cmdname)); return PS_ERROR; }
#define PRINTHELP() PRINTHELPEX(line.gettoken_str(0))
static void PREPROCESSONLY_BEGINCOMMENT() { extern FILE *g_output; _ftprintf(g_output, _T("!if 0 /*\n")); }
static void PREPROCESSONLY_ENDCOMMENT() { extern FILE *g_output; _ftprintf(g_output, _T("*/\n!endif\n")); }


static UINT read_line_helper(NStreamLineReader&lr, TCHAR*buf, UINT cch)
{
  // Helper function for reading lines from text files. buf MUST be valid and cch MUST be > 1!
  // Returns 0 on error or the number of characters read including the first \n, \r or \0.
  // When it returns 0, buf[0] is 0 for EOF and NStream::ERR_* for errors.
  UINT lrr = lr.ReadLine(buf, cch), eof = 0;
  if (NStream::OK != lrr)
  {
    ++eof;
    if (!lr.IsEOF())
    {
      buf[0] = (TCHAR) lrr;
      return 0;
    }
  }
  const bool unicode = lr.IsUnicode();
  for(cch = 0;; ++cch)
    if (!buf[cch] || NStream::IsNewline(buf[cch], unicode))
      break;
  if (cch) eof = 0; // Read something, postpone EOF
  return ++cch - eof;
}

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
TCHAR *CEXEBuild::set_file_predefine(const TCHAR *filename)
{
  TCHAR *oldfileinfo = NULL;
  TCHAR *oldfilename = definedlist.find(_T("__FILE__"));
  TCHAR *oldfiledir = definedlist.find(_T("__FILEDIR__"));
  if (oldfilename && oldfiledir)
  {
    oldfileinfo = new TCHAR[_tcslen(oldfilename)+1+_tcslen(oldfiledir)+1];
    _tcscpy(oldfileinfo, oldfilename);
    _tcscat(oldfileinfo, _T("|"));
    _tcscat(oldfileinfo, oldfiledir);
    definedlist.del(_T("__FILE__"));
    definedlist.del(_T("__FILEDIR__"));
  }
  const TCHAR *p = _tcsrchr(filename,_T('\\')), *p2 = _tcsrchr(filename,_T('/'));
  if (p2 > p) p = p2;
  if (p) p++; else p = filename;
  definedlist.add(_T("__FILE__"),p);
  TCHAR dir[260]; // BUGBUG: MAX_PATH outside #ifdef _WIN32, should be PATH/NAME_MAX on POSIX?
#ifdef _WIN32
  LPTSTR lpFilePart;
  if (!GetFullPathName(filename, COUNTOF(dir), dir, &lpFilePart)) *dir = _T('\0');
  PathRemoveFileSpec(dir);
#else
  if (p == filename)
    _tcscpy(dir, _T("."));
  else
    my_strncpy(dir, filename, p-filename+!0);
#endif
  definedlist.add(_T("__FILEDIR__"),dir);
  return oldfileinfo;
}
void CEXEBuild::restore_file_predefine(TCHAR *oldfilename)
{
  definedlist.del(_T("__FILEDIR__"));
  definedlist.del(_T("__FILE__"));
  if (oldfilename)
  {
    TCHAR *oldfiledir = _tcschr(oldfilename, _T('|'));
    definedlist.add(_T("__FILEDIR__"),oldfiledir+1);
    *oldfiledir = '\0';
    definedlist.add(_T("__FILE__"),oldfilename);
    delete[] oldfilename;
  }
}

TCHAR *CEXEBuild::set_timestamp_predefine(const TCHAR *filename)
{
  TCHAR *oldtimestamp = definedlist.find(_T("__TIMESTAMP__"));
  if (oldtimestamp)
  {
    oldtimestamp = _tcsdup(oldtimestamp);
    definedlist.del(_T("__TIMESTAMP__"));
  }
#ifdef _WIN32
  TCHAR datebuf[128] = _T(""), timebuf[128] = _T(""), timestampbuf[256];
  WIN32_FIND_DATA fd;
  FILETIME floctime;
  SYSTEMTIME stime;

  HANDLE hSearch = FindFirstFile(filename, &fd);
  if (hSearch != INVALID_HANDLE_VALUE)
  {
    FindClose(hSearch);

    FileTimeToLocalFileTime(&fd.ftLastWriteTime, &floctime);
    FileTimeToSystemTime(&floctime, &stime);
    GetDateFormat(LOCALE_USER_DEFAULT, DATE_LONGDATE, &stime, NULL, datebuf, COUNTOF(datebuf));
    GetTimeFormat(LOCALE_USER_DEFAULT, 0, &stime, NULL, timebuf, COUNTOF(timebuf));
    wsprintf(timestampbuf,_T("%") NPRIs _T(" %") NPRIs,datebuf,timebuf);
    definedlist.add(_T("__TIMESTAMP__"),timestampbuf);
  }
#else
  struct stat st;
  if (!_tstat(filename, &st))
    definedlist.add(_T("__TIMESTAMP__"),PosixBug_CtoTString(ctime(&st.st_mtime)));
#endif
  return oldtimestamp;
}
void CEXEBuild::restore_timestamp_predefine(TCHAR *oldtimestamp)
{
  definedlist.del(_T("__TIMESTAMP__"));
  if (oldtimestamp)
  {
    definedlist.add(_T("__TIMESTAMP__"),oldtimestamp);
    free(oldtimestamp);
  }
}

TCHAR *CEXEBuild::set_line_predefine(int linecnt, BOOL is_macro)
{
  TCHAR* linebuf = NULL;
  MANAGE_WITH(linebuf, free);

  TCHAR temp[128] = _T("");
  _stprintf(temp,_T("%d"),linecnt);

  TCHAR *oldline = definedlist.find(_T("__LINE__"));
  if (oldline)
  {
    oldline = _tcsdup(oldline);
    definedlist.del(_T("__LINE__"));
  }
  if (is_macro && oldline)
  {
    linebuf = (TCHAR *)malloc((_tcslen(oldline)+_tcslen(temp)+2)*sizeof(TCHAR));
    _stprintf(linebuf,_T("%") NPRIs _T(".%") NPRIs,oldline,temp);
  }
  else
  {
    linebuf = _tcsdup(temp);
  }
  definedlist.add(_T("__LINE__"),linebuf);

  return oldline;
}
void CEXEBuild::restore_line_predefine(TCHAR *oldline)
{
  definedlist.del(_T("__LINE__"));
  if (oldline)
  {
    definedlist.add(_T("__LINE__"),oldline);
    free(oldline);
  }
}

void CEXEBuild::set_date_time_predefines()
{
  time_t etime;
  struct tm *ltime;
  TCHAR datebuf[128], timebuf[128];

  time(&etime);
  ltime = localtime(&etime);
#ifdef _WIN32
  SYSTEMTIME st;
  st.wYear = ltime->tm_year+1900, st.wMonth = ltime->tm_mon + 1, st.wDay = ltime->tm_mday;
  st.wHour = ltime->tm_hour, st.wMinute = ltime->tm_min, st.wSecond = ltime->tm_sec;
  st.wMilliseconds = 0;
  GetDateFormat(LOCALE_USER_DEFAULT, DATE_SHORTDATE, &st, NULL, datebuf, sizeof(datebuf));
  definedlist.add(_T("__DATE__"), datebuf);
  GetTimeFormat(LOCALE_USER_DEFAULT, 0, &st, NULL, timebuf, sizeof(timebuf));
  definedlist.add(_T("__TIME__"), timebuf);
#else
  my_strftime(datebuf, sizeof(datebuf), _T("%x"), ltime);
  definedlist.add(_T("__DATE__"), datebuf);
  my_strftime(timebuf, sizeof(timebuf), _T("%X"), ltime);
  definedlist.add(_T("__TIME__"), timebuf);
#endif
}
void CEXEBuild::del_date_time_predefines()
{
  definedlist.del(_T("__DATE__"));
  definedlist.del(_T("__TIME__"));
}
#endif

TCHAR* CEXEBuild::GetMacro(const TCHAR *macroname, TCHAR**macroend /*= 0*/)
{
  TCHAR *t = (TCHAR*)m_macros.get(), *mbeg, *mbufbeg = t;
  size_t cbAll = m_macros.getlen();
  for (; t && *t; ++t)
  {
    mbeg = t;
    if ((size_t)t - (size_t)mbufbeg >= cbAll) break;
    const bool foundit = !_tcsicmp(mbeg, macroname);
    t += _tcslen(t) + 1; // advance over macro name

    // advance over parameters
    while (*t) t += _tcslen(t) + 1;
    t++; // Separator between parameters and data

    // advance over data
    while (*t) t += _tcslen(t) + 1;

    if (foundit)
    {
      if (macroend) *macroend = ++t;
      return mbeg;
    }
  }
  return 0;
}

TCHAR* CEXEBuild::GetMacro(size_t idx)
{
  TCHAR *t = (TCHAR*)m_macros.get(), *mbufbeg = t;
  for (size_t i = 0, cbAll = m_macros.getlen(); t && *t; ++t)
  {
    if ((size_t)t - (size_t)mbufbeg >= cbAll) break;
    if (i++ == idx) return t;
    t += _tcslen(t) + 1; // advance over macro name
    while (*t) t += _tcslen(t) + 1; // advance over parameters
    t++; // Separator between parameters and data
    while (*t) t += _tcslen(t) + 1; // advance over data
  }
  return 0;
}

int CEXEBuild::pp_macro(LineParser&line)
{
  const TCHAR*const macroname = line.gettoken_str(1), *tokstr;
  if (!macroname[0]) PRINTHELP()
  if (MacroExists(macroname))
  {
    ERROR_MSG(_T("!macro: macro named \"%") NPRIs _T("\" already exists!\n"), macroname);
    return PS_ERROR;
  }
  m_macros.add(macroname, (int)(_tcslen(macroname)+1)*sizeof(TCHAR));

  for (int pc=2; pc < line.getnumtokens(); pc++)
  {
    if (!(tokstr = line.gettoken_str(pc))[0])
    {
      ERROR_MSG(_T("!macro: macro parameter %d is empty, not valid!\n"), pc-1);
      return PS_ERROR;
    }
    for (int a = 2; a < pc; a++)
    {
      if (!_tcsicmp(tokstr, line.gettoken_str(a)))
      {
        ERROR_MSG(_T("!macro: macro parameter named %") NPRIs _T(" is used multiple times!\n"), tokstr);
        return PS_ERROR;
      }
    }
    m_macros.add(tokstr, (int)(_tcslen(tokstr)+1)*sizeof(TCHAR));
  }
  m_macros.add(_T(""), sizeof(_T(""))); // Separator between parameters and data

  for (;;)
  {
    TCHAR *str = m_templinebuf, *p = str;
    UINT lrres = curlinereader->ReadLine(str, MAX_LINELENGTH);
    if (NStream::OK != lrres)
    {
      if (curlinereader->IsEOF())
      {
        if (!str[0])
        {
          ERROR_MSG(_T("!macro \"%") NPRIs _T("\": unterminated (no !macroend found in file)!\n"), macroname);
          return PS_ERROR;
        }
      }
      else
      {
        ERROR_MSG(curlinereader->GetErrorMessage(lrres).c_str());
        return PS_ERROR;
      }
    }
    //SCRIPT_MSG(_T("%") NPRIs _T("%") NPRIs, str, str[_tcslen(str)-1] == _T('\n') ? _T("") : _T("\n"));
    // remove trailing whitespace
    while (*p) p++;
    if (p > str) p--;
    while (p >= str && (*p == _T('\r') || *p == _T('\n') || *p == _T(' ') || *p == _T('\t'))) p--;
    *++p = 0;
    LineParser l2(false);
    if (!l2.parse(str))
    {
      if (!_tcsicmp(l2.gettoken_str(0), _T("!macroend")))
      {
        linecnt++;
        break;
      }
      if (!_tcsicmp(l2.gettoken_str(0), _T("!macro")))
      {
        ERROR_MSG(_T("Error: can't define a macro inside a macro!\n"));
        return PS_ERROR;
      }
    }
    if (str[0]) m_macros.add(str, (int)(_tcslen(str)+1)*sizeof(TCHAR));
    else m_macros.add(_T(" "), sizeof(_T(" ")));
    linecnt++;
  }
  m_macros.add(_T(""), sizeof(_T(""))); // End of data
  return PS_OK;
}

int CEXEBuild::pp_macroundef(LineParser&line)
{
  const TCHAR*const mname = line.gettoken_str(1);
  if (!mname[0]) PRINTHELP()
  TCHAR *mend, *mbeg = GetMacro(mname, &mend);
  if (!mbeg)
  {
    ERROR_MSG(_T("!macroundef: \"%") NPRIs _T("\" does not exist!\n"), mname);
    return PS_ERROR;
  }
  TCHAR *mbufb = (TCHAR*)m_macros.get();
  const size_t mcb = ((mend)-mbeg)*sizeof(TCHAR), mbufcb = m_macros.getlen();
  memmove(mbeg, mend, mbufcb-(((mbeg-mbufb)*sizeof(TCHAR))+mcb));
  m_macros.resize(truncate_cast(int,(size_t)(mbufcb-mcb)));
  SCRIPT_MSG(_T("!macroundef: %") NPRIs _T("\n"), mname);
  return PS_OK;
}

int CEXEBuild::pp_insertmacro(LineParser&line)
{
  static unsigned char g_insertmacrorecursion = 0;
  const TCHAR*const macroname = line.gettoken_str(1);
  if (!macroname[0]) PRINTHELP()
  TCHAR *t = GetMacro(macroname), *m = (TCHAR *)m_macros.get();
  SCRIPT_MSG(_T("!insertmacro: %") NPRIs _T("\n"), macroname);
  if (!t)
  {
    ERROR_MSG(_T("!insertmacro: macro named \"%") NPRIs _T("\" not found!\n"), macroname);
    return PS_ERROR;
  }
  t+=_tcslen(t)+1;

  GrowBuf l_define_names;
  DefineList l_define_saves;
  int npr = 0;
  // advance over params
  while (*t)
  {
    TCHAR *v = definedlist.find(t);
    if (v)
    {
      l_define_saves.add(t,v);
      definedlist.del(t);
    }
    l_define_names.add(t, (int)(_tcslen(t)+1)*sizeof(TCHAR));
    definedlist.add(t, line.gettoken_str(npr+2));

    npr++;
    t += _tcslen(t)+1;
  }
  l_define_names.add(_T(""), sizeof(_T("")));
  t++;
  if (npr != line.getnumtokens()-2)
  {
    ERROR_MSG(_T("!insertmacro: macro \"%") NPRIs _T("\" requires %d parameter(s), passed %d!\n"), macroname, npr,line.getnumtokens()-2);
    return PS_ERROR;
  }
  if (++g_insertmacrorecursion > MAX_MACRORECURSION)
  {
    ERROR_MSG(_T("!insertmacro: insert depth is limited to %u macros!\n"), MAX_MACRORECURSION);
    return PS_ERROR;
  }
  const bool oldparserinsidecomment = inside_comment;
  inside_comment = false; // "!insertmacro foo /*" does not mean that the macro body is a comment
  TCHAR str[1024];
  wsprintf(str, _T("macro:%") NPRIs, macroname);
  const TCHAR *oldmacroname = m_currentmacroname;
  m_currentmacroname = macroname;
  definedlist.set(_T("__MACRO__"), m_currentmacroname);
  int lp = 0;
  while (*t)
  {
    lp++;
    if (_tcscmp(t, _T(" ")))
    {
      int ret = process_oneline(t, str, lp, PLF_MACRO);
      if (ret != PS_OK)
      {
        ERROR_MSG(_T("Error in macro %") NPRIs _T(" on macroline %d\n"), macroname, lp);
        return ret;
      }
    }
    {
      // fix t if process_oneline changed m_macros
      TCHAR *nm = (TCHAR*)m_macros.get();
      if (nm != m) t += nm - m, m = nm;
    }
    t += _tcslen(t)+1;
  }
  {
    TCHAR *p = (TCHAR*)l_define_names.get();
    while (*p)
    {
      definedlist.del(p);
      TCHAR *v;
      if ((v = l_define_saves.find(p)))
        definedlist.add(p,v);
      p += _tcslen(p)+1;
    }
  }
  definedlist.del(_T("__MACRO__"));
  m_currentmacroname = oldmacroname;
  if (oldmacroname) definedlist.add(_T("__MACRO__"), oldmacroname);
  inside_comment = oldparserinsidecomment;
  --g_insertmacrorecursion;
  SCRIPT_MSG(_T("!insertmacro: end of %") NPRIs _T("\n"), macroname);
  return PS_OK;
}

int CEXEBuild::pp_tempfile(LineParser&line)
{
  TCHAR *symbol = line.gettoken_str(1);
  TCHAR *tfpath = create_tempfile_path();
  if (!tfpath)
  {
    ERROR_MSG(_T("!tempfile: Unable to create temporary file!\n"));
    return PS_ERROR;
  }
  int symexisted = definedlist.add(symbol, tfpath);
  free(tfpath);
  if (symexisted)
  {
    ERROR_MSG(_T("!tempfile: \"%") NPRIs _T("\" already defined!\n"), symbol);
    return PS_ERROR;
  }
  return PS_OK;
}

int CEXEBuild::pp_delfile(LineParser&line)
{
  UINT fatal = true, a = 1, matchcount = 0;
  const TCHAR *fc = line.gettoken_str(a);
  if (line.getnumtokens()==3)
  {
    if (!_tcsicmp(fc,_T("/nonfatal")))
      fatal = 0, fc = line.gettoken_str(++a);
    else
      PRINTHELP();
  }

  SCRIPT_MSG(_T("!delfile: \"%") NPRIs _T("\"\n"), fc);
  const TCHAR *fmt = _T("!delfile: \"%") NPRIs _T("\" couldn't be deleted.\n");

  tstring dir = get_dir_name(fc), spec = get_file_name(fc);
  tstring basedir = dir + PLATFORM_PATH_SEPARATOR_STR;
  if (dir == spec) dir = _T("."), basedir = _T(""); // no path, just file name

  boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
  dr->read(dir); // BUGBUG: PATH_CONVERT?
  dir_reader::iterator files_itr = dr->files().begin();
  for (; files_itr != dr->files().end(); files_itr++)
  {
    if (!dir_reader::matches(*files_itr, spec))
      continue;

    ++matchcount;
    tstring file = basedir + *files_itr; // BUGBUG: PATH_CONVERT?
    fc = file.c_str();
    if (-1 == _tunlink(fc))
    {
      if (fatal)
        return (ERROR_MSG(fmt, fc), PS_ERROR);
      else
        warning_fl(DW_PP_DELFILE_DELERROR, fmt, fc);
    }
    else
      SCRIPT_MSG(_T("!delfile: deleted \"%") NPRIs _T("\"\n"), fc);
  }

  if (!matchcount)
  {
    if (fatal)
      return (ERROR_MSG(fmt, fc), PS_ERROR);
    else
      warning_fl(DW_PP_DELFILE_NOMATCH, fmt, fc);
  }
  return PS_OK;
}

int CEXEBuild::pp_appendfile(LineParser&line)
{
  WORD tok = 0, cp = 0, forceEnc = false, rawnl = false;
  bool bom = false;
  TCHAR *param, buf[9+!0];
  for (;;)
  {
    param = line.gettoken_str(++tok);
    my_strncpy(buf, param, COUNTOF(buf));
    if (!_tcsicmp(param,_T("/RawNL")))
      ++rawnl;
    else if (!_tcsicmp(buf,_T("/CharSet=")))
    {
      ++forceEnc, cp = GetEncodingFromString(param+9, bom);
      if (NStreamEncoding::UNKNOWN == cp)
      {
        ERROR_MSG(_T("!appendfile: Invalid parameter \"%") NPRIs _T("\"!\n"), param);
        return PS_ERROR;
      }
    }
    else
      break;
  }
  if (line.getnumtokens() != 2 + tok)
  {
    PRINTHELP();
    return PS_ERROR;
  }
  param = line.gettoken_str(tok);
  NOStream ostrm;
  if (!ostrm.CreateFileForAppending(param, NStreamEncoding::ACP))
  {
    ERROR_MSG(_T("!appendfile: \"%") NPRIs _T("\" couldn't be opened.\n"), param);
    return PS_ERROR;
  }
  if (ostrm.IsUnicode()) bom = false;
  if (forceEnc) ostrm.StreamEncoding().SetCodepage(cp);
  const TCHAR *const text = line.gettoken_str(++tok);
  bool succ = bom ? ostrm.WriteBOM(ostrm.StreamEncoding()) : true;
  if (!succ || (rawnl ? !ostrm.WriteString(text) : !ostrm.WritePlatformNLString(text)))
  {
    ERROR_MSG(_T("!appendfile: error writing to \"%") NPRIs _T("\".\n"), param);
    return PS_ERROR;
  }
  SCRIPT_MSG(_T("!appendfile: \"%") NPRIs _T("\" \"%") NPRIs _T("\"\n"), param, text);
  return PS_OK;
}


static TinyGrowBuf* getmemfileinfo(CEXEBuild&bld, const TCHAR *id, const TCHAR **start = 0, const TCHAR **end = 0)
{
  id = bld.definedlist.find(id);
  if (!id || id[0] != _T('~') || id[1] != _T('M')) return 0;
  TinyGrowBuf *buf;
  if (strtoptr(id + 2, buf) && start)
  {
    *start = (TCHAR*) buf->get();
    *end = *start + (buf->getlen() / sizeof(TCHAR));
  }
  return buf;
}

int CEXEBuild::pp_appendmemfile(LineParser&line)
{
  bool del = line.gettoken_str(1)[0] == _T('\0');
  const TCHAR *name = line.gettoken_str(1 + del);
  TinyGrowBuf *file;

  if (line.getnumtokens() == 2) // Create file
  {
    if (!*name) return PS_ERROR;
    file = new TinyGrowBuf;
    TCHAR buf[42];
    buf[0] = _T('~'), buf[1] = _T('M');
    ptrtostr(file, buf + 2);
    definedlist.set(name, buf);
    return PS_OK;
  }

  file = getmemfileinfo(*this, name);
  if (!file) return PS_ERROR;
  if (del) // Delete file
  {
    delete file;
    definedlist.del(name);
  }
  else
  {
    const TCHAR *data = line.gettoken_str(2);
    file->add(data, GrowBuf::size_type(_tcslen(data) * sizeof(*data)));
  }
  return PS_OK;
}

enum { PPGVHF_VALID = 0x01, PPGVHF_NOERRORS = 0x02, PPGVHF_PACKED = 0x04, PPGVHF_TLB = 0x08 };
int CEXEBuild::pp_getversionhelper(const TCHAR *cmdname, const TCHAR *path, const TCHAR *basesymname, DWORD high, DWORD low, DWORD flags)
{
  TCHAR *symbuf = m_templinebuf;
  DWORD tlb = (flags & PPGVHF_TLB), noerrors = (flags & PPGVHF_NOERRORS);
  FILE *pF = tlb ? MSTLB_fopen(path) : FOPEN(path, ("rb"));
  if (pF) fclose(pF);
  bool vnum = pF && (flags & PPGVHF_VALID); // LibraryLocal users want to detect "file not found" vs "no version info"
  if (!vnum) high = low = 0;
  DWORD vals[] = { high >> 16, high & 0xffff, low >> 16, low & 0xffff }, count = 4;
  if (tlb) count = 2, vals[0] = high, vals[1] = low, vals[2] = vals[3] = 0;

  if (!pF)
  {
    if (noerrors) return PS_OK;
    ERROR_MSG(_T("%") NPRIs _T(": error reading version info from \"%") NPRIs _T("\"\n"), cmdname, path);
    return PS_ERROR;
  }

  if (flags & PPGVHF_PACKED)
  {
    SCRIPT_MSG(_T("%") NPRIs _T(": %") NPRIs _T(" (%lu.%lu)->(%") NPRIs _T("<HIGH/LOW>)\n"),
      cmdname, path, high, low, basesymname);
    _stprintf(symbuf,_T("%") NPRIs _T("HIGH"), basesymname), vnum ? definedlist.set_ui32(symbuf, high) : definedlist.set(symbuf, _T(""));
    _stprintf(symbuf,_T("%") NPRIs _T("LOW"), basesymname), vnum ? definedlist.set_ui32(symbuf, low) : definedlist.set(symbuf, _T(""));
  }
  else
  {
    SCRIPT_MSG(_T("%") NPRIs _T(": %") NPRIs _T(" (%u.%u.%u.%u)->(%") NPRIs _T("<1..%d>)\n"),
      cmdname, path, vals[0], vals[1], vals[2], vals[3], basesymname, (int) count);
    for (UINT i = 0; i < count; ++i)
    {
      _stprintf(symbuf,_T("%") NPRIs _T("%u"), basesymname, i+1);
      vnum ? definedlist.set_ui32(symbuf, vals[i]) : definedlist.set(symbuf, _T(""));
    }
  }
  return PS_OK;
}

int CEXEBuild::pp_getversion(int which_token, LineParser&line)
{
  const bool tlb = TOK_P_GETTLBVERSION == which_token;
  const TCHAR *cmdname = tlb ? _T("!gettlbversion") : _T("!getdllversion"), *path;
  DWORD ti = 1, flags = tlb ? PPGVHF_TLB : 0, low, high, prod = 0;
  for (;; ++ti)
  {
    if (!_tcsicmp(line.gettoken_str(ti), _T("/noerrors")))
      flags |= PPGVHF_NOERRORS;
    else if (!_tcsicmp(line.gettoken_str(ti), _T("/packed")))
      flags |= PPGVHF_PACKED;
    else if (!_tcsicmp(line.gettoken_str(ti), _T("/productversion")))
      ++prod;
    else
      break;
  }
  if ((tlb ? GetTLBVersion : GetDLLVersion)(path = line.gettoken_str(ti), high, low, !!prod)) flags |= PPGVHF_VALID;
  return pp_getversionhelper(cmdname, path, line.gettoken_str(ti+1), high, low, flags);
}

int CEXEBuild::pp_searchreplacestring(LineParser&line)
{
  int ignoreCase = !_tcsicmp(line.gettoken_str(1), _T("/ignorecase"));
  if (line.getnumtokens() != 5+ignoreCase) PRINTHELP()

  TCHAR *define = line.gettoken_str(1+ignoreCase);
  TCHAR *src = line.gettoken_str(2+ignoreCase);
  TCHAR *search = line.gettoken_str(3+ignoreCase);
  TCHAR *replace = line.gettoken_str(4+ignoreCase);
  int searchlen = (int)_tcslen(search), replacelen = (int)_tcslen(replace);
  if (!searchlen)
  {
    ERROR_MSG(_T("!searchreplace: search string must not be empty for search/replace!\n"));
    return PS_ERROR;
  }

  GrowBuf valout;
  while (*src)
  {
    if (ignoreCase ? _tcsnicmp(src, search, searchlen) : _tcsncmp(src, search, searchlen)) 
      valout.add(src++, sizeof(TCHAR));
    else
    {
      valout.add(replace, sizeof(TCHAR)*replacelen);
      src += searchlen;
    }
  }
  valout.add(_T(""),sizeof(TCHAR));
  
  definedlist.del(define); // allow changing variables since we'll often use this in series
  if (definedlist.add(define, (TCHAR*)valout.get()))
  {
    ERROR_MSG(_T("!searchreplace: error defining \"%") NPRIs _T("\"!\n"), define);
    return PS_ERROR;
  }
  SCRIPT_MSG(_T("!searchreplace: \"%") NPRIs _T("\"=\"%") NPRIs _T("\"\n"), define,(TCHAR*)valout.get());
  return S_OK;
}

int CEXEBuild::pp_searchparsestring(LineParser&line)
{
  bool ignCase = false, noErrors = false, isFile = false;
  int parmOffs = 1;
  while (parmOffs < line.getnumtokens())
  {
    if (!_tcsicmp(line.gettoken_str(parmOffs), _T("/ignorecase"))) { ignCase = true; parmOffs++; }
    else if (!_tcsicmp(line.gettoken_str(parmOffs), _T("/noerrors"))) { noErrors = true; parmOffs++; }
    else if (!_tcsicmp(line.gettoken_str(parmOffs), _T("/file"))) { isFile = true; parmOffs++; }
    else break;
  }
  if (parmOffs+3 > line.getnumtokens())
  {
    ERROR_MSG(_T("!searchparse: not enough parameters\n"));
    return PS_ERROR;
  }
  const TCHAR *source_string = line.gettoken_str(parmOffs++);
  DefineList *list = 0;

  if (isFile)
  {
    const TCHAR *const filename = source_string;
    NIStream filestrm;
    if (!filestrm.OpenFileForReading(filename))
    {
      ERROR_MSG(_T("!searchparse /file: error opening \"%") NPRIs _T("\"\n"), filename);
      return PS_ERROR;
    }
    UINT req_parm = (line.getnumtokens() - parmOffs)/2, fail_parm = 0, linnum = 0;
    NStreamLineReader lr(filestrm);
    GrowBuf tmpstr;
    TCHAR *str = m_templinebuf;
    for (;;)
    {
      tmpstr.resize(0);
      for (;;)
      {
        ++linnum;
        UINT cch = read_line_helper(lr, str, MAX_LINELENGTH);
        if (!cch)
        {
          if (*str)
          {
            tstring lrmsg = lr.GetErrorMessage((UINT)*str, filename, linnum);
            ERROR_MSG(_T("!searchparse: %") NPRIs, lrmsg.c_str());
            return PS_ERROR;
          }
          break; // EOF
        }
        str[--cch] = _T('\0'); // remove newline

        const bool endSlash = (cch && _T('\\') == str[cch-1]);
        if (endSlash) --cch; // don't include the slash character
        if (tmpstr.getlen() || endSlash) tmpstr.add(str,cch*sizeof(TCHAR));

        // if we have valid contents and not ending on slash, then done
        if (!endSlash && (str[0] || tmpstr.getlen())) break;
      }

      if (!str[0] && !tmpstr.getlen()) break; // reached eof

      TCHAR *thisline = str;
      if (tmpstr.getlen()) 
      {
        tmpstr.add(_T("\0"),sizeof(TCHAR));
        thisline = (TCHAR*)tmpstr.get();
      }
      UINT linefailparm;
      DefineList *tlist = searchParseString(thisline, line, parmOffs, ignCase, true, &linefailparm);
      if (linefailparm > fail_parm) fail_parm = linefailparm;
      if (tlist && tlist->getnum())
      {
        if (!list || tlist->getnum() > list->getnum())
        {
          delete list;
          list = tlist, tlist = 0;
          if ((unsigned)list->getnum() >= req_parm)
          {
            fail_parm = -1; // success
            break; // we found all the tokens, stop parsing the file
          }
        }
      }
      delete tlist;
      // parse line
    }
    if ((UINT)-1 != fail_parm && !noErrors)
    {
      const TCHAR *msgprefix = !fail_parm ? _T("starting ") : _T("");
      TCHAR *p = line.gettoken_str(parmOffs + (fail_parm*2));
      ERROR_MSG(_T("!searchparse: %") NPRIs _T("string \"%") NPRIs _T("\" not found in file!\n"), msgprefix, p ? p : _T("(null)"));
      return PS_ERROR;
    }
  }
  else
  {
    list = searchParseString(source_string, line, parmOffs, ignCase, noErrors);
    if (!list && !noErrors) return PS_ERROR;
  }

  if (list) // if we got our list, merge them defines in
  {
    for (int i=0; i < list->getnum(); i++)
    {
      TCHAR *def = list->getname(i), *val = list->getvalue(i);
      if (def && val) definedlist.set(def,val);
    }
  }
  delete list;
  return PS_OK;
}

DefineList *CEXEBuild::searchParseString(const TCHAR *source_string, LineParser&line, int parmOffs, bool ignCase, bool noErrors, UINT*failParam)
{
  const bool allowEmptyFirstTok = true;
  if (failParam) *failParam = 0;
  DefineList *ret = NULL;
  const TCHAR *defout = 0, *src_start = 0, *tok;
  int toklen = 0, maxlen;
  for (;;)
  {
    tok = line.gettoken_str(parmOffs++);
    const bool lasttoken = parmOffs > line.getnumtokens();
    if (!*tok)
      tok = 0, maxlen = -1; // No more tokens to search for, save the rest of the string
    else
    {
      toklen = (int) _tcslen(tok);
      while (*source_string && (ignCase?_tcsnicmp(source_string, tok, toklen):_tcsncmp(source_string, tok, toklen))) source_string++;
      maxlen = (int)(source_string - src_start); // Length of previous string
    }
    if (defout && defout[0]) // We now know the start and length of the previous string, add it to the list
    {
      if (!ret) ret = new DefineList();
      if (maxlen < 0)
        ret->add(defout, src_start);
      else
        ret->addn(defout, maxlen, src_start);
    }
    if (!tok && lasttoken) break;
    if (!*source_string || (allowEmptyFirstTok ? false : !tok)) // We did not find the requested token!
    {
      if (failParam) *failParam = ret ? ret->getnum() : 0;
      if (noErrors) break; // Caller is OK with a incomplete list of matched strings
      const TCHAR *msgprefix = src_start ? _T("") : _T("starting ");
      ERROR_MSG(_T("!searchparse: %") NPRIs _T("string \"%") NPRIs _T("\" not found, aborted search!\n"), msgprefix, tok ? tok : _T("(null)"));
      delete ret;
      return NULL;
    }
    defout = line.gettoken_str(parmOffs++), src_start = source_string += toklen;
  }
  return ret;
}

int CEXEBuild::pp_verbose(LineParser&line)
{
  for(int argi = 1; argi < line.getnumtokens(); ++argi)
  {
    int k = line.gettoken_enum(argi, _T("push\0pop\0")), v, convsucc;
    if (k < 0) // not push/pop, just set the level
    {
      v = line.gettoken_int(argi, &convsucc);
      if (!convsucc || v < 0 || v > 4 )
      {
        // < 2.47 would reset level to 0 without warning!
        ERROR_MSG(_T("!verbose: Invalid verbose level\n"));
        return PS_ERROR;
      }
    }
    else
    {
      if (k) // pop
      {
        int l = verbose_stack.getlen();
        if (l)
        {
          v= ((int*)verbose_stack.get())[(l/sizeof(int))-1];
          verbose_stack.resize(l-sizeof(int));
        }
        else
        {
          warning_fl(DW_PP_VERBOSE_POP_EMPTY_STACK, _T("!verbose: Pop failed, stack is empty"));
          continue; // Pop failed, should still process the next parameter
        }
      }
      else // push
      {
        v = get_verbosity();
        verbose_stack.add(&v,sizeof(int));
        continue;
      }
    }
    set_verbosity(v);
  }
  return PS_OK;
}

int CEXEBuild::pp_define(LineParser&line)
{
  const TCHAR *cmdnam = line.gettoken_str(0), *define = line.gettoken_str(1);
  GrowBuf file_buf;
  TCHAR datebuf[256], mathbuf[256], *value;
  int dupemode = 0;

  if (!_tcsicmp(define, _T("/ifndef")))
    dupemode = 1;
  else if (!_tcsicmp(define, _T("/redef")))
    dupemode = 2;

  if (dupemode != 0)
  {
    line.eattoken();
    define = line.gettoken_str(1);
  }

  if (!_tcsicmp(define, _T("/date")) || !_tcsicmp(define, _T("/utcdate")))
  {
    if (line.getnumtokens() != 4) PRINTHELPEX(cmdnam)

    const TCHAR *date_type = define;
    time_t rawtime;
    time(&rawtime);
    define = line.gettoken_str(2), value = line.gettoken_str(3);

    if (!_tcsicmp(date_type, _T("/utcdate")))
      rawtime = mktime(gmtime(&rawtime));

    datebuf[0] = 0;
    size_t s = _tcsftime(datebuf, COUNTOF(datebuf), value, localtime(&rawtime));
    if (s == 0)
      datebuf[0] = _T('\0');
    else
      datebuf[max(s, COUNTOF(datebuf)-1)] = _T('\0');

    value = datebuf;
  }
  else if (!_tcsicmp(define, _T("/file")) || !_tcsicmp(define, _T("/file_noerr")))
  {
    if (line.getnumtokens() != 4) PRINTHELPEX(cmdnam)
    const TCHAR *const filename = line.gettoken_str(3), *const swit = define;
    NIStream filestrm;
    if (!filestrm.OpenFileForReading(filename))
    {
      if (!swit[5])
      { // "/file" vs "/file_noerr"
        ERROR_MSG(_T("!define /file: file not found (\"%") NPRIs _T("\")\n"), filename);
        return PS_ERROR;
      }
    }
    else
    {
      NStreamLineReader lr(filestrm);
      TCHAR *str = m_templinebuf;
      for (UINT linnum = 0;;) {
        ++linnum;
        UINT cch = read_line_helper(lr, str, MAX_LINELENGTH);
        if (!cch) {
          if (*str) {
            tstring lrmsg = lr.GetErrorMessage((UINT)*str, filename, linnum);
            ERROR_MSG(_T("!define %") NPRIs _T(": %") NPRIs, swit, lrmsg.c_str());
            return PS_ERROR;
          }
          break; // EOF
        }
        str[--cch] = _T('\0'); // Remove \r or \n, we always append \n
        if (file_buf.getlen()) file_buf.add(_T("\n"), sizeof(TCHAR));
        file_buf.add(str, cch*sizeof(TCHAR));
      }
    }
    define = line.gettoken_str(2);
    file_buf.add(_T("\0"), sizeof(TCHAR));
    value = (TCHAR *)file_buf.get();
  }
  else if (!_tcsicmp(define, _T("/math")))
  {
    int value1, value2, tc = line.getnumtokens(), onlyval1 = 0;
    TCHAR *mathop;

    if (tc != 5 && tc != 6) badmathsyntax: PRINTHELPEX(cmdnam)
    define = line.gettoken_str(2);
    value1 = line.gettoken_int(3);
    mathop = line.gettoken_str(4);
    value2 = line.gettoken_int(5);
    value = mathbuf;

    if (!_tcscmp(mathop,_T("+"))) {
      _stprintf(value, _T("%d"), value1 + value2);
    } else if (!_tcscmp(mathop, _T("-"))) {
      _stprintf(value, _T("%d"), value1 - value2);
    } else if (!_tcscmp(mathop, _T("*"))) {
      _stprintf(value, _T("%d"), value1 * value2);
    } else if (!_tcscmp(mathop, _T("&"))) {
      _stprintf(value, _T("%d"), value1 & value2);
    } else if (!_tcscmp(mathop, _T("|"))) {
      _stprintf(value, _T("%d"), value1 | value2);
    } else if (!_tcscmp(mathop, _T("^"))) {
      _stprintf(value, _T("%d"), value1 ^ value2);
    } else if (!_tcscmp(mathop, _T("~"))) {
      _stprintf(value, _T("%d"), ~ value1), ++onlyval1;
    } else if (!_tcscmp(mathop, _T("!"))) {
      _stprintf(value, _T("%d"), ! value1), ++onlyval1;
    } else if (!_tcscmp(mathop, _T("&&"))) {
      _stprintf(value, _T("%d"), value1 && value2);
    } else if (!_tcscmp(mathop, _T("||"))) {
      _stprintf(value, _T("%d"), value1 || value2);
    } else if (!_tcscmp(mathop, _T("<<")) || !_tcscmp(mathop, _T("<<<")) ) {
      _stprintf(value, _T("%d"), value1 << value2);
    } else if (!_tcscmp(mathop, _T(">>"))) {
      _stprintf(value, _T("%d"), (signed int)value1 >> (signed int)value2);
    } else if (!_tcscmp(mathop, _T(">>>"))) {
      _stprintf(value, _T("%u"), (unsigned int)value1 >> (unsigned int)value2);
    } else if (!_tcscmp(mathop, _T("/"))) {
      if (value2 == 0)
      {
        ERROR_MSG(_T("!define /math: division by zero! (\"%i %") NPRIs _T(" %i\")\n"), value1, mathop, value2);
        return PS_ERROR;
      }
      _stprintf(value, _T("%d"), value1 / value2);
    } else if (!_tcscmp(mathop, _T("%"))) {
      if (value2 == 0)
      {
        ERROR_MSG(_T("!define /math: division by zero! (\"%i %") NPRIs _T(" %i\")\n"),value1,mathop,value2);
        return PS_ERROR;
      }
      _stprintf(value, _T("%d"), value1 % value2);
    }
    else
      goto badmathsyntax;

    if (tc + onlyval1 != 6) goto badmathsyntax;
  }
  else if (!_tcsicmp(define, _T("/intfmt")))
  {
    if (line.getnumtokens() != 5) PRINTHELPEX(cmdnam)
    define = line.gettoken_str(2);
    _stprintf(value = mathbuf, line.gettoken_str(3), line.gettoken_int(4));
  }
  else
  {
    if (line.getnumtokens() >= 4) PRINTHELPEX(cmdnam)
    value = line.gettoken_str(2);
  }

  if (dupemode == 2) definedlist.del(define);
  if (definedlist.add(define, value))
  {
    if (dupemode == 1) return PS_OK;
    ERROR_MSG(_T("!define: \"%") NPRIs _T("\" already defined!\n"), define);
    return PS_ERROR;
  }
  SCRIPT_MSG(_T("!define: \"%") NPRIs _T("\"=\"%") NPRIs _T("\"\n"), define, value);
  return PS_OK;
}

int CEXEBuild::pp_undef(LineParser&line)
{
  UINT noerr = false, stopswitch = false, handled = 0;
  for (int ti = 1; ti < line.getnumtokens(); ++ti)
  {
    const TCHAR *name = line.gettoken_str(ti);
    if (!stopswitch && !_tcsicmp(name, _T("/noerrors")))
    {
      ++noerr;
      continue;
    }
    stopswitch = ++handled;
    if (definedlist.del(name) && !noerr)
      warning_fl(DW_PP_UNDEF_UNDEFINED, _T("!undef: \"%") NPRIs _T("\" not defined!"), name);
    else
      SCRIPT_MSG(_T("!undef: \"%") NPRIs _T("\"\n"), name);
  }
  if (!handled)
  {
    PRINTHELP();
    return PS_ERROR;
  }
  return PS_OK;
}

int CEXEBuild::pp_packhdr(LineParser&line)
{
  unsigned int bufOf = false;
  TCHAR *packname = line.gettoken_str(1);
  PATH_CONVERT(packname);
  if (!strtrycpy(build_packname, packname, COUNTOF(build_packname))) ++bufOf;
  if (!strtrycpy(build_packcmd, line.gettoken_str(2), COUNTOF(build_packcmd))) ++bufOf;
  SCRIPT_MSG(_T("!packhdr: filename=\"%") NPRIs _T("\", command=\"%") NPRIs _T("\"\n"), build_packname, build_packcmd);
  return bufOf ? PS_ERROR : PS_OK;
}

template<class T> void slist_append(T&list, T&item)
{
  T prev;
  for (prev = list; prev && prev->next;)
    prev = prev->next;
  (prev ? prev->next : list) = item;
}

int CEXEBuild::pp_finalize(int which_token, LineParser&line)
{
#ifndef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (which_token == TOK_P_UNINSTFINALIZE)
  {
    ERROR_MSG(_T("Error: %") NPRIs _T(" specified, %") NPRIns _T(" not defined.\n"), line.gettoken_str(0), "NSIS_CONFIG_UNINSTALL_SUPPORT");
    return PS_ERROR;
  }
#endif
  TCHAR* cmdstr = line.gettoken_str(1);
  int validparams = false;
  postbuild_cmd *newcmd = postbuild_cmd::make(cmdstr, line.gettoken_enum(2, _T("<\0>\0<>\0=\0ignore\0")), line.gettoken_int(3, &validparams));
  if (line.getnumtokens() == 1+1)
    newcmd->cmpop = 4, validparams = true; // Just a command, ignore the exit code
  if (newcmd->cmpop == -1 || !validparams)
    PRINTHELP();
  slist_append(which_token == TOK_P_UNINSTFINALIZE ? postubuild_cmds : postbuild_cmds, newcmd);
  SCRIPT_MSG(_T("!%") NPRIns _T("finalize: \"%") NPRIs _T("\"\n"), which_token == TOK_P_UNINSTFINALIZE ? "uninst" : "", cmdstr);
  return PS_OK;
}

int CEXEBuild::pp_execute(int which_token, LineParser&line)
{
  const TCHAR *cmdname=get_commandtoken_name(which_token);
  const TCHAR *exec=line.gettoken_str(1), *define=0;
  TCHAR buf[33];
  int comp=line.gettoken_enum(2,_T("<\0>\0<>\0=\0ignore\0"));
  int validparams=true, ret=-1, cmpv=0, forceutf8=0;
  switch(line.getnumtokens()-1)
  {
  case 1: comp=4; break;
  case 2: comp=5, validparams=!!*(define=line.gettoken_str(2)); break;
  case 3: cmpv=line.gettoken_int(3,&validparams); break;
  default: forceutf8=comp=-1;
  }
  if (!validparams || comp == -1) PRINTHELP()
  tstring compile;
  if (TOK_P_MAKENSIS == which_token)
  {
    extern const TCHAR *g_argv0;
    compile=_T("\""), compile+=get_executable_path(g_argv0), compile+= _T("\"");
    compile+= _T(" ") OPT_STR _T("v"), wsprintf(buf,_T("%d"),get_verbosity()), compile+=buf;
#if defined(_WIN32) && defined(_UNICODE) // POSIX does not support -OUTPUTCHARSET
    compile+= _T(" ") OPT_STR _T("OCS UTF8"), forceutf8++; // Force UTF-8 and disable batch-file workaround in RunChildProcessRedirected
#endif
    if (*exec) compile+= _T(" "), compile+=exec;
    exec=compile.c_str();
  }
  SCRIPT_MSG(_T("%") NPRIs _T(": \"%") NPRIs _T("\"\n"),cmdname,exec);
  if (preprocessonly) PREPROCESSONLY_BEGINCOMMENT();
#ifdef _WIN32
  if (TOK_P_SYSTEMEXEC != which_token)
    ret=RunChildProcessRedirected(exec, forceutf8 ? true : false);
  else
#endif //~ _WIN32
    ret=sane_system(exec), (void)forceutf8; // forceutf8 is not used on POSIX

  if (comp == 5)
  {
    definedlist.set_si32(define,ret);
  }
  else if (!check_external_exitcode(ret,comp,cmpv))
  {
    ERROR_MSG(_T("%") NPRIs _T(": returned %d, aborting\n"),cmdname,ret);
    return PS_ERROR;
  }
  if (preprocessonly) PREPROCESSONLY_ENDCOMMENT();
  SCRIPT_MSG(_T("%") NPRIs _T(": returned %d\n"),cmdname,ret);
  return PS_OK;
}

int CEXEBuild::pp_addincludedir(LineParser&line)
{
  TCHAR *f = line.gettoken_str(1);
  PATH_CONVERT(f);
  include_dirs.add(f, 0);
  return PS_OK;
}

static const TCHAR *g_incerrfmtstr = _T("!include: error in script: \"%") NPRIs _T("\" on line %d\n");
static const TCHAR *g_incerrlvlfmtstr = _T("!include: too many levels of includes (%d max).\n");

int CEXEBuild::includeScript(const TCHAR *file, NStreamEncoding&enc)
{
  NIStream incstrm;
  const bool openok = incstrm.OpenFileForReading(file, enc);
  if (NStreamEncoding::AUTO == enc.GetCodepage()
   && build_unicode && !enc.IsUnicodeCodepage(enc.GetPlatformDefaultCodepage())
   && enc.GetPlatformDefaultCodepage() == incstrm.StreamEncoding().GetCodepage() )
  {
    incstrm.StreamEncoding().SetCodepage(NStreamEncoding::UTF8); // !include defaults to UTF-8 after "Unicode true" 
  }
  enc = incstrm.StreamEncoding();

  TCHAR bufcpdisp[20];
  incstrm.StreamEncoding().GetCPDisplayName(bufcpdisp);
  SCRIPT_MSG(_T("!include: \"%") NPRIs _T("\" (%") NPRIs _T(")\n"), file, bufcpdisp);
  if (!openok)
  {
    ERROR_MSG(_T("!include: could not open file: \"%") NPRIs _T("\"\n"), file);
    return PS_ERROR;
  }

  if (build_include_depth >= MAX_INCLUDEDEPTH)
  {
    ERROR_MSG(g_incerrlvlfmtstr, MAX_INCLUDEDEPTH);
    return PS_ERROR;
  }
  build_include_depth++;

  const int last_linecnt = linecnt;
  const TCHAR *last_filename = curfilename;
  curfilename = file, linecnt = 0;
  NStreamLineReader linereader(incstrm);
  NStreamLineReader *last_linereader = curlinereader;
  curlinereader = &linereader;
#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  TCHAR *oldfilename = set_file_predefine(curfilename);
  TCHAR *oldtimestamp = set_timestamp_predefine(curfilename);
#endif

  int r = parseScript();
  const int errline = linecnt;

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  restore_file_predefine(oldfilename);
  restore_timestamp_predefine(oldtimestamp);
#endif
  curfilename = last_filename, linecnt = last_linecnt;
  curlinereader = last_linereader;

  build_include_depth--;
  if (r != PS_EOF && r != PS_OK)
  {
    ERROR_MSG(g_incerrfmtstr, file, errline);
    return PS_ERROR;
  }
  SCRIPT_MSG(_T("!include: closed: \"%") NPRIs _T("\"\n"), file);
  return PS_OK;
}

int CEXEBuild::includeScriptLines(const TCHAR *start, const TCHAR *end, const TCHAR *name)
{
  if (build_include_depth >= MAX_INCLUDEDEPTH)
  {
    ERROR_MSG(g_incerrlvlfmtstr, MAX_INCLUDEDEPTH);
    return PS_ERROR;
  }

  build_include_depth++;
  tstring buf;
  int ret = PS_OK;
  bool unicode = sizeof(*start) > sizeof(char);
  for (unsigned int ln = 0; start < end && ret == PS_OK;)
  {
    const TCHAR *line = start, eot = 4;
    size_t i, nl;
    for (i = 0, nl = 0;; ++i)
    {
      nl = &line[i] < end ? NStream::IsNewline(line[i], unicode) : eot;
      if (nl || !line[i]) break;
    }
    if (nl)
    {
      buf.assign(line, i);
      line = buf.c_str();
    }
    if (*line) ret = process_oneline(line, name, ++ln, PLF_VIRTUALFILE);
    if (ret != PS_OK) ERROR_MSG(g_incerrfmtstr, name, ln);
    start += i + 1;
  }
  build_include_depth--;
  return ret;
}

int CEXEBuild::pp_include(LineParser&line)
{
  bool required = true, memfile = false, done = false;
  NStreamEncoding enc(NStreamEncoding::AUTO);
  const TCHAR *f;
  unsigned int toks = line.getnumtokens() - 1, included = 0;
  for(unsigned int tok = 0; toks;)
  {
    f = line.gettoken_str(++tok);
    if (tok >= toks) break;
    if (!_tcsicmp(f,_T("/memfile"))) memfile = true;
    if (!_tcsicmp(f,_T("/nonfatal"))) required = false;
    TCHAR buf[9+1];
    my_strncpy(buf, f, COUNTOF(buf));
    if (!_tcsicmp(buf,_T("/charset=")))
    {
      WORD cp = GetEncodingFromString(f+9);
      if (NStreamEncoding::UNKNOWN == cp) toks = 0;
      enc.SafeSetCodepage(cp);
    }
  }
  if (!toks || !*f) PRINTHELP();

  if (memfile)
  {
    const TCHAR *s, *e, *n = get_memorycode_filename();
    if (getmemfileinfo(*this, f, &s, &e))
    {
      return includeScriptLines(s, e, n);
    }
    done = true, f = n;
  }

  const TCHAR *fc = my_convert(f);
  tstring dir = get_dir_name(fc), spec = get_file_name(fc), basedir = dir;
  my_convert_free(const_cast<TCHAR*>(fc));
  path_append_separator(basedir);
  if (dir == spec) basedir = _T(""), dir = _T("."); // no path, just file name

  // search working directory
  boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
  dr->read(dir);
  for (dir_reader::iterator files_itr = dr->files().begin();
       !done && files_itr != dr->files().end();
       files_itr++)
  {
    if (!dir_reader::matches(*files_itr, spec)) continue;

    tstring incfile = basedir + *files_itr;
    if (includeScript(incfile.c_str(), enc) != PS_OK)
      return PS_ERROR;
    else
      included++;
  }
  if (included) return PS_OK;

  // search include dirs
  TCHAR *incdir = include_dirs.get();
  int incdirs = include_dirs.getnum();
  for (int i = 0; !done && i < incdirs; i++, incdir += _tcslen(incdir) + 1)
  {
    tstring curincdir(incdir), incfile;
    if (_T(".") != dir) path_append(curincdir, dir);

    boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
    dr->read(curincdir);
    for (dir_reader::iterator incdir_itr = dr->files().begin();
         incdir_itr != dr->files().end();
         incdir_itr++)
    {
      if (!dir_reader::matches(*incdir_itr, spec)) continue;

      path_append(incfile = curincdir, *incdir_itr);
      if (includeScript(incfile.c_str(), enc) != PS_OK)
        return PS_ERROR;
      else
        included++;
    }
    if (included) return PS_OK;
  }

  if (!included) // nothing found?
  {
    if (required)
    {
      ERROR_MSG(_T("!include: could not find: \"%") NPRIs _T("\"\n"), f);
      return PS_ERROR;
    }
    else
      warning_fl(DW_INCLUDE_NONFATAL_NOT_FOUND, _T("!include: could not find: \"%") NPRIs _T("\""), f);
  }
  return PS_OK;
}

int CEXEBuild::pp_cd(LineParser&line)
{
  const TCHAR *dir = line.gettoken_str(1);
  if (!dir[0] || _tchdir(dir))
  {
    ERROR_MSG(_T("!cd: error changing to: \"%") NPRIs _T("\"\n"), dir);
    return PS_ERROR;
  }
  return PS_OK;
}

int CEXEBuild::pp_boolifyexpression(LineParser&line, int &result, bool allow_logicneg, int ignore_last_tokens)
{
  const TCHAR *cmdnam = line.gettoken_str(0); // Must save name now before eattoken!
  int istrue = 0, mod = 0, logicneg = 0;

  if (allow_logicneg && !_tcscmp(line.gettoken_str(1),_T("!")))
    logicneg++, line.eattoken();

  int numtokens = line.getnumtokens() - ignore_last_tokens;
  if (numtokens == 2)
  {
    istrue = line.gettoken_number(1) || line.gettoken_int(1);
  }
  else if (numtokens == 3)
  {
    if (!_tcsicmp(line.gettoken_str(1),_T("/fileexists")))
    {
      TCHAR *fc = my_convert(line.gettoken_str(2));
      tstring dir = get_dir_name(fc), spec = get_file_name(fc);
      my_convert_free(fc);
      if (dir == spec) dir = _T("."); 
  
      boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
      dr->hack_simpleexcluded().erase(_T("."));
      dr->read(dir);
  
      for (dir_reader::iterator fit = dr->files().begin();
         fit != dr->files().end() && !istrue; fit++)
      {
        if (dir_reader::matches(*fit, spec)) istrue = true;
      }
      if (!istrue) for (dir_reader::iterator dit = dr->dirs().begin();
         dit != dr->dirs().end() && !istrue; dit++)
      {
        if (dir_reader::matches(*dit, spec)) istrue = true;
      }
    }
    else 
      PRINTHELPEX(cmdnam)
  }
  else if (numtokens == 4) 
  {
    int cnv1 = 1, cnv2 = 1;
    mod = line.gettoken_enum(2,_T("==\0!=\0S==\0S!=\0=\0<>\0<=\0<\0>\0>=\0&\0&&\0|\0||\0"));
    switch(mod) 
    {
    case 0:
      istrue = _tcsicmp(line.gettoken_str(1),line.gettoken_str(3)) == 0; break;
    case 1:
      istrue = _tcsicmp(line.gettoken_str(1),line.gettoken_str(3)) != 0; break;
    case 2:
      istrue = _tcscmp(line.gettoken_str(1),line.gettoken_str(3)) == 0; break;
    case 3:
      istrue = _tcscmp(line.gettoken_str(1),line.gettoken_str(3)) != 0; break;
    case 4:
       istrue = line.gettoken_number(1,&cnv1) == line.gettoken_number(3,&cnv2); break;
    case 5:
       istrue = line.gettoken_number(1,&cnv1) != line.gettoken_number(3,&cnv2); break;
    case 6:
      istrue = line.gettoken_number(1,&cnv1) <= line.gettoken_number(3,&cnv2); break;
    case 7:
      istrue = line.gettoken_number(1,&cnv1) <  line.gettoken_number(3,&cnv2); break;
    case 8:
      istrue = line.gettoken_number(1,&cnv1) >  line.gettoken_number(3,&cnv2); break;
    case 9:
      istrue = line.gettoken_number(1,&cnv1) >= line.gettoken_number(3,&cnv2); break;
    case 10:
      istrue = (line.gettoken_int(1,&cnv1) & line.gettoken_int(3,&cnv2)) != 0; break;
    case 11:
      istrue = line.gettoken_int(1,&cnv1) && line.gettoken_int(3,&cnv2); break;
    case 12:
    case 13:
      istrue = line.gettoken_int(1,&cnv1) || line.gettoken_int(3,&cnv2); break;
    default:
      PRINTHELPEX(cmdnam)
    }
    if (!cnv1 || !cnv2) 
      warning_fl(DW_PARSE_BADNUMBER, _T("Invalid number: \"%") NPRIs _T("\""), line.gettoken_str(!cnv1 ? 1 : 3));
  }
  else
  {
    PRINTHELPEX(cmdnam)
  }

  result = logicneg ? !istrue : istrue;
  return PS_OK;
}

int CEXEBuild::pp_assert(LineParser&line)
{
  const TCHAR *message = line.gettoken_str(line.getnumtokens() - 1);
  int istrue, ec = pp_boolifyexpression(line, istrue, false, 1);

  if (ec != PS_ERROR && !istrue)
  {
    tstring buf = _T("");
    if (!*message)
    {
      for (int i = 1, c = line.getnumtokens() - 1, any = 0; i < c; ++i)
        buf += (_T(" ") + !(any++)), buf += line.gettoken_str(i);
      message = buf.c_str();
    }
    ERROR_MSG(_T("%") NPRIs _T(": %") NPRIs _T("\n"), get_commandtoken_name(TOK_P_ASSERT), message);
    return PS_ERROR;
  }
  return ec;
}
