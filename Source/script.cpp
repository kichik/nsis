/*
 * script.cpp
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
 *
 * Unicode support by Jim Park -- 08/09/2007
 */

#include "Platform.h"
#include <stdio.h>
#include <ctype.h>
#include "tokens.h"
#include "build.h"
#include "util.h"
#include "winchar.h"
#include "ResourceEditor.h"
#include "DialogTemplate.h"
#include "lang.h"
#include "dirreader.h"
#include <nsis-version.h>
#include "icon.h"
#include "exehead/api.h"
#include "exehead/resource.h"
#include <cassert> // for assert(3)
#include <time.h>
#include "tstring.h"
#include "utf.h"
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
#define MAX_LINELENGTH 16384

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
// Added by Sunil Kamath 11 June 2003
TCHAR *CEXEBuild::set_file_predefine(const TCHAR *filename)
{
  TCHAR *oldfileinfo = NULL;
  TCHAR *oldfilename = definedlist.find(_T("__FILE__"));
  TCHAR *oldfiledir = definedlist.find(_T("__FILEDIR__"));
  if(oldfilename && oldfiledir)
  {
    oldfileinfo = new TCHAR[_tcslen(oldfilename)+_tcslen(oldfiledir)+2];
    _tcscpy(oldfileinfo, oldfilename);
    _tcscat(oldfileinfo, _T("|"));
    _tcscat(oldfileinfo, oldfiledir);
    definedlist.del(_T("__FILE__"));
    definedlist.del(_T("__FILEDIR__"));
  }
  const TCHAR *p = _tcsrchr(filename,_T('\\'));
  if(p) {
    p++;
  }
  else {
    p = filename;
  }
  definedlist.add(_T("__FILE__"),p);
  TCHAR dir[MAX_PATH];
#ifdef _WIN32
  LPTSTR lpFilePart;
  GetFullPathName(filename, COUNTOF(dir), dir, &lpFilePart);
  PathRemoveFileSpec(dir);
#else
  if (p == filename)
    _tcscpy(dir, _T("."));
  else
    _tcsncpy(dir, filename, p-filename-1);
#endif
  definedlist.add(_T("__FILEDIR__"),dir);

  return oldfileinfo;
}

void CEXEBuild::restore_file_predefine(TCHAR *oldfilename)
{
  definedlist.del(_T("__FILEDIR__"));
  definedlist.del(_T("__FILE__"));
  if(oldfilename) {
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
  if(oldtimestamp) {
    oldtimestamp = _tcsdup(oldtimestamp);
    definedlist.del(_T("__TIMESTAMP__"));
  }

#ifdef _WIN32
  TCHAR timestampbuf[256] = _T("");
  TCHAR datebuf[128] = _T("");
  TCHAR timebuf[128] = _T("");
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
    wsprintf(timestampbuf,_T("%s %s"),datebuf,timebuf);

    definedlist.add(_T("__TIMESTAMP__"),timestampbuf);
  }
#else
  struct stat st;
  if (!stat(filename, &st))
    definedlist.add(_T("__TIMESTAMP__"),_tctime(&st.st_mtime));
#endif

  return oldtimestamp;
}

void CEXEBuild::restore_timestamp_predefine(TCHAR *oldtimestamp)
{
  definedlist.del(_T("__TIMESTAMP__"));
  if(oldtimestamp) {
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
  if(oldline) {
    oldline = _tcsdup(oldline);
    definedlist.del(_T("__LINE__"));
  }
  if(is_macro && oldline) {
    linebuf = (TCHAR *)malloc((_tcslen(oldline)+_tcslen(temp)+2)*sizeof(TCHAR));
    _stprintf(linebuf,_T("%s.%s"),oldline,temp);
  }
  else {
    linebuf = _tcsdup(temp);
  }
  definedlist.add(_T("__LINE__"),linebuf);

  return oldline;
}

void CEXEBuild::restore_line_predefine(TCHAR *oldline)
{
  definedlist.del(_T("__LINE__"));
  if(oldline) {
      definedlist.add(_T("__LINE__"),oldline);
      free(oldline);
  }
}

void CEXEBuild::set_date_time_predefines()
{
  time_t etime;
  struct tm * ltime;
  TCHAR datebuf[128];
  TCHAR timebuf[128];

  time(&etime);
  ltime = localtime(&etime);
#ifdef _WIN32
  SYSTEMTIME stime;
  stime.wYear = ltime->tm_year+1900;
  stime.wMonth = ltime->tm_mon + 1;
  stime.wDay = ltime->tm_mday;
  stime.wHour= ltime->tm_hour;
  stime.wMinute= ltime->tm_min;
  stime.wSecond= ltime->tm_sec;
  stime.wMilliseconds= 0;
  GetDateFormat(LOCALE_USER_DEFAULT, DATE_SHORTDATE, &stime, NULL, datebuf, sizeof(datebuf));
  definedlist.add(_T("__DATE__"),(TCHAR *)datebuf);
  GetTimeFormat(LOCALE_USER_DEFAULT, 0, &stime, NULL, timebuf, sizeof(timebuf));
  definedlist.add(_T("__TIME__"),(TCHAR *)timebuf);
#else
  my_strftime(datebuf, sizeof(datebuf), _T("%x"), ltime);
  definedlist.add(_T("__DATE__"),(TCHAR *)datebuf);
  my_strftime(timebuf, sizeof(timebuf), _T("%X"), ltime);
  definedlist.add(_T("__TIME__"),(TCHAR *)timebuf);
#endif
}

void CEXEBuild::del_date_time_predefines()
{
  definedlist.del(_T("__DATE__"));
  definedlist.del(_T("__TIME__"));
}
#endif

int CEXEBuild::process_script(FILE *filepointer, const TCHAR *filename, BOOL unicode)
{
  linecnt = 0;
  fp = filepointer;
  curfilename = filename;
  curfile_unicode = unicode;

  if (has_called_write_output)
  {
    ERROR_MSG(_T("Error (process_script): write_output already called, can't continue\n"));
    return PS_ERROR;
  }

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  set_date_time_predefines();
  TCHAR *oldfilename = set_file_predefine(curfilename);
  TCHAR *oldtimestamp = set_timestamp_predefine(curfilename);
#endif

  int ret=parseScript();

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  restore_file_predefine(oldfilename);
  restore_timestamp_predefine(oldtimestamp);
  del_date_time_predefines();
#endif

  fp = 0;
  curfilename = 0;
  curfile_unicode = FALSE;

  if (m_linebuild.getlen())
  {
    ERROR_MSG(_T("Error: invalid script: last line ended with \\\n"));
    return PS_ERROR;
  }

  if (ret == PS_EOF && num_ifblock())
  {
    ERROR_MSG(_T("!if[macro][n]def: open at EOF - need !endif\n"));
    return PS_ERROR;
  }

  return ret;
}

#define PRINTHELP() { print_help(line.gettoken_str(0)); return PS_ERROR; }

void CEXEBuild::start_ifblock()
{
  ifblock ib = {0, };
  if (cur_ifblock)
    ib.inherited_ignore = cur_ifblock->ignore || cur_ifblock->inherited_ignore;
  int num = build_preprocessor_data.getlen() / sizeof(ifblock);
  build_preprocessor_data.add(&ib, sizeof(ifblock));
  cur_ifblock = (ifblock *) build_preprocessor_data.get() + num;
}

void CEXEBuild::end_ifblock()
{
  if (build_preprocessor_data.getlen())
  {
    cur_ifblock--;
    build_preprocessor_data.resize(build_preprocessor_data.getlen() - sizeof(ifblock));
    if (!build_preprocessor_data.getlen())
      cur_ifblock = 0;
  }
}

int CEXEBuild::num_ifblock()
{
  return build_preprocessor_data.getlen() / sizeof(ifblock);
}

// Func size: just under 200 lines (orip)
int CEXEBuild::doParse(const TCHAR *str)
{
  LineParser line(inside_comment);
  int res;

  while (*str == _T(' ') || *str == _T('\t')) str++;

  // remove trailing slash and null, if there's a previous line
  if (m_linebuild.getlen()>1)
    m_linebuild.resize(m_linebuild.getlen()-(2*sizeof(TCHAR)));

  // warn of comment with line-continuation
  if (m_linebuild.getlen())
  {
    LineParser prevline(inside_comment);
    prevline.parse((TCHAR*)m_linebuild.get());
    LineParser thisline(inside_comment);
    thisline.parse((TCHAR*)str);

    if (prevline.inComment() && !thisline.inComment())
    {
      warning_fl(_T("comment contains line-continuation character, following line will be ignored"));
    }
  }

  // add new line to line buffer
  m_linebuild.add(str,(_tcslen(str)+1)*sizeof(TCHAR));

  // keep waiting for more lines, if this line ends with a backslash
  if (str[0] && CharPrev(str,str+_tcslen(str))[0] == _T('\\'))
  {
    return PS_OK;
  }

  // parse before checking if the line should be ignored, so block comments won't be missed
  
  // escaped quotes should be ignored for compile time commands that set defines
  // because defines can be inserted in commands at a later stage
  bool ignore_escaping = (!_tcsnicmp((TCHAR*)m_linebuild.get(),_T("!define"),7) || !_tcsncicmp((TCHAR*)m_linebuild.get(),_T("!insertmacro"),12));
  res=line.parse((TCHAR*)m_linebuild.get(), ignore_escaping);

  inside_comment = line.inCommentBlock();

  // if ignoring, ignore all lines that don't begin with an exclamation mark
  {
    bool ignore_line = cur_ifblock && (cur_ifblock->ignore || cur_ifblock->inherited_ignore);
    TCHAR first_char = *(TCHAR *) m_linebuild.get();
    if (ignore_line && (first_char!=_T('!') || !is_valid_token(line.gettoken_str(0))))
    {
      m_linebuild.resize(0);
      return PS_OK;
    }
  }

  m_linebuild.resize(0);

  if (res)
  {
    if (res==-2) ERROR_MSG(_T("Error: unterminated string parsing line at %s:%d\n"),curfilename,linecnt);
    else ERROR_MSG(_T("Error: error parsing line (%s:%d)\n"),curfilename,linecnt);
    return PS_ERROR;
  }

parse_again:
  if (line.getnumtokens() < 1) return PS_OK;

  int np,op,pos;
  int tkid=get_commandtoken(line.gettoken_str(0),&np,&op,&pos);
  if (tkid == -1)
  {
    TCHAR *p=line.gettoken_str(0);
    if (p[0] && p[_tcslen(p)-1]==_T(':'))
    {
      if (p[0] == _T('!') || (p[0] >= _T('0') && p[0] <= _T('9')) || p[0] == _T('$') || p[0] == _T('-') || p[0] == _T('+'))
      {
        ERROR_MSG(_T("Invalid label: %s (labels cannot begin with !, $, -, +, or 0-9)\n"),line.gettoken_str(0));
        return PS_ERROR;
      }
      if (add_label(line.gettoken_str(0))) return PS_ERROR;
      line.eattoken();
      goto parse_again;
    }

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    // Added by Ximon Eighteen 5th August 2002
    // We didn't recognise this command, could it be the name of a
    // function exported from a dll?
    if (m_plugins.IsPluginCommand(line.gettoken_str(0)))
    {
      np   = 0;   // parameters are optional
      op   = -1;  // unlimited number of optional parameters
      pos  = -1;  // placement will tested later
      tkid = TOK__PLUGINCOMMAND;
    }
    else
#endif
    {
      ERROR_MSG(_T("Invalid command: %s\n"),line.gettoken_str(0));
      return PS_ERROR;
    }
  }

  if (IsTokenPlacedRight(pos, line.gettoken_str(0)) != PS_OK)
    return PS_ERROR;

  int v=line.getnumtokens()-(np+1);
  if (v < 0 || (op >= 0 && v > op)) // opt_parms is -1 for unlimited
  {
    ERROR_MSG(_T("%s expects %d"),line.gettoken_str(0),np);
    if (op < 0) ERROR_MSG(_T("+"));
    if (op > 0) ERROR_MSG(_T("-%d"),op+np);
    ERROR_MSG(_T(" parameters, got %d.\n"),line.getnumtokens()-1);
    PRINTHELP()
  }

  int if_from_else = 0;

  if (tkid == TOK_P_ELSE)
  {
    if (cur_ifblock && cur_ifblock->inherited_ignore)
      return PS_OK;

    if (!num_ifblock())
    {
      ERROR_MSG(_T("!else: no if block open (!if[macro][n][def])\n"));
      return PS_ERROR;
    }
    
    if (cur_ifblock->elseused)
    {
      ERROR_MSG(_T("!else: else already used in current if block\n"));
      return PS_ERROR;
    }

    if (cur_ifblock->hasexeced)
    {
      cur_ifblock->ignore++;
      return PS_OK;
    }

    if (line.getnumtokens() == 1)
    {
      cur_ifblock->ignore = !cur_ifblock->ignore;
      // if not executed up until now, it will now
      cur_ifblock->hasexeced++;
      cur_ifblock->elseused++;
      return PS_OK;
    }

    line.eattoken();

    int v=line.gettoken_enum(0,_T("if\0ifdef\0ifndef\0ifmacrodef\0ifmacrondef\0"));
    if (v < 0) PRINTHELP()
    if (line.getnumtokens() == 1) PRINTHELP()
    int cmds[] = {TOK_P_IF, TOK_P_IFDEF, TOK_P_IFNDEF, TOK_P_IFMACRODEF, TOK_P_IFMACRONDEF};
    tkid = cmds[v];
    if_from_else++;
  }

  if (tkid == TOK_P_IFNDEF || tkid == TOK_P_IFDEF ||
      tkid == TOK_P_IFMACRODEF || tkid == TOK_P_IFMACRONDEF ||
      tkid == TOK_P_IF)
  {
    if (!if_from_else)
      start_ifblock();

    if (cur_ifblock && cur_ifblock->inherited_ignore)
    {
      return PS_OK;
    }

    int istrue=0;

    int mod=0;
  
    int p=0;
    
    if (tkid == TOK_P_IF) {
      if(!_tcscmp(line.gettoken_str(1),_T("!"))) {
        p = 1;
        line.eattoken();
      }
      
      if(line.getnumtokens() == 2)
        istrue = line.gettoken_int(1);

      else if (line.getnumtokens() == 3) {
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
        else PRINTHELP()
      }

      else if (line.getnumtokens() == 4) {
        mod = line.gettoken_enum(2,_T("=\0==\0!=\0<=\0<\0>\0>=\0&\0&&\0|\0||\0"));
        
        switch(mod) {
          case 0:
          case 1:
            istrue = _tcsicmp(line.gettoken_str(1),line.gettoken_str(3)) == 0; break;
          case 2:
            istrue = _tcsicmp(line.gettoken_str(1),line.gettoken_str(3)) != 0; break;
          case 3:
            istrue = line.gettoken_float(1) <= line.gettoken_float(3); break;
          case 4:
            istrue = line.gettoken_float(1) <  line.gettoken_float(3); break;
          case 5:
            istrue = line.gettoken_float(1) >  line.gettoken_float(3); break;
          case 6:
            istrue = line.gettoken_float(1) >= line.gettoken_float(3); break;
          case 7:
            istrue = (line.gettoken_int(1) & line.gettoken_int(3)) != 0; break;
          case 8:
            istrue = line.gettoken_int(1) && line.gettoken_int(3); break;
          case 9:
          case 10:
            istrue = line.gettoken_int(1) || line.gettoken_int(3); break;
          default:
            PRINTHELP()
        }
      }
      else PRINTHELP()
        
      if(p) istrue = !istrue;
    }

    else {
  
      // pure left to right precedence. Not too powerful, but useful.
      for (p = 1; p < line.getnumtokens(); p ++)
      {
        if (p & 1)
        {
          int new_s;
          if (tkid == TOK_P_IFNDEF || tkid == TOK_P_IFDEF)
            new_s=!!definedlist.find(line.gettoken_str(p));
          else
            new_s=MacroExists(line.gettoken_str(p));
          if (tkid == TOK_P_IFNDEF || tkid == TOK_P_IFMACRONDEF)
            new_s=!new_s;
  
          if (mod == 0) istrue = istrue || new_s;
          else istrue = istrue && new_s;
        }
        else
        {
          mod=line.gettoken_enum(p,_T("|\0&\0||\0&&\0"));
          if (mod == -1) PRINTHELP()
          mod &= 1;
        }
      }
    }

    if (istrue)
    {
      cur_ifblock->hasexeced++;
      cur_ifblock->ignore = 0;
    }
    else
      cur_ifblock->ignore++;

    return PS_OK;
  }
  if (tkid == TOK_P_ENDIF) {
    if (!num_ifblock())
    {
      ERROR_MSG(_T("!endif: no if block open (!if[macro][n][def])\n"));
      return PS_ERROR;
    }
    end_ifblock();
    return PS_OK;
  }
  if (!cur_ifblock || (!cur_ifblock->ignore && !cur_ifblock->inherited_ignore))
  {
    return doCommand(tkid,line);
  }

  return PS_OK;
}

// Func size: about 140 lines (orip)
#ifdef NSIS_FIX_DEFINES_IN_STRINGS
void CEXEBuild::ps_addtoline(const TCHAR *str, GrowBuf &linedata, StringList &hist, bool bIgnoreDefines /*= false*/)
#else
void CEXEBuild::ps_addtoline(const TCHAR *str, GrowBuf &linedata, StringList &hist)
#endif
{
  // convert $\r, $\n to their literals
  // preprocessor replace ${VAR} and $%VAR% with whatever value
  // note that if VAR does not exist, ${VAR} or $%VAR% will go through unmodified
  const TCHAR *in=str;
  while (*in)
  {
    int add=1;
    TCHAR *t;
    TCHAR c=*in;
    t=CharNext(in);

    if (t-in > 1) // handle multibyte chars (no escape)
    {
      linedata.add((void*)in,(t-in)*sizeof(TCHAR));
      in=t;
      continue;
    }
    in=t;

    if (c == _T('$'))
    {
      if (in[0] == _T('\\'))
      {
        if (in[1] == _T('r'))
        {
          in+=2;
          c=_T('\r');
        }
        else if (in[1] == _T('n'))
        {
          in+=2;
          c=_T('\n');
        }
        else if (in[1] == _T('t'))
        {
          in+=2;
          c=_T('\t');
        }
      }
      else if (in[0] == _T('{'))
      {
        TCHAR *s=_tcsdup(in+1);
        MANAGE_WITH(s, free);
        TCHAR *t=s;
        unsigned int bn = 0;
        while (*t)
        {
          if (*t == _T('{')) bn++;
          if (*t == _T('}') && bn-- == 0) break;
          t=CharNext(t);
        }
        if (*t && t!=s
#ifdef NSIS_FIX_DEFINES_IN_STRINGS
          && !bIgnoreDefines
#endif
          )
        {
          *t=0;
          // check for defines inside the define name - ${bla${blo}}
          GrowBuf defname;
          ps_addtoline(s,defname,hist);
          defname.add(_T(""),sizeof(_T("")));
          t=definedlist.find((TCHAR*)defname.get());
          if (t && hist.find((TCHAR*)defname.get(),0)<0)
          {
            in+=_tcslen(s)+2;
            add=0;
            hist.add((TCHAR*)defname.get(),0);
#ifdef NSIS_FIX_DEFINES_IN_STRINGS
            ps_addtoline(t,linedata,hist,true);
#else
            ps_addtoline(t,linedata,hist);
#endif
            hist.delbypos(hist.find((TCHAR*)defname.get(),0));
          }
        }
      }
      else if (in[0] == _T('%'))
      {
        TCHAR *s=_tcsdup(in+1);
        MANAGE_WITH(s, free);
        TCHAR *t=s;
        while (*t)
        {
          if (*t == _T('%')) break;
          t=CharNext(t);
        }
        if (*t && t!=s)
        {
          *t=0;
          // check for defines inside the define name - ${bla${blo}}
          GrowBuf defname;
          ps_addtoline(s,defname,hist);
          defname.add(_T(""),sizeof(_T("")));
          t=_tgetenv((TCHAR*)defname.get());
          if (t && hist.find((TCHAR*)defname.get(),0)<0)
          {
            in+=_tcslen(s)+2;
            add=0;
            hist.add((TCHAR*)defname.get(),0);
#ifdef NSIS_FIX_DEFINES_IN_STRINGS
            ps_addtoline(t,linedata,hist,true);
#else
            ps_addtoline(t,linedata,hist);
#endif
            hist.delbypos(hist.find((TCHAR*)defname.get(),0));
          }
        }
      }
#ifdef NSIS_FIX_DEFINES_IN_STRINGS
      else if (in[0] == _T('$'))
      {
        if (in[1] == _T('{')) // Found $$ before - Don't replace this define
        {
          TCHAR *s=_tcsdup(in+2);
          MANAGE_WITH(s, free);
          TCHAR *t=s;
          unsigned int bn = 0;
          while (*t)
          {
            if (*t == _T('{')) bn++;
            if (*t == _T('}') && bn-- == 0) break;
            t=CharNext(t);
          }
          if (*t && t!=s)
          {
            *t=0;
            // add text unchanged
            GrowBuf defname;
            ps_addtoline(s,defname,hist);
            in++;
          }
        }
        else
        {
          linedata.add((void*)&c,1*sizeof(TCHAR));
          in++;
        }
      }
#endif
    }
    if (add) linedata.add((void*)&c,1*sizeof(TCHAR));
  }
}

int CEXEBuild::parseScript()
{
  TCHAR str[MAX_LINELENGTH];

  for (;;)
  {
    TCHAR *p=str;
    *p=0;
    _fgetts(str,MAX_LINELENGTH,fp);
    linecnt++;
    if (feof(fp)&&!str[0]) break;

    // remove trailing whitespace
    while (*p) p++;
    if (p > str) p--;
    while (p >= str && (*p == _T('\r') || *p == _T('\n') || *p == _T(' ') || *p == _T('\t'))) p--;
    *++p=0;

    StringList hist;
    GrowBuf linedata;

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
    TCHAR *oldline = set_line_predefine(linecnt, FALSE);
#endif

    ps_addtoline(str,linedata,hist);
    linedata.add(_T(""),sizeof(_T("")));
    int ret=doParse((TCHAR*)linedata.get());

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
    // Added by Sunil Kamath 11 June 2003
    restore_line_predefine(oldline);
#endif

    if (ret != PS_OK) return ret;
  }

  return PS_EOF;
}

int CEXEBuild::includeScript(TCHAR *f)
{
  SCRIPT_MSG(_T("!include: \"%s\"\n"),f);
  BOOL unicode;
  FILE *incfp=FOPENTEXT2(f,"rt",&unicode);
  if (!incfp)
  {
    ERROR_MSG(_T("!include: could not open file: \"%s\"\n"),f);
    return PS_ERROR;
  }

  // auto-fclose(3) incfp
  MANAGE_WITH(incfp, fclose);

  if (build_include_depth >= MAX_INCLUDEDEPTH)
  {
    ERROR_MSG(_T("parseScript: too many levels of includes (%d max).\n"),MAX_INCLUDEDEPTH);
    return PS_ERROR;
  }
  build_include_depth++;
#ifndef _UNICODE
  const bool org_build_include_isutf8 = build_include_isutf8;
  build_include_isutf8 = IsUTF8BOM(incfp);
#endif

  int last_linecnt=linecnt;
  linecnt=0;
  const TCHAR *last_filename=curfilename;
  BOOL last_unicode=curfile_unicode;
  curfilename=f;
  curfile_unicode=unicode;
  FILE *last_fp=fp;
  fp=incfp;

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  TCHAR *oldfilename = set_file_predefine(curfilename);
  TCHAR *oldtimestamp = set_timestamp_predefine(curfilename);
#endif

  int r=parseScript();

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  restore_file_predefine(oldfilename);
  restore_timestamp_predefine(oldtimestamp);
#endif

#ifndef _UNICODE
  build_include_isutf8 = org_build_include_isutf8;
#endif

  int errlinecnt=linecnt;

  linecnt=last_linecnt;
  curfilename=last_filename;
  curfile_unicode=last_unicode;
  fp=last_fp;

  build_include_depth--;
  if (r != PS_EOF && r != PS_OK)
  {
    ERROR_MSG(_T("!include: error in script: \"%s\" on line %d\n"),f,errlinecnt);
    return PS_ERROR;
  }
  SCRIPT_MSG(_T("!include: closed: \"%s\"\n"),f);
  return PS_OK;
}

// !ifmacro[n]def based on Anders Kjersem's code
int CEXEBuild::MacroExists(const TCHAR *macroname)
{
  TCHAR *m = (TCHAR *) m_macros.get();

  while (m && *m)
  {
    // check if macroname matches
    if (!_tcsicmp(m, macroname))
      return 1;

    // skip macro name
    m += _tcslen(m) + 1;

    // skip params
    while (*m) m += _tcslen(m) + 1;
    m++;

    // skip data
    while (*m) m += _tcslen(m) + 1;
    if (m - (TCHAR *) m_macros.get() >= m_macros.getlen() - 1) break;
    m++;
  }
  return 0;
}

int CEXEBuild::LoadLicenseFile(TCHAR *file, TCHAR** pdata, LineParser &line, BOOL* unicode) // caller must free *pdata, even on error result
{
  FILE *fp=FOPENTEXT2(file,"rt",unicode);
  if (!fp)
  {
    ERROR_MSG(_T("%s: open failed \"%s\"\n"),line.gettoken_str(0),file);
    PRINTHELP()
  }
  MANAGE_WITH(fp, fclose);
  unsigned int beginning=ftell(fp); // (we might be positionned after a BOM)
  fseek(fp,0,SEEK_END);
  unsigned int datalen=ftell(fp)-beginning; // size of file in bytes! not a number of character
  if (!datalen)
  {
    ERROR_MSG(_T("%s: empty license file \"%s\"\n"),line.gettoken_str(0),file);
    return PS_ERROR;
  }
  fseek(fp,beginning,SEEK_SET);
  TCHAR *data=(TCHAR*)malloc((datalen+2)*sizeof(TCHAR)); // alloc enough for worst-case scenario (ANSI/UTF8 characters read in WCHARs)
  if (!data)
  {
    ERROR_MSG(_T("Internal compiler error #12345: %s malloc(%d) failed.\n"),line.gettoken_str(0),(datalen+2)*sizeof(TCHAR));
    return PS_ERROR;
  }
  *pdata = data; // memory will be released by caller
  TCHAR *ldata=data+1;
  while (_fgetts(ldata, data+datalen+2-ldata, fp)) // _fgetts translate ANSI/UTF8 characters to TCHAR
      ldata += _tcslen(ldata);
  if (ferror(fp))
  {
    ERROR_MSG(_T("%s: can't read file.\n"),line.gettoken_str(0));
    return PS_ERROR;
  }
  if (!memcmp(data+1,_T("{\\rtf"),5*sizeof(TCHAR)))
    *data = SF_RTF;
  else if (*unicode)
    *data = SF_TEXT|SF_UNICODE;
  else
    *data = SF_TEXT;
  return PS_OK;
}

int CEXEBuild::process_oneline(TCHAR *line, const TCHAR *filename, int linenum)
{
  const TCHAR *last_filename=curfilename;
  curfilename=filename;
  int last_linecnt=linecnt;
  linecnt=linenum;

  StringList hist;
  GrowBuf linedata;

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  TCHAR *oldfilename = NULL;
  TCHAR *oldtimestamp = NULL;
  TCHAR *oldline = NULL;
  BOOL is_commandline = !_tcscmp(filename,_T("command line"));
  BOOL is_macro = !_tcsncmp(filename,_T("macro:"),_tcslen(_T("macro:")));

  if(!is_commandline) { // Don't set the predefines for command line /X option
    if(!is_macro) {
      oldfilename = set_file_predefine(curfilename);
      oldtimestamp = set_timestamp_predefine(curfilename);
    }
    oldline = set_line_predefine(linecnt, is_macro);
  }
#endif

  ps_addtoline(line,linedata,hist);
  linedata.add(_T(""),sizeof(_T("")));
  int ret=doParse((TCHAR*)linedata.get());

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  if(!is_commandline) { // Don't set the predefines for command line /X option
    if(!is_macro) {
      restore_file_predefine(oldfilename);
      restore_timestamp_predefine(oldtimestamp);
    }
    restore_line_predefine(oldline);
  }
#endif

  linecnt=last_linecnt;
  curfilename=last_filename;

  return ret;
}

int CEXEBuild::process_jump(LineParser &line, int wt, int *offs)
{
  const TCHAR *s=line.gettoken_str(wt);
  int v;

  if (!_tcsicmp(s,_T("0")) || !_tcsicmp(s,_T(""))) *offs=0;
  else if ((v=GetUserVarIndex(line, wt))>=0)
  {
    *offs=-v-1; // to jump to a user variable target, -variable_index-1 is stored.
  }
  else
  {
    if ((s[0] == _T('-') || s[0] == _T('+')) && !_ttoi(s+1))
    {
      ERROR_MSG(_T("Error: Goto targets beginning with '+' or '-' must be followed by nonzero integer (relative jump)\n"));
      return 1;
    }
    if ((s[0] >= _T('0') && s[0] <= _T('9')) || s[0] == _T('$') || s[0] == _T('!'))
    {
      ERROR_MSG(_T("Error: Goto targets cannot begin with 0-9, $, !\n"));
      return 1;
    }
    *offs=ns_label.add(s,0);
  }
  return 0;
}

#define FLAG_OFFSET(flag) (FIELD_OFFSET(exec_flags_t, flag)/sizeof(int))
#define SECTION_FIELD_GET(field) (FIELD_OFFSET(section, field)/sizeof(int))
#define SECTION_FIELD_SET(field) (-1 - (int)(FIELD_OFFSET(section, field)/sizeof(int)))

// Func size: about 5000 lines (orip)
int CEXEBuild::doCommand(int which_token, LineParser &line)
{
  static const TCHAR *rootkeys[2] = {
    _T("HKCR\0HKLM\0HKCU\0HKU\0HKCC\0HKDD\0HKPD\0SHCTX\0"),
    _T("HKEY_CLASSES_ROOT\0HKEY_LOCAL_MACHINE\0HKEY_CURRENT_USER\0HKEY_USERS\0HKEY_CURRENT_CONFIG\0HKEY_DYN_DATA\0HKEY_PERFORMANCE_DATA\0SHELL_CONTEXT\0")
  };
  static HKEY rootkey_tab[] = {
    HKEY_CLASSES_ROOT,HKEY_LOCAL_MACHINE,HKEY_CURRENT_USER,HKEY_USERS,HKEY_CURRENT_CONFIG,HKEY_DYN_DATA,HKEY_PERFORMANCE_DATA,0
  };

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  build_plugin_table();
#endif

  multiple_entries_instruction=0;

  entry ent={0,};
  switch (which_token)
  {
    // macro stuff
    ///////////////////////////////////////////////////////////////////////////////
    case TOK_P_MACRO:
      {
        if (!line.gettoken_str(1)[0]) PRINTHELP()
        TCHAR *t=(TCHAR *)m_macros.get();
        while (t && *t)
        {
          if (!_tcsicmp(t,line.gettoken_str(1))) break;
          t+=_tcslen(t)+1;

          // advance over parameters
          while (*t) t+=_tcslen(t)+1;
          t++;

          // advance over data
          while (*t) t+=_tcslen(t)+1;
          if (t-(TCHAR *)m_macros.get() >= m_macros.getlen()-1)
            break;
          t++;
        }
        if (t && *t)
        {
          ERROR_MSG(_T("!macro: macro named \"%s\" already found!\n"),line.gettoken_str(1));
          return PS_ERROR;
        }
        m_macros.add(line.gettoken_str(1),(_tcslen(line.gettoken_str(1))+1)*sizeof(TCHAR));

        int pc;
        for (pc=2; pc < line.getnumtokens(); pc ++)
        {
          if (!line.gettoken_str(pc)[0])
          {
            ERROR_MSG(_T("!macro: macro parameter %d is empty, not valid!\n"),pc-1);
            return PS_ERROR;
          }
          int a;
          for (a=2; a < pc; a ++)
          {
            if (!_tcsicmp(line.gettoken_str(pc),line.gettoken_str(a)))
            {
              ERROR_MSG(_T("!macro: macro parameter named %s is used multiple times!\n"),
                line.gettoken_str(pc));
              return PS_ERROR;
            }
          }
          m_macros.add(line.gettoken_str(pc),(_tcslen(line.gettoken_str(pc))+1)*sizeof(TCHAR));
        }
        m_macros.add(_T(""),sizeof(_T("")));

        for (;;)
        {
          TCHAR str[MAX_LINELENGTH];
          TCHAR *p=str;
          str[0]=0;
          _fgetts(str,MAX_LINELENGTH,fp);
          //SCRIPT_MSG(_T("%s%s"), str, str[_tcslen(str)-1]==_T('\n')?_T(""):_T("\n"));
          if (feof(fp) && !str[0])
          {
            ERROR_MSG(_T("!macro \"%s\": unterminated (no !macroend found in file)!\n"),line.gettoken_str(1));
            return PS_ERROR;
          }
          // remove trailing whitespace
          while (*p) p++;
          if (p > str) p--;
          while (p >= str && (*p == _T('\r') || *p == _T('\n') || *p == _T(' ') || *p == _T('\t'))) p--;
          *++p=0;
          LineParser l2(false);
          if (!l2.parse(str))
          {
            if (!_tcsicmp(l2.gettoken_str(0),_T("!macroend")))
            {
              linecnt++;
              break;
            }
            if (!_tcsicmp(l2.gettoken_str(0),_T("!macro")))
            {
              ERROR_MSG(_T("Error: can't define a macro inside a macro!\n"));
              return PS_ERROR;
            }
          }
          if (str[0]) m_macros.add(str,(_tcslen(str)+1)*sizeof(TCHAR));
          else m_macros.add(_T(" "),sizeof(_T(" ")));
          linecnt++;
        }
        m_macros.add(_T(""),sizeof(_T("")));
      }
    return PS_OK;
    case TOK_P_MACROEND:
      ERROR_MSG(_T("!macroend: no macro currently open.\n"));
    return PS_ERROR;
    case TOK_P_MACROUNDEF:
      {
        const TCHAR* mname=line.gettoken_str(1);
        if (!mname[0]) PRINTHELP()
        TCHAR *t=(TCHAR *)m_macros.get(), *mbeg, *mend=0, *mbufb=t;
        while (t && *t)
        {
          const bool foundit = !_tcsicmp((mbeg=t),mname);
          t+=_tcslen(t)+1;

          // advance over parameters
          while (*t) t+=_tcslen(t)+1;
          t++;

          // advance over data
          while (*t) t+=_tcslen(t)+1;
          if (foundit)
          {
            mend=t;
            break;
          }
          if (t-mbufb >= m_macros.getlen()-1)
            break;
          t++;
        }
        if (!mend)
        {
          ERROR_MSG(_T("!macroundef: \"%s\" does not exist!\n"),mname);
          return PS_ERROR;
        }
        const unsigned int mcb=mend-mbeg, mbufcb=m_macros.getlen();
        memmove(mbeg,mend+1,mbufcb-(mcb+(mbeg-mbufb)));
        m_macros.resize(mbufcb-(mcb+1));

        SCRIPT_MSG(_T("!macroundef: %s\n"),mname);
      }
    return PS_OK;
    case TOK_P_INSERTMACRO:
      {
        if (!line.gettoken_str(1)[0]) PRINTHELP()
        TCHAR *t=(TCHAR *)m_macros.get();
        TCHAR *m=t;
        while (t && *t)
        {
          if (!_tcsicmp(t,line.gettoken_str(1))) break;
          t+=_tcslen(t)+1;

          // advance over parms
          while (*t) t+=_tcslen(t)+1;
          t++;

          // advance over data
          while (*t) t+=_tcslen(t)+1;
          if (t-(TCHAR *)m_macros.get() >= m_macros.getlen()-1)
            break;
          t++;
        }
        SCRIPT_MSG(_T("!insertmacro: %s\n"),line.gettoken_str(1));
        if (!t || !*t)
        {
          ERROR_MSG(_T("!insertmacro: macro named \"%s\" not found!\n"),line.gettoken_str(1));
          return PS_ERROR;
        }
        t+=_tcslen(t)+1;


        GrowBuf l_define_names;
        DefineList l_define_saves;
        int npr=0;
        // advance over parms
        while (*t)
        {
          TCHAR *v=definedlist.find(t);
          if (v)
          {
            l_define_saves.add(t,v);
            definedlist.del(t);
          }
          l_define_names.add(t,(_tcslen(t)+1)*sizeof(TCHAR));
          definedlist.add(t,line.gettoken_str(npr+2));

          npr++;
          t+=_tcslen(t)+1;
        }
        l_define_names.add(_T(""),sizeof(_T("")));
        t++;
        if (npr != line.getnumtokens()-2)
        {
          ERROR_MSG(_T("!insertmacro: macro \"%s\" requires %d parameter(s), passed %d!\n"),
            line.gettoken_str(1),npr,line.getnumtokens()-2);
          return PS_ERROR;
        }

        int lp=0;
        TCHAR str[1024];
        if (m_macro_entry.find(line.gettoken_str(1),0)>=0)
        {
          ERROR_MSG(_T("!insertmacro: macro \"%s\" already being inserted!\n"),line.gettoken_str(1));
          return PS_ERROR;
        }
        int npos=m_macro_entry.add(line.gettoken_str(1),0);

        wsprintf(str,_T("macro:%s"),line.gettoken_str(1));
        const TCHAR* oldmacroname=m_currentmacroname;
        m_currentmacroname=line.gettoken_str(1);
        definedlist.del(_T("__MACRO__"));
        definedlist.add(_T("__MACRO__"),m_currentmacroname);
        while (*t)
        {
          lp++;
          if (_tcscmp(t,_T(" ")))
          {
            int ret=process_oneline(t,str,lp);
            if (ret != PS_OK)
            {
              ERROR_MSG(_T("Error in macro %s on macroline %d\n"),line.gettoken_str(1),lp);
              return ret;
            }
          }
          {
            // fix t if process_oneline changed m_macros
            TCHAR *nm=(TCHAR *)m_macros.get();
            if (nm != m)
            {
              t += nm - m;
              m = nm;
            }
          }
          t+=_tcslen(t)+1;
        }
        m_macro_entry.delbypos(npos);
        {
          TCHAR *p=(TCHAR*)l_define_names.get();
          while (*p)
          {
            definedlist.del(p);
            TCHAR *v;
            if ((v=l_define_saves.find(p))) definedlist.add(p,v);
            p+=_tcslen(p)+1;
          }
        }
        definedlist.del(_T("__MACRO__"));
        m_currentmacroname = oldmacroname;
        if (oldmacroname) definedlist.add(_T("__MACRO__"),oldmacroname);
        SCRIPT_MSG(_T("!insertmacro: end of %s\n"),line.gettoken_str(1));
      }
    return PS_OK;

    // preprocessor files fun
    ///////////////////////////////////////////////////////////////////////////////

    case TOK_P_TEMPFILE:
      {
        TCHAR *symbol = line.gettoken_str(1);
        TCHAR *fpath;

#ifdef _WIN32
        TCHAR buf[MAX_PATH], buf2[MAX_PATH];

        GetTempPath(MAX_PATH, buf);
        if (!GetTempFileName(buf, _T("nst"), 0, buf2))
        {
          ERROR_MSG(_T("!tempfile: unable to create temporary file.\n"));
          return PS_ERROR;
        }

        fpath = buf2;
#else
        TCHAR t[] = _T("/tmp/makensisXXXXXX");

        mode_t old_umask = umask(0077);

        int fd = mkstemp(t);
        if (fd == -1) {
          ERROR_MSG(_T("!tempfile: unable to create temporary file.\n"));
          return PS_ERROR;
        }
        close(fd);

        umask(old_umask);

        fpath = t;
#endif

        if (definedlist.add(symbol, fpath))
        {
          ERROR_MSG(_T("!tempfile: \"%s\" already defined!\n"), symbol);
          return PS_ERROR;
        }

        SCRIPT_MSG(_T("!tempfile: \"%s\"=\"%s\"\n"), symbol, fpath);
      }
    return PS_OK;

    case TOK_P_DELFILE:
      {
        int fatal = 1;
        int a = 1;
        TCHAR *fc = line.gettoken_str(a);
        if (line.getnumtokens()==3)
        {
          if(!_tcsicmp(fc,_T("/nonfatal")))
          {
            fatal = 0;
            fc = line.gettoken_str(++a);
          }
          else PRINTHELP();
        }

        SCRIPT_MSG(_T("!delfile: \"%s\"\n"), line.gettoken_str(a));

        tstring dir = get_dir_name(fc);
        tstring spec = get_file_name(fc);
        tstring basedir = dir + PLATFORM_PATH_SEPARATOR_STR;
        if (dir == spec) {
          // no path, just file name
          dir = _T(".");
          basedir = _T("");
        }

        boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
        dr->read(dir);

        for (dir_reader::iterator files_itr = dr->files().begin();
             files_itr != dr->files().end();
             files_itr++)
        {
          if (!dir_reader::matches(*files_itr, spec))
            continue;

          tstring file = basedir + *files_itr;

          int result = _tunlink(file.c_str());
          if (result == -1) {
            ERROR_MSG(_T("!delfile: \"%s\" couldn't be deleted.\n"), file.c_str());
            if (fatal)
            {
              return PS_ERROR;
            }
          }
          else
          {
            SCRIPT_MSG(_T("!delfile: deleted \"%s\"\n"), file.c_str());
          }
        }
      }
    return PS_OK;

    case TOK_P_APPENDFILE:
      {
        TCHAR *file = line.gettoken_str(1);
        TCHAR *text = line.gettoken_str(2);

        FILE *fp = FOPENTEXT(file, "a");
        if (!fp)
        {
          ERROR_MSG(_T("!appendfile: \"%s\" couldn't be opened.\n"), file);
          return PS_ERROR;
        }

        if (_fputts(text, fp) < 0)
        {
          ERROR_MSG(_T("!appendfile: error writing to \"%s\".\n"), file);
          return PS_ERROR;
        }

        fclose(fp);

        SCRIPT_MSG(_T("!appendfile: \"%s\" \"%s\"\n"), file, text);
      }
    return PS_OK;

    case TOK_P_GETDLLVERSION:
    {
      const TCHAR *cmdname = _T("!getdllversion");
      DWORD low, high; 
      if (!GetDLLVersion(line.gettoken_str(1), high, low))
      {
        ERROR_MSG(_T("%s: error reading version info from \"%s\"\n"), cmdname, line.gettoken_str(1));
        return PS_ERROR;
      }
      TCHAR symbuf[MAX_LINELENGTH], numbuf[30], *basesymname = line.gettoken_str(2);
      DWORD vals[] = { high>>16, high&0xffff, low>>16, low&0xffff };
      SCRIPT_MSG(_T("%s: %s (%u.%u.%u.%u)->(%s<1..4>)\n"),
        cmdname, line.gettoken_str(1), vals[0], vals[1], vals[2], vals[3], basesymname);
      for (UINT i = 0; i < 4; ++i)
      {
        _stprintf(symbuf,_T("%s%u"), basesymname, i+1);
        _stprintf(numbuf,_T("%u"), vals[i]);
        definedlist.add(symbuf, numbuf);
      }
    }
    return PS_OK;

    // page ordering stuff
    ///////////////////////////////////////////////////////////////////////////////
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
    case TOK_UNINSTPAGE:
      set_uninstall_mode(1);
    case TOK_PAGE:
      {
        if (!uninstall_mode) {
          enable_last_page_cancel = 0;
          if (!_tcsicmp(line.gettoken_str(line.getnumtokens()-1),_T("/ENABLECANCEL")))
            enable_last_page_cancel = 1;
        }
        else {
          uenable_last_page_cancel = 0;
          if (!_tcsicmp(line.gettoken_str(line.getnumtokens()-1),_T("/ENABLECANCEL")))
            uenable_last_page_cancel = 1;
        }

        int k = line.gettoken_enum(1,_T("custom\0license\0components\0directory\0instfiles\0uninstConfirm\0"));

        if (k < 0) PRINTHELP();

        if (add_page(k) != PS_OK)
          return PS_ERROR;

#ifndef NSIS_SUPPORT_CODECALLBACKS
        if (!k) {
          ERROR_MSG(_T("Error: custom page specified, NSIS_SUPPORT_CODECALLBACKS not defined.\n"));
          return PS_ERROR;
        }
#endif//!NSIS_SUPPORT_CODECALLBACKS

        if (k) {
          // not custom
#ifdef NSIS_SUPPORT_CODECALLBACKS
          switch (line.getnumtokens() - enable_last_page_cancel) {
            case 6:
              PRINTHELP();
            case 5:
              if (*line.gettoken_str(4))
                cur_page->leavefunc = ns_func.add(line.gettoken_str(4),0);
            case 4:
              if (*line.gettoken_str(3))
                cur_page->showfunc = ns_func.add(line.gettoken_str(3),0);
            case 3:
              if (*line.gettoken_str(2))
                cur_page->prefunc = ns_func.add(line.gettoken_str(2),0);
          }
#endif//NSIS_SUPPORT_CODECALLBACKS
        }
#ifdef NSIS_SUPPORT_CODECALLBACKS
        else {
          // a custom page
          switch (line.getnumtokens() - enable_last_page_cancel) {
            case 6:
              PRINTHELP();
            case 5:
              cur_page->caption = add_string(line.gettoken_str(4));
            case 4:
              if (*line.gettoken_str(3))
                cur_page->leavefunc = ns_func.add(line.gettoken_str(3),0);
            case 3:
              if (*line.gettoken_str(2))
                cur_page->prefunc = ns_func.add(line.gettoken_str(2),0);
              break;
            case 2:
              ERROR_MSG(_T("Error: custom page must have a creator function!\n"));
              PRINTHELP();
          }
        }
#endif//NSIS_SUPPORT_CODECALLBACKS

        SCRIPT_MSG(_T("%sPage: %s"), uninstall_mode?_T("Uninst"):_T(""), line.gettoken_str(1));

#ifdef NSIS_SUPPORT_CODECALLBACKS
        if (cur_page->prefunc>=0)
          SCRIPT_MSG(_T(" (%s:%s)"), k?_T("pre"):_T("creator"), line.gettoken_str(2));
        if (cur_page->showfunc>=0 && k)
          SCRIPT_MSG(_T(" (show:%s)"), line.gettoken_str(3));
        if (cur_page->leavefunc>=0)
          SCRIPT_MSG(_T(" (leave:%s)"), line.gettoken_str(4-!k));
        else if (cur_page->caption && !k)
          SCRIPT_MSG(_T(" (caption:%s)"), line.gettoken_str(3));
#endif
        SCRIPT_MSG(_T("\n"));

        page_end();

        if (k == PAGE_INSTFILES) {
          add_page(PAGE_COMPLETED);
          page_end();
        }

        set_uninstall_mode(0);
      }
    return PS_OK;

    // extended page setting
    case TOK_PAGEEX:
    {
      int k = line.gettoken_enum(1,_T("custom\0license\0components\0directory\0instfiles\0uninstConfirm\0"));
      if (k < 0) {
        k = line.gettoken_enum(1,_T("un.custom\0un.license\0un.components\0un.directory\0un.instfiles\0un.uninstConfirm\0"));
        if (k < 0) PRINTHELP();
        set_uninstall_mode(1);
      }

      SCRIPT_MSG(_T("PageEx: %s\n"), line.gettoken_str(1));

      if (add_page(k) != PS_OK)
        return PS_ERROR;

      cur_page->flags |= PF_PAGE_EX;
    }
    return PS_OK;

    case TOK_PAGEEXEND:
    {
      SCRIPT_MSG(_T("PageExEnd\n"));

#ifdef NSIS_SUPPORT_CODECALLBACKS
      if (cur_page_type == PAGE_CUSTOM && !cur_page->prefunc) {
        ERROR_MSG(_T("Error: custom pages must have a creator function.\n"));
        return PS_ERROR;
      }
#endif

      page_end();

      if (cur_page_type == PAGE_INSTFILES) {
        add_page(PAGE_COMPLETED);
        page_end();
      }

      set_uninstall_mode(0);
    }
    return PS_OK;
    case TOK_PAGECALLBACKS:
#ifdef NSIS_SUPPORT_CODECALLBACKS
    {
      SCRIPT_MSG(_T("PageCallbacks:"));

      if (cur_page_type == PAGE_CUSTOM)
      {
        switch (line.getnumtokens())
        {
          case 4:
          {
            PRINTHELP();
          }
          case 3:
          {
            if (*line.gettoken_str(2))
            {
              if (_tcsnicmp(line.gettoken_str(2), _T("un."), 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              cur_page->leavefunc = ns_func.add(line.gettoken_str(2),0);
            }
          }
          case 2:
          {
            if (*line.gettoken_str(1))
            {
              if (_tcsnicmp(line.gettoken_str(1), _T("un."), 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              cur_page->prefunc = ns_func.add(line.gettoken_str(1),0);
            }
          }
        }
      }
      else
      {
        switch (line.getnumtokens())
        {
          case 4:
          {
            if (*line.gettoken_str(3))
            {
              if (_tcsnicmp(line.gettoken_str(3), _T("un."), 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              cur_page->leavefunc = ns_func.add(line.gettoken_str(3),0);
            }
          }
          case 3:
          {
            if (*line.gettoken_str(2))
            {
              if (_tcsnicmp(line.gettoken_str(2), _T("un."), 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              cur_page->showfunc = ns_func.add(line.gettoken_str(2),0);
            }
          }
          case 2:
          {
            if (*line.gettoken_str(1))
            {
              if (_tcsnicmp(line.gettoken_str(1), _T("un."), 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG(_T("\nError: function names must start with \"un.\" in an uninstall page.\n"));
                  return PS_ERROR;
                }
              }
              cur_page->prefunc = ns_func.add(line.gettoken_str(1),0);
            }
          }
        }
      }

      int custom = cur_page_type == PAGE_CUSTOM ? 1 : 0;

      if (cur_page->prefunc>=0)
        SCRIPT_MSG(_T(" %s:%s"), !custom?_T("pre"):_T("creator"), line.gettoken_str(1));
      if (cur_page->showfunc>=0 && !custom)
        SCRIPT_MSG(_T(" show:%s"), line.gettoken_str(2));
      if (cur_page->leavefunc>=0)
        SCRIPT_MSG(_T(" leave:%s"), line.gettoken_str(3-custom));

      SCRIPT_MSG(_T("\n"));
    }
    return PS_OK;
#else
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_CODECALLBACKS not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_SUPPORT_CODECALLBACKS
#else
    case TOK_PAGE:
    case TOK_UNINSTPAGE:
    case TOK_PAGEEX:
    case TOK_PAGEEXEND:
    case TOK_PAGECALLBACKS:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_VISIBLE_SUPPORT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
    // header flags
    ///////////////////////////////////////////////////////////////////////////////
    case TOK_LANGSTRING:
    {
      TCHAR *name = line.gettoken_str(1);
      LANGID lang = line.gettoken_int(2);
      TCHAR *str = line.gettoken_str(3);
      int ret;
#ifndef _UNICODE
        if (build_include_isutf8)
          ret = SetUTF8LangString(name, lang, str);
        else
#endif
          ret = SetLangString(name, lang, str, curfile_unicode);

      if (ret == PS_WARNING)
        warning_fl(_T("LangString \"%s\" set multiple times for %d, wasting space"), name, lang);
      else if (ret == PS_ERROR) {
        ERROR_MSG(_T("Error: can't set LangString \"%s\"!\n"), name);
        return PS_ERROR;
      }
      // BUGBUG: Does not display UTF-8 properly.
      SCRIPT_MSG(_T("LangString: \"%s\" %d \"%s\"\n"), name, lang, str);
    }
    return PS_OK;
    case TOK_LANGSTRINGUP:
      SCRIPT_MSG(_T("Error: LangStringUP is obsolete, there are no more unprocessed strings. Use LangString.\n"));
    return PS_ERROR;
    case TOK_LICENSELANGSTRING:
    {
#ifdef NSIS_CONFIG_SILENT_SUPPORT
      if (build_header.flags&(CH_FLAGS_SILENT|CH_FLAGS_SILENT_LOG))
      {
        warning_fl(_T("LicenseLangString: SilentInstall enabled, wasting space"));
      }
#endif
      TCHAR *name = line.gettoken_str(1);
      LANGID lang = line.gettoken_int(2);
      TCHAR *file = line.gettoken_str(3);

      TCHAR *data = NULL;
      MANAGE_WITH(data, free);
      BOOL unicode;

      int ret = LoadLicenseFile(file, &data, line, &unicode);
      if (ret != PS_OK)
          return ret;

      ret = SetLangString(name, lang, data, unicode);
      if (ret == PS_WARNING)
        warning_fl(_T("LicenseLangString \"%s\" set multiple times for %d, wasting space"), name, lang);
      else if (ret == PS_ERROR)
      {
        ERROR_MSG(_T("Error: can't set LicenseLangString \"%s\"!\n"), name);
        return PS_ERROR;
      }

      SCRIPT_MSG(_T("LicenseLangString: \"%s\" %d \"%s\"\n"), name, lang, file);
    }
    return PS_OK;
    case TOK_NAME:
      {
        if (SetInnerString(NLF_NAME,line.gettoken_str(1)) == PS_WARNING)
          warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
        SetInnerString(NLF_NAME_DA,line.gettoken_str(2));
        SCRIPT_MSG(_T("Name: \"%s\""),line.gettoken_str(1));
        if (*line.gettoken_str(2))
          SCRIPT_MSG(_T(" \"%s\""),line.gettoken_str(2));
        SCRIPT_MSG(_T("\n"));
      }
    return PS_OK;
    case TOK_CAPTION:
      {
        if (!cur_page)
        {
          if (SetInnerString(NLF_CAPTION,line.gettoken_str(1)) == PS_WARNING)
            warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
        }
        else
        {
          cur_page->caption = add_string(line.gettoken_str(1));
        }
        SCRIPT_MSG(_T("Caption: \"%s\"\n"),line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_ICON:
      SCRIPT_MSG(_T("Icon: \"%s\"\n"),line.gettoken_str(1));
      try {
        free_loaded_icon(installer_icon);
        installer_icon = load_icon_file(line.gettoken_str(1));
      }
      catch (exception& err) {
        ERROR_MSG(_T("Error while loading icon from \"%s\": %s\n"), line.gettoken_str(1), CtoTString(err.what()));
        return PS_ERROR;
      }
    return PS_OK;
#ifdef NSIS_CONFIG_COMPONENTPAGE
    case TOK_CHECKBITMAP:
      SCRIPT_MSG(_T("CheckBitmap: \"%s\"\n"),line.gettoken_str(1));
      try {
        init_res_editor();
        int err = update_bitmap(res_editor, IDB_BITMAP1, line.gettoken_str(1), 96, 16, 8);
        if (err) {
          switch (err) {
            case -1:
              ERROR_MSG(_T("Error: can't find bitmap\n"));
              break;
            case -2:
              ERROR_MSG(_T("Error: invalid bitmap file - corrupted or not a bitmap\n"));
              break;
            case -3:
              ERROR_MSG(_T("Error: bitmap isn't 96x16 in size\n"));
              break;
            case -4:
              ERROR_MSG(_T("Error: bitmap has more than 8bpp\n"));
              break;
          }
          return PS_ERROR;
        }
      }
      catch (exception& err) {
        ERROR_MSG(_T("Error while replacing bitmap: %s\n"), CtoTString(err.what()));
        return PS_ERROR;
      }
    return PS_OK;
#else//NSIS_CONFIG_COMPONENTPAGE
    case TOK_CHECKBITMAP:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_COMPONENTPAGE not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_COMPONENTPAGE
    case TOK_DIRTEXT:
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
      {
        if (!cur_page) {
          if (SetInnerString(NLF_DIR_TEXT, line.gettoken_str(1)) == PS_WARNING)
            warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
          if (line.getnumtokens() > 2)
            SetInnerString(NLF_DIR_SUBTEXT, line.gettoken_str(2));
          if (line.getnumtokens() > 3)
            SetInnerString(NLF_BTN_BROWSE, line.gettoken_str(3));
          if (line.getnumtokens() > 4)
            SetInnerString(NLF_DIR_BROWSETEXT, line.gettoken_str(4));
        }
        else {
          if (cur_page_type != PAGE_DIRECTORY) {
            ERROR_MSG(_T("Error: DirText can only be used inside PageEx directory.\n"));
            return PS_ERROR;
          }
          cur_page->parms[0] = add_string(line.gettoken_str(1));
          if (line.getnumtokens() > 2)
            cur_page->parms[1] = add_string(line.gettoken_str(2));
          if (line.getnumtokens() > 3)
            cur_page->parms[2] = add_string(line.gettoken_str(3));
          if (line.getnumtokens() > 4)
            cur_page->parms[3] = add_string(line.gettoken_str(4));
        }
        SCRIPT_MSG(_T("DirText: \"%s\" \"%s\" \"%s\" \"%s\"\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
      }
    return PS_OK;
#else//NSIS_CONFIG_VISIBLE_SUPPORT
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_VISIBLE_SUPPORT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_VISIBLE_SUPPORT
    case TOK_DIRVAR:
    {
      if (cur_page_type != PAGE_DIRECTORY && cur_page_type != PAGE_UNINSTCONFIRM) {
        ERROR_MSG(_T("Error: can't use DirVar outside of PageEx directory|uninstConfirm.\n"));
        return PS_ERROR;
      }
      cur_page->parms[4] = GetUserVarIndex(line, 1) + 1;
      if (cur_page->parms[4] <= 0) PRINTHELP();
      SCRIPT_MSG(_T("DirVar: %s\n"), line.gettoken_str(1));
    }
    return PS_OK;
    case TOK_DIRVERIFY:
    {
      if (cur_page_type != PAGE_DIRECTORY) {
        ERROR_MSG(_T("Error: can't use DirVerify outside of PageEx directory.\n"));
        return PS_ERROR;
      }
      cur_page->flags &= ~PF_DIR_NO_BTN_DISABLE;
      int k = line.gettoken_enum(1,_T("auto\0leave\0"));
      if (k == -1)
        PRINTHELP();
      if (k)
        cur_page->flags |= PF_DIR_NO_BTN_DISABLE;
      SCRIPT_MSG(_T("DirVerify: %s\n"), line.gettoken_str(1));
    }
    return PS_OK;
    case TOK_GETINSTDIRERROR:
      ent.which = EW_GETFLAG;
      ent.offsets[0] = GetUserVarIndex(line, 1);
      ent.offsets[1] = FLAG_OFFSET(instdir_error);
    return add_entry(&ent);
#ifdef NSIS_CONFIG_COMPONENTPAGE
    case TOK_COMPTEXT:
      {
        if (!cur_page) {
          if (SetInnerString(NLF_COMP_TEXT, line.gettoken_str(1)) == PS_WARNING)
            warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
          if (line.getnumtokens() > 2)
            SetInnerString(NLF_COMP_SUBTEXT1, line.gettoken_str(2));
          if (line.getnumtokens() > 3)
            SetInnerString(NLF_COMP_SUBTEXT2, line.gettoken_str(3));
        }
        else {
          if (cur_page_type != PAGE_COMPONENTS) {
            ERROR_MSG(_T("Error: ComponentText can only be used inside PageEx components.\n"));
            return PS_ERROR;
          }
          cur_page->parms[0] = add_string(line.gettoken_str(1));
          cur_page->parms[1] = add_string(line.gettoken_str(2));
          cur_page->parms[2] = add_string(line.gettoken_str(3));
          cur_page->parms[3] = cur_page->parms[1];
          cur_page->parms[4] = cur_page->parms[2];
        }
        SCRIPT_MSG(_T("ComponentText: \"%s\" \"%s\" \"%s\"\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
      }
    return PS_OK;
    case TOK_INSTTYPE:
      {
        int x;

        if (!_tcsicmp(line.gettoken_str(1),_T("/NOCUSTOM")))
        {
          build_header.flags|=CH_FLAGS_NO_CUSTOM;
          SCRIPT_MSG(_T("InstType: disabling custom install type\n"));
        }
        else if (!_tcsicmp(line.gettoken_str(1),_T("/COMPONENTSONLYONCUSTOM")))
        {
          build_header.flags|=CH_FLAGS_COMP_ONLY_ON_CUSTOM;
          SCRIPT_MSG(_T("InstType: making components viewable only on custom install type\n"));
        }
        else if (!_tcsnicmp(line.gettoken_str(1),_T("/CUSTOMSTRING="),14))
        {
          SCRIPT_MSG(_T("InstType: setting custom text to: \"%s\"\n"),line.gettoken_str(1)+14);
          if (SetInnerString(NLF_COMP_CUSTOM,line.gettoken_str(1)+14) == PS_WARNING)
            warning_fl(_T("%s: specified multiple times, wasting space"),_T("InstType /CUSTOMSTRING"));
        }
        else if (line.gettoken_str(1)[0]==_T('/'))
        {
          PRINTHELP()
        }
        else
        {
          TCHAR *itname = line.gettoken_str(1);

          if (!_tcsnicmp(itname, _T("un."), 3)) {
            set_uninstall_mode(1);
            itname += 3;
          }

          for (x = 0; x < NSIS_MAX_INST_TYPES && cur_header->install_types[x]; x ++);
          if (x == NSIS_MAX_INST_TYPES)
          {
            ERROR_MSG(_T("InstType: no more than %d install types allowed. %d specified\n"), NSIS_MAX_INST_TYPES, NSIS_MAX_INST_TYPES + 1);
            return PS_ERROR;
          }
          else
          {
            cur_header->install_types[x] = add_string(itname);
            SCRIPT_MSG(_T("InstType: %s%d=\"%s\"\n"), uninstall_mode ? _T("(uninstall) ") : _T(""), x+1, itname);
          }

          set_uninstall_mode(0);
        }
      }
    return PS_OK;
#else//NSIS_CONFIG_COMPONENTPAGE
    case TOK_COMPTEXT:
    case TOK_INSTTYPE:
      ERROR_MSG(_T("Error: %s specified but NSIS_CONFIG_COMPONENTPAGE not defined\n"),line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_COMPONENTPAGE
#ifdef NSIS_CONFIG_LICENSEPAGE
    case TOK_LICENSETEXT:
      {
        if (!cur_page) {
          if (SetInnerString(NLF_LICENSE_TEXT, line.gettoken_str(1)) == PS_WARNING)
            warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
          SetInnerString(NLF_LICENSE_TEXT_FSRB, line.gettoken_str(1));
          SetInnerString(NLF_LICENSE_TEXT_FSCB, line.gettoken_str(1));
          if (line.getnumtokens() > 2)
            SetInnerString(NLF_BTN_LICENSE, line.gettoken_str(2));
        }
        else {
          if (cur_page_type != PAGE_LICENSE) {
            ERROR_MSG(_T("Error: LicenseText can only be used inside PageEx license.\n"));
            return PS_ERROR;
          }
          cur_page->parms[0] = add_string(line.gettoken_str(1));
          cur_page->next = add_string(line.gettoken_str(2));
        }
        SCRIPT_MSG(_T("LicenseText: \"%s\" \"%s\"\n"),line.gettoken_str(1),line.gettoken_str(2));
      }
    return PS_OK;
    case TOK_LICENSEDATA:
      {
        int idx = 0;
        TCHAR *file = line.gettoken_str(1);
        TCHAR *data = NULL;
        TCHAR *filedata = NULL;
        MANAGE_WITH(filedata, free);

        if (file[0] == _T('$') && file[1] == _T('('))
        {
          TCHAR *cp = _tcsdup(file+2);
          MANAGE_WITH(cp, free);
          TCHAR *p = _tcschr(cp, _T(')'));
          if (p && p[1] == 0) { // if string is only a language str identifier
            *p = 0;
            idx = DefineLangString(cp, 0);
          }
          data = file;
        }

        if (!idx)
        {
          BOOL unicode;
          int ret = LoadLicenseFile(file, &filedata, line, &unicode);
          if (ret != PS_OK)
              return ret;
          data = filedata;
        }

        if (!cur_page) {
          if (SetInnerString(NLF_LICENSE_DATA,data) == PS_WARNING)
            warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
        }
        else {
          if (cur_page_type != PAGE_LICENSE) {
            ERROR_MSG(_T("Error: LicenseData can only be used inside PageEx license.\n"));
            return PS_ERROR;
          }

          cur_page->parms[1] = add_string(data, 0);
        }

        SCRIPT_MSG(_T("LicenseData: \"%s\"\n"),file);
      }
    return PS_OK;
    case TOK_LICENSEFORCESELECTION:
    {
      int k=line.gettoken_enum(1,_T("off\0checkbox\0radiobuttons\0"));
      if (k == -1) PRINTHELP()
      if (k < line.getnumtokens() - 2) PRINTHELP()

      if (!cur_page) {
        switch (line.getnumtokens()) {
          case 4:
            SetInnerString(NLF_BTN_LICENSE_DISAGREE, line.gettoken_str(3));
          case 3:
            SetInnerString(NLF_BTN_LICENSE_AGREE, line.gettoken_str(2));
            break;
        }

        switch (k) {
          case 0:
            license_res_id = IDD_LICENSE;
            break;
          case 1:
            license_res_id = IDD_LICENSE_FSCB;
            break;
          case 2:
            license_res_id = IDD_LICENSE_FSRB;
            break;
        }
      }
      else {
        if (cur_page_type != PAGE_LICENSE) {
          ERROR_MSG(_T("Error: LicenseForceSelection can only be used inside PageEx license.\n"));
          return PS_ERROR;
        }
        switch (line.getnumtokens()) {
          case 4:
            cur_page->parms[3] = add_string(line.gettoken_str(3));
          case 3:
            cur_page->parms[2] = add_string(line.gettoken_str(2));
            break;
        }

        cur_page->flags &= ~(PF_LICENSE_FORCE_SELECTION | PF_LICENSE_NO_FORCE_SELECTION);

        switch (k) {
          case 0:
            cur_page->dlg_id = IDD_LICENSE;
            cur_page->flags |= PF_LICENSE_NO_FORCE_SELECTION;
            break;
          case 1:
            cur_page->dlg_id = IDD_LICENSE_FSCB;
            cur_page->flags |= PF_LICENSE_FORCE_SELECTION;
            break;
          case 2:
            cur_page->dlg_id = IDD_LICENSE_FSRB;
            cur_page->flags |= PF_LICENSE_FORCE_SELECTION;
            break;
        }
      }

      SCRIPT_MSG(_T("LicenseForceSelection: %s \"%s\" \"%s\"\n"), line.gettoken_str(1), line.gettoken_str(2), line.gettoken_str(3));
    }
    return PS_OK;
    case TOK_LICENSEBKCOLOR:
      {
        TCHAR *p = line.gettoken_str(1);
        if (!_tcsicmp(p,_T("/windows")))
        {
          build_header.license_bg=-COLOR_WINDOW;
          SCRIPT_MSG(_T("LicenseBkColor: /windows\n"));
        }
        else if (!_tcsicmp(p,_T("/grey")) || !_tcsicmp(p,_T("/gray")))
        {
          build_header.license_bg=-COLOR_BTNFACE;
          SCRIPT_MSG(_T("LicenseBkColor: /grey\n"));
        }
        else
        {
        int v=_tcstoul(p,&p,16);
        build_header.license_bg=((v&0xff)<<16)|(v&0xff00)|((v&0xff0000)>>16);
        build_uninst.license_bg=build_header.license_bg;
        SCRIPT_MSG(_T("LicenseBkColor: %06X\n"),v);
      }
      }
    return PS_OK;
#else//!NSIS_CONFIG_LICENSEPAGE
    case TOK_LICENSETEXT:
    case TOK_LICENSEDATA:
    case TOK_LICENSEBKCOLOR:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_LICENSEPAGE not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_LICENSEPAGE
#ifdef NSIS_CONFIG_SILENT_SUPPORT
    case TOK_SILENTINST:
    {
      int k=line.gettoken_enum(1,_T("normal\0silent\0silentlog\0"));
      if (k<0) PRINTHELP()
#ifndef NSIS_CONFIG_LOG
      if (k == 2)
      {
        ERROR_MSG(_T("SilentInstall: silentlog specified, no log support compiled in (use NSIS_CONFIG_LOG)\n"));
        return PS_ERROR;
      }
#endif//NSIS_CONFIG_LOG
      SCRIPT_MSG(_T("SilentInstall: %s\n"),line.gettoken_str(1));
#ifdef NSIS_CONFIG_LICENSEPAGE
      if (k && HasUserDefined(NLF_LICENSE_DATA))
      {
        warning_fl(_T("SilentInstall: LicenseData already specified. wasting space"));
      }
      if (k) {
        build_header.flags|=CH_FLAGS_SILENT;
        if (k == 2)
          build_header.flags|=CH_FLAGS_SILENT_LOG;
      }
      else {
        build_header.flags&=~CH_FLAGS_SILENT;
        build_header.flags&=~CH_FLAGS_SILENT_LOG;
      }
#endif//NSIS_CONFIG_LICENSEPAGE
    }
    return PS_OK;
    case TOK_SILENTUNINST:
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    {
      int k=line.gettoken_enum(1,_T("normal\0silent\0"));
      if (k<0) PRINTHELP()
      if (k)
        build_uninst.flags|=CH_FLAGS_SILENT;
      else
        build_uninst.flags&=~CH_FLAGS_SILENT;
      SCRIPT_MSG(_T("SilentUnInstall: %s\n"),line.gettoken_str(1));
    }
    return PS_OK;
#else
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_UNINSTALL_SUPPORT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif
    case TOK_IFSILENT:
      ent.which=EW_IFFLAG;
      if (process_jump(line,1,&ent.offsets[0]) ||
          process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=FLAG_OFFSET(silent);
      ent.offsets[3]=~0;//new value mask - keep flag
      SCRIPT_MSG(_T("IfSilent ?%s:%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SETSILENT:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(silent);
      int k=line.gettoken_enum(1,_T("normal\0silent\0"));
      if (k<0) PRINTHELP()
      ent.offsets[1]=add_intstring(k);
      SCRIPT_MSG(_T("SetSilent: %s\n"),line.gettoken_str(1));
    }
    return add_entry(&ent);
#else//!NSIS_CONFIG_SILENT_SUPPORT
    case TOK_SILENTINST:
    case TOK_SILENTUNINST:
    case TOK_IFSILENT:
    case TOK_SETSILENT:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_SILENT_SUPPORT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_CONFIG_SILENT_SUPPORT
    case TOK_OUTFILE:
      _tcsnccpy(build_output_filename,line.gettoken_str(1),1024-1);
      SCRIPT_MSG(_T("OutFile: \"%s\"\n"),build_output_filename);
    return PS_OK;
    case TOK_INSTDIR:
    {
      TCHAR *p = line.gettoken_str(1);
      if (build_header.install_directory_ptr)
      {
        warning_fl(_T("%s: specified multiple times. wasting space"),line.gettoken_str(0));
      }
      build_header.install_directory_ptr = add_string(p);
      build_header.install_directory_auto_append = 0;
      TCHAR *p2 = p + _tcslen(p);
      if (*p && *CharPrev(p, p2) != _T('\\'))
      {
        // we risk hitting $\r or something like $(bla\ad) or ${bla\ad} here, but it's better
        // than hitting backslashes in processed strings
        while (p2 > p && *p2 != _T('\\'))
          p2 = CharPrev(p, p2);
        if (*p2 == _T('\\'))
        {
          build_header.install_directory_auto_append = add_string(p2 + 1);
        }
      }
      SCRIPT_MSG(_T("InstallDir: \"%s\"\n"),line.gettoken_str(1));
    }
    return PS_OK;
    case TOK_INSTALLDIRREGKEY: // InstallDirRegKey
      {
        if (build_header.install_reg_key_ptr)
        {
          warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
        }
        int k=line.gettoken_enum(1,rootkeys[0]);
        if (k == -1) k=line.gettoken_enum(1,rootkeys[1]);
        if (k == -1) PRINTHELP()
        build_header.install_reg_rootkey=(INT_PTR)rootkey_tab[k];
        if (!build_header.install_reg_rootkey) PRINTHELP() // SHCTX is invalid here
        build_header.install_reg_key_ptr = add_string(line.gettoken_str(2),0);
        if (line.gettoken_str(2)[0] == _T('\\'))
          warning_fl(_T("%s: registry path name begins with \'\\\', may cause problems"),line.gettoken_str(0));
        build_header.install_reg_value_ptr = add_string(line.gettoken_str(3),0);
        SCRIPT_MSG(_T("InstallRegKey: \"%s\\%s\\%s\"\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
      }
    return PS_OK;
    case TOK_CRCCHECK:
      build_crcchk=line.gettoken_enum(1,_T("off\0on\0force\0"));
      if (build_crcchk==-1) PRINTHELP()
      SCRIPT_MSG(_T("CRCCheck: %s\n"),line.gettoken_str(1));
    return PS_OK;
    case TOK_INSTPROGRESSFLAGS:
      {
        int x;
        int smooth=0;
        build_header.flags&=~CH_FLAGS_PROGRESS_COLORED;
        for (x = 1; x < line.getnumtokens(); x ++)
        {
          if (!_tcsicmp(line.gettoken_str(x),_T("smooth"))) smooth=1;
          else if (!_tcsicmp(line.gettoken_str(x),_T("colored"))) build_header.flags|=CH_FLAGS_PROGRESS_COLORED;
          else PRINTHELP()
        }
        try {
          init_res_editor();

          BYTE* dlg = res_editor->GetResource(RT_DIALOG, IDD_INSTFILES, NSIS_DEFAULT_LANG);
          if (!dlg) throw runtime_error("IDD_INSTFILES doesn't exist!");
          CDialogTemplate dt(dlg,build_unicode,uDefCodePage);
          free(dlg);
          DialogItemTemplate* progress = dt.GetItem(IDC_PROGRESS);
          if (!progress) {
            throw runtime_error("IDC_PROGRESS doesn't exist!");
          }

          if (smooth)
            progress->dwStyle |= PBS_SMOOTH;
          else
            progress->dwStyle &= ~PBS_SMOOTH;

          DWORD dwSize;
          dlg = dt.Save(dwSize);
          res_editor->UpdateResource(RT_DIALOG, IDD_INSTFILES, NSIS_DEFAULT_LANG, dlg, dwSize);
          delete [] dlg;
        }
        catch (exception& err) {
          ERROR_MSG(_T("Error setting smooth progress bar: %s\n"), CtoTString(err.what()));
          return PS_ERROR;
        }
        SCRIPT_MSG(_T("InstProgressFlags: smooth=%d, colored=%d\n"),smooth,
          !!(build_header.flags&CH_FLAGS_PROGRESS_COLORED));
      }
    return PS_OK;
    case TOK_AUTOCLOSE:
      {
        int k=line.gettoken_enum(1,_T("false\0true\0"));
        if (k == -1) PRINTHELP();
        if (k)
          build_header.flags|=CH_FLAGS_AUTO_CLOSE;
        else
          build_header.flags&=~CH_FLAGS_AUTO_CLOSE;
        SCRIPT_MSG(_T("AutoCloseWindow: %s\n"),k?_T("true"):_T("false"));
      }
    return PS_OK;
    case TOK_WINDOWICON:
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
      disable_window_icon=line.gettoken_enum(1,_T("on\0off\0"));
      if (disable_window_icon == -1) PRINTHELP();
      SCRIPT_MSG(_T("WindowIcon: %s\n"),line.gettoken_str(1));
    return PS_OK;
#else
    ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_VISIBLE_SUPPORT not defined.\n"),line.gettoken_str(0));
    return PS_ERROR;
#endif // NSIS_CONFIG_VISIBLE_SUPPORT
    case TOK_SHOWDETAILSUNINST:
#ifndef NSIS_CONFIG_UNINSTALL_SUPPORT
      ERROR_MSG(_T("Error: ShowUninstDetails specified but NSIS_CONFIG_UNINSTALL_SUPPORT not defined\n"));
      return PS_ERROR;
#endif
    case TOK_SHOWDETAILS:
      {
        int k=line.gettoken_enum(1,_T("hide\0show\0nevershow\0"));
        if (k == -1) PRINTHELP()
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
        if (which_token == TOK_SHOWDETAILSUNINST)
        {
          build_uninst.flags&=~(CH_FLAGS_DETAILS_NEVERSHOW|CH_FLAGS_DETAILS_SHOWDETAILS);
          if (k==1)
            build_uninst.flags|=CH_FLAGS_DETAILS_SHOWDETAILS;
          else if (k==2)
            build_uninst.flags|=CH_FLAGS_DETAILS_NEVERSHOW;
        }
        else
#endif
        {
          build_header.flags&=~(CH_FLAGS_DETAILS_NEVERSHOW|CH_FLAGS_DETAILS_SHOWDETAILS);
          if (k==1)
            build_header.flags|=CH_FLAGS_DETAILS_SHOWDETAILS;
          else if (k==2)
            build_header.flags|=CH_FLAGS_DETAILS_NEVERSHOW;
        }
        SCRIPT_MSG(_T("%s: %s\n"),line.gettoken_str(0),line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_DIRSHOW:
      /*{
        int k=line.gettoken_enum(1,_T("show\0hide\0"));
        if (k == -1) PRINTHELP();
        if (k)
          build_header.flags|=CH_FLAGS_DIR_NO_SHOW;
        else
          build_header.flags&=~CH_FLAGS_DIR_NO_SHOW;
        SCRIPT_MSG(_T("DirShow: %s\n"),k?_T("hide"):_T("show"));
      }*/
      ERROR_MSG(_T("Error: DirShow doesn't currently work\n"));
    return PS_ERROR;
    case TOK_ROOTDIRINST:
      {
        int k=line.gettoken_enum(1,_T("true\0false\0"));
        if (k == -1) PRINTHELP();
        if (k)
          build_header.flags|=CH_FLAGS_NO_ROOT_DIR;
        else
          build_header.flags&=~CH_FLAGS_NO_ROOT_DIR;
        SCRIPT_MSG(_T("AllowRootDirInstall: %s\n"),k?_T("false"):_T("true"));
      }
    return PS_OK;
    case TOK_BGFONT:
#ifndef NSIS_SUPPORT_BGBG
      ERROR_MSG(_T("Error: BGFont specified but NSIS_SUPPORT_BGBG not defined\n"));
      return PS_ERROR;
#else//NSIS_SUPPORT_BGBG
      if (line.getnumtokens()==1)
      {
        memcpy(&bg_font,&bg_default_font,sizeof(LOGFONT));
        SCRIPT_MSG(_T("BGFont: default font\n"));
        return PS_OK;
      }

      LOGFONT newfont;
      newfont.lfHeight=40;
      newfont.lfWidth=0;
      newfont.lfEscapement=0;
      newfont.lfOrientation=0;
      newfont.lfWeight=FW_NORMAL;
      newfont.lfItalic=FALSE;
      newfont.lfUnderline=FALSE;
      newfont.lfStrikeOut=FALSE;
      newfont.lfCharSet=DEFAULT_CHARSET;
      newfont.lfOutPrecision=OUT_DEFAULT_PRECIS;
      newfont.lfClipPrecision=CLIP_DEFAULT_PRECIS;
      newfont.lfQuality=DEFAULT_QUALITY;
      newfont.lfPitchAndFamily=DEFAULT_PITCH;

      _tcsnccpy(newfont.lfFaceName,line.gettoken_str(1),LF_FACESIZE);

      SCRIPT_MSG(_T("BGFont: \"%s\""),line.gettoken_str(1));
      {
        bool height=false;
        bool weight=false;
        for (int i = 2; i < line.getnumtokens(); i++) {
          TCHAR *tok=line.gettoken_str(i);
          if (tok[0]==_T('/')) {
            if (!_tcsicmp(tok,_T("/ITALIC"))) {
              SCRIPT_MSG(_T(" /ITALIC"));
              newfont.lfItalic=TRUE;
            }
            else if (!_tcsicmp(tok,_T("/UNDERLINE"))) {
              SCRIPT_MSG(_T(" /UNDERLINE"));
              newfont.lfUnderline=TRUE;
            }
            else if (!_tcsicmp(tok,_T("/STRIKE"))) {
              SCRIPT_MSG(_T(" /STRIKE"));
              newfont.lfStrikeOut=TRUE;
            }
            else {
              SCRIPT_MSG(_T("\n"));
              PRINTHELP();
            }
          }
          else {
            if (!height) {
              SCRIPT_MSG(_T(" height=%s"),tok);
              newfont.lfHeight=line.gettoken_int(i);
              height=true;
            }
            else if (!weight) {
              SCRIPT_MSG(_T(" weight=%s"),tok);
              newfont.lfWeight=line.gettoken_int(i);
              weight=true;
            }
            else {
              SCRIPT_MSG(_T("\n"));
              PRINTHELP();
            }
          }
        }
      }
      SCRIPT_MSG(_T("\n"));
      memcpy(&bg_font, &newfont, sizeof(LOGFONT));
    return PS_OK;
#endif//NSIS_SUPPORT_BGBG
    case TOK_BGGRADIENT:
#ifndef NSIS_SUPPORT_BGBG
      ERROR_MSG(_T("Error: BGGradient specified but NSIS_SUPPORT_BGBG not defined\n"));
      return PS_ERROR;
#else//NSIS_SUPPORT_BGBG
      if (line.getnumtokens()==1)
      {
        SCRIPT_MSG(_T("BGGradient: default colors\n"));
        build_header.bg_color1=0;
        build_header.bg_color2=RGB(0,0,255);
      }
      else if (!_tcsicmp(line.gettoken_str(1),_T("off")))
      {
        build_header.bg_color1=build_header.bg_color2=build_header.bg_textcolor=-1;
        SCRIPT_MSG(_T("BGGradient: off\n"));
        if (line.getnumtokens()>2) PRINTHELP()
      }
      else
      {
        TCHAR *p = line.gettoken_str(1);
        int v1,v2,v3=-1;
        v1=_tcstoul(p,&p,16);
        build_header.bg_color1=((v1&0xff)<<16)|(v1&0xff00)|((v1&0xff0000)>>16);
        p=line.gettoken_str(2);
        v2=_tcstoul(p,&p,16);
        build_header.bg_color2=((v2&0xff)<<16)|(v2&0xff00)|((v2&0xff0000)>>16);

        p=line.gettoken_str(3);
        if (*p)
        {
          if (!_tcsicmp(p,_T("notext"))) build_header.bg_textcolor=-1;
          else
          {
            v3=_tcstoul(p,&p,16);
            build_header.bg_textcolor=((v3&0xff)<<16)|(v3&0xff00)|((v3&0xff0000)>>16);
          }
        }

        SCRIPT_MSG(_T("BGGradient: 0x%06X->0x%06X (text=0x%06X)\n"),v1,v2,v3);
      }

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      build_uninst.bg_color1=build_header.bg_color1;
      build_uninst.bg_color2=build_header.bg_color2;
      build_uninst.bg_textcolor=build_header.bg_textcolor;
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT
#endif//NSIS_SUPPORT_BGBG
    return PS_OK;
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
    case TOK_INSTCOLORS:
    {
      TCHAR *p = line.gettoken_str(1);
      if (p[0]==_T('/'))
      {
        if (_tcsicmp(p,_T("/windows")) || line.getnumtokens()!=2) PRINTHELP()
        build_header.lb_fg=build_header.lb_bg=-1;
        SCRIPT_MSG(_T("InstallColors: windows default colors\n"));
      }
      else
      {
        int v1,v2;
        if (line.getnumtokens()!=3) PRINTHELP()
        v1=_tcstoul(p,&p,16);
        build_header.lb_fg=((v1&0xff)<<16)|(v1&0xff00)|((v1&0xff0000)>>16);
        p=line.gettoken_str(2);
        v2=_tcstoul(p,&p,16);
        build_header.lb_bg=((v2&0xff)<<16)|(v2&0xff00)|((v2&0xff0000)>>16);
        SCRIPT_MSG(_T("InstallColors: fg=%06X bg=%06X\n"),v1,v2);
      }

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      build_uninst.lb_fg=build_header.lb_fg;
      build_uninst.lb_bg=build_header.lb_bg;
#endif
    }
    return PS_OK;
    case TOK_XPSTYLE:
      {
        int k=line.gettoken_enum(1,_T("on\0off\0"));
        if (k == -1) PRINTHELP()
        SCRIPT_MSG(_T("XPStyle: %s\n"), line.gettoken_str(1));
        if (!k)
          manifest_comctl = manifest::comctl_xp;
        else
          manifest_comctl = manifest::comctl_old;
      }
    return PS_OK;
    case TOK_CHANGEUI:
      try {
        DWORD dwSize;
        int k=line.gettoken_enum(1, _T("all\0IDD_LICENSE\0IDD_DIR\0IDD_SELCOM\0IDD_INST\0IDD_INSTFILES\0IDD_UNINST\0IDD_VERIFY\0IDD_LICENSE_FSRB\0IDD_LICENSE_FSCB\0"));
        if (k<0) PRINTHELP();

        FILE *fui = FOPEN(line.gettoken_str(2), _T("rb"));
        if (!fui) {
          ERROR_MSG(_T("Error: Can't open \"%s\"!\n"), line.gettoken_str(2));
          return PS_ERROR;
        }
        MANAGE_WITH(fui, fclose);

        fseek(fui, 0, SEEK_END);
        unsigned int len = ftell(fui);
        fseek(fui, 0, SEEK_SET);
        LPBYTE ui = (LPBYTE) malloc(len);
        if (!ui) {
          ERROR_MSG(_T("Internal compiler error #12345: malloc(%d) failed\n"), len);
          extern void quit(); quit();
        }
        MANAGE_WITH(ui, free);
        if (fread(ui, 1, len, fui) != len) {
          ERROR_MSG(_T("Error: Can't read \"%s\"!\n"), line.gettoken_str(2));
          return PS_ERROR;
        }

        CResourceEditor *uire = new CResourceEditor(ui, len);

        init_res_editor();

        // Search for required items
        #define GET(x) dlg = uire->GetResource(RT_DIALOG, x, 0); if (!dlg) return PS_ERROR; CDialogTemplate UIDlg(dlg, build_unicode, uDefCodePage);
        #define SEARCH(x) if (!UIDlg.GetItem(x)) {ERROR_MSG(_T("Error: Can't find %s (%u) in the custom UI!\n"), _T(#x), x);delete [] dlg;delete uire;return PS_ERROR;}
        #define SAVE(x) dwSize = UIDlg.GetSize(); res_editor->UpdateResource(RT_DIALOG, x, NSIS_DEFAULT_LANG, dlg, dwSize); delete [] dlg;

        LPBYTE dlg = NULL;

        if (k == 0 || k == 1) {
          GET(IDD_LICENSE);
          SEARCH(IDC_EDIT1);
          SAVE(IDD_LICENSE);
        }

        if (k == 0 || k == 2) {
          GET(IDD_DIR);
          SEARCH(IDC_DIR);
          SEARCH(IDC_BROWSE);
#ifdef NSIS_CONFIG_LOG
          SEARCH(IDC_CHECK1);
#endif
          SAVE(IDD_DIR);
        }

        if (k == 0 || k == 3) {
          GET(IDD_SELCOM);
          SEARCH(IDC_TREE1);
          SEARCH(IDC_COMBO1);
          SAVE(IDD_SELCOM);
        }

        if (k == 0 || k == 4) {
          GET(IDD_INST);
          SEARCH(IDC_BACK);
          SEARCH(IDC_CHILDRECT);
          SEARCH(IDC_VERSTR);
          SEARCH(IDOK);
          SEARCH(IDCANCEL);

          // Search for bitmap holder (default for SetBrandingImage)
          branding_image_found = false;
          DialogItemTemplate* dlgItem = 0;
          for (int i = 0; (dlgItem = UIDlg.GetItemByIdx(i)); i++) {
            bool check = false;

            if (IS_INTRESOURCE(dlgItem->szClass)) {
              if (dlgItem->szClass == MAKEINTRESOURCEW(0x0082)) {
                check = true;
              }
            } else {
              check = _wcsicmp(dlgItem->szClass, L"Static") == 0;
            }

            if (check) {
              if ((dlgItem->dwStyle & SS_BITMAP) == SS_BITMAP) {
                branding_image_found = true;
                branding_image_id = dlgItem->wId;
                break;
              }
            }
          }

          SAVE(IDD_INST);
        }

        if (k == 0 || k == 5) {
          GET(IDD_INSTFILES);
          SEARCH(IDC_LIST1);
          SEARCH(IDC_PROGRESS);
          SEARCH(IDC_SHOWDETAILS);
          SAVE(IDD_INSTFILES);
        }

        if (k == 0 || k == 6) {
          GET(IDD_UNINST);
          SEARCH(IDC_EDIT1);
          SAVE(IDD_UNINST);
        }

        if (k == 0 || k == 7) {
          GET(IDD_VERIFY);
          SEARCH(IDC_STR);
          SAVE(IDD_VERIFY);
        }

        if (k == 0 || k == 8) {
          GET(IDD_LICENSE_FSRB);
          SEARCH(IDC_EDIT1);
          SEARCH(IDC_LICENSEAGREE);
          SEARCH(IDC_LICENSEDISAGREE);
          SAVE(IDD_LICENSE_FSRB);
        }

        if (k == 0 || k == 9) {
          GET(IDD_LICENSE_FSCB);
          SEARCH(IDC_EDIT1);
          SEARCH(IDC_LICENSEAGREE);
          SAVE(IDD_LICENSE_FSCB);
        }

        delete uire;

        SCRIPT_MSG(_T("ChangeUI: %s %s%s\n"), line.gettoken_str(1), line.gettoken_str(2), branding_image_found?_T(" (branding image holder found)"):_T(""));
      }
      catch (exception& err) {
        ERROR_MSG(_T("Error while changing UI: %s\n"), CtoTString(err.what()));
        return PS_ERROR;
      }
    return PS_OK;
    case TOK_ADDBRANDINGIMAGE:
#ifdef _WIN32
      try {
        int k=line.gettoken_enum(1,_T("top\0left\0bottom\0right\0"));
        int wh=line.gettoken_int(2);
        if (k == -1) PRINTHELP();
        int padding = 2;
        if (line.getnumtokens() == 4)
          padding = line.gettoken_int(3);

        init_res_editor();
        BYTE* dlg = res_editor->GetResource(RT_DIALOG, IDD_INST, NSIS_DEFAULT_LANG);

        CDialogTemplate dt(dlg, build_unicode, uDefCodePage);

        res_editor->FreeResource(dlg);

        DialogItemTemplate brandingCtl = {0,};

        brandingCtl.dwStyle = SS_BITMAP | WS_CHILD | WS_VISIBLE;
        brandingCtl.sX = padding;
        brandingCtl.sY = padding;
        brandingCtl.szClass = MAKEINTRESOURCEW(0x0082);
        brandingCtl.szTitle = NULL;
        brandingCtl.wId = IDC_BRANDIMAGE;

        brandingCtl.sHeight = wh;
        brandingCtl.sWidth = wh;
        dt.PixelsToDlgUnits(brandingCtl.sWidth, brandingCtl.sHeight);
        if (k%2) {
          // left (1) / right (3)

          if (k & 2) // right
            brandingCtl.sX += dt.GetWidth();
          else // left
            dt.MoveAll(brandingCtl.sWidth + (padding * 2), 0);

          dt.Resize(brandingCtl.sWidth + (padding * 2), 0);

          brandingCtl.sHeight = dt.GetHeight() - (padding * 2);
        }
        else {
          // top (0) / bottom (2)

          if (k & 2) // bottom
            brandingCtl.sY += dt.GetHeight();
          else // top
            dt.MoveAll(0, brandingCtl.sHeight + (padding * 2));

          dt.Resize(0, brandingCtl.sHeight + (padding * 2));

          brandingCtl.sWidth = dt.GetWidth() - (padding * 2);
        }

        dt.AddItem(brandingCtl);

        DWORD dwDlgSize;
        dlg = dt.Save(dwDlgSize);

        res_editor->UpdateResource(RT_DIALOG, IDD_INST, NSIS_DEFAULT_LANG, dlg, dwDlgSize);

        delete [] dlg;

        dt.DlgUnitsToPixels(brandingCtl.sWidth, brandingCtl.sHeight);
        SCRIPT_MSG(_T("AddBrandingImage: %s %ux%u\n"), line.gettoken_str(1), brandingCtl.sWidth, brandingCtl.sHeight);

        branding_image_found = true;
        branding_image_id = IDC_BRANDIMAGE;
      }
      catch (exception& err) {
        ERROR_MSG(_T("Error while adding image branding support: %s\n"), CtoTString(err.what()));
        return PS_ERROR;
      }
    return PS_OK;
#else
      ERROR_MSG(_T("Error: AddBrandingImage is disabled for non Win32 platforms.\n"));
    return PS_ERROR;
#endif
    case TOK_SETFONT:
    {
      if (!_tcsnicmp(line.gettoken_str(1), _T("/LANG="), 6))
      {
        LANGID lang_id = _ttoi(line.gettoken_str(1) + 6);
        LanguageTable *table = GetLangTable(lang_id);
        table->nlf.m_szFont = (TCHAR*)malloc((_tcslen(line.gettoken_str(2))+1)*sizeof(TCHAR));
        _tcscpy(table->nlf.m_szFont, line.gettoken_str(2));
        table->nlf.m_iFontSize = line.gettoken_int(3);

        SCRIPT_MSG(_T("SetFont: lang=%d \"%s\" %s\n"), lang_id, line.gettoken_str(2), line.gettoken_str(3));
      }
      else
      {
        _tcsnccpy(build_font, line.gettoken_str(1), COUNTOF(build_font));
        build_font_size = line.gettoken_int(2);

        SCRIPT_MSG(_T("SetFont: \"%s\" %s\n"), line.gettoken_str(1), line.gettoken_str(2));
      }
    }
    return PS_OK;
#else
    case TOK_INSTCOLORS:
    case TOK_XPSTYLE:
    case TOK_CHANGEUI:
    case TOK_ADDBRANDINGIMAGE:
    case TOK_SETFONT:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_VISIBLE_SUPPORT not defined.\n"),line.gettoken_str(0));
    return PS_ERROR;
#endif// NSIS_CONFIG_VISIBLE_SUPPORT

    case TOK_REQEXECLEVEL:
    {
      int k=line.gettoken_enum(1,_T("none\0user\0highest\0admin\0"));
      switch (k)
      {
      case 0:
        manifest_exec_level = manifest::exec_level_none;
        break;
      case 1:
        manifest_exec_level = manifest::exec_level_user;
        break;
      case 2:
        manifest_exec_level = manifest::exec_level_highest;
        break;
      case 3:
        manifest_exec_level = manifest::exec_level_admin;
        break;
      default:
        PRINTHELP();
      }
    }
    return PS_OK;

#ifdef _UNICODE
    case TOK_TARGETMINIMALOS:
    {
      if (build_compressor_set) {
        ERROR_MSG(_T("Error: can't change target minimal OS version after data already got compressed or header already changed!\n"));
        return PS_ERROR;
      }
      int major = 0, minor = 0;
      if (_stscanf(line.gettoken_str(1), _T("%d.%d"), &major, &minor) == 0)
        PRINTHELP()
      if (set_target_minimal_OS(major,minor) != PS_OK)   // Windows 5.0 or more recent requested? => Unicode support
      {
        ERROR_MSG(_T("Error: error while setting target minimal OS! (adequate stub not found?)\n"));
        return PS_ERROR;
      }
      SCRIPT_MSG(_T("TargetMinimalOS: %d.%d %s\n"),major,minor,build_unicode?_T("(Unicode installer)"):_T(""));
    }
    return PS_OK;
#endif

    // Ability to change compression methods from within the script
    case TOK_SETCOMPRESSOR:
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    {
      if (build_compressor_set) {
        ERROR_MSG(_T("Error: can't change compressor after data already got compressed or header already changed!\n"));
        return PS_ERROR;
      }

      if (build_compressor_final)
      {
        warning_fl(_T("SetCompressor ignored due to previous call with the /FINAL switch"));
        return PS_OK;
      }

      int a = 1;

      build_compress_whole = false;

      while (line.gettoken_str(a)[0] == _T('/'))
      {
        if (!_tcsicmp(line.gettoken_str(a),_T("/FINAL")))
        {
          build_compressor_final = true;
          a++;
        }
        else if (!_tcsicmp(line.gettoken_str(a),_T("/SOLID")))
        {
          build_compress_whole = true;
          a++;
        }
        else PRINTHELP();
      }

      if (a != line.getnumtokens() - 1)
      {
        ERROR_MSG(_T("%s expects %d parameters, got %d.\n"), line.gettoken_str(0), a + 1, line.getnumtokens());
        PRINTHELP();
      }

      int k=line.gettoken_enum(a, _T("zlib\0bzip2\0lzma\0"));
      switch (k) {
        case 0:
          compressor = &zlib_compressor;
        break;

        case 1:
          compressor = &bzip2_compressor;
          break;

        case 2:
          compressor = &lzma_compressor;
        break;

        default:
          PRINTHELP();
      }

      tstring compressor_name = line.gettoken_str(a);
      compressor_name = lowercase(compressor_name);

      if (set_compressor(compressor_name, build_compress_whole) != PS_OK)
      {
        SCRIPT_MSG(_T("SetCompressor: error loading stub for \"%s\" compressor.\n"), compressor_name.c_str());
        return PS_ERROR;
      }

      SCRIPT_MSG(_T("SetCompressor: %s%s%s\n"), build_compressor_final ? _T("/FINAL ") : _T(""), build_compress_whole ? _T("/SOLID ") : _T(""), line.gettoken_str(a));
    }
    return PS_OK;
#else//NSIS_CONFIG_COMPRESSION_SUPPORT
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_COMPRESSION_SUPPORT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_CONFIG_COMPRESSION_SUPPORT
    case TOK_LOADNLF:
    {
      SCRIPT_MSG(_T("LoadLanguageFile: %s\n"), line.gettoken_str(1));

      LanguageTable *table = LoadLangFile(line.gettoken_str(1));

      if (!table)
        return PS_ERROR;

      if (!defcodepage_set)
      {
        uDefCodePage = table->nlf.m_uCodePage;
        defcodepage_set = true;
      }

      last_used_lang = table->lang_id;
      // define LANG_LangName as "####" (lang id)
      // for example ${LANG_ENGLISH} = 1033
      TCHAR lang_id[16];
      TCHAR lang_cp[16];
      TCHAR lang_name[1024];
      wsprintf(lang_name, _T("LANG_%s"), table->nlf.m_szName);
      wsprintf(lang_id, _T("%u"), table->lang_id);
      wsprintf(lang_cp, _T("%u"), table->nlf.m_uCodePage);
      definedlist.add(lang_name, lang_id);
      wsprintf(lang_name, _T("LANG_%s_CP"), table->nlf.m_szName);
      definedlist.add(lang_name, lang_cp);
    }
    return PS_OK;

    // preprocessor-ish (ifdef/ifndef/else/endif are handled one step out from here)
    ///////////////////////////////////////////////////////////////////////////////
    case TOK_P_DEFINE:
    {
      TCHAR *define=line.gettoken_str(1);
      TCHAR *value;
      GrowBuf file_buf;
      TCHAR datebuf[256];
      TCHAR mathbuf[256];
      int dupemode=0;


      if (!_tcsicmp(define,_T("/ifndef")))
        dupemode=1;
      else if (!_tcsicmp(define,_T("/redef")))
        dupemode=2;

      if (dupemode!=0)
      {
        line.eattoken();
        define=line.gettoken_str(1);
        if (dupemode==1 && definedlist.find(define))return PS_OK;
      }


      if (!_tcsicmp(define,_T("/date")) || !_tcsicmp(define,_T("/utcdate"))) {
        if (line.getnumtokens()!=4) PRINTHELP()

        TCHAR *date_type = define;

        define=line.gettoken_str(2);
        value=line.gettoken_str(3);

        time_t rawtime;
        time(&rawtime);

        if (!_tcsicmp(date_type,_T("/utcdate")))
          rawtime = mktime(gmtime(&rawtime));

        datebuf[0]=0;
        size_t s=_tcsftime(datebuf,COUNTOF(datebuf),value,localtime(&rawtime));

        if (s == 0)
          datebuf[0]=0;
        else
          datebuf[max(s,COUNTOF(datebuf)-1)]=0;

        value=datebuf;
      } else if (!_tcsicmp(define,_T("/file")) || !_tcsicmp(define,_T("/file_noerr"))) {
        
        if (line.getnumtokens()!=4) PRINTHELP()

        define=line.gettoken_str(2);
        const TCHAR *filename=line.gettoken_str(3);
        FILE *fp=FOPENTEXT(filename,"r");

        if (!fp && _tcsicmp(define,_T("/file_noerr"))) {
          ERROR_MSG(_T("!define /file: file not found (\"%s\")\n"),filename);
          return PS_ERROR;
        }

        if (fp)  {
          TCHAR str[MAX_LINELENGTH];
          for (;;) {
            TCHAR *p=str;
            *p=0;
            _fgetts(str,MAX_LINELENGTH,fp);
            linecnt++;
            if (feof(fp)&&!str[0]) break;

            while (*p) p++;
            if (p > str) p--;
            while (p >= str && (*p == _T('\r') || *p == _T('\n'))) p--;
            *++p=0;
            if (file_buf.getlen()) file_buf.add(_T("\n"),1);
            file_buf.add(str,_tcslen(str));
          }
          fclose(fp);
        }
        file_buf.add(_T("\0"),1);
        value = (TCHAR *)file_buf.get();

      } else if (!_tcsicmp(define,_T("/math"))) {
      
        int value1;
        int value2;
        TCHAR *mathop;
        
        if (line.getnumtokens()!=6) PRINTHELP()

        define = line.gettoken_str(2);
        value1 = line.gettoken_int(3);
        mathop = line.gettoken_str(4);
        value2 = line.gettoken_int(5);
        value = mathbuf;

        if (!_tcscmp(mathop,_T("+"))) {
          _stprintf(value,_T("%d"),value1+value2);
        } else if (!_tcscmp(mathop,_T("-"))) {
          _stprintf(value,_T("%d"),value1-value2);
        } else if (!_tcscmp(mathop,_T("*"))) {
          _stprintf(value,_T("%d"),value1*value2);
        } else if (!_tcscmp(mathop,_T("&"))) {
          _stprintf(value,_T("%d"),value1&value2);
        } else if (!_tcscmp(mathop,_T("|"))) {
          _stprintf(value,_T("%d"),value1|value2);
        } else if (!_tcscmp(mathop,_T("^"))) {
          _stprintf(value,_T("%d"),value1^value2);
        } else if (!_tcscmp(mathop,_T("/"))) {
          if (value2==0) {
            ERROR_MSG(_T("!define /math: division by zero! (\"%i / %i\")\n"),value1,value2);
            return PS_ERROR;
          }
          _stprintf(value,_T("%d"),value1/value2);
        } else if (!_tcscmp(mathop,_T("%"))) {
          if (value2==0) {
            ERROR_MSG(_T("!define /math: division by zero! (\"%i %% %i\")\n"),value1,value2);
            return PS_ERROR;
          }
          _stprintf(value,_T("%d"),value1%value2);
        } else PRINTHELP()

      } else {
        if (line.getnumtokens()==4) PRINTHELP()

        value=line.gettoken_str(2);
      }

      if (dupemode==2)definedlist.del(define);
      if (definedlist.add(define,value))
      {
        ERROR_MSG(_T("!define: \"%s\" already defined!\n"),define);
        return PS_ERROR;
      }
      SCRIPT_MSG(_T("!define: \"%s\"=\"%s\"\n"),define,value);
    }
    return PS_OK;
    case TOK_P_UNDEF:
      if (definedlist.del(line.gettoken_str(1)))
      {
        ERROR_MSG(_T("!undef: \"%s\" not defined!\n"),line.gettoken_str(1));
        return PS_ERROR;
      }
      SCRIPT_MSG(_T("!undef: \"%s\"\n"),line.gettoken_str(1));
    return PS_OK;
    case TOK_P_PACKEXEHEADER:
      _tcsnccpy(build_packname,line.gettoken_str(1),COUNTOF(build_packname)-1);
      _tcsnccpy(build_packcmd,line.gettoken_str(2),COUNTOF(build_packcmd)-1);
      SCRIPT_MSG(_T("!packhdr: filename=\"%s\", command=\"%s\"\n"),
        build_packname, build_packcmd);
    return PS_OK;
    case TOK_P_FINALIZE:
      {
        TCHAR* cmdstr=line.gettoken_str(1);
        struct postbuild_cmd *newcmd, *prevcmd;
        newcmd = (struct postbuild_cmd*) new BYTE[FIELD_OFFSET(struct postbuild_cmd,cmd[_tcsclen(cmdstr)+1])];
        newcmd->next=NULL;
        _tcscpy(newcmd->cmd,cmdstr);
        for (prevcmd=postbuild_cmds; prevcmd && prevcmd->next;) prevcmd = prevcmd->next;
        if (prevcmd) prevcmd->next = newcmd; else postbuild_cmds = newcmd;
        SCRIPT_MSG(_T("!finalize: \"%s\"\n"),cmdstr);
      }
    return PS_OK;
    case TOK_P_SYSTEMEXEC:
      {
        TCHAR *exec=line.gettoken_str(1);
        int comp=line.gettoken_enum(2,_T("<\0>\0<>\0=\0ignore\0"));
        if (line.getnumtokens() == 2) comp = 4;
        if (comp == -1 && line.getnumtokens() == 3) comp=4;
        if (comp == -1) PRINTHELP()
        int success=0;
        int cmpv=line.gettoken_int(3,&success);
        if (!success && comp != 4) PRINTHELP()
        SCRIPT_MSG(_T("!system: \"%s\"\n"),exec);
#ifdef _WIN32
        int ret=sane_system(exec);
#else
        PATH_CONVERT(exec);
        int ret=system(exec);
#endif
        if (comp == 0 && ret < cmpv);
        else if (comp == 1 && ret > cmpv);
        else if (comp == 2 && ret != cmpv);
        else if (comp == 3 && ret == cmpv);
        else if (comp == 4);
        else
        {
          ERROR_MSG(_T("!system: returned %d, aborting\n"),ret);
          return PS_ERROR;
        }
        SCRIPT_MSG(_T("!system: returned %d\n"),ret);
      }
    return PS_OK;
    case TOK_P_EXECUTE:
      {
        TCHAR *exec=line.gettoken_str(1);
#ifdef _WIN32
        PROCESS_INFORMATION pi;
        STARTUPINFO si={sizeof(STARTUPINFO),};
        if (CreateProcess(NULL,exec,NULL,NULL,FALSE,0,NULL,NULL,&si,&pi))
        {
          WaitForSingleObject(pi.hProcess,INFINITE);
          CloseHandle(pi.hThread);
          CloseHandle(pi.hProcess);
        }
#else
        TCHAR *execfixed = my_convert(exec);
        system(execfixed);
        my_convert_free(execfixed);
#endif
        SCRIPT_MSG(_T("!execute: \"%s\"\n"),exec);
      }
    case TOK_P_ADDINCLUDEDIR:
      {
        TCHAR *f = line.gettoken_str(1);
        PATH_CONVERT(f);
        include_dirs.add(f,0);
      }
    return PS_OK;
    case TOK_P_INCLUDE:
      {
        bool required = true;

        TCHAR *f = line.gettoken_str(1);
        
        if(!_tcsicmp(f,_T("/nonfatal"))) {
          if (line.getnumtokens()!=3)
            PRINTHELP();
        
          f = line.gettoken_str(2);
            required = false;
        } else if (line.getnumtokens()!=2) {
          PRINTHELP();
        }
        
        TCHAR *fc = my_convert(f);
        int included = 0;

        tstring dir = get_dir_name(fc);
        tstring spec = get_file_name(fc);
        tstring basedir = dir + PLATFORM_PATH_SEPARATOR_STR;
        if (dir == spec) {
          // no path, just file name
          dir = _T(".");
          basedir = _T("");
        }

        my_convert_free(fc);

        // search working directory
        boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
        dr->read(dir);

        for (dir_reader::iterator files_itr = dr->files().begin();
             files_itr != dr->files().end();
             files_itr++)
        {
          if (!dir_reader::matches(*files_itr, spec))
            continue;

          tstring incfile = basedir + *files_itr;

          if (includeScript((TCHAR *) incfile.c_str()) != PS_OK) {
            return PS_ERROR;
          }

          included++;
        }

        if (included)
            return PS_OK;

        // search include dirs
        TCHAR *incdir = include_dirs.get();
        int incdirs = include_dirs.getnum();

        for (int i = 0; i < incdirs; i++, incdir += _tcslen(incdir) + 1) {
          tstring curincdir = tstring(incdir) + PLATFORM_PATH_SEPARATOR_STR + dir;

          boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
          dr->read(curincdir);

          for (dir_reader::iterator incdir_itr = dr->files().begin();
               incdir_itr != dr->files().end();
               incdir_itr++)
          {
            if (!dir_reader::matches(*incdir_itr, spec))
              continue;

            tstring incfile = tstring(incdir) + PLATFORM_PATH_SEPARATOR_STR + basedir + *incdir_itr;

            if (includeScript((TCHAR *) incfile.c_str()) != PS_OK) {
              return PS_ERROR;
            }

            included++;
          }

          if (included)
            return PS_OK;

        }

        // nothing found
        if (!included)
        {
          if(required) {
          ERROR_MSG(_T("!include: could not find: \"%s\"\n"),f);
          return PS_ERROR;
        } else {
          warning_fl(_T("!include: could not find: \"%s\""),f);
        }
      }
      }
    return PS_OK;
    case TOK_P_CD:
      if (!line.gettoken_str(1)[0] || _tchdir(line.gettoken_str(1)))
      {
        ERROR_MSG(_T("!cd: error changing to: \"%s\"\n"),line.gettoken_str(1));
        return PS_ERROR;
      }
    return PS_OK;
    case TOK_P_ERROR:
      ERROR_MSG(_T("!error: %s\n"),line.gettoken_str(1));
    return PS_ERROR;
    case TOK_P_WARNING:
      warning_fl(_T("!warning: %s"),line.gettoken_str(1));
    return PS_OK;
    case TOK_P_ECHO:
      SCRIPT_MSG(_T("%s (%s:%d)\n"), line.gettoken_str(1),curfilename,linecnt);
    return PS_OK;
    case TOK_P_SEARCHPARSESTRING:
      {
        bool ignCase=false;
        bool noErrors=false;
        bool isFile=false;
        int parmOffs=1;
        while (parmOffs < line.getnumtokens())
        {
          if (!_tcsicmp(line.gettoken_str(parmOffs),_T("/ignorecase"))) { ignCase=true; parmOffs++; }
          else if (!_tcsicmp(line.gettoken_str(parmOffs),_T("/noerrors"))) { noErrors=true; parmOffs++; }
          else if (!_tcsicmp(line.gettoken_str(parmOffs),_T("/file"))) { isFile=true; parmOffs++; }
          else break;
        }
        if (parmOffs+3 > line.getnumtokens())
        {
          ERROR_MSG(_T("!searchparse: not enough parameters\n"));
          return PS_ERROR;
        }

        const TCHAR *source_string = line.gettoken_str(parmOffs++);
        DefineList *list=NULL;

        if (isFile)
        {
          FILE *fp=FOPENTEXT(source_string,"r");
          if (!fp)
          {
            ERROR_MSG(_T("!searchparse /file: error opening \"%s\"\n"),source_string);
            return PS_ERROR;
          }

          int req_parm = (line.getnumtokens() - parmOffs)/2;

          GrowBuf tmpstr;
          TCHAR str[MAX_LINELENGTH];
          for (;;)
          {
            tmpstr.resize(0);
            for (;;)
            {
              str[0]=0;
              _fgetts(str,COUNTOF(str),fp);
              if (!str[0]) break; // eof

              TCHAR *p=str;
              while (*p) p++;
              if (p > str) p--;
              while (p >= str && (*p == _T('\r') || *p == _T('\n'))) p--;
              *++p=0;

              bool endSlash = (str[0] && str[_tcslen(str)-1] == _T('\\'));
              if (tmpstr.getlen() || endSlash) tmpstr.add(str,sizeof(TCHAR)*_tcslen(str));

              // if we have valid contents and not ending on slash, then done
              if (!endSlash && (str[0] || tmpstr.getlen())) break;
            }

            if (!str[0] && !tmpstr.getlen()) break; // reached eof

            TCHAR *thisline=str;
            if (tmpstr.getlen()) 
            {
              tmpstr.add(_T("\0"),sizeof(TCHAR));
              thisline=(TCHAR *)tmpstr.get();
            }

            DefineList *tlist = searchParseString(thisline,&line,parmOffs,ignCase,true);
            if (tlist && tlist->getnum())
            {
              if (!list || tlist->getnum() > list->getnum())
              {
                delete list;
                list=tlist;
                if (tlist->getnum() >= req_parm) break; // success!
              }
              else delete list;
            }
            // parse line
          }
          fclose(fp);
          if (!noErrors)
          {
            if (!list)  
            {
              ERROR_MSG(_T("!searchparse: starting string \"%s\" not found in file!\n"),line.gettoken_str(parmOffs));
              return PS_ERROR;
            }
            else if (list->getnum() < req_parm)
            {
              TCHAR *p=line.gettoken_str(parmOffs + list->getnum()*2);
              ERROR_MSG(_T("!searchparse: failed search at string \"%s\" not found in file!\n"),p?p:_T("(null)"));
              return PS_ERROR;
            }
          }
        }
        else
        {
          list=searchParseString(source_string,&line,parmOffs,ignCase,noErrors);
          if (!list && !noErrors) return PS_ERROR;
        }

        if (list) // if we got our list, merge them defines in
        {
          int i;
          for (i=0;i<list->getnum(); i ++)
          {
            TCHAR *def=list->getname(i);
            TCHAR *val=list->getvalue(i);
            if (def && val)
            {
              if (definedlist.find(def)) definedlist.del(def);
              definedlist.add(def,val);
            }
          }
        }
        delete list;
      }
    return PS_OK;
    case TOK_P_SEARCHREPLACESTRING:
      {
        int ignoreCase=!_tcsicmp(line.gettoken_str(1),_T("/ignorecase"));
        if (line.getnumtokens()!=5+ignoreCase) PRINTHELP()

        TCHAR *define=line.gettoken_str(1+ignoreCase);
        TCHAR *src = line.gettoken_str(2+ignoreCase);
        TCHAR *search = line.gettoken_str(3+ignoreCase);
        TCHAR *replace = line.gettoken_str(4+ignoreCase);
        int searchlen=_tcslen(search);
        int replacelen=_tcslen(replace);
        if (!searchlen)
        {
          ERROR_MSG(_T("!searchreplace: search string must not be empty for search/replace!\n"));
          return PS_ERROR;
        }

        GrowBuf valout;

        while (*src)
        {
          if (ignoreCase ? _tcsnicmp(src,search,searchlen) : _tcsncmp(src,search,searchlen)) 
            valout.add(src++,sizeof(TCHAR));
          else
          {
            valout.add(replace,sizeof(TCHAR)*replacelen);
            src+=searchlen;
          }
        }

        valout.add(_T(""),sizeof(TCHAR));
       
        definedlist.del(define); // allow changing variables since we'll often use this in series

        if (definedlist.add(define,(TCHAR*)valout.get()))
        {
          ERROR_MSG(_T("!searchreplace: error defining \"%s\"!\n"),define);
          return PS_ERROR;
        }
        SCRIPT_MSG(_T("!searchreplace: \"%s\"=\"%s\"\n"),define,(TCHAR*)valout.get());

      }
    return PS_OK;

    case TOK_P_VERBOSE:
    {
      extern int g_display_errors;
      for(int argi=1; argi<line.getnumtokens(); ++argi)
      {
        int v,k=line.gettoken_enum(argi,_T("push\0pop\0"));
        if (k < 0)
        {
          // just set
          int numconv;
          v=line.gettoken_int(argi,&numconv);
          if (!numconv || v < 0 || v > 4 )
          {
            // < 2.47 would reset level to 0 without warning!
            ERROR_MSG("!verbose: Invalid verbose level\n");
            return PS_ERROR;
          }
        }
        else
        {
          if (k)
          {
            // pop
            int l=verbose_stack.getlen();
            if (l)
            {
              v=((int*)verbose_stack.get())[(l/sizeof(int))-1];
              verbose_stack.resize(l-sizeof(int));
            }
            else
            {
              warning_fl("!verbose: Pop failed, stack is empty");
              continue; // Pop failed, should still process the next parameter
            }
          }
          else
          {
            // push
            v=0;
            if (display_errors)
            {
              v++;
              if (display_warnings)
              {
                v++;
                if (display_info)
                {
                  v++;
                  if (display_script)
                  {
                    v++;
                  }
                }
              }
            }
            verbose_stack.add(&v,sizeof(int));
            continue;
          }
        }
        display_script=v>3;
        display_info=v>2;
        display_warnings=v>1;
        display_errors=v>0;
        g_display_errors=display_errors;
      }
    }
    return PS_OK;

    case TOK_UNINSTALLEXENAME: PRINTHELP()


#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    case TOK_UNINSTCAPTION:
      {
        if (SetInnerString(NLF_UCAPTION,line.gettoken_str(1)) == PS_WARNING)
          warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
        SCRIPT_MSG(_T("UninstCaption: \"%s\"\n"),line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_UNINSTICON:
      SCRIPT_MSG(_T("UninstallIcon: \"%s\"\n"),line.gettoken_str(1));
      try {
        free_loaded_icon(uninstaller_icon);
        uninstaller_icon = load_icon_file(line.gettoken_str(1));
      }
      catch (exception& err) {
        ERROR_MSG(_T("Error while loading icon from \"%s\": %s\n"), line.gettoken_str(1), CtoTString(err.what()));
        return PS_ERROR;
      }
    return PS_OK;
    case TOK_UNINSTTEXT:
      {
        if (!cur_page) {
          if (SetInnerString(NLF_UNINST_TEXT, line.gettoken_str(1)) == PS_WARNING)
            warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
          SetInnerString(NLF_UNINST_SUBTEXT, line.gettoken_str(2));
        }
        else {
          if (cur_page_type != PAGE_UNINSTCONFIRM) {
            ERROR_MSG(_T("Error: UninstallText can only be used inside PageEx uninstConfirm.\n"));
            return PS_ERROR;
          }
          cur_page->parms[0] = add_string(line.gettoken_str(1));
          cur_page->parms[1] = add_string(line.gettoken_str(2));
        }
        SCRIPT_MSG(_T("UninstallText: \"%s\" \"%s\"\n"),line.gettoken_str(1),line.gettoken_str(2));
      }
    return PS_OK;
    case TOK_UNINSTSUBCAPTION:
      {
        int s;
        int w=line.gettoken_int(1,&s);
        if (!s || w < 0 || w > 2) PRINTHELP()
        SetInnerString(NLF_USUBCAPTION_CONFIRM+w,line.gettoken_str(2));
        SCRIPT_MSG(_T("UninstSubCaption: page:%d, text=%s\n"),w,line.gettoken_str(2));
      }
    return PS_OK;
    case TOK_WRITEUNINSTALLER:
    {
      if (uninstall_mode)
      {
        ERROR_MSG(_T("WriteUninstaller only valid from install, not from uninstall.\n"));
        PRINTHELP()
      }
      uninstaller_writes_used++;
      ent.which=EW_WRITEUNINSTALLER;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      tstring full = tstring(_T("$INSTDIR\\")) + tstring(line.gettoken_str(1));
      ent.offsets[3]=add_string(full.c_str());
      // ent.offsets[1] and ent.offsets[2] are set in CEXEBuild::uninstall_generate()
      if (!ent.offsets[0]) PRINTHELP()
      SCRIPT_MSG(_T("WriteUninstaller: \"%s\"\n"),line.gettoken_str(1));

      DefineInnerLangString(NLF_ERR_CREATING);
      DefineInnerLangString(NLF_CREATED_UNINST);
    }
    return add_entry(&ent);
#else//!NSIS_CONFIG_UNINSTALL_SUPPORT
    case TOK_WRITEUNINSTALLER:
    case TOK_UNINSTCAPTION:
    case TOK_UNINSTICON:
    case TOK_UNINSTTEXT:
    case TOK_UNINSTSUBCAPTION:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_UNINSTALL_SUPPORT not defined.\n"), line.gettoken_str(0));
    return PS_ERROR;
#endif



    // section/function stuff
    ///////////////////////////////////////////////////////////////////////////////

    case TOK_SECTION:
    {
      int a=1,unselected = 0;
      if (!_tcsicmp(line.gettoken_str(1),_T("/o")))
      {
        unselected = 1;
        a++;
      }
      else if (line.getnumtokens() > 3)
        PRINTHELP();
      SCRIPT_MSG(_T("Section: \"%s\""),line.gettoken_str(a));
      if (line.gettoken_str(a+1)[0]) SCRIPT_MSG(_T(" ->(%s)"),line.gettoken_str(a+1));
      SCRIPT_MSG(_T("\n"));
#ifndef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (!_tcsicmp(line.gettoken_str(a),_T("uninstall")))
      {
        ERROR_MSG(_T("Error: Uninstall section declared, no NSIS_CONFIG_UNINSTALL_SUPPORT\n"));
        return PS_ERROR;
      }
#endif

      int ret;

      ret=add_section(line.gettoken_str(a),line.gettoken_str(a+1));
      if (ret != PS_OK) return ret;

      if (unselected)
        build_cursection->flags &= ~SF_SELECTED;

      return PS_OK;
    }
    case TOK_SECTIONEND:
      SCRIPT_MSG(_T("SectionEnd\n"));
    return section_end();
    case TOK_SECTIONIN:
      {
        SCRIPT_MSG(_T("SectionIn: "));
        int wt;
        for (wt = 1; wt < line.getnumtokens(); wt ++)
        {
          TCHAR *p=line.gettoken_str(wt);
          if (!_tcsicmp(p, _T("RO")))
          {
            if (section_add_flags(SF_RO) != PS_OK) return PS_ERROR;
            SCRIPT_MSG(_T("[RO] "));
          }
          else
          {
            int x=_ttoi(p)-1;
            if (x >= 0 && x < NSIS_MAX_INST_TYPES)
            {
              if (section_add_install_type(1<<x) != PS_OK) return PS_ERROR;
              SCRIPT_MSG(_T("[%d] "),x);
            }
            else if (x < 0)
            {
              PRINTHELP()
            }
            else
            {
              ERROR_MSG(_T("Error: SectionIn section %d out of range 1-%d\n"),x+1,NSIS_MAX_INST_TYPES);
              return PS_ERROR;
            }
            p++;
          }
        }
        SCRIPT_MSG(_T("\n"));
      }
    return PS_OK;
    case TOK_SECTIONGROUPEND:
    case TOK_SUBSECTIONEND:
    case TOK_SECTIONGROUP:
    case TOK_SUBSECTION:
    {
      TCHAR buf[1024];
      int a=1,ex = 0;
      if (!_tcsicmp(line.gettoken_str(1),_T("/e")))
      {
        ex = 1;
        a++;
      }
      wsprintf(buf,_T("\x1F%s"),line.gettoken_str(a));
      if (which_token == TOK_SECTIONGROUP || which_token == TOK_SUBSECTION)
      {
        TCHAR *s = line.gettoken_str(a);
        if (!s[0] || (!_tcsicmp(s, _T("un.")) && !s[3]))
          PRINTHELP();
      }

      SCRIPT_MSG(_T("%s %s"),line.gettoken_str(0),line.gettoken_str(a));
      if (line.gettoken_str(a+1)[0]) SCRIPT_MSG(_T(" ->(%s)"),line.gettoken_str(a+1));
      SCRIPT_MSG(_T("\n"));
      return add_section(buf,line.gettoken_str(a+1),ex);
    }
    case TOK_FUNCTION:
      if (!line.gettoken_str(1)[0]) PRINTHELP()
      if (line.gettoken_str(1)[0]==_T(':') || line.gettoken_str(1)[0]==_T('/'))
      {
        ERROR_MSG(_T("Function: function name cannot begin with : or /.\n"));
        PRINTHELP()
      }
      SCRIPT_MSG(_T("Function: \"%s\"\n"),line.gettoken_str(1));
#ifndef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (!_tcsnicmp(line.gettoken_str(1),_T("un."),3))
      {
        ERROR_MSG(_T("Error: Uninstall function declared, no NSIS_CONFIG_UNINSTALL_SUPPORT\n"));
        return PS_ERROR;
      }
#endif
      return add_function(line.gettoken_str(1));
    case TOK_FUNCTIONEND:
      SCRIPT_MSG(_T("FunctionEnd\n"));
    return function_end();

    // flag setters
    ///////////////////////////////////////////////////////////////////////////////

    // BEGIN - Added by ramon 23 May 2003
    case TOK_ALLOWSKIPFILES:
      build_allowskipfiles=line.gettoken_enum(1,_T("off\0on\0"));
      if (build_allowskipfiles==-1) PRINTHELP()
      SCRIPT_MSG(_T("AllowSkipFiles: %s\n"),line.gettoken_str(1));
    return PS_OK;
    // END - Added by ramon 23 May 2003
    case TOK_SETDATESAVE:
      build_datesave=line.gettoken_enum(1,_T("off\0on\0"));
      if (build_datesave==-1) PRINTHELP()
      SCRIPT_MSG(_T("SetDateSave: %s\n"),line.gettoken_str(1));
    return PS_OK;
    case TOK_SETOVERWRITE:
    {
      int k=line.gettoken_enum(1,_T("on\0off\0try\0ifnewer\0ifdiff\0lastused\0"));
      if (k==-1) PRINTHELP()
      if (k==5)
      {
        k=build_overwrite;
        build_overwrite=build_last_overwrite;
        build_last_overwrite=k;
      }
      else
      {
        build_last_overwrite=build_overwrite;
        build_overwrite=k;
      }
      SCRIPT_MSG(_T("SetOverwrite: %s\n"),line.gettoken_str(1));
    }
    return PS_OK;
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    case TOK_SETPLUGINUNLOAD:
      build_plugin_unload=line.gettoken_enum(1,_T("manual\0alwaysoff\0"));
      if (build_plugin_unload==-1) PRINTHELP()
      SCRIPT_MSG(_T("SetPluginUnload: %s\n"),line.gettoken_str(1));
    return PS_OK;
#endif //NSIS_CONFIG_PLUGIN_SUPPORT
    case TOK_SETCOMPRESS:
      build_compress=line.gettoken_enum(1,_T("off\0auto\0force\0"));
      if (build_compress==-1) PRINTHELP()
      if (build_compress==0 && build_compress_whole)
      {
        warning_fl(_T("'SetCompress off' encountered, and in whole compression mode. Effectively ignored."));
      }
      SCRIPT_MSG(_T("SetCompress: %s\n"),line.gettoken_str(1));
    return PS_OK;
    case TOK_DBOPTIMIZE:
      build_optimize_datablock=line.gettoken_enum(1,_T("off\0on\0"));
      if (build_optimize_datablock==-1) PRINTHELP()
      SCRIPT_MSG(_T("SetDatablockOptimize: %s\n"),line.gettoken_str(1));
    return PS_OK;
    case TOK_FILEBUFSIZE:
      build_filebuflen=line.gettoken_int(1);
      build_filebuflen<<=20;
      if (build_filebuflen<=0)
      {
        ERROR_MSG(_T("Error: FileBufSize: invalid buffer size -- %d\n"),build_filebuflen);
        return PS_ERROR;
      }
      SCRIPT_MSG(_T("FileBufSize: %smb (%d bytes)\n"),line.gettoken_str(1),build_filebuflen);
    return PS_OK;
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    case TOK_SETCOMPRESSIONLEVEL:
    {
      if (compressor == &lzma_compressor)
        warning_fl(_T("SetCompressionLevel: compressor is set to LZMA. Effectively ignored."));
      if (build_compressor_set && build_compress_whole)
        warning_fl(_T("SetCompressionLevel: data already compressed in compress whole mode. Effectively ignored."));

      int s;
      build_compress_level=line.gettoken_int(1,&s);
      if (!s || build_compress_level < 0 || build_compress_level > 9) PRINTHELP();
      SCRIPT_MSG(_T("SetCompressionLevel: %u\n"), build_compress_level);
    }
    return PS_OK;
    case TOK_SETCOMPRESSORDICTSIZE:
    {
      if (compressor != &lzma_compressor)
        warning_fl(_T("SetCompressorDictSize: compressor is not set to LZMA. Effectively ignored."));
      if (build_compressor_set && build_compress_whole)
        warning_fl(_T("SetCompressorDictSize: data already compressed in compress whole mode. Effectively ignored."));

      int s;
      build_compress_dict_size=line.gettoken_int(1,&s);
      if (!s) PRINTHELP();
      SCRIPT_MSG(_T("SetCompressorDictSize: %u mb\n"), build_compress_dict_size);
      build_compress_dict_size <<= 20;
    }
    return PS_OK;
#else
    case TOK_SETCOMPRESSIONLEVEL:
    case TOK_SETCOMPRESSORDICTSIZE:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_COMPRESSION_SUPPORT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_CONFIG_COMPRESSION_SUPPORT
    case TOK_ADDSIZE:
      {
        int s;
        int size_kb=line.gettoken_int(1,&s);
        if (!s) PRINTHELP()
        SCRIPT_MSG(_T("AddSize: %d kb\n"),size_kb);
        section_add_size_kb(size_kb);
      }
    return PS_OK;
    case TOK_SUBCAPTION:
      {
        int s;
        int w=line.gettoken_int(1,&s);
        if (!s || w < 0 || w > 4) PRINTHELP()
        SetInnerString(NLF_SUBCAPTION_LICENSE+w,line.gettoken_str(2));
        SCRIPT_MSG(_T("SubCaption: page:%d, text=%s\n"),w,line.gettoken_str(2));
      }
    return PS_OK;
    case TOK_FILEERRORTEXT:
#ifdef NSIS_SUPPORT_FILE
      {
        SetInnerString(NLF_FILE_ERROR,line.gettoken_str(1));
        SetInnerString(NLF_FILE_ERROR_NOIGNORE,line.gettoken_str(2));
        SCRIPT_MSG(_T("FileErrorText: \"%s\" \"%s\"\n"),line.gettoken_str(1),line.gettoken_str(2));
      }
    return PS_OK;
#else
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_FILE not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif
    case TOK_BRANDINGTEXT:
      {
        int a = 1;
        int trim = 0;
        while (line.gettoken_str(a)[0] == _T('/')) {
          if (!_tcsnicmp(line.gettoken_str(a),_T("/TRIM"),5)) {
            if (!_tcsicmp(line.gettoken_str(a)+5,_T("LEFT"))) trim = 1;
            else if (!_tcsicmp(line.gettoken_str(a)+5,_T("RIGHT"))) trim = 2;
            else if (!_tcsicmp(line.gettoken_str(a)+5,_T("CENTER"))) trim = 3;
            else PRINTHELP();
            a++;
          }
          else break;
        }
        if (line.getnumtokens()!=a+1 && !trim) PRINTHELP();
        if (line.getnumtokens()==a+1)
          SetInnerString(NLF_BRANDING,line.gettoken_str(a));
#ifdef _WIN32
        if (trim) try {
          init_res_editor();

          BYTE* dlg = res_editor->GetResource(RT_DIALOG, IDD_INST, NSIS_DEFAULT_LANG);
          CDialogTemplate td(dlg,build_unicode,uDefCodePage);
          free(dlg);

          if (trim) {
            TCHAR str[512];
            if (line.getnumtokens()==a+1 && line.gettoken_str(a)[0])
              _tcscpy(str, line.gettoken_str(a));
            else
              wsprintf(str, _T("Nullsoft Install System %s"), NSIS_VERSION);

            short old_width = td.GetItem(IDC_VERSTR)->sWidth;

            switch (trim) {
              case 1: td.LTrimToString(IDC_VERSTR, str, 4); break;
              case 2: td.RTrimToString(IDC_VERSTR, str, 4); break;
              case 3: td.CTrimToString(IDC_VERSTR, str, 4); break;
            }

            if (td.GetItem(IDC_VERSTR)->sWidth > old_width)
            {
              warning_fl(_T("BrandingText: \"%s\" is too long, trimming has expanded the label"), str);
            }
          }

          DWORD dwSize;
          dlg = td.Save(dwSize);
          res_editor->UpdateResource(RT_DIALOG, IDD_INST, NSIS_DEFAULT_LANG, dlg, dwSize);
          res_editor->FreeResource(dlg);
        }
        catch (exception& err) {
          ERROR_MSG(_T("Error while triming branding text control: %s\n"), CtoTString(err.what()));
          return PS_ERROR;
        }
#else
        if (trim)
        {
          ERROR_MSG(_T("Error: BrandingText /TRIM* is disabled for non Win32 platforms.\n"));
          return PS_ERROR;
        }
#endif
        SCRIPT_MSG(_T("BrandingText: \"%s\"\n"),line.gettoken_str(a));
      }
    return PS_OK;
    case TOK_MISCBUTTONTEXT:
      {
        SetInnerString(NLF_BTN_BACK,line.gettoken_str(1));
        SetInnerString(NLF_BTN_NEXT,line.gettoken_str(2));
        SetInnerString(NLF_BTN_CANCEL,line.gettoken_str(3));
        SetInnerString(NLF_BTN_CLOSE,line.gettoken_str(4));
        SCRIPT_MSG(_T("MiscButtonText: back=\"%s\" next=\"%s\" cancel=\"%s\" close=\"%s\"\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
      }
    return PS_OK;
    case TOK_SPACETEXTS:
      {
        if (!_tcsicmp(line.gettoken_str(1), _T("none"))) {
          no_space_texts=true;
          SCRIPT_MSG(_T("SpaceTexts: none\n"));
        }
        else {
          no_space_texts=false;
          SetInnerString(NLF_SPACE_REQ,line.gettoken_str(1));
          SetInnerString(NLF_SPACE_AVAIL,line.gettoken_str(2));
          SCRIPT_MSG(_T("SpaceTexts: required=\"%s\" available=\"%s\"\n"),line.gettoken_str(1),line.gettoken_str(2));
        }
      }
    return PS_OK;
    case TOK_INSTBUTTONTEXT:
      {
        SetInnerString(NLF_BTN_INSTALL,line.gettoken_str(1));
        SCRIPT_MSG(_T("InstallButtonText: \"%s\"\n"),line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_DETAILSBUTTONTEXT:
      {
        if (!cur_page) {
          if (SetInnerString(NLF_BTN_DETAILS,line.gettoken_str(1)) == PS_WARNING)
            warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
        }
        else {
          if (cur_page_type != PAGE_INSTFILES) {
            ERROR_MSG(_T("Error: DetailsButtonText can only be used inside PageEx instfiles.\n"));
            return PS_ERROR;
          }
          cur_page->parms[1] = add_string(line.gettoken_str(1));
        }
        SCRIPT_MSG(_T("DetailsButtonText: \"%s\"\n"),line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_COMPLETEDTEXT:
      {
        if (!cur_page) {
          if (SetInnerString(NLF_COMPLETED,line.gettoken_str(1)) == PS_WARNING)
            warning_fl(_T("%s: specified multiple times, wasting space"),line.gettoken_str(0));
        }
        else {
          if (cur_page_type != PAGE_INSTFILES) {
            ERROR_MSG(_T("Error: CompletedText can only be used inside PageEx instfiles.\n"));
            return PS_ERROR;
          }
          cur_page->parms[2] = add_string(line.gettoken_str(1));
        }
        SCRIPT_MSG(_T("CompletedText: \"%s\"\n"),line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_UNINSTBUTTONTEXT:
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      {
        SetInnerString(NLF_BTN_UNINSTALL,line.gettoken_str(1));
        SCRIPT_MSG(_T("UninstButtonText: \"%s\"\n"),line.gettoken_str(1));
      }
    return PS_OK;
#else
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_UNINSTALL_SUPPORT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif

    // instructions
    ///////////////////////////////////////////////////////////////////////////////
    case TOK_NOP:
      SCRIPT_MSG(_T("Nop\n"));
      ent.which=EW_NOP;
    return add_entry(&ent);
    case TOK_GOTO:
      ent.which=EW_NOP;
      if (process_jump(line,1,&ent.offsets[0])) PRINTHELP()
      SCRIPT_MSG(_T("Goto: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_SETREGVIEW:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(alter_reg_view);
      // "64" results in setting the flag to 1 which alters the view
      int k=line.gettoken_enum(1,_T("32\0") _T("64\0lastused\0"));
      if (k<0) PRINTHELP()
      if (k == 0) // 32
        ent.offsets[1]=add_intstring(0);
      else if (k == 1) // 64
        ent.offsets[1]=add_intstring(KEY_WOW64_64KEY);
      else if (k == 2) // last used
        ent.offsets[2]=1;
      SCRIPT_MSG(_T("SetRegView: %s\n"),line.gettoken_str(1));
    }
    return add_entry(&ent);
    case TOK_SETSHELLVARCONTEXT:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(all_user_var);
      int k=line.gettoken_enum(1,_T("current\0all\0"));
      if (k<0) PRINTHELP()
      ent.offsets[1]=add_intstring(k);
      SCRIPT_MSG(_T("SetShellVarContext: %s\n"),line.gettoken_str(1));
    }
    return add_entry(&ent);
    case TOK_RET:
      SCRIPT_MSG(_T("Return\n"));
      ent.which=EW_RET;
    return add_entry(&ent);
    case TOK_CALL:
      if (!line.gettoken_str(1)[0] || (line.gettoken_str(1)[0]==_T(':') && !line.gettoken_str(1)[1] )) PRINTHELP()
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (uninstall_mode && _tcsnicmp(line.gettoken_str(1),_T("un."),3)
          && (GetUserVarIndex(line,1) < 0) && line.gettoken_str(1)[0]!=_T(':'))
      {
        ERROR_MSG(_T("Call must be used with function names starting with \"un.\" in the uninstall section.\n"));
        PRINTHELP()
      }
      if (!uninstall_mode && !_tcsnicmp(line.gettoken_str(1),_T("un."),3))
      {
        ERROR_MSG(_T("Call must not be used with functions starting with \"un.\" in the non-uninstall sections.\n"));
        PRINTHELP()
      }
#endif
      ent.which=EW_CALL;
      ent.offsets[1]=0;
      {
        int v;
        if ((v=GetUserVarIndex(line, 1))>=0)
        {
          ent.offsets[0]=-v-2;
        }
        else
        {
          if (line.gettoken_str(1)[0] == _T(':'))
          {
            ent.offsets[1]=1;
            ent.offsets[0]=ns_label.add(line.gettoken_str(1)+1,0);
          }
          else ent.offsets[0]=ns_func.add(line.gettoken_str(1),0);
        }
      }
      SCRIPT_MSG(_T("Call \"%s\"\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_SETOUTPATH:
      {
        const TCHAR *op=line.gettoken_str(1);
        if (!_tcscmp(op,_T("-")))
        {
          op=_T("$INSTDIR");
        }
        SCRIPT_MSG(_T("SetOutPath: \"%s\"\n"),op);
        ent.which=EW_CREATEDIR;
        ent.offsets[0]=add_string(op);
        ent.offsets[1]=1;

        DefineInnerLangString(NLF_OUTPUT_DIR);
      }
    return add_entry(&ent);
    case TOK_CREATEDIR:
      {
        TCHAR out_path[1024];
        TCHAR *p=line.gettoken_str(1);
        if (*p == _T('-')) out_path[0]=0;
        else
        {
          if (p[0] == _T('\\') && p[1] != _T('\\')) p++;
          _tcsnccpy(out_path,p,1024-1);
          if (*CharPrev(out_path,out_path+_tcslen(out_path))==_T('\\'))
            *CharPrev(out_path,out_path+_tcslen(out_path))=0; // remove trailing slash
        }
        if (!*out_path) PRINTHELP()
        SCRIPT_MSG(_T("CreateDirectory: \"%s\"\n"),out_path);
        ent.which=EW_CREATEDIR;
        ent.offsets[0]=add_string(out_path);

        DefineInnerLangString(NLF_CREATE_DIR);
      }
    return add_entry(&ent);
    case TOK_EXEC:
    case TOK_EXECWAIT:
#ifdef NSIS_SUPPORT_EXECUTE
      ent.which=EW_EXECUTE;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[2]=0;
      if (which_token == TOK_EXECWAIT)
      {
        ent.offsets[2]=1;
        ent.offsets[1]=GetUserVarIndex(line, 2);
        if (line.gettoken_str(2)[0] && ent.offsets[1]<0) PRINTHELP()
      }
      SCRIPT_MSG(_T("%s: \"%s\" (->%s)\n"),ent.offsets[2]?_T("ExecWait"):_T("Exec"),line.gettoken_str(1),line.gettoken_str(2));

      DefineInnerLangString(NLF_EXEC);
    return add_entry(&ent);
#else//!NSIS_SUPPORT_EXECUTE
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_EXECUTE not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_EXECUTE
    case TOK_EXECSHELL: // this uses improvements of Andras Varga
#ifdef NSIS_SUPPORT_SHELLEXECUTE
    {
      ent.which=EW_SHELLEXEC;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=add_string(line.gettoken_str(3));
      ent.offsets[3]=SW_SHOWNORMAL;
      if (line.getnumtokens() > 4)
      {
        int tab[5]={SW_SHOWDEFAULT,SW_SHOWNORMAL,SW_SHOWMAXIMIZED,SW_SHOWMINIMIZED,SW_HIDE};
        int a=line.gettoken_enum(4,_T("SW_SHOWDEFAULT\0SW_SHOWNORMAL\0SW_SHOWMAXIMIZED\0SW_SHOWMINIMIZED\0SW_HIDE\0"));
        if (a < 0) PRINTHELP()
        ent.offsets[3]=tab[a];
      }
      tstring detail=tstring(line.gettoken_str(1))+_T(" ")+tstring(line.gettoken_str(2));
      ent.offsets[5]=add_string(detail.c_str());
      SCRIPT_MSG(_T("ExecShell: %s: \"%s\" \"%s\" %s\n"),line.gettoken_str(1),line.gettoken_str(2),
                                                 line.gettoken_str(3),line.gettoken_str(4));

      DefineInnerLangString(NLF_EXEC_SHELL);
    }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_SHELLEXECUTE
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_SHELLEXECUTE not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_SHELLEXECUTE
    case TOK_CALLINSTDLL:
    case TOK_REGDLL:
    case TOK_UNREGDLL:
#ifndef NSIS_SUPPORT_ACTIVEXREG
      ERROR_MSG(_T("%s: support not compiled in (NSIS_SUPPORT_ACTIVEXREG)\n"),line.gettoken_str(0));
      return PS_ERROR;
#else//NSIS_SUPPORT_ACTIVEXREG
      ent.which=EW_REGISTERDLL;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      if (which_token == TOK_UNREGDLL)
      {
        ent.offsets[1]=add_string(_T("DllUnregisterServer"));
        ent.offsets[2]=DefineInnerLangString(NLF_UNREGISTERING);
      }
      else if (which_token == TOK_CALLINSTDLL)
      {
        int a = 2;
        if (!_tcsicmp(line.gettoken_str(a), _T("/NOUNLOAD"))) {
          ent.offsets[3]=1;
          a++;
        }
        if (a+1 != line.getnumtokens()) PRINTHELP();
        ent.offsets[1]=add_string(line.gettoken_str(a));
        if (!ent.offsets[1]) PRINTHELP()
        ent.offsets[2]=0;
      }
      else // register
      {
        ent.offsets[1] = add_string(line.gettoken_str(2));
        if (!ent.offsets[1]) ent.offsets[1]=add_string(_T("DllRegisterServer"));
        ent.offsets[2]=DefineInnerLangString(NLF_REGISTERING);
      }

      SCRIPT_MSG(_T("%s: \"%s\" %s\n"),line.gettoken_str(0),line.gettoken_str(1), line.gettoken_str(ent.offsets[3]?3:2));

      DefineInnerLangString(NLF_SYMBOL_NOT_FOUND);
      DefineInnerLangString(NLF_COULD_NOT_LOAD);
      DefineInnerLangString(NLF_NO_OLE);
      // not used anywhere - DefineInnerLangString(NLF_ERR_REG_DLL);
    return add_entry(&ent);
#endif//NSIS_SUPPORT_ACTIVEXREG
    case TOK_RENAME:
#ifdef NSIS_SUPPORT_RENAME
      {
        int a=1;
        ent.which=EW_RENAME;
        if (!_tcsicmp(line.gettoken_str(1),_T("/REBOOTOK")))
        {
          ent.offsets[2]=1;
          a++;
#ifndef NSIS_SUPPORT_MOVEONREBOOT
          ERROR_MSG(_T("Error: /REBOOTOK specified, NSIS_SUPPORT_MOVEONREBOOT not defined\n"));
          PRINTHELP()
#endif
        }
        else if (line.gettoken_str(1)[0]==_T('/'))
        {
          a=line.getnumtokens(); // cause usage to go here:
        }
        if (line.getnumtokens()!=a+2) PRINTHELP()
        ent.offsets[0]=add_string(line.gettoken_str(a));
        ent.offsets[1]=add_string(line.gettoken_str(a+1));
        tstring print = tstring(line.gettoken_str(a)) + _T("->") + tstring(line.gettoken_str(a+1));
        ent.offsets[3]=add_string(print.c_str());
        SCRIPT_MSG(_T("Rename: %s%s->%s\n"),ent.offsets[2]?_T("/REBOOTOK "):_T(""),line.gettoken_str(a),line.gettoken_str(a+1));

        DefineInnerLangString(NLF_RENAME);
#ifdef NSIS_SUPPORT_MOVEONREBOOT
        DefineInnerLangString(NLF_RENAME_ON_REBOOT);
#endif
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_RENAME
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_RENAME not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_RENAME
    case TOK_MESSAGEBOX:
#ifdef NSIS_SUPPORT_MESSAGEBOX
      {
        #define MBD(x) {x,_T(#x)},
        struct
        {
          int id;
          const TCHAR *str;
        } list[]=
        {
          MBD(MB_ABORTRETRYIGNORE)
          MBD(MB_OK)
          MBD(MB_OKCANCEL)
          MBD(MB_RETRYCANCEL)
          MBD(MB_YESNO)
          MBD(MB_YESNOCANCEL)
          MBD(MB_ICONEXCLAMATION)
          MBD(MB_ICONINFORMATION)
          MBD(MB_ICONQUESTION)
          MBD(MB_ICONSTOP)
          MBD(MB_USERICON)
          MBD(MB_TOPMOST)
          MBD(MB_SETFOREGROUND)
          MBD(MB_RIGHT)
          MBD(MB_RTLREADING)
          MBD(MB_DEFBUTTON1)
          MBD(MB_DEFBUTTON2)
          MBD(MB_DEFBUTTON3)
          MBD(MB_DEFBUTTON4)
        };
        #undef MBD
        int r=0;
        int x;
        TCHAR *p=line.gettoken_str(1);

        while (*p)
        {
          TCHAR *np=p;
          while (*np && *np != _T('|')) np++;
          if (*np) *np++=0;
          for (x = 0 ; (unsigned) x < COUNTOF(list) && _tcsicmp(list[x].str, p); x++);
          if ((unsigned) x < COUNTOF(list))
          {
            r|=list[x].id;
          }
          else PRINTHELP()
          p=np;
        }
        ent.which=EW_MESSAGEBOX;
        ent.offsets[0]=r;
        ent.offsets[1]=add_string(line.gettoken_str(2));
        int rettab[] =
        {
          0,IDABORT,IDCANCEL,IDIGNORE,IDNO,IDOK,IDRETRY,IDYES
        };
        const TCHAR *retstr=_T("0\0IDABORT\0IDCANCEL\0IDIGNORE\0IDNO\0IDOK\0IDRETRY\0IDYES\0");
        int a=3;
        if (line.getnumtokens() > 3)
        {
          if (!_tcsicmp(line.gettoken_str(3),_T("/SD")))
          {
            int k=line.gettoken_enum(4,retstr);
            if (k <= 0) PRINTHELP();
            ent.offsets[0]|=rettab[k]<<21;
            a=5;
          }
          else if (line.getnumtokens() > 7)
            PRINTHELP();

          if (line.getnumtokens() > a)
          {
            ent.offsets[2]=line.gettoken_enum(a,retstr);
            if (ent.offsets[2] < 0)
              PRINTHELP();
            ent.offsets[2] = rettab[ent.offsets[2]];
            if (process_jump(line,a+1,&ent.offsets[3]))
              PRINTHELP();
            if (line.getnumtokens() > a+2)
            {
              int v=line.gettoken_enum(a+2,retstr);
              if (v < 0)
                PRINTHELP();
              ent.offsets[4] = rettab[v];
              if (process_jump(line,a+3,&ent.offsets[5]))
                PRINTHELP();
            }
          }
        }
        SCRIPT_MSG(_T("MessageBox: %d: \"%s\""),r,line.gettoken_str(2));
        if (line.getnumtokens()>a+1) SCRIPT_MSG(_T(" (on %s goto %s)"),line.gettoken_str(a),line.gettoken_str(a+1));
        SCRIPT_MSG(_T("\n"));
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_MESSAGEBOX
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_MESSAGEBOX not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_MESSAGEBOX
    case TOK_CREATESHORTCUT:
#ifdef NSIS_SUPPORT_CREATESHORTCUT
      ent.which=EW_CREATESHORTCUT;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=add_string(line.gettoken_str(3));
      ent.offsets[3]=add_string(line.gettoken_str(4));
      ent.offsets[5]=add_string(line.gettoken_str(8));
      int s;
      ent.offsets[4]=line.gettoken_int(5,&s)&0xff;
      if (!s)
      {
        if (line.getnumtokens() > 5 && *line.gettoken_str(5))
        {
          ERROR_MSG(_T("CreateShortCut: cannot interpret icon index\n"));
          PRINTHELP()
        }
        ent.offsets[4]=0;
      }
      if (line.getnumtokens() > 6 && *line.gettoken_str(6))
      {
        int tab[3]={SW_SHOWNORMAL,SW_SHOWMAXIMIZED,SW_SHOWMINNOACTIVE/*SW_SHOWMINIMIZED doesn't work*/};
        int a=line.gettoken_enum(6,_T("SW_SHOWNORMAL\0SW_SHOWMAXIMIZED\0SW_SHOWMINIMIZED\0"));
        if (a < 0)
        {
          ERROR_MSG(_T("CreateShortCut: unknown show mode \"%s\"\n"),line.gettoken_str(6));
          PRINTHELP()
        }
        ent.offsets[4]|=tab[a]<<8;
      }
      if (line.getnumtokens() > 7)
      {
        TCHAR *s=(line.gettoken_str(7));

        TCHAR b[255];
        for (unsigned int spos=0; (spos <= _tcslen(s)) && (spos <= 255); spos++)
          b[spos]=_totupper(*(s+spos));
        _tcscpy(s,b);

        if (*s)
        {
          int c=0;
          if (_tcsstr(s,_T("ALT|"))) ent.offsets[4]|=HOTKEYF_ALT << 24;
          if (_tcsstr(s,_T("CONTROL|"))) ent.offsets[4]|=HOTKEYF_CONTROL << 24;
          if (_tcsstr(s,_T("EXT|"))) ent.offsets[4]|=HOTKEYF_EXT << 24;
          if (_tcsstr(s,_T("SHIFT|"))) ent.offsets[4]|=HOTKEYF_SHIFT << 24;
          while (_tcsstr(s,_T("|")))
          {
            s=_tcsstr(s,_T("|"))+1;
          }
          if ((s[0] == _T('F')) && (s[1] >= _T('1') && s[1] <= _T('9')))
          {
            c=VK_F1-1+_ttoi(s+1);
            if (_ttoi(s+1) < 1 || _ttoi(s+1) > 24)
            {
              warning_fl(_T("CreateShortCut: F-key \"%s\" out of range"),s);
            }
          }
          else if (((s[0] >= _T('A') && s[0] <= _T('Z')) || (s[0] >= _T('0') && s[0] <= _T('9'))) && !s[1])
            c=s[0];
          else
          {
            c=s[0];
            warning_fl(_T("CreateShortCut: unrecognized hotkey \"%s\""),s);
          }
          ent.offsets[4] |= (c) << 16;
        }
      }
      SCRIPT_MSG(_T("CreateShortCut: \"%s\"->\"%s\" %s icon:%s,%d, showmode=0x%X, hotkey=0x%X, comment=%s\n"),
        line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),
        line.gettoken_str(4),ent.offsets[4]&0xff,(ent.offsets[4]>>8)&0xff,ent.offsets[4]>>16,line.gettoken_str(8));

      DefineInnerLangString(NLF_CREATE_SHORTCUT);
      DefineInnerLangString(NLF_ERR_CREATING_SHORTCUT);
    return add_entry(&ent);
#else//!NSIS_SUPPORT_CREATESHORTCUT
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_CREATESHORTCUT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_SUPPORT_CREATESHORTCUT
#ifdef NSIS_SUPPORT_HWNDS
    case TOK_FINDWINDOW:
      ent.which=EW_FINDWINDOW;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0) PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=add_string(line.gettoken_str(3));
      ent.offsets[3]=add_string(line.gettoken_str(4));
      ent.offsets[4]=add_string(line.gettoken_str(5));
      SCRIPT_MSG(_T("FindWindow: output=%s, class=\"%s\", text=\"%s\" hwndparent=\"%s\" hwndafter=\"%s\"\n"),
        line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4),line.gettoken_str(5));
    return add_entry(&ent);
    case TOK_SENDMESSAGE:
      ent.which=EW_SENDMESSAGE;

      if (line.gettoken_str(1)[0] == _T('/') || line.gettoken_str(2)[0] == _T('/') ||
          line.gettoken_str(3)[0] == _T('/') || line.gettoken_str(4)[0] == _T('/'))
      {
        PRINTHELP()
      }

      SCRIPT_MSG(_T("SendMessage:"));
      {
        int a=5;
        ent.offsets[0]=GetUserVarIndex(line, 5);
        if (ent.offsets[0]>=0)
        {
          SCRIPT_MSG(_T("(->%s)"),line.gettoken_str(5));
          a++;
        }

        if (!_tcsncmp(line.gettoken_str(a),_T("/TIMEOUT="),9))
        {
          ent.offsets[5]|=_ttoi(line.gettoken_str(a)+9)<<2;
          SCRIPT_MSG(_T(" (timeout=%d)"),ent.offsets[5]>>2);
          a++;
        }

        if (line.getnumtokens()>a)
        {
          PRINTHELP()
        }
      }

      if (!_tcsncmp(line.gettoken_str(3),_T("STR:"),4))
      {
        ent.offsets[5]|=1;
        ent.offsets[3]=add_string(line.gettoken_str(3)+4);
      }
      else ent.offsets[3]=add_string(line.gettoken_str(3));
      if (!_tcsncmp(line.gettoken_str(4),_T("STR:"),4))
      {
        ent.offsets[5]|=2;
        ent.offsets[4]=add_string(line.gettoken_str(4)+4);
      }
      else ent.offsets[4]=add_string(line.gettoken_str(4));

      ent.offsets[1]=add_string(line.gettoken_str(1));
      ent.offsets[2]=add_string(line.gettoken_str(2));

      SCRIPT_MSG(_T("(%s,%s,%s,%s)\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
    return add_entry(&ent);
    case TOK_ISWINDOW:
      ent.which=EW_ISWINDOW;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      if (process_jump(line,2,&ent.offsets[1])||
          process_jump(line,3,&ent.offsets[2])) PRINTHELP()
      SCRIPT_MSG(_T("IsWindow(%s): %s:%s\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
    case TOK_GETDLGITEM:
      ent.which=EW_GETDLGITEM;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0]<0) PRINTHELP();
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=add_string(line.gettoken_str(3));
      SCRIPT_MSG(_T("GetDlgItem: output=%s dialog=%s item=%s\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
    case TOK_SETCTLCOLORS:
    {
      ctlcolors c={0, };

      ent.which=EW_SETCTLCOLORS;
      ent.offsets[0]=add_string(line.gettoken_str(1));

      int a = 2;

      if (!_tcsicmp(line.gettoken_str(2),_T("/BRANDING")))
        a++;

      {
        TCHAR *p;

        if (a == 2 && line.getnumtokens() == 5) {
          ERROR_MSG(_T("Error: SetCtlColors expected 3 parameters, got 4\n"));
          return PS_ERROR;
        }

        if (!_tcsicmp(line.gettoken_str(a+1),_T("transparent"))) {
          c.flags|=CC_BKB;
          c.lbStyle=BS_NULL;
          c.bkmode=TRANSPARENT;
        }
        else {
          p=line.gettoken_str(a+1);
          if (*p) {
            int v=_tcstoul(p,&p,16);
            c.bkc=((v&0xff)<<16)|(v&0xff00)|((v&0xff0000)>>16);
            c.flags|=CC_BK|CC_BKB;
          }

          c.lbStyle=BS_SOLID;
          c.bkmode=OPAQUE;
        }

        p=line.gettoken_str(a);
        if (*p) {
          int v=_tcstoul(p,&p,16);
          c.text=((v&0xff)<<16)|(v&0xff00)|((v&0xff0000)>>16);
          c.flags|=CC_TEXT;
        }
      }

      if (a == 3)
      {
        c.flags|=CC_BK|CC_BKB;
        c.lbStyle=BS_NULL;
        if (!*line.gettoken_str(a+1))
        {
          c.bkc=COLOR_BTNFACE;
          c.flags|=CC_BK_SYS;
        }
        c.flags|=CC_TEXT;
        if (!*line.gettoken_str(a))
        {
          c.text=COLOR_BTNFACE;
          c.flags|=CC_TEXT_SYS;
        }
        c.bkmode=OPAQUE;
      }

      int i;
      int l=cur_ctlcolors->getlen()/sizeof(ctlcolors);
      for (i=0; i<l; i++) {
        if (!memcmp((ctlcolors*)cur_ctlcolors->get()+i,&c,sizeof(ctlcolors))) {
          ent.offsets[1]=i*sizeof(ctlcolors);
          break;
        }
      }
      if (i>=l) {
        ent.offsets[1]=cur_ctlcolors->add(&c,sizeof(ctlcolors));
      }

      SCRIPT_MSG(_T("SetCtlColors: hwnd=%s %stext=%s background=%s\n"),line.gettoken_str(1),a==2?_T(""):_T("/BRANDING "),line.gettoken_str(a),line.gettoken_str(a+1));
    }
    return add_entry(&ent);
    case TOK_CREATEFONT:
      ent.which=EW_CREATEFONT;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0) PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(2));
      SCRIPT_MSG(_T("CreateFont: output=%s \"%s\""),line.gettoken_str(1),line.gettoken_str(2));
      {
        int height=0;
        int weight=0;
        int flags=0;
        for (int i = 3; i < line.getnumtokens(); i++) {
          TCHAR *tok=line.gettoken_str(i);
          if (tok[0]=='/') {
            if (!_tcsicmp(tok,_T("/ITALIC"))) {
              SCRIPT_MSG(_T(" /ITALIC"));
              flags|=1;
            }
            else if (!_tcsicmp(tok,_T("/UNDERLINE"))) {
              SCRIPT_MSG(_T(" /UNDERLINE"));
              flags|=2;
            }
            else if (!_tcsicmp(tok,_T("/STRIKE"))) {
              SCRIPT_MSG(_T(" /STRIKE"));
              flags|=4;
            }
            else {
              SCRIPT_MSG(_T("\n"));
              PRINTHELP();
            }
          }
          else {
            if (!height) {
              SCRIPT_MSG(_T(" height=%s"),tok);
              height=add_string(tok);
            }
            else if (!weight) {
              SCRIPT_MSG(_T(" weight=%s"),tok);
              weight=add_string(tok);
            }
            else {
              SCRIPT_MSG(_T("\n"));
              PRINTHELP();
            }
          }
        }
        ent.offsets[2]=height;
        ent.offsets[3]=weight;
        ent.offsets[4]=flags;
      }
      SCRIPT_MSG(_T("\n"));
    return add_entry(&ent);
    case TOK_ENABLEWINDOW:
      ent.which=EW_SHOWWINDOW;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[3]=1;
      SCRIPT_MSG(_T("EnableWindow: handle=%s enable=%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SHOWWINDOW:
      ent.which=EW_SHOWWINDOW;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      SCRIPT_MSG(_T("ShowWindow: handle=%s show state=%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_HIDEWINDOW:
      ent.which=EW_SHOWWINDOW;
      ent.offsets[0]=add_string(_T("$HWNDPARENT"));
      ent.offsets[1]=add_string(_T("0")/*SW_HIDE*/);
      ent.offsets[2]=1;
      SCRIPT_MSG(_T("HideWindow\n"));
    return add_entry(&ent);
    case TOK_BRINGTOFRONT:
    {
      int ret;
      ent.which=EW_SHOWWINDOW;
      ent.offsets[0]=add_string(_T("$HWNDPARENT"));
      ent.offsets[1]=add_string(_T("5")/*SW_SHOW*/);
      ret = add_entry(&ent);
      if (ret != PS_OK) return ret;
      ent.which=EW_BRINGTOFRONT;
      ent.offsets[0]=0;
      ent.offsets[1]=0;
      SCRIPT_MSG(_T("BringToFront\n"));
    }
    return add_entry(&ent);
#else//NSIS_CONFIG_ENHANCEDUI_SUPPORT
    case TOK_GETDLGITEM:
    case TOK_SETCTLCOLORS:
    case TOK_SHOWWINDOW:
    case TOK_BRINGTOFRONT:
    case TOK_CREATEFONT:
    case TOK_HIDEWINDOW:
    case TOK_ENABLEWINDOW:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_ENHANCEDUI_SUPPORT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_CONFIG_ENHANCEDUI_SUPPORT
#else//!NSIS_SUPPORT_HWNDS
    case TOK_ISWINDOW:
    case TOK_SENDMESSAGE:
    case TOK_FINDWINDOW:
    case TOK_GETDLGITEM:
    case TOK_SETCTLCOLORS:
    case TOK_SHOWWINDOW:
    case TOK_ENABLEWINDOW:
    case TOK_CREATEFONT:
    case TOK_HIDEWINDOW:
    case TOK_BRINGTOFRONT:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_HWNDS not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_HWNDS
    case TOK_DELETE:
#ifdef NSIS_SUPPORT_DELETE
      {
        int a=1;
        ent.which=EW_DELETEFILE;
        if (!_tcsicmp(line.gettoken_str(a),_T("/REBOOTOK")))
        {
          a++;
          ent.offsets[1]=DEL_REBOOT;
#ifndef NSIS_SUPPORT_MOVEONREBOOT
          ERROR_MSG(_T("Error: /REBOOTOK specified, NSIS_SUPPORT_MOVEONREBOOT not defined\n"));
          PRINTHELP()
#endif
        }
        else if (line.gettoken_str(1)[0]==_T('/'))
        {
          a=line.getnumtokens();
        }
        if (line.getnumtokens() != a+1) PRINTHELP()
        ent.offsets[0]=add_string(line.gettoken_str(a));
        SCRIPT_MSG(_T("Delete: %s\"%s\"\n"),ent.offsets[1]?_T("/REBOOTOK "):_T(""),line.gettoken_str(a));

        DefineInnerLangString(NLF_DEL_FILE);
#ifdef NSIS_SUPPORT_MOVEONREBOOT
        DefineInnerLangString(NLF_DEL_ON_REBOOT);
#endif
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_DELETE
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_DELETE not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_DELETE
    case TOK_RMDIR:
#ifdef NSIS_SUPPORT_RMDIR
      {
        int a=1;
        ent.which=EW_RMDIR;
        ent.offsets[1]=DEL_DIR;
        while (line.gettoken_str(a)[0]==_T('/'))
        {
          if (!_tcsicmp(line.gettoken_str(a),_T("/r")))
          {
            if (a == 3) PRINTHELP();
            a++;
            ent.offsets[1]|=DEL_RECURSE;
          }
          else if (!_tcsicmp(line.gettoken_str(a),_T("/REBOOTOK")))
          {
            if (a == 3) PRINTHELP();
            a++;
            ent.offsets[1]|=DEL_REBOOT;
          }
          else PRINTHELP();
        }
        if (a < line.getnumtokens() - 1) PRINTHELP();
        ent.offsets[0]=add_string(line.gettoken_str(a));
        SCRIPT_MSG(_T("RMDir: "));
        if (a>1)
          SCRIPT_MSG(_T("%s "),line.gettoken_str(1));
        if (a>2)
          SCRIPT_MSG(_T("%s "),line.gettoken_str(2));
        SCRIPT_MSG(_T("\"%s\"\n"),line.gettoken_str(a));

        DefineInnerLangString(NLF_REMOVE_DIR);
        DefineInnerLangString(NLF_DEL_FILE);
#ifdef NSIS_SUPPORT_MOVEONREBOOT
        DefineInnerLangString(NLF_DEL_ON_REBOOT);
#endif
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_RMDIR
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_RMDIR not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_RMDIR
    case TOK_RESERVEFILE:
    case TOK_FILE:
#ifdef NSIS_SUPPORT_FILE
      {
        set<tstring> excluded;
        int a=1,attrib=0,rec=0,fatal=1;
        if (!_tcsicmp(line.gettoken_str(a),_T("/nonfatal"))) {
          fatal=0;
          a++;
        }
        if (which_token == TOK_FILE && !_tcsicmp(line.gettoken_str(a),_T("/a")))
        {
#ifdef _WIN32
          attrib=1;
#else
          warning_fl(_T("%sFile /a is disabled for non Win32 platforms."),(which_token == TOK_FILE)?_T(""):_T("Reserve"));
#endif
          a++;
        }
        if (!_tcsicmp(line.gettoken_str(a),_T("/r")))
        {
          rec=1;
          a++;
        }
        else if (which_token == TOK_FILE && !_tcsnicmp(line.gettoken_str(a),_T("/oname="),7))
        {
          TCHAR *on=line.gettoken_str(a)+7;
          a++;
          if (!*on||line.getnumtokens()!=a+1||_tcsstr(on,_T("*")) || _tcsstr(on,_T("?"))) PRINTHELP()

          if (on[0]==_T('"'))
          {
            ERROR_MSG(_T("%sFile: output name must not begin with a quote, use \"/oname=name with spaces\".\n"),(which_token == TOK_FILE)?_T(""):_T("Reserve"),line.gettoken_str(a));
            PRINTHELP();
          }

          int tf=0;
          TCHAR *fn = line.gettoken_str(a);
          PATH_CONVERT(fn);
          int v=do_add_file(fn, attrib, 0, &tf, on);
          if (v != PS_OK) return v;
          if (tf > 1) PRINTHELP()
          if (!tf)
          {
            if (fatal)
            {
              ERROR_MSG(_T("%sFile: \"%s\" -> no files found.\n"),(which_token == TOK_FILE)?_T(""):_T("Reserve"),line.gettoken_str(a));
              PRINTHELP()
            }
            else
            {
              warning_fl(_T("%sFile: \"%s\" -> no files found"),(which_token == TOK_FILE)?_T(""):_T("Reserve"),line.gettoken_str(a));

              // workaround for bug #1299100
              // add a nop opcode so relative jumps will work as expected
              add_entry_direct(EW_NOP);
            }
          }

          return PS_OK;
        }
        if (!_tcsnicmp(line.gettoken_str(a),_T("/x"),2))
        {
          while (!_tcsnicmp(line.gettoken_str(a),_T("/x"),2))
          {
            a++;

            if (line.getnumtokens() < a+1) PRINTHELP()

            excluded.insert(line.gettoken_str(a));
            a++;
          }
        }
#ifdef _WIN32
        if (line.gettoken_str(a)[0] == _T('/')) PRINTHELP()
#endif
        if (line.getnumtokens()<a+1) PRINTHELP()
        while (a < line.getnumtokens())
        {
#ifdef _WIN32
          if (line.gettoken_str(a)[0]==_T('/')) PRINTHELP()
#endif
          TCHAR buf[32];
          TCHAR *t=line.gettoken_str(a++);
          if (t[0] && CharNext(t)[0] == _T(':') && CharNext(t)[1] == _T('\\') && !CharNext(t)[2])
          {
            _tcscpy(buf,_T("X:\\*.*"));
            buf[0]=t[0];
            t=buf;
          }
          int tf=0;
          TCHAR *fn = my_convert(t);
          int v=do_add_file(fn, attrib, rec, &tf, NULL, which_token == TOK_FILE, NULL, excluded);
          my_convert_free(fn);
          if (v != PS_OK) return v;
          if (!tf)
          {
            if (fatal)
            {
              ERROR_MSG(_T("%sFile: \"%s\" -> no files found.\n"),(which_token == TOK_FILE)?_T(""):_T("Reserve"),t);
              PRINTHELP();
            }
            else
            {
              warning_fl(_T("%sFile: \"%s\" -> no files found."),(which_token == TOK_FILE)?_T(""):_T("Reserve"),t);
            }
          }
        }
      }
    return PS_OK;
#else//!NSIS_SUPPORT_FILE
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_FILE not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_FILE
#ifdef NSIS_SUPPORT_COPYFILES
    case TOK_COPYFILES:
      {
        ent.which=EW_COPYFILES;
        ent.offsets[2]=FOF_NOCONFIRMATION|FOF_NOCONFIRMMKDIR|FOF_NOERRORUI|FOF_SIMPLEPROGRESS;

        int a=1;
        int x;
        for (x = 0; x < 2; x ++)
        {
          if (!_tcsicmp(line.gettoken_str(a),_T("/SILENT")))
          {
            a++;
            ent.offsets[2]&=~FOF_SIMPLEPROGRESS;
            ent.offsets[2]|=FOF_SILENT;
          }
          else if (!_tcsicmp(line.gettoken_str(a),_T("/FILESONLY")))
          {
            a++;
            ent.offsets[2]|=FOF_FILESONLY;
          }
          else if (line.gettoken_str(a)[0]==_T('/')) PRINTHELP()
          else break;
        }
        if (line.getnumtokens() < a+2) PRINTHELP()
        ent.offsets[0]=add_string(line.gettoken_str(a));
        ent.offsets[1]=add_string(line.gettoken_str(a+1));
        tstring copy_to = tstring(_T("$(^CopyTo)")) + line.gettoken_str(a+1);
        ent.offsets[3]=add_string(copy_to.c_str());
        int s;
        int size_kb=line.gettoken_int(a+2,&s);
        if (!s && line.gettoken_str(a+2)[0]) PRINTHELP()
        section_add_size_kb(size_kb);
        SCRIPT_MSG(_T("CopyFiles: %s\"%s\" -> \"%s\", size=%iKB\n"),ent.offsets[2]&FOF_SILENT?_T("(silent) "):_T(""), line.gettoken_str(a),line.gettoken_str(a+1),size_kb);

        DefineInnerLangString(NLF_COPY_FAILED);
        DefineInnerLangString(NLF_COPY_TO);
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_COPYFILES
    case TOK_COPYFILES:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_COPYFILES not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_COPYFILES

    case TOK_SETFILEATTRIBUTES:
      {
        #define MBD(x) {x,_T(#x)},
        struct
        {
          int id;
          const TCHAR *str;
        } list[]=
        {
          MBD(FILE_ATTRIBUTE_NORMAL)
          MBD(FILE_ATTRIBUTE_ARCHIVE)
          MBD(FILE_ATTRIBUTE_HIDDEN)
          MBD(FILE_ATTRIBUTE_OFFLINE)
          MBD(FILE_ATTRIBUTE_READONLY)
          MBD(FILE_ATTRIBUTE_SYSTEM)
          MBD(FILE_ATTRIBUTE_TEMPORARY)
          {FILE_ATTRIBUTE_NORMAL,_T("NORMAL")},
          {FILE_ATTRIBUTE_ARCHIVE,_T("ARCHIVE")},
          {FILE_ATTRIBUTE_HIDDEN,_T("HIDDEN")},
          {FILE_ATTRIBUTE_OFFLINE,_T("OFFLINE")},
          {FILE_ATTRIBUTE_READONLY,_T("READONLY")},
          {FILE_ATTRIBUTE_SYSTEM,_T("SYSTEM")},
          {FILE_ATTRIBUTE_TEMPORARY,_T("TEMPORARY")},
          {FILE_ATTRIBUTE_NORMAL,_T("0")},
        };
        #undef MBD
        int r=0;
        int x;
        TCHAR *p=line.gettoken_str(2);

        while (*p)
        {
          TCHAR *np=p;
          while (*np && *np != _T('|')) np++;
          if (*np) *np++=0;
          for (x = 0 ; (unsigned) x < COUNTOF(list) && _tcsicmp(list[x].str,p); x ++);

          if ((unsigned) x < COUNTOF(list))
          {
            r|=list[x].id;
          }
          else PRINTHELP()
          p=np;
        }
        ent.which=EW_SETFILEATTRIBUTES;
        ent.offsets[0]=add_string(line.gettoken_str(1));
        ent.offsets[1]=r;
      }
    return add_entry(&ent);
    case TOK_SLEEP:
      {
        ent.which=EW_SLEEP;
        ent.offsets[0]=add_string(line.gettoken_str(1));
        SCRIPT_MSG(_T("Sleep: %s ms\n"),line.gettoken_str(1));
      }
    return add_entry(&ent);
    case TOK_IFFILEEXISTS:
      ent.which=EW_IFFILEEXISTS;
      ent.offsets[0] = add_string(line.gettoken_str(1));
      if (process_jump(line,2,&ent.offsets[1]) ||
          process_jump(line,3,&ent.offsets[2])) PRINTHELP()
      SCRIPT_MSG(_T("IfFileExists: \"%s\" ? %s : %s\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
    case TOK_QUIT:
      ent.which=EW_QUIT;
      SCRIPT_MSG(_T("Quit\n"));
    return add_entry(&ent);
    case TOK_ABORT:
      ent.which=EW_ABORT;
      ent.offsets[0] = add_string(line.gettoken_str(1));
      SCRIPT_MSG(_T("Abort: \"%s\"\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_SETDETAILSVIEW:
      {
        int v=line.gettoken_enum(1,_T("hide\0show\0"));
        ent.which=EW_CHDETAILSVIEW;
        if (v < 0) PRINTHELP()
        ent.offsets[0] = v?SW_SHOWNA:SW_HIDE;
        ent.offsets[1] = v?SW_HIDE:SW_SHOWNA;
        SCRIPT_MSG(_T("SetDetailsView: %s\n"),line.gettoken_str(1));
      }
    return add_entry(&ent);
    case TOK_SETDETAILSPRINT:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(status_update);
      int k=line.gettoken_enum(1,_T("both\0textonly\0listonly\0none\0lastused\0"));
      if (k<0) PRINTHELP()
      if (k == 4)
      {
        ent.offsets[2]=1;
      }
      else
      {
        // both     0
        // textonly 2
        // listonly 4
        // none     6
        ent.offsets[1]=add_intstring(k*2);
      }
      SCRIPT_MSG(_T("SetDetailsPrint: %s\n"),line.gettoken_str(1));
    }
    return add_entry(&ent);
    case TOK_SETAUTOCLOSE:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(autoclose);
      int k=line.gettoken_enum(1,_T("false\0true\0"));
      if (k < 0) PRINTHELP()
      ent.offsets[1]=add_intstring(k);
      SCRIPT_MSG(_T("SetAutoClose: %s\n"),line.gettoken_str(1));
    }
    return add_entry(&ent);
    case TOK_IFERRORS:
      ent.which=EW_IFFLAG;
      if (process_jump(line,1,&ent.offsets[0]) ||
          process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=FLAG_OFFSET(exec_error);
      ent.offsets[3]=0;//new value mask - clean error
      SCRIPT_MSG(_T("IfErrors ?%s:%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_IFABORT:
      ent.which=EW_IFFLAG;
      if (process_jump(line,1,&ent.offsets[0]) ||
          process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=FLAG_OFFSET(abort);
      ent.offsets[3]=~0;//new value mask - keep flag
      SCRIPT_MSG(_T("IfAbort ?%s:%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_CLEARERRORS:
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(exec_error);
      ent.offsets[1]=add_intstring(0);
      SCRIPT_MSG(_T("ClearErrors\n"));
    return add_entry(&ent);
    case TOK_SETERRORS:
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(exec_error);
      ent.offsets[1]=add_intstring(1);
      SCRIPT_MSG(_T("SetErrors\n"));
    return add_entry(&ent);
    case TOK_SETERRORLEVEL:
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(errlvl);
      ent.offsets[1]=add_string(line.gettoken_str(1));
      SCRIPT_MSG(_T("SetErrorLevel: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_GETERRORLEVEL:
      ent.which=EW_GETFLAG;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=FLAG_OFFSET(errlvl);
      if (line.gettoken_str(1)[0] && ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG(_T("GetErrorLevel: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
#ifdef NSIS_SUPPORT_STROPTS
    case TOK_STRLEN:
      ent.which=EW_STRLEN;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=add_string(line.gettoken_str(2));
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG(_T("StrLen %s \"%s\"\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_STRCPY:
    case TOK_UNSAFESTRCPY:
      {
        ent.which = EW_ASSIGNVAR;
        const TCHAR* msgprefix = _T("");
        int idx = -1;
        if (TOK_STRCPY == which_token)
        {
          idx = GetUserVarIndex(line, 1);
        }
        else
        {
          msgprefix = _T("Unsafe");
          TCHAR *p = line.gettoken_str(1);
          if (*p == _T('$') && *++p) idx = m_UserVarNames.get(p);
          if (-1 != idx && m_UserVarNames.get_reference(idx) != -1) m_UserVarNames.inc_reference(idx);
        }
        if (idx < 0) PRINTHELP()
        ent.offsets[0]=idx;
        ent.offsets[1]=add_string(line.gettoken_str(2));
        ent.offsets[2]=add_string(line.gettoken_str(3));
        ent.offsets[3]=add_string(line.gettoken_str(4));

        SCRIPT_MSG(_T("%sStrCpy %s \"%s\" (%s) (%s)\n"),
          msgprefix,line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
        return add_entry(&ent);
      }
    case TOK_GETFUNCTIONADDR:
      ent.which=EW_GETFUNCTIONADDR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=ns_func.add(line.gettoken_str(2),0);
      ent.offsets[2]=0;
      ent.offsets[3]=0;
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG(_T("GetFunctionAddress: %s %s"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_GETLABELADDR:
      ent.which=EW_GETLABELADDR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0 || process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=0;
      ent.offsets[3]=0;
      SCRIPT_MSG(_T("GetLabelAddress: %s %s"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_GETCURRENTADDR:
      ent.which=EW_ASSIGNVAR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=add_intstring(1+(cur_header->blocks[NB_ENTRIES].num));
      if (ent.offsets[0] < 0) PRINTHELP()
      ent.offsets[2]=0;
      ent.offsets[3]=0;
      SCRIPT_MSG(_T("GetCurrentAddress: %s"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_STRCMP:
    case TOK_STRCMPS:
      ent.which=EW_STRCMP;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[4]=which_token == TOK_STRCMPS;
      if (process_jump(line,3,&ent.offsets[2]) ||
          process_jump(line,4,&ent.offsets[3])) PRINTHELP()
      SCRIPT_MSG(_T("%s \"%s\" \"%s\" equal=%s, nonequal=%s\n"),line.gettoken_str(0),line.gettoken_str(1),line.gettoken_str(2), line.gettoken_str(3),line.gettoken_str(4));
    return add_entry(&ent);
    case TOK_GETDLLVERSIONLOCAL:
      {
        const TCHAR*cmdname=_T("GetDLLVersionLocal");
        DWORD low, high;
        if (!GetDLLVersion(line.gettoken_str(1),high,low))
        {
          ERROR_MSG(_T("%s: error reading version info from \"%s\"\n"),cmdname,line.gettoken_str(1));
          return PS_ERROR;
        }
        ent.which=EW_ASSIGNVAR;
        ent.offsets[0]=GetUserVarIndex(line, 2);
        ent.offsets[1]=add_intstring(high);
        ent.offsets[2]=0;
        ent.offsets[3]=0;
        if (ent.offsets[0]<0) PRINTHELP()
        add_entry(&ent);

        ent.offsets[0]=GetUserVarIndex(line, 3);
        ent.offsets[1]=add_intstring(low);
        ent.offsets[2]=0;
        ent.offsets[3]=0;
        if (ent.offsets[0]<0) PRINTHELP()
        SCRIPT_MSG(_T("%s: %s (%u,%u)->(%s,%s)\n"),
          cmdname,line.gettoken_str(1),high,low,line.gettoken_str(2),line.gettoken_str(3));
      }
    return add_entry(&ent);
    case TOK_GETFILETIMELOCAL:
      {
        TCHAR buf[129];
        DWORD high=0,low=0;
#ifdef _WIN32
        int flag=0;
        HANDLE hFile=CreateFile(line.gettoken_str(1),0,0,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
        if (hFile != INVALID_HANDLE_VALUE)
        {
          FILETIME ft;
          if (GetFileTime(hFile,NULL,NULL,&ft))
          {
            high=ft.dwHighDateTime;
            low=ft.dwLowDateTime;
            flag=1;
          }
          CloseHandle(hFile);
        }
        if (!flag)
        {
          ERROR_MSG(_T("GetFileTimeLocal: error reading date from \"%s\"\n"),line.gettoken_str(1));
          return PS_ERROR;
        }
#else
        struct stat st;
        if (!stat(line.gettoken_str(1), &st))
        {
          unsigned long long ll = (st.st_mtime * 10000000LL) + 116444736000000000LL;
          high = (DWORD) (ll >> 32);
          low = (DWORD) ll;
        }
        else
        {
          ERROR_MSG(_T("GetFileTimeLocal: error reading date from \"%s\"\n"),line.gettoken_str(1));
          return PS_ERROR;
        }
#endif

        ent.which=EW_ASSIGNVAR;
        ent.offsets[0]=GetUserVarIndex(line, 2);
        wsprintf(buf,_T("%u"),high);
        ent.offsets[1]=add_string(buf);
        ent.offsets[2]=0;
        ent.offsets[3]=0;
        if (ent.offsets[0]<0) PRINTHELP()
        add_entry(&ent);

        ent.offsets[0]=GetUserVarIndex(line, 3);
        wsprintf(buf,_T("%u"),low);
        ent.offsets[1]=add_string(buf);
        ent.offsets[2]=0;
        ent.offsets[3]=0;
        if (ent.offsets[0]<0) PRINTHELP()
        SCRIPT_MSG(_T("GetFileTimeLocal: %s (%u,%u)->(%s,%s)\n"),
          line.gettoken_str(1),high,low,line.gettoken_str(2),line.gettoken_str(3));
      }
    return add_entry(&ent);

#else//!NSIS_SUPPORT_STROPTS
    case TOK_GETDLLVERSIONLOCAL:
    case TOK_GETFILETIMELOCAL:
    case TOK_GETFUNCTIONADDR:
    case TOK_GETLABELADDR:
    case TOK_GETCURRENTADDR:
    case TOK_STRLEN:
    case TOK_STRCPY:
    case TOK_STRCMP:
    case TOK_STRCMPS:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_STROPTS not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_STROPTS
#ifdef NSIS_SUPPORT_INIFILES
    case TOK_DELETEINISEC:
    case TOK_DELETEINISTR:
      {
        const TCHAR *vname=_T("");
        const TCHAR *space=_T("");
        ent.which=EW_WRITEINI;
        ent.offsets[0]=add_string(line.gettoken_str(2)); // section name
        if (line.getnumtokens() > 3)
        {
          vname=line.gettoken_str(3);
          ent.offsets[1]=add_string(vname); // value name
          space=_T(" ");
        }
        else ent.offsets[1]=0;
        ent.offsets[2]=0;
        ent.offsets[3]=add_string(line.gettoken_str(1));
        SCRIPT_MSG(_T("DeleteINI%s: [%s] %s%sin %s\n"),*vname?_T("Str"):_T("Sec"),
          line.gettoken_str(2),vname,space,line.gettoken_str(1));
      }
    return add_entry(&ent);
    case TOK_FLUSHINI:
      ent.which=EW_WRITEINI;
      ent.offsets[3]=add_string(line.gettoken_str(1));
      SCRIPT_MSG(_T("FlushINI: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_WRITEINISTR:
      ent.which=EW_WRITEINI;
      ent.offsets[0]=add_string(line.gettoken_str(2));
      ent.offsets[1]=add_string(line.gettoken_str(3));
      ent.offsets[2]=add_string(line.gettoken_str(4));
      ent.offsets[3]=add_string(line.gettoken_str(1));
      ent.offsets[4]=1; // write
      SCRIPT_MSG(_T("WriteINIStr: [%s] %s=%s in %s\n"),
        line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_READINISTR:
      ent.which=EW_READINISTR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0) PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(3));
      ent.offsets[2]=add_string(line.gettoken_str(4));
      ent.offsets[3]=add_string(line.gettoken_str(2));
      SCRIPT_MSG(_T("ReadINIStr %s [%s]:%s from %s\n"),line.gettoken_str(1),line.gettoken_str(3),line.gettoken_str(4),line.gettoken_str(2));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_INIFILES
    case TOK_DELETEINISEC:
    case TOK_DELETEINISTR:
    case TOK_FLUSHINI:
    case TOK_WRITEINISTR:
    case TOK_READINISTR:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_INIFILES not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_INIFILES
    case TOK_DETAILPRINT:
      ent.which=EW_UPDATETEXT;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=0;
      SCRIPT_MSG(_T("DetailPrint: \"%s\"\n"),line.gettoken_str(1));
    return add_entry(&ent);
#ifdef NSIS_SUPPORT_FNUTIL
    case TOK_GETTEMPFILENAME:
      ent.which=EW_GETTEMPFILENAME;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (line.getnumtokens() == 3)
        ent.offsets[1]=add_string(line.gettoken_str(2));
      else
        ent.offsets[1]=add_string(_T("$TEMP"));
      if (ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG(_T("GetTempFileName -> %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_GETFULLPATHNAME:
      {
        int a=0;
        ent.which=EW_GETFULLPATHNAME;
        if (line.getnumtokens()==4 && !_tcsicmp(line.gettoken_str(1),_T("/SHORT"))) a++;
        else if (line.getnumtokens()==4 || *line.gettoken_str(1)==_T('/')) PRINTHELP()
        ent.offsets[0]=add_string(line.gettoken_str(2+a));
        ent.offsets[1]=GetUserVarIndex(line, 1+a);
        ent.offsets[2]=!a;
        if (ent.offsets[1]<0) PRINTHELP()
        SCRIPT_MSG(_T("GetFullPathName: %s->%s (%d)\n"),
          line.gettoken_str(2+a),line.gettoken_str(1+a),a?_T("sfn"):_T("lfn"));
      }
    return add_entry(&ent);
    case TOK_SEARCHPATH:
      ent.which=EW_SEARCHPATH;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0) PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(2));
      SCRIPT_MSG(_T("SearchPath %s %s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
#else
    case TOK_SEARCHPATH:
    case TOK_GETTEMPFILENAME:
    case TOK_GETFULLPATHNAME:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_FNUTIL not defined.\n"),  line.gettoken_str(0));
      return PS_ERROR;
#endif
    case TOK_GETDLLVERSION:
#ifdef NSIS_SUPPORT_GETDLLVERSION
      ent.which=EW_GETDLLVERSION;
      ent.offsets[0]=GetUserVarIndex(line, 2);
      ent.offsets[1]=GetUserVarIndex(line, 3);
      ent.offsets[2]=add_string(line.gettoken_str(1));
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("GetDLLVersion: %s->%s,%s\n"),
        line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_GETDLLVERSION
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_GETDLLVERSION not defined.\n"),  line.gettoken_str(0));
      return PS_ERROR;
#endif//!NSIS_SUPPORT_GETDLLVERSION
    case TOK_GETFILETIME:
#ifdef NSIS_SUPPORT_GETFILETIME
      ent.which=EW_GETFILETIME;
      ent.offsets[0]=GetUserVarIndex(line, 2);
      ent.offsets[1]=GetUserVarIndex(line, 3);
      ent.offsets[2]=add_string(line.gettoken_str(1));
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("GetFileTime: %s->%s,%s\n"),
        line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_GETFILETIME
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_GETFILETIME not defined.\n"),  line.gettoken_str(0));
      return PS_ERROR;
#endif//!NSIS_SUPPORT_GETFILETIME
#ifdef NSIS_SUPPORT_INTOPTS
    case TOK_INTOP:
      ent.which=EW_INTOP;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[3]=line.gettoken_enum(3,_T("+\0-\0*\0/\0|\0&\0^\0!\0||\0&&\0%\0<<\0>>\0~\0"));
      if (ent.offsets[0] < 0 || ent.offsets[3] < 0 ||
        ((ent.offsets[3] == 7 || ent.offsets[3] == 13) && line.getnumtokens() > 4))
        PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(2));
      if (ent.offsets[3] != 7 && ent.offsets[3] != 13) ent.offsets[2]=add_string(line.gettoken_str(4));
      if (ent.offsets[3] == 13) {
        ent.offsets[3]=6;
        ent.offsets[2]=add_string(_T("0xFFFFFFFF"));
      }
      SCRIPT_MSG(_T("IntOp: %s=%s%s%s\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
    return add_entry(&ent);
    case TOK_INTFMT:
      ent.which=EW_INTFMT;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0]<0) PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=add_string(line.gettoken_str(3));
      SCRIPT_MSG(_T("IntFmt: %s->%s (fmt:%s)\n"),line.gettoken_str(3),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_INTCMP:
    case TOK_INTCMPU:
      ent.which=EW_INTCMP;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[5]=which_token == TOK_INTCMPU;
      if (process_jump(line,3,&ent.offsets[2]) ||
          process_jump(line,4,&ent.offsets[3]) ||
          process_jump(line,5,&ent.offsets[4]))  PRINTHELP()
      SCRIPT_MSG(_T("%s %s:%s equal=%s, < %s, > %s\n"),line.gettoken_str(0),
        line.gettoken_str(1),line.gettoken_str(2), line.gettoken_str(3),line.gettoken_str(4),line.gettoken_str(5));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_INTOPTS
    case TOK_INTOP:
    case TOK_INTCMP:
    case TOK_INTFMT:
    case TOK_INTCMPU:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_INTOPTS not defined.\n"),  line.gettoken_str(0));
      return PS_ERROR;
#endif//!NSIS_SUPPORT_INTOPTS
#ifdef NSIS_SUPPORT_REGISTRYFUNCTIONS
    case TOK_READREGSTR:
    case TOK_READREGDWORD:
      {
        ent.which=EW_READREGSTR;
        ent.offsets[0]=GetUserVarIndex(line, 1);
        int k=line.gettoken_enum(2,rootkeys[0]);
        if (k == -1) k=line.gettoken_enum(2,rootkeys[1]);
        if (ent.offsets[0] == -1 || k == -1) PRINTHELP()
        ent.offsets[1]=(INT_PTR)rootkey_tab[k];
        ent.offsets[2]=add_string(line.gettoken_str(3));
        ent.offsets[3]=add_string(line.gettoken_str(4));
        if (which_token == TOK_READREGDWORD) ent.offsets[4]=1;
        else ent.offsets[4]=0;
        if (line.gettoken_str(3)[0] == _T('\\'))
          warning_fl(_T("%s: registry path name begins with \'\\\', may cause problems"),line.gettoken_str(0));

        SCRIPT_MSG(_T("%s %s %s\\%s\\%s\n"),line.gettoken_str(0),
          line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
      }
    return add_entry(&ent);
    case TOK_DELETEREGVALUE:
    case TOK_DELETEREGKEY:
      {
        int a=1;
        if (which_token==TOK_DELETEREGKEY)
        {
          ent.offsets[4]=1;
          TCHAR *s=line.gettoken_str(a);
          if (s[0] == _T('/'))
          {
            if (_tcsicmp(s,_T("/ifempty"))) PRINTHELP()
            a++;
            ent.offsets[4]=3;
          }
          if (line.gettoken_str(a+2)[0]) PRINTHELP()
        }
        int k=line.gettoken_enum(a,rootkeys[0]);
        if (k == -1) k=line.gettoken_enum(a,rootkeys[1]);
        if (k == -1) PRINTHELP()
        ent.which=EW_DELREG;
        ent.offsets[1]=(INT_PTR)rootkey_tab[k];
        ent.offsets[2]=add_string(line.gettoken_str(a+1));
        ent.offsets[3]=(which_token==TOK_DELETEREGKEY)?0:add_string(line.gettoken_str(a+2));
        if (line.gettoken_str(a+1)[0] == _T('\\'))
          warning_fl(_T("%s: registry path name begins with \'\\\', may cause problems"),line.gettoken_str(0));
        if (which_token==TOK_DELETEREGKEY)
          SCRIPT_MSG(_T("DeleteRegKey: %s\\%s\n"),line.gettoken_str(a),line.gettoken_str(a+1));
        else
          SCRIPT_MSG(_T("DeleteRegValue: %s\\%s\\%s\n"),line.gettoken_str(a),line.gettoken_str(a+1),line.gettoken_str(a+2));
      }
    return add_entry(&ent);
    case TOK_WRITEREGSTR:
    case TOK_WRITEREGEXPANDSTR:
    case TOK_WRITEREGBIN:
    case TOK_WRITEREGDWORD:
      {
        int k=line.gettoken_enum(1,rootkeys[0]);
        if (k == -1) k=line.gettoken_enum(1,rootkeys[1]);
        if (k == -1) PRINTHELP()
        ent.which=EW_WRITEREG;
        ent.offsets[0]=(INT_PTR)rootkey_tab[k];
        ent.offsets[1]=add_string(line.gettoken_str(2));
        if (line.gettoken_str(2)[0] == _T('\\'))
          warning_fl(_T("%s: registry path name begins with \'\\\', may cause problems"),line.gettoken_str(0));
        ent.offsets[2]=add_string(line.gettoken_str(3));
        if (which_token == TOK_WRITEREGSTR || which_token == TOK_WRITEREGEXPANDSTR)
        {
          SCRIPT_MSG(_T("%s: %s\\%s\\%s=%s\n"),
            line.gettoken_str(0),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
          ent.offsets[3]=add_string(line.gettoken_str(4));
          ent.offsets[4]=ent.offsets[5]=REG_SZ;
          if (which_token == TOK_WRITEREGEXPANDSTR)
          {
            ent.offsets[5]=REG_EXPAND_SZ;
          }
        }
        if (which_token == TOK_WRITEREGBIN)
        {
          // Jim Park: Keep the data as char / 8 bits
          char data[3*NSIS_MAX_STRLEN];
          TCHAR *p=line.gettoken_str(4);
          int data_len=0;
          while (*p)
          {
            int c;
            int a,b;
            a=*p;
            if (a >= _T('0') && a <= _T('9')) a-=_T('0');
            else if (a >= _T('a') && a <= _T('f')) a-=_T('a')-10;
            else if (a >= _T('A') && a <= _T('F')) a-=_T('A')-10;
            else break;
            b=*++p;
            if (b >= _T('0') && b <= _T('9')) b-=_T('0');
            else if (b >= _T('a') && b <= _T('f')) b-=_T('a')-10;
            else if (b >= _T('A') && b <= _T('F')) b-=_T('A')-10;
            else break;
            p++;
            c=(a<<4)|b;
            if (data_len >= 3*NSIS_MAX_STRLEN)
            {
              ERROR_MSG(_T("WriteRegBin: %d bytes of data exceeded\n"),3*NSIS_MAX_STRLEN);
              return PS_ERROR;
            }
            data[data_len++]=c;
          }
          if (*p) PRINTHELP()
          SCRIPT_MSG(_T("WriteRegBin: %s\\%s\\%s=%s\n"),
            line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
          ent.offsets[3]=add_db_data(data,data_len);
          if (ent.offsets[3] < 0) return PS_ERROR;
          ent.offsets[4]=ent.offsets[5]=REG_BINARY;
        }
        if (which_token == TOK_WRITEREGDWORD)
        {
          ent.offsets[3]=add_string(line.gettoken_str(4));
          ent.offsets[4]=ent.offsets[5]=REG_DWORD;

          SCRIPT_MSG(_T("WriteRegDWORD: %s\\%s\\%s=%s\n"),
            line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
        }
      }
    return add_entry(&ent);
    case TOK_ENUMREGKEY:
    case TOK_ENUMREGVAL:
      {
        ent.which=EW_REGENUM;
        ent.offsets[0]=GetUserVarIndex(line, 1);
        int k=line.gettoken_enum(2,rootkeys[0]);
        if (k == -1) k=line.gettoken_enum(2,rootkeys[1]);
        if (ent.offsets[0] == -1 || k == -1) PRINTHELP()
        ent.offsets[1]=(INT_PTR)rootkey_tab[k];
        ent.offsets[2]=add_string(line.gettoken_str(3));
        ent.offsets[3]=add_string(line.gettoken_str(4));
        ent.offsets[4]=which_token == TOK_ENUMREGKEY;
        if (line.gettoken_str(3)[0] == _T('\\')) warning_fl(_T("%s: registry path name begins with \'\\\', may cause problems"),line.gettoken_str(0));
        SCRIPT_MSG(_T("%s %s %s\\%s\\%s\n"),which_token == TOK_ENUMREGKEY ? _T("EnumRegKey") : _T("EnumRegValue"),
          line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_REGISTRYFUNCTIONS
    case TOK_READREGSTR:
    case TOK_READREGDWORD:
    case TOK_DELETEREGVALUE:
    case TOK_DELETEREGKEY:
    case TOK_WRITEREGSTR:
    case TOK_WRITEREGEXPANDSTR:
    case TOK_WRITEREGBIN:
    case TOK_WRITEREGDWORD:
    case TOK_ENUMREGKEY:
    case TOK_ENUMREGVAL:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_REGISTRYFUNCTIONS not defined.\n"),  line.gettoken_str(0));
      return PS_ERROR;
#endif//!NSIS_SUPPORT_REGISTRYFUNCTIONS
#ifdef NSIS_SUPPORT_STACK
    case TOK_EXCH:
      {
        int swapitem=1;
        int save=GetUserVarIndex(line, 1);
        ent.which=EW_PUSHPOP;
        if (line.gettoken_str(1)[0] && save<0)
        {
          int s=0;
          swapitem=line.gettoken_int(1,&s);
          if (!s || swapitem <= 0) PRINTHELP()
        }
        if (save>=0)
        {
          SCRIPT_MSG(_T("Exch(%s,0)\n"),line.gettoken_str(1));
          ent.offsets[0]=add_string(line.gettoken_str(1));
          ent.offsets[1]=0;
          ent.offsets[2]=0;
          add_entry(&ent);
        }
        else SCRIPT_MSG(_T("Exch(st(%d),0)\n"),swapitem);

        ent.offsets[0]=0;
        ent.offsets[1]=0;
        ent.offsets[2]=swapitem;

        if (save>=0)
        {
          add_entry(&ent);
          ent.offsets[0]=save;
          ent.offsets[1]=1;
          ent.offsets[2]=0;
        }

        DefineInnerLangString(NLF_INST_CORRUPTED);
      }
    return add_entry(&ent);
    case TOK_PUSH:
      ent.which=EW_PUSHPOP;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=0;
      SCRIPT_MSG(_T("Push: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_POP:
      ent.which=EW_PUSHPOP;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=1;
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG(_T("Pop: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_STACK
    case TOK_POP:
    case TOK_PUSH:
    case TOK_EXCH:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_STACK not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_STACK
#ifdef NSIS_SUPPORT_ENVIRONMENT
    case TOK_READENVSTR:
      ent.which=EW_READENVSTR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      {
        TCHAR str[NSIS_MAX_STRLEN];
        _tcscpy(str, _T("%"));
        _tcscat(str, line.gettoken_str(2));
        _tcscat(str, _T("%"));
        ent.offsets[1]=add_string(str);
        if (ent.offsets[0] < 0 || _tcslen(line.gettoken_str(2))<1) PRINTHELP()
      }
      ent.offsets[2]=1;
      SCRIPT_MSG(_T("ReadEnvStr: %s->%s\n"),line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_EXPANDENVSTRS:
      ent.which=EW_READENVSTR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=0;
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG(_T("ExpandEnvStrings: %s->%s\n"),line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_ENVIRONMENT
    case TOK_EXPANDENVSTRS:
    case TOK_READENVSTR:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_ENVIRONMENT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_ENVIRONMENT
#ifdef NSIS_SUPPORT_FINDFIRST
    case TOK_FINDFIRST:
      ent.which=EW_FINDFIRST;
      ent.offsets[0]=GetUserVarIndex(line, 2); // out
      ent.offsets[1]=GetUserVarIndex(line, 1); // handleout
      ent.offsets[2]=add_string(line.gettoken_str(3)); // filespec
      if (ent.offsets[0] < 0 || ent.offsets[1] < 0) PRINTHELP()
      SCRIPT_MSG(_T("FindFirst: spec=\"%s\" handle=%s output=%s\n"),line.gettoken_str(3),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_FINDNEXT:
      ent.which=EW_FINDNEXT;
      ent.offsets[0]=GetUserVarIndex(line, 2);
      ent.offsets[1]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0 || ent.offsets[1] < 0) PRINTHELP()
      SCRIPT_MSG(_T("FindNext: handle=%s output=%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_FINDCLOSE:
      ent.which=EW_FINDCLOSE;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG(_T("FindClose: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_FINDFIRST
    case TOK_FINDCLOSE:
    case TOK_FINDNEXT:
    case TOK_FINDFIRST:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_FINDFIRST not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;

#endif//!NSIS_SUPPORT_FINDFIRST


#ifdef NSIS_SUPPORT_FILEFUNCTIONS
    case TOK_FILEOPEN:
      {
        ent.which=EW_FOPEN;
        ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
        ent.offsets[3]=add_string(line.gettoken_str(2));
        ent.offsets[1]=0; //openmode
        if (!_tcsicmp(line.gettoken_str(3),_T("r")))
        {
          ent.offsets[1]=GENERIC_READ;
          ent.offsets[2]=OPEN_EXISTING;
        }
        else if (!_tcsicmp(line.gettoken_str(3),_T("w")))
        {
          ent.offsets[1]=GENERIC_WRITE;
          ent.offsets[2]=CREATE_ALWAYS;
        }
        else if (!_tcsicmp(line.gettoken_str(3),_T("a")))
        {
          ent.offsets[1]=GENERIC_WRITE|GENERIC_READ;
          ent.offsets[2]=OPEN_ALWAYS;
        }

        if (ent.offsets[0] < 0 || !ent.offsets[1]) PRINTHELP()
      }
      SCRIPT_MSG(_T("FileOpen: %s as %s -> %s\n"),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_FILECLOSE:
      ent.which=EW_FCLOSE;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG(_T("FileClose: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_FILEREAD:
      ent.which=EW_FGETS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=GetUserVarIndex(line, 2); // output string
      if (line.gettoken_str(3)[0])
        ent.offsets[2]=add_string(line.gettoken_str(3));
      else
        ent.offsets[2]=add_intstring(NSIS_MAX_STRLEN-1);
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("FileRead: %s->%s (max:%s)\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
    case TOK_FILEWRITE:
      ent.which=EW_FPUTS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=add_string(line.gettoken_str(2));
      if (ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG(_T("FileWrite: %s->%s\n"),line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_FILEREADBYTE:
      ent.which=EW_FGETS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=GetUserVarIndex(line, 2); // output string
      ent.offsets[2]=add_string(_T("1"));
      ent.offsets[3]=1;
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("FileReadByte: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_FILEWRITEBYTE:
      ent.which=EW_FPUTS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=1;
      if (ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG(_T("FileWriteByte: %s->%s\n"),line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
#ifdef _UNICODE
    case TOK_FILEREADUTF16LE:
      if (!build_unicode)
      {
        ERROR_MSG(_T("Error: %s is only available when building a Unicode installer\n"),  line.gettoken_str(0));
        return PS_ERROR;
      }
      ent.which=EW_FGETWS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=GetUserVarIndex(line, 2); // output string
      if (line.gettoken_str(3)[0])
        ent.offsets[2]=add_string(line.gettoken_str(3));
      else
        ent.offsets[2]=add_intstring(NSIS_MAX_STRLEN-1);
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("FileReadUTF16LE: %s->%s (max:%s)\n"),line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
    case TOK_FILEWRITEUTF16LE:
      if (!build_unicode)
      {
        ERROR_MSG(_T("Error: %s is only available when building a Unicode installer\n"),  line.gettoken_str(0));
        return PS_ERROR;
      }
      ent.which=EW_FPUTWS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=add_string(line.gettoken_str(2));
      if (ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG(_T("FileWriteUTF16LE: %s->%s\n"),line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_FILEREADWORD:
      if (!build_unicode)
      {
        ERROR_MSG(_T("Error: %s is only available when building a Unicode installer\n"),  line.gettoken_str(0));
        return PS_ERROR;
      }
      ent.which=EW_FGETWS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=GetUserVarIndex(line, 2); // output string
      ent.offsets[2]=add_string(_T("1"));
      ent.offsets[3]=1;
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("FileReadWord: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_FILEWRITEWORD:
      if (!build_unicode)
      {
        ERROR_MSG(_T("Error: %s is only available when building a Unicode installer\n"),  line.gettoken_str(0));
        return PS_ERROR;
      }
      ent.which=EW_FPUTWS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=1;
      if (ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG(_T("FileWriteWord: %s->%s\n"),line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
#endif
    case TOK_FILESEEK:
      {
        const TCHAR *modestr;
        int tab[3]={FILE_BEGIN,FILE_CURRENT,FILE_END};
        int mode=line.gettoken_enum(3,_T("SET\0CUR\0END\0"));
        ent.which=EW_FSEEK;
        ent.offsets[0]=GetUserVarIndex(line, 1);
        ent.offsets[1]=GetUserVarIndex(line, 4);
        ent.offsets[2]=add_string(line.gettoken_str(2));

        if (mode<0 && !line.gettoken_str(3)[0])
        {
          mode=0;
          modestr=_T("SET");
        }
        else modestr=line.gettoken_str(3);

        if (mode<0 || ent.offsets[0] < 0 || (ent.offsets[1]<0 && line.gettoken_str(4)[0])) PRINTHELP()
        ent.offsets[3]=tab[mode];
        SCRIPT_MSG(_T("FileSeek: fp=%s, ofs=%s, mode=%s, output=%s\n"),
          line.gettoken_str(1),
          line.gettoken_str(2),
          modestr,
          line.gettoken_str(4));
      }

    return add_entry(&ent);
#else//!NSIS_SUPPORT_FILEFUNCTIONS
    case TOK_FILEOPEN:
    case TOK_FILECLOSE:
    case TOK_FILESEEK:
    case TOK_FILEREAD:
    case TOK_FILEWRITE:
    case TOK_FILEREADBYTE:
    case TOK_FILEWRITEBYTE:
#ifdef _UNICODE
    case TOK_FILEREADUTF16LE:
    case TOK_FILEWRITEUTF16LE:
    case TOK_FILEREADWORD:
    case TOK_FILEWRITEWORD:
#endif
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_FILEFUNCTIONS not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;

#endif//!NSIS_SUPPORT_FILEFUNCTIONS
#ifdef NSIS_SUPPORT_REBOOT
    case TOK_REBOOT:
    {
      int ret = add_entry_direct(EW_REBOOT, 0xbadf00d);
      if (ret != PS_OK) return ret;

      ret = add_entry_direct(EW_QUIT);
      if (ret != PS_OK) return ret;

      SCRIPT_MSG(_T("Reboot! (WOW)\n"));

      DefineInnerLangString(NLF_INST_CORRUPTED);
    }
    return PS_OK;
    case TOK_IFREBOOTFLAG:
      ent.which=EW_IFFLAG;
      if (process_jump(line,1,&ent.offsets[0]) ||
          process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=FLAG_OFFSET(exec_reboot);
      ent.offsets[3]=~0;//new value mask - keep flag
      SCRIPT_MSG(_T("IfRebootFlag ?%s:%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SETREBOOTFLAG:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(exec_reboot);
      int k=line.gettoken_enum(1,_T("false\0true\0"));
      if (k < 0) PRINTHELP()
      ent.offsets[1]=add_intstring(k);
    }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_REBOOT
    case TOK_REBOOT:
    case TOK_IFREBOOTFLAG:
    case TOK_SETREBOOTFLAG:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_REBOOT not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_REBOOT
#ifdef NSIS_CONFIG_LOG
    case TOK_LOGSET:
      ent.which=EW_LOG;
      ent.offsets[0]=1;
      ent.offsets[1]=line.gettoken_enum(1,_T("off\0on\0"));
      if (ent.offsets[1]<0) PRINTHELP()

      SCRIPT_MSG(_T("LogSet: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_LOGTEXT:
      ent.which=EW_LOG;
      ent.offsets[0]=0;
      ent.offsets[1]=add_string(line.gettoken_str(1));
      SCRIPT_MSG(_T("LogText \"%s\"\n"),line.gettoken_str(1));
    return add_entry(&ent);
#else//!NSIS_CONFIG_LOG

    case TOK_LOGSET:
    case TOK_LOGTEXT:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_LOG not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_LOG
#ifdef NSIS_CONFIG_COMPONENTPAGE
    case TOK_SECTIONSETTEXT:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[2]=SECTION_FIELD_SET(name_ptr);
      ent.offsets[4]=add_string(line.gettoken_str(2));
      SCRIPT_MSG(_T("SectionSetText: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONGETTEXT:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=SECTION_FIELD_GET(name_ptr);
      if (line.gettoken_str(2)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("SectionGetText: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONSETFLAGS:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=SECTION_FIELD_SET(flags);
      ent.offsets[3]=1;
      SCRIPT_MSG(_T("SectionSetFlags: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONGETFLAGS:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=SECTION_FIELD_GET(flags);
      if (line.gettoken_str(2)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("SectionGetFlags: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_INSTTYPESETTEXT:
      ent.which=EW_INSTTYPESET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=1;
      SCRIPT_MSG(_T("InstTypeSetText: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_INSTTYPEGETTEXT:
      ent.which=EW_INSTTYPESET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=0;
      if (line.gettoken_str(1)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("InstTypeGetText: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONSETINSTTYPES:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=SECTION_FIELD_SET(install_types);
      SCRIPT_MSG(_T("SectionSetInstTypes: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONGETINSTTYPES:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=SECTION_FIELD_GET(install_types);
      if (line.gettoken_str(2)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("SectionGetInstTypes: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONSETSIZE:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=SECTION_FIELD_SET(size_kb);
      SCRIPT_MSG(_T("SectionSetSize: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONGETSIZE:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=SECTION_FIELD_GET(size_kb);
      if (line.gettoken_str(2)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("SectionGetSize: %s->%s\n"),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SETCURINSTTYPE:
      ent.which=EW_INSTTYPESET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=0;
      ent.offsets[2]=1;
      ent.offsets[3]=1;
      SCRIPT_MSG(_T("SetCurInstType: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_GETCURINSTTYPE:
      ent.which=EW_INSTTYPESET;
      ent.offsets[0]=0;
      ent.offsets[1]=GetUserVarIndex(line,1);
      ent.offsets[2]=0;
      ent.offsets[3]=1;
      if (line.gettoken_str(1)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG(_T("GetCurInstType: %s\n"),line.gettoken_str(1));
    return add_entry(&ent);
#else//!NSIS_CONFIG_COMPONENTPAGE
    case TOK_SECTIONSETTEXT:
    case TOK_SECTIONGETTEXT:
    case TOK_SECTIONSETFLAGS:
    case TOK_SECTIONGETFLAGS:
    case TOK_SECTIONSETSIZE:
    case TOK_SECTIONGETSIZE:
    case TOK_SECTIONSETINSTTYPES:
    case TOK_SECTIONGETINSTTYPES:
    case TOK_SETCURINSTTYPE:
    case TOK_GETCURINSTTYPE:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_COMPONENTPAGE not defined.\n"),  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_COMPONENTPAGE
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
    case TOK_SETBRANDINGIMAGE:
    {
      SCRIPT_MSG(_T("SetBrandingImage: "));
      if (!branding_image_found) {
        ERROR_MSG(_T("\nError: no branding image found in chosen UI!\n"));
        return PS_ERROR;
      }
      ent.which=EW_SETBRANDINGIMAGE;
      for (int i = 1; i < line.getnumtokens(); i++)
        if (!_tcsnicmp(line.gettoken_str(i),_T("/IMGID="),7)) {
          ent.offsets[1]=_ttoi(line.gettoken_str(i)+7);
          SCRIPT_MSG(_T("/IMGID=%d "),ent.offsets[1]);
        }
        else if (!_tcsicmp(line.gettoken_str(i),_T("/RESIZETOFIT"))) {
          ent.offsets[2]=1; // must be 1 or 0
          SCRIPT_MSG(_T("/RESIZETOFIT "));
        }
        else if (!ent.offsets[0]) {
          ent.offsets[0]=add_string(line.gettoken_str(i));
          SCRIPT_MSG(_T("\"%s\" "), line.gettoken_str(i));
        }
        else {
          SCRIPT_MSG(_T("\n"));
          PRINTHELP();
        }

      if (!ent.offsets[1])
        ent.offsets[1]=branding_image_id;
      SCRIPT_MSG(_T("\n"));
    }
    return add_entry(&ent);
#else//NSIS_CONFIG_ENHANCEDUI_SUPPORT
    case TOK_SETBRANDINGIMAGE:
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_ENHANCEDUI_SUPPORT not defined.\n"),line.gettoken_str(0));
      return PS_ERROR;
#endif//!NSIS_SUPPORT_CREATEFONT

    // Added by ramon 3 jun 2003
    case TOK_DEFVAR:
    {
      int a=1;

      if (!_tcsicmp(line.gettoken_str(1),_T("/GLOBAL")))
      {
        a++;
      }
      else if (line.getnumtokens() == 3)
      {
        PRINTHELP();
      }

      if (build_cursection)
      {
        if (a==1)
        {
          ERROR_MSG(_T("Var: currently, only global variables can be defined.\n"));
          PRINTHELP();
        }
      }

      SCRIPT_MSG(_T("Var: \"%s\"\n"),line.gettoken_str(a));

      int res = DeclaredUserVar(line.gettoken_str(a));
      if (res != PS_OK)
        return res;

    }
    return PS_OK;

    // Added by ramon 6 jun 2003
#ifdef NSIS_SUPPORT_VERSION_INFO
    case TOK_VI_ADDKEY:
    {
      LANGID LangID=0;
      int a = 1;
      // Allow people to force Neutral, if /LANG=* is not present it uses the default
      const bool forceneutrallang = !_tcsicmp(line.gettoken_str(a),_T("/LANG=0"));

      if (!_tcsnicmp(line.gettoken_str(a),_T("/LANG="),6))
        LangID=_ttoi(line.gettoken_str(a++)+6);
      if (line.getnumtokens()!=a+2) PRINTHELP();
      TCHAR *pKey = line.gettoken_str(a);
      TCHAR *pValue = line.gettoken_str(a+1);
      if ( !(*pKey) )
      {
         ERROR_MSG(_T("Error: empty name for version info key!\n"));
         return PS_ERROR;
      }
      else
      {
        SCRIPT_MSG(_T("%s: \"%s\" \"%s\"\n"), line.gettoken_str(0), line.gettoken_str(a), line.gettoken_str(a+1));
        const bool allowdeflangfallback = a <= 1 && !forceneutrallang;
        if ( a > 1 && 0 == LangID && !forceneutrallang)
        {
          ERROR_MSG(_T("%s: \"%s\" is not a valid language code!\n"),line.gettoken_str(0), line.gettoken_str(1));
          return PS_ERROR;
        }

        unsigned int codepage;
        // We rely on GetLangNameAndCPForVersionResource to update LangID if required
        const TCHAR *lang_name = GetLangNameAndCPForVersionResource(LangID, &codepage, allowdeflangfallback);

        if ( rVersionInfo.SetKeyValue(LangID, codepage, pKey, pValue) )
        {
          ERROR_MSG(_T("%s: \"%s\" \"%04d-%s\" already defined!\n"),line.gettoken_str(0), line.gettoken_str(2), LangID, lang_name);
          return PS_ERROR;
        }

        return PS_OK;
      }
    }
      case TOK_VI_SETPRODUCTVERSION:
      case TOK_VI_SETFILEVERSION:
      // Probably not required, but this code retains the <= 2.46 behaviour and
      // does not fail on bad product version number here, it "validates" in CEXEBuild::AddVersionInfo()
      //
      // It is ok for us to use rVersionInfo as storage since VIProductVersion is required by VIAddVersionKey 
      {
        const bool settingFileVer = TOK_VI_SETFILEVERSION == which_token;
        const unsigned int reuseFlag = settingFileVer ? 4 : 1;
        if (reuseFlag & version_fixedflags) 
        {
          ERROR_MSG(_T("Error: %s already defined!\n"), line.gettoken_str(0));
          return PS_ERROR;
        }
        version_fixedflags |= reuseFlag;
        int imm, iml, ilm, ill;
        const bool validInput = _stscanf(line.gettoken_str(1), _T("%d.%d.%d.%d"), &imm, &iml, &ilm, &ill) == 4;
        if (settingFileVer)
        {
          if (!validInput) 
          {
            ERROR_MSG(_T("Error: invalid %s format, should be X.X.X.X\n"),line.gettoken_str(0));
            return PS_ERROR;
          }
          rVersionInfo.SetFileVersion(MAKELONG(iml, imm),MAKELONG(ill, ilm));
        }
        else
        {
          if (validInput) 
          {
            version_fixedflags |= 2;
            rVersionInfo.SetProductVersion(MAKELONG(iml, imm),MAKELONG(ill, ilm));
            // FileVersion defaults to ProductVersion
            if (!(4 & version_fixedflags)) rVersionInfo.SetFileVersion(MAKELONG(iml, imm),MAKELONG(ill, ilm));
          }
        }
      }
      return PS_OK;

#else
    case TOK_VI_ADDKEY:
    case TOK_VI_SETPRODUCTVERSION:
    case TOK_VI_SETFILEVERSION:
      ERROR_MSG(_T("Error: %s specified, NSIS_SUPPORT_VERSION_INFO not defined.\n"),line.gettoken_str(0));
      return PS_ERROR;
#endif

    // end of instructions
    ///////////////////////////////////////////////////////////////////////////////

    // Added by Ximon Eighteen 5th August 2002
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    case TOK_PLUGINDIR:
    {
      if (line.getnumtokens() == 2)
      {
        SCRIPT_MSG(_T("PluginDir: \"%s\"\n"),line.gettoken_str(1));
        TCHAR *path = line.gettoken_str(1);
        PATH_CONVERT(path);
        m_plugins.FindCommands(path, display_info?true:false);
        return PS_OK;
      }
    }
    return PS_ERROR;
    case TOK__PLUGINCOMMAND:
    {
      int ret;

      tstring command = m_plugins.NormalizedCommand(line.gettoken_str(0));
#ifdef _UNICODE
      if (build_unicode)
        command = m_plugins.UseUnicodeVariant(command);
#endif
      tstring dllPath = m_plugins.GetPluginPath(command);
      tstring dllName = get_file_name(dllPath);
      int data_handle = m_plugins.GetPluginHandle(uninstall_mode?true:false, command);

      if (uninstall_mode) uninst_plugin_used = true;
      else plugin_used = true;

      // Initialize $PLUGINSDIR
      ent.which=EW_CALL;
      ent.offsets[0]=ns_func.add(uninstall_mode?_T("un.Initialize_____Plugins"):_T("Initialize_____Plugins"),0);
      ret=add_entry(&ent);
      if (ret != PS_OK) {
        return ret;
      }

      // DLL name on the user machine
      TCHAR tempDLL[NSIS_MAX_STRLEN];
      wsprintf(tempDLL, _T("$PLUGINSDIR\\%s"), dllName.c_str());

      // Add the DLL to the installer
      if (data_handle == -1)
      {
        int files_added;
        int old_build_allowskipfiles=build_allowskipfiles;
        build_allowskipfiles=1; // on
        int old_build_overwrite=build_overwrite;
        build_overwrite=1; // off
        int old_build_datesave=build_datesave;
        build_datesave=0; // off
        
        // Jim Park: While the code looks as if the same DLL is added multiple
        // times for each command in the DLL, this is actually not the case
        // because of CEXEBuild::datablock_optimize() that tries to discover
        // duplicates and reuse them.
        ret=do_add_file(dllPath.c_str(),0,0,&files_added,tempDLL,2,&data_handle); // 2 means no size add
        if (ret != PS_OK) {
          return ret;
        }
        m_plugins.SetDllDataHandle(uninstall_mode?true:false, line.gettoken_str(0), data_handle);
        build_overwrite=old_build_overwrite;
        build_datesave=old_build_datesave;
        // Added by ramon 23 May 2003
        build_allowskipfiles=old_build_allowskipfiles;
      }
      else
      {
        ent.which=EW_EXTRACTFILE;

        DefineInnerLangString(NLF_SKIPPED);
        DefineInnerLangString(NLF_ERR_DECOMPRESSING);
        DefineInnerLangString(NLF_ERR_WRITING);
        DefineInnerLangString(NLF_EXTRACT);
        DefineInnerLangString(NLF_CANT_WRITE);

        ent.offsets[0]=1; // overwrite off
        ent.offsets[0]|=(MB_RETRYCANCEL|MB_ICONSTOP|(IDCANCEL<<21))<<3;
        ent.offsets[1]=add_string(tempDLL);
        ent.offsets[2]=data_handle;
        ent.offsets[3]=0xffffffff;
        ent.offsets[4]=0xffffffff;
        ent.offsets[5]=DefineInnerLangString(NLF_FILE_ERROR);
        ret=add_entry(&ent);
        if (ret != PS_OK) {
          return ret;
        }
      }

      // SetDetailsPrint lastused
      ret=add_entry_direct(EW_SETFLAG, FLAG_OFFSET(status_update), 0, 1);
      if (ret != PS_OK) {
        return ret;
      }

      // Call the DLL
      tstring funcname = get_string_suffix(command, _T("::"));
      SCRIPT_MSG(_T("Plugin Command: %s"),funcname.c_str());

      int i = 1;
      int nounload = 0;
      if (!_tcsicmp(line.gettoken_str(i), _T("/NOUNLOAD"))) {
        i++;
        nounload++;
      }

      // First push dll args

      int parmst=i; // we push  em in reverse order
      int nounloadmisused=0;
      for (; i < line.getnumtokens(); i++) {
        int w=parmst + (line.getnumtokens()-i - 1);
        ent.which=EW_PUSHPOP;
        ent.offsets[0]=add_string(line.gettoken_str(w));
        if (!_tcsicmp(line.gettoken_str(w), _T("/NOUNLOAD"))) nounloadmisused=1;
        ent.offsets[1]=0;
        ent.offsets[2]=0;
        ret=add_entry(&ent);
        if (ret != PS_OK) {
          return ret;
        }
        SCRIPT_MSG(_T(" %s"),line.gettoken_str(i));
      }
      SCRIPT_MSG(_T("\n"));
      if (nounloadmisused)
        warning_fl(_T("/NOUNLOAD must come first before any plugin parameter. Unless the plugin you are trying to use has a parameter /NOUNLOAD, you are doing something wrong"));

      // next, call it
      ent.which=EW_REGISTERDLL;
      ent.offsets[0]=add_string(tempDLL);
      ent.offsets[1]=add_string(funcname.c_str());
      ent.offsets[2]=0;
      ent.offsets[3]=nounload|build_plugin_unload;
      ent.offsets[4]=1;
      ret=add_entry(&ent);
      if (ret != PS_OK) {
        return ret;
      }

      DefineInnerLangString(NLF_SYMBOL_NOT_FOUND);
      DefineInnerLangString(NLF_COULD_NOT_LOAD);
      DefineInnerLangString(NLF_NO_OLE);
      // not used anywhere - DefineInnerLangString(NLF_ERR_REG_DLL);

      return PS_OK;
    }
    case TOK_INITPLUGINSDIR:
    {
      int ret;
      SCRIPT_MSG(_T("%s\n"),line.gettoken_str(0));
      if (uninstall_mode) uninst_plugin_used = true;
      else plugin_used = true;
      // Call [un.]Initialize_____Plugins
      ent.which=EW_CALL;
      ent.offsets[0]=ns_func.add(uninstall_mode?_T("un.Initialize_____Plugins"):_T("Initialize_____Plugins"),0);
      ret=add_entry(&ent);
      if (ret != PS_OK) return ret;
      // SetDetailsPrint lastused
      ret=add_entry_direct(EW_SETFLAG, FLAG_OFFSET(status_update), 0, 1);
      if (ret != PS_OK) return ret;
    }
    return PS_OK;
#else
    case TOK_PLUGINDIR:
    case TOK__PLUGINCOMMAND:
    case TOK_INITPLUGINSDIR:
    {
      ERROR_MSG(_T("Error: %s specified, NSIS_CONFIG_PLUGIN_SUPPORT not defined.\n"),line.gettoken_str(0));
    }
    return PS_ERROR;
#endif// NSIS_CONFIG_PLUGIN_SUPPORT

#ifdef NSIS_LOCKWINDOW_SUPPORT
    case TOK_LOCKWINDOW:
      SCRIPT_MSG(_T("LockWindow: lock state=%d\n"),line.gettoken_str(1));
      ent.which=EW_LOCKWINDOW;
      ent.offsets[0]=line.gettoken_enum(1,_T("on\0off\0"));
      if (ent.offsets[0] == -1)
        PRINTHELP();
    return add_entry(&ent);
#else
    case TOK_LOCKWINDOW:
    {
      ERROR_MSG(_T("Error: %s specified, NSIS_LOCKWINDOW_SUPPORT not defined.\n"),line.gettoken_str(0));
    }
    return PS_ERROR;
#endif // NSIS_LOCKWINDOW_SUPPORT

    default: break;

  }
  ERROR_MSG(_T("Error: doCommand: Invalid token \"%s\".\n"),line.gettoken_str(0));
  return PS_ERROR;
}

#ifdef NSIS_SUPPORT_FILE
int CEXEBuild::do_add_file(const TCHAR *lgss, int attrib, int recurse, int *total_files, const TCHAR *name_override, int generatecode, int *data_handle, const set<tstring>& excluded, const tstring& basedir, bool dir_created)
{
  assert(!name_override || !recurse);

  tstring dir = get_dir_name(lgss);
  tstring spec;

  if (dir == lgss) {
    dir = _T(".");
    spec = lgss;
  } else {
    spec = tstring(lgss).substr(dir.length() + 1, tstring::npos);
  }

  if (spec == _T("")) {
    spec = _T("*");
  }

  if (basedir == _T("")) {
    dir_created = true;

    if (recurse) {
      // save $OUTDIR into $_OUTDIR [StrCpy $_OUTDIR $OUTDIR]
      if (add_entry_direct(EW_ASSIGNVAR, m_UserVarNames.get(_T("_OUTDIR")), add_string(_T("$OUTDIR"))) != PS_OK) {
        return PS_ERROR;
      }
    }
  }

  boost::scoped_ptr<dir_reader> dr( new_dir_reader() );
  dr->exclude(excluded);
  dr->read(dir);

  // add files in the current directory
  for (dir_reader::iterator files_itr = dr->files().begin();
       files_itr != dr->files().end();
       files_itr++)
  {
    if (!dir_reader::matches(*files_itr, spec))
      continue;

    if (!dir_created && generatecode) {
      SCRIPT_MSG(_T("%sFile: Descending to: \"%s\"\n"), generatecode ? _T("") : _T("Reserve"), dir.c_str());

      if (do_add_file_create_dir(dir, basedir, attrib) != PS_OK) {
        return PS_ERROR;
      }

      dir_created = true;
    }

    if (add_file(dir, *files_itr, attrib, name_override, generatecode, data_handle) != PS_OK) {
      return PS_ERROR;
    }

    (*total_files)++;
  }

  if (!recurse) {
    return PS_OK;
  }

  // recurse into directories
  for (dir_reader::iterator dirs_itr = dr->dirs().begin();
       dirs_itr != dr->dirs().end();
       dirs_itr++)
  {
    tstring new_dir;
    bool created = false;

    if (basedir == _T("")) {
      new_dir = *dirs_itr;
    } else {
      new_dir = basedir + _T('\\') + *dirs_itr;
    }

    tstring new_spec = dir + PLATFORM_PATH_SEPARATOR_STR + *dirs_itr + PLATFORM_PATH_SEPARATOR_STR;

    if (!dir_reader::matches(*dirs_itr, spec)) {
      new_spec += spec;
    } else if (generatecode) {
      // always create directories that match

      SCRIPT_MSG(_T("%sFile: Descending to: \"%s\"\n"), generatecode ? _T("") : _T("Reserve"), new_spec.c_str());

      if (do_add_file_create_dir(dir + _T('\\') + *dirs_itr, new_dir, attrib) != PS_OK) {
        return PS_ERROR;
      }

      created = true;
    }

    const TCHAR *new_spec_c = new_spec.c_str();

    int res = do_add_file(new_spec_c, attrib, 1, total_files, NULL, generatecode, NULL, excluded, new_dir, created);
    if (res != PS_OK) {
      return PS_ERROR;
    }
  }

  if (basedir == _T("")) {
    SCRIPT_MSG(_T("%sFile: Returning to: \"%s\"\n"), generatecode ? _T("") : _T("Reserve"), dir.c_str());

    // restore $OUTDIR from $_OUTDIR [SetOutPath $_OUTDIR]
    if (add_entry_direct(EW_CREATEDIR, add_string(_T("$_OUTDIR")), 1) != PS_OK) {
      return PS_ERROR;
    }
  }

  return PS_OK;
}

int CEXEBuild::add_file(const tstring& dir, const tstring& file, int attrib, const TCHAR *name_override, int generatecode, int *data_handle) {
  tstring newfn_s = dir + PLATFORM_PATH_SEPARATOR_C + file;
  const TCHAR *newfn = newfn_s.c_str();
  const TCHAR *filename = file.c_str();

  MMapFile mmap;
  DWORD len;

#ifdef _WIN32
  HANDLE hFile = CreateFile(
    newfn,
    GENERIC_READ,
    FILE_SHARE_READ,
    NULL,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,
    NULL
  );
  if (hFile == INVALID_HANDLE_VALUE)
  {
    ERROR_MSG(_T("%sFile: failed opening file \"%s\"\n"),generatecode?_T(""):_T("Reserve"),newfn);
    return PS_ERROR;
  }

  // Will auto-CloseHandle hFile
  MANAGE_WITH(hFile, CloseHandle);

  len = GetFileSize(hFile, NULL);
  if (len && !mmap.setfile(hFile, len))
  {
    ERROR_MSG(_T("%sFile: failed creating mmap of \"%s\"\n"),generatecode?_T(""):_T("Reserve"),newfn);
    return PS_ERROR;
  }
#else
  struct stat s;
  if (stat(newfn, &s)) {
    ERROR_MSG(_T("%sFile: failed stating file \"%s\"\n"),generatecode?_T(""):_T("Reserve"),newfn);
    return PS_ERROR;
  }

  len = (DWORD) s.st_size;

  int fd = OPEN(newfn, O_RDONLY);
  if (fd == -1)
  {
    ERROR_MSG(_T("%sFile: failed opening file \"%s\"\n"),generatecode?_T(""):_T("Reserve"),newfn);
    return PS_ERROR;
  }

  // Will auto-close(2) fd
  MANAGE_WITH(fd, close);

  if (len && !mmap.setfile(fd, len))
  {
    ERROR_MSG(_T("%sFile: failed creating mmap of \"%s\"\n"),generatecode?_T(""):_T("Reserve"),newfn);
    return PS_ERROR;
  }
#endif

  if (generatecode&1)
    section_add_size_kb((len+1023)/1024);
  if (name_override) SCRIPT_MSG(_T("%sFile: \"%s\"->\"%s\""),generatecode?_T(""):_T("Reserve"),filename,name_override);
  else SCRIPT_MSG(_T("%sFile: \"%s\""),generatecode?_T(""):_T("Reserve"),filename);
  if (!build_compress_whole)
    if (build_compress) SCRIPT_MSG(_T(" [compress]"));
  fflush(stdout);
  TCHAR buf[1024];
  int last_build_datablock_used=getcurdbsize();
  entry ent={0,};
  if (generatecode)
  {
    ent.which=EW_EXTRACTFILE;

    DefineInnerLangString(NLF_SKIPPED);
    DefineInnerLangString(NLF_ERR_DECOMPRESSING);
    DefineInnerLangString(NLF_ERR_WRITING);
    DefineInnerLangString(NLF_EXTRACT);
    DefineInnerLangString(NLF_CANT_WRITE);

    ent.offsets[0]=build_overwrite;
    if (name_override)
    {
      ent.offsets[1]=add_string(name_override);
    }
    else
    {
      const TCHAR *i=filename;
      TCHAR *o=buf;
      while (*i)
      {
        const TCHAR c=*i++;
        *o++=c;
        if (c == _T('$')) *o++=_T('$');
      }
      *o=0;
      ent.offsets[1]=add_string(buf);
    }
  }
  ent.offsets[2]=add_db_data(&mmap);

  mmap.clear();

  if (ent.offsets[2] < 0)
  {
    return PS_ERROR;
  }

  if (data_handle)
  {
    *data_handle=ent.offsets[2];
  }

  {
    DWORD s=getcurdbsize()-last_build_datablock_used;
    if (s) s-=4;
    if (s != len) SCRIPT_MSG(_T(" %d/%d bytes\n"),s,len);
    else SCRIPT_MSG(_T(" %d bytes\n"),len);
  }

  if (generatecode)
  {
    if (build_datesave || build_overwrite>=0x3 /*ifnewer or ifdiff*/)
    {
#ifdef _WIN32
      FILETIME ft;
      if (GetFileTime(hFile,NULL,NULL,&ft))
      {
        // FAT write time has a resolution of 2 seconds
        PULONGLONG fti = (PULONGLONG) &ft;
        *fti -= *fti % 20000000;

        ent.offsets[3]=ft.dwLowDateTime;
        ent.offsets[4]=ft.dwHighDateTime;
      }
#else
      struct stat st;
      if (!fstat(fd, &st))
      {
        unsigned long long ll = (st.st_mtime * 10000000LL) + 116444736000000000LL;

        // FAT write time has a resolution of 2 seconds
        ll -= ll % 20000000;

        ent.offsets[3] = (int) ll;
        ent.offsets[4] = (int) (ll >> 32);
      }
#endif
      else
      {
        ERROR_MSG(_T("%sFile: failed getting file date from \"%s\"\n"),generatecode?_T(""):_T("Reserve"),newfn);
        return PS_ERROR;
      }
    }
    else
    {
      ent.offsets[3]=0xffffffff;
      ent.offsets[4]=0xffffffff;
    }

    // overwrite flag can be 0, 1, 2 or 3. in all cases, 2 bits
    int mb = 0;
    if (build_allowskipfiles)
    {
      mb = MB_ABORTRETRYIGNORE | MB_ICONSTOP;
      // default for silent installers
      mb |= IDIGNORE << 21;
    }
    else
    {
      mb = MB_RETRYCANCEL | MB_ICONSTOP;
      // default for silent installers
      mb |= IDCANCEL << 21;
    }
    ent.offsets[0] |= mb << 3;

    ent.offsets[5] = DefineInnerLangString(build_allowskipfiles ? NLF_FILE_ERROR : NLF_FILE_ERROR_NOIGNORE);
  }

  if (generatecode)
  {
    int a=add_entry(&ent);
    if (a != PS_OK)
    {
      return a;
    }
    if (attrib)
    {
#ifdef _WIN32
      ent.which=EW_SETFILEATTRIBUTES;
      // $OUTDIR is the working directory
      ent.offsets[0]=add_string(name_override?name_override:buf);
      ent.offsets[1]=GetFileAttributes(newfn);
      ent.offsets[2]=0;
      ent.offsets[3]=0;
      ent.offsets[4]=0;
      ent.offsets[5]=0;

      if (INVALID_FILE_ATTRIBUTES != (unsigned)ent.offsets[1])
      {
        a=add_entry(&ent);
        if (a != PS_OK)
        {
          return a;
        }
      }
#endif
    }
  }

  return PS_OK;
}

int CEXEBuild::do_add_file_create_dir(const tstring& local_dir, const tstring& dir, int attrib) {
  tstring outdir_s = _T("$_OUTDIR\\") + dir;

  tstring::size_type pos = 1;
  pos = outdir_s.find(_T('$'), pos);
  while (pos != tstring::npos) {
    outdir_s = outdir_s.insert(pos, _T("$"));
    pos = outdir_s.find(_T('$'), pos + 2);
  }

  int outdir = add_string(outdir_s.c_str());

  if (add_entry_direct(EW_CREATEDIR, outdir, 1) != PS_OK) {
    return PS_ERROR;
  }

#ifdef _WIN32
  if (attrib) {
    int ndc = add_string(_T("."));

    DWORD attr = GetFileAttributes(local_dir.c_str());

    if (attr != INVALID_FILE_ATTRIBUTES)
    {
      if (add_entry_direct(EW_SETFILEATTRIBUTES, ndc, attr) != PS_OK)
      {
        return PS_ERROR;
      }
    }
  }
#endif

  return PS_OK;
}
#endif



DefineList *CEXEBuild::searchParseString(const TCHAR *source_string, LineParser *line, int parmOffs, bool ignCase, bool noErrors)
{
  const TCHAR *tok = line->gettoken_str(parmOffs++);
  if (tok && *tok)
  {
    int toklen = _tcslen(tok);
    while (*source_string && (ignCase?_tcsnicmp(source_string,tok,toklen):_tcsncmp(source_string,tok,toklen))) source_string++;

    if (!*source_string) 
    {
      if (!noErrors) ERROR_MSG(_T("!searchparse: starting string \"%s\" not found, aborted search!\n"),tok);
      return NULL;
    }
    if (*source_string) source_string+=toklen;
  }

  DefineList *ret = NULL;

  while (parmOffs < line->getnumtokens())
  {
    const TCHAR *defout = line->gettoken_str(parmOffs++);
    if (parmOffs < line->getnumtokens()) tok=line->gettoken_str(parmOffs++);
    else tok=NULL;


    int maxlen=-1;
    const TCHAR *src_start = source_string;
    if (tok && *tok)
    {
      int toklen = _tcslen(tok);
      while (*source_string && (ignCase?_tcsnicmp(source_string,tok,toklen):_tcsncmp(source_string,tok,toklen))) source_string++;

      maxlen = source_string - src_start;

      if (*source_string) source_string += toklen;
      else if (!noErrors)
      {
        ERROR_MSG(_T("!searchparse: string \"%s\" not found, aborted search!\n"),tok);
        delete ret;
        return NULL;
      }
      
    }
 
    if (defout && defout[0])
    {
      if (!ret) ret = new DefineList;

      if (maxlen < 0) ret->add(defout,src_start);
      else
      {
        TCHAR *p=_tcsdup(src_start);
        if (p)
        {
          p[maxlen]=0;
          ret->add(defout,p);
          free(p);
        }
      }
    }
  }
  return ret;
}
