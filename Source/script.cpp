#include "Platform.h"
#include <stdio.h>
#include <ctype.h>
#include "tokens.h"
#include "build.h"
#include "util.h"
#include "exedata.h"
#include "ResourceEditor.h"
#include "DialogTemplate.h"
#include "lang.h"
#include "exehead/resource.h"
#include <cassert> // for assert(3)

#ifdef _WIN32
#  include <direct.h>
#else
#  include <sys/stat.h>
#  include <time.h>
#  include <glob.h>
#  include <fcntl.h> // for O_RDONLY
#  include <unistd.h>
#endif

#define MAX_INCLUDEDEPTH 10
#define MAX_LINELENGTH 16384

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
// Added by Sunil Kamath 11 June 2003
char *CEXEBuild::set_file_predefine(char *filename)
{
  char *oldfilename = definedlist.find("__FILE__");
  if(oldfilename)
  {
    oldfilename = strdup(oldfilename);
    definedlist.del("__FILE__");
  }
  char *p = strrchr(filename,'\\');
  if(p) {
    p++;
  }
  else {
    p = curfilename;
  }
  definedlist.add("__FILE__",p);

  return oldfilename;
}

void CEXEBuild::restore_file_predefine(char *oldfilename)
{
  definedlist.del("__FILE__");
  if(oldfilename) {
      definedlist.add("__FILE__",oldfilename);
      free(oldfilename);
  }
}

char *CEXEBuild::set_timestamp_predefine(char *filename)
{
  char *oldtimestamp = definedlist.find("__TIMESTAMP__");
  if(oldtimestamp) {
    oldtimestamp = strdup(oldtimestamp);
    definedlist.del("__TIMESTAMP__");
  }

#ifdef _WIN32
  char timestampbuf[256] = "";
  char datebuf[128] = "";
  char timebuf[128] = "";
  WIN32_FIND_DATA fd;
  FILETIME floctime;
  SYSTEMTIME stime;

  HANDLE hSearch = FindFirstFile(filename, &fd);
  if (hSearch != INVALID_HANDLE_VALUE)
  {
    FindClose(hSearch);

    FileTimeToLocalFileTime(&fd.ftLastWriteTime, &floctime);
    FileTimeToSystemTime(&floctime, &stime);

    GetDateFormat(LOCALE_USER_DEFAULT, DATE_LONGDATE, &stime, NULL, datebuf, sizeof(datebuf)); 
    GetTimeFormat(LOCALE_USER_DEFAULT, 0, &stime, NULL, timebuf, sizeof(timebuf)); 
    wsprintf(timestampbuf,"%s %s",datebuf,timebuf);

    definedlist.add("__TIMESTAMP__",timestampbuf);
  }
#else
  struct stat st;
  if (!stat(filename, &st))
    definedlist.add("__TIMESTAMP__",ctime(&st.st_mtime));
#endif

  return oldtimestamp;
}

void CEXEBuild::restore_timestamp_predefine(char *oldtimestamp)
{
  definedlist.del("__TIMESTAMP__");
  if(oldtimestamp) {
      definedlist.add("__TIMESTAMP__",oldtimestamp);
      free(oldtimestamp);
  }
}

char *CEXEBuild::set_line_predefine(int linecnt, BOOL is_macro)
{
  char* linebuf = NULL;
  MANAGE_WITH(linebuf, free);

  char temp[128] = "";
  sprintf(temp,"%d",linecnt);

  char *oldline = definedlist.find("__LINE__");
  if(oldline) {
    oldline = strdup(oldline);
    definedlist.del("__LINE__");
  }
  if(is_macro && oldline) {
    linebuf = (char *)malloc(strlen(oldline)+strlen(temp)+2);
    sprintf(linebuf,"%s.%s",oldline,temp);
  }
  else {
    linebuf = strdup(temp);
  }
  definedlist.add("__LINE__",linebuf);

  return oldline;
}

void CEXEBuild::restore_line_predefine(char *oldline)
{
  definedlist.del("__LINE__");
  if(oldline) {
      definedlist.add("__LINE__",oldline);
      free(oldline);
  }
}

void CEXEBuild::set_date_time_predefines()
{
  time_t etime;
  struct tm * ltime;
  char datebuf[128];
  char timebuf[128];

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
  definedlist.add("__DATE__",(char *)datebuf);
  GetTimeFormat(LOCALE_USER_DEFAULT, 0, &stime, NULL, timebuf, sizeof(timebuf));
  definedlist.add("__TIME__",(char *)timebuf);
#else
  my_strftime(datebuf, sizeof(datebuf), "%x", ltime);
  definedlist.add("__DATE__",(char *)datebuf);
  my_strftime(timebuf, sizeof(timebuf), "%X", ltime);
  definedlist.add("__TIME__",(char *)timebuf);
#endif
}

void CEXEBuild::del_date_time_predefines()
{
  definedlist.del("__DATE__");
  definedlist.del("__TIME__");
}
#endif

int CEXEBuild::process_script(FILE *filepointer, char *filename)
{
  linecnt = 0;
  fp = filepointer;
  curfilename = filename;

  if (has_called_write_output)
  {
    ERROR_MSG("Error (process_script): write_output already called, can't continue\n");
    return PS_ERROR;
  }

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  set_date_time_predefines();
  char *oldfilename = set_file_predefine(curfilename);
  char *oldtimestamp = set_timestamp_predefine(curfilename);
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

  if (m_linebuild.getlen())
  {
    ERROR_MSG("Error: invalid script: last line ended with \\\n");
    return PS_ERROR;
  }

  if (ret == PS_EOF && num_ifblock())
  {
    ERROR_MSG("!if[macro][n]def: open at EOF - need !endif\n");
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
int CEXEBuild::doParse(const char *str)
{
  LineParser line(inside_comment);
  int res;

  while (*str == ' ' || *str == '\t') str++;

  // if ignoring, ignore all lines that don't begin with !.
  if (cur_ifblock && (cur_ifblock->ignore || cur_ifblock->inherited_ignore) && *str!='!' && !last_line_had_slash) return PS_OK;

  if (m_linebuild.getlen()>1) m_linebuild.resize(m_linebuild.getlen()-2);

  m_linebuild.add(str,strlen(str)+1);

  // remove trailing slash and null
  if (str[0] && CharPrev(str,str+strlen(str))[0] == '\\') {
    last_line_had_slash = 1;
    return PS_OK;
  }
  else last_line_had_slash = 0;

  res=line.parse((char*)m_linebuild.get(),!strnicmp((char*)m_linebuild.get(),"!define",7));

  inside_comment = line.InCommentBlock();

  m_linebuild.resize(0);

  if (res)
  {
    if (res==-2) ERROR_MSG("Error: unterminated string parsing line at %s:%d\n",curfilename,linecnt);
    else ERROR_MSG("Error: error parsing line (%s:%d)\n",curfilename,linecnt);
    return PS_ERROR;
  }

parse_again:
  if (line.getnumtokens() < 1) return PS_OK;

  int np,op,pos;
  int tkid=get_commandtoken(line.gettoken_str(0),&np,&op,&pos);
  if (tkid == -1)
  {
    char *p=line.gettoken_str(0);
    if (p[0] && p[strlen(p)-1]==':')
    {
      if (p[0] == '!' || (p[0] >= '0' && p[0] <= '9') || p[0] == '$' || p[0] == '-' || p[0] == '+')
      {
        ERROR_MSG("Invalid label: %s (labels cannot begin with !, $, -, +, or 0-9)\n",line.gettoken_str(0));
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
      ERROR_MSG("Invalid command: %s\n",line.gettoken_str(0));
      return PS_ERROR;
    }
  }

  if (IsTokenPlacedRight(pos, line.gettoken_str(0)) != PS_OK)
    return PS_ERROR;

  int v=line.getnumtokens()-(np+1);
  if (v < 0 || (op >= 0 && v > op)) // opt_parms is -1 for unlimited
  {
    ERROR_MSG("%s expects %d",line.gettoken_str(0),np);
    if (op < 0) ERROR_MSG("+");
    if (op > 0) ERROR_MSG("-%d",op+np);
    ERROR_MSG(" parameters, got %d.\n",line.getnumtokens()-1);
    PRINTHELP()
  }

  int if_from_else = 0;

  if (tkid == TOK_P_ELSE)
  {
    if (cur_ifblock && cur_ifblock->inherited_ignore)
      return PS_OK;

    if (!num_ifblock() || cur_ifblock->elseused)
    {
      ERROR_MSG("!else: stray !else\n");
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

    int v=line.gettoken_enum(0,"ifdef\0ifndef\0ifmacrodef\0ifmacrondef\0");
    if (v < 0) PRINTHELP()
    if (line.getnumtokens() == 1) PRINTHELP()
    int cmds[] = {TOK_P_IFDEF, TOK_P_IFNDEF, TOK_P_IFMACRODEF, TOK_P_IFMACRONDEF};
    tkid = cmds[v];
    if_from_else++;
  }

  if (tkid == TOK_P_IFNDEF || tkid == TOK_P_IFDEF ||
      tkid == TOK_P_IFMACRODEF || tkid == TOK_P_IFMACRONDEF)
  {
    if (!if_from_else)
      start_ifblock();

    if (cur_ifblock && cur_ifblock->inherited_ignore)
    {
      return PS_OK;
    }

    int istrue=0;
    
    int mod=0;
    int p;

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
        mod=line.gettoken_enum(p,"|\0&\0||\0&&\0");
        if (mod == -1) PRINTHELP()
        mod &= 1;
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
      ERROR_MSG("!endif: no !ifdef open\n");
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
void CEXEBuild::ps_addtoline(const char *str, GrowBuf &linedata, StringList &hist, bool bIgnoreDefines /*= false*/)
#else
void CEXEBuild::ps_addtoline(const char *str, GrowBuf &linedata, StringList &hist)
#endif
{
  // convert $\r, $\n to their literals
  // preprocessor replace ${VAR} and $%VAR% with whatever value
  // note that if VAR does not exist, ${VAR} or $%VAR% will go through unmodified
  const char *in=str;
  while (*in)
  {
    int add=1;
    char *t;
    char c=*in;
    t=CharNext(in);

    if (t-in > 1) // handle multibyte chars (no escape)
    {
      linedata.add((void*)in,t-in);
      in=t;
      continue;
    }
    in=t;

    if (c == '$')
    {
      if (in[0] == '\\')
      {
        if (in[1] == 'r')
        {
          in+=2;
          c='\r';
        }
        else if (in[1] == 'n')
        {
          in+=2;
          c='\n';
        }
        else if (in[1] == 't')
        {
          in+=2;
          c='\t';
        }
      }
      else if (in[0] == '{')
      {
        char *s=strdup(in+1);
        MANAGE_WITH(s, free);
        char *t=s;
        unsigned int bn = 0;
        while (*t)
        {
          if (*t == '{') bn++;
          if (*t == '}' && bn-- == 0) break;
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
          defname.add("",1);
          t=definedlist.find((char*)defname.get());
          if (t && hist.find((char*)defname.get(),0)<0)
          {
            in+=strlen(s)+2;
            add=0;
            hist.add((char*)defname.get(),0);
#ifdef NSIS_FIX_DEFINES_IN_STRINGS
            ps_addtoline(t,linedata,hist,true);
#else
            ps_addtoline(t,linedata,hist);
#endif
            hist.delbypos(hist.find((char*)defname.get(),0));
          }
        }
      }
      else if (in[0] == '%')
      {
        char *s=strdup(in+1);
        MANAGE_WITH(s, free);
        char *t=s;
        while (*t)
        {
          if (*t == '%') break;
          t=CharNext(t);
        }
        if (*t && t!=s)
        {
          *t=0;
          // check for defines inside the define name - ${bla${blo}}
          GrowBuf defname;
          ps_addtoline(s,defname,hist);
          defname.add("",1);
          t=getenv((char*)defname.get());
          if (t && hist.find((char*)defname.get(),0)<0)
          {
            in+=strlen(s)+2;
            add=0;
            hist.add((char*)defname.get(),0);
#ifdef NSIS_FIX_DEFINES_IN_STRINGS
            ps_addtoline(t,linedata,hist,true);
#else
            ps_addtoline(t,linedata,hist);
#endif
            hist.delbypos(hist.find((char*)defname.get(),0));
          }
        }
      }
#ifdef NSIS_FIX_DEFINES_IN_STRINGS
      else if (in[0] == '$')
      {
        if (in[1] == '{') // Found $$ before - Don't replace this define
        {
          char *s=strdup(in+2);
          MANAGE_WITH(s, free);
          char *t=s;
          unsigned int bn = 0;
          while (*t)
          {
            if (*t == '{') bn++;
            if (*t == '}' && bn-- == 0) break;
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
          linedata.add((void*)&c,1);
          in++;
        }
      }
#endif
    }
    if (add) linedata.add((void*)&c,1);
  }
}

int CEXEBuild::parseScript()
{
  char str[MAX_LINELENGTH];

  for (;;)
  {
    char *p=str;
    *p=0;
    fgets(str,MAX_LINELENGTH,fp);
    linecnt++;
    if (feof(fp)&&!str[0]) break;

    // remove trailing whitespace
    while (*p) p++;
    if (p > str) p--;
    while (p >= str && (*p == '\r' || *p == '\n' || *p == ' ' || *p == '\t')) p--;
    *++p=0;

    StringList hist;
    GrowBuf linedata;

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
    char *oldline = set_line_predefine(linecnt, FALSE);
#endif

    ps_addtoline(str,linedata,hist);
    linedata.add((void*)"",1);
    int ret=doParse((char*)linedata.get());

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
    // Added by Sunil Kamath 11 June 2003
    restore_line_predefine(oldline);
#endif
    
    if (ret != PS_OK) return ret;
  }

  return PS_EOF;
}

int CEXEBuild::includeScript(char *f)
{
  SCRIPT_MSG("!include: \"%s\"\n",f);
  FILE *incfp=FOPEN(f,"rt");
  if (!incfp)
  {
    ERROR_MSG("!include: could not open file: \"%s\"\n",f);
    return PS_ERROR;
  }

  // auto-fclose(3) incfp
  MANAGE_WITH(incfp, fclose);
  
  if (build_include_depth >= MAX_INCLUDEDEPTH)
  {
    ERROR_MSG("parseScript: too many levels of includes (%d max).\n",MAX_INCLUDEDEPTH);
    return PS_ERROR;
  }
  build_include_depth++;

  int last_linecnt=linecnt;
  linecnt=0;
  char *last_filename=curfilename;
  curfilename=f;
  FILE *last_fp=fp;
  fp=incfp;

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  char *oldfilename = set_file_predefine(curfilename);
  char *oldtimestamp = set_timestamp_predefine(curfilename);
#endif

  int r=parseScript();

#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  restore_file_predefine(oldfilename);
  restore_timestamp_predefine(oldtimestamp);
#endif

  int errlinecnt=linecnt;

  linecnt=last_linecnt;
  curfilename=last_filename;
  fp=last_fp;

  build_include_depth--;
  if (r != PS_EOF && r != PS_OK)
  {
    ERROR_MSG("!include: error in script: \"%s\" on line %d\n",f,errlinecnt);
    return PS_ERROR;
  }
  SCRIPT_MSG("!include: closed: \"%s\"\n",f);
  return PS_OK;
}

// !ifmacro[n]def based on Anders Kjersem's code
int CEXEBuild::MacroExists(const char *macroname)
{
  char *m = (char *) m_macros.get();

  while (m && *m)
  {
    // check if macroname matches
    if (!stricmp(m, macroname))
      return 1;

    // skip macro name
    m += strlen(m) + 1;

    // skip params
    while (*m) m += strlen(m) + 1;
    m++;

    // skip data
    while (*m) m += strlen(m) + 1;
    if (m - (char *) m_macros.get() >= m_macros.getlen() - 1) break;
    m++;
  }
  return 0;
}

int CEXEBuild::process_oneline(char *line, char *filename, int linenum)
{
  char *last_filename=curfilename;
  curfilename=filename;
  int last_linecnt=linecnt;
  linecnt=linenum;

  StringList hist;
  GrowBuf linedata;
  
#ifdef NSIS_SUPPORT_STANDARD_PREDEFINES
  // Added by Sunil Kamath 11 June 2003
  char *oldfilename = NULL;
  char *oldtimestamp = NULL;
  char *oldline = NULL;
  BOOL is_commandline = !strcmp(filename,"command line");
  BOOL is_macro = !strncmp(filename,"macro:",strlen("macro:"));

  if(!is_commandline) { // Don't set the predefines for command line /X option
    if(!is_macro) {
      oldfilename = set_file_predefine(curfilename);
      oldtimestamp = set_timestamp_predefine(curfilename);
    }
    oldline = set_line_predefine(linecnt, is_macro);
  }
#endif

  ps_addtoline(line,linedata,hist);
  linedata.add((void*)"",1);
  int ret=doParse((char*)linedata.get());

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
  const char *s=line.gettoken_str(wt);
  int v;

  if (!stricmp(s,"0") || !stricmp(s,"")) *offs=0;
  else if ((v=GetUserVarIndex(line, wt))>=0)
  {
    *offs=-v-1; // to jump to a user variable target, -variable_index-1 is stored.
  }
  else
  {
    if ((s[0] == '-' || s[0] == '+') && !atoi(s+1))
    {
      ERROR_MSG("Error: Goto targets beginning with '+' or '-' must be followed by nonzero integer (relative jump)\n");
      return 1;
    }
    if ((s[0] >= '0' && s[0] <= '9') || s[0] == '$' || s[0] == '!')
    {
      ERROR_MSG("Error: Goto targets cannot begin with 0-9, $, !\n");
      return 1;
    }
    *offs=ns_label.add(s,0);
  }
  return 0;
}

#define FLAG_OFFSET(flag) (FIELD_OFFSET(exec_flags, flag)/sizeof(int))
#define SECTION_FIELD_GET(field) (FIELD_OFFSET(section, field)/sizeof(int))
#define SECTION_FIELD_SET(field) (-1 - (int)(FIELD_OFFSET(section, field)/sizeof(int)))

// Func size: about 5000 lines (orip)
int CEXEBuild::doCommand(int which_token, LineParser &line)
{
  static const char *rootkeys[2] = {
    "HKCR\0HKLM\0HKCU\0HKU\0HKCC\0HKDD\0HKPD\0",
    "HKEY_CLASSES_ROOT\0HKEY_LOCAL_MACHINE\0HKEY_CURRENT_USER\0HKEY_USERS\0HKEY_CURRENT_CONFIG\0HKEY_DYN_DATA\0HKEY_PERFORMANCE_DATA\0"
  };
  static HKEY rootkey_tab[] = {
    HKEY_CLASSES_ROOT,HKEY_LOCAL_MACHINE,HKEY_CURRENT_USER,HKEY_USERS,HKEY_CURRENT_CONFIG,HKEY_DYN_DATA,HKEY_PERFORMANCE_DATA
  };

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  build_plugin_table();
#endif

  multiple_entries_instruction=0;

  entry ent={0,};
  switch (which_token)
  {
    // macro shit
    ///////////////////////////////////////////////////////////////////////////////
    case TOK_P_MACRO:
      {
        if (!line.gettoken_str(1)[0]) PRINTHELP()
        char *t=(char *)m_macros.get();
        while (t && *t)
        {
          if (!stricmp(t,line.gettoken_str(1))) break;
          t+=strlen(t)+1;

          // advance over parameters
          while (*t) t+=strlen(t)+1;
          t++;

          // advance over data
          while (*t) t+=strlen(t)+1;
          if (t-(char *)m_macros.get() >= m_macros.getlen()-1)
            break;
          t++;
        }
        if (t && *t)
        {
          ERROR_MSG("!macro: macro named \"%s\" already found!\n",line.gettoken_str(1));
          return PS_ERROR;
        }
        m_macros.add(line.gettoken_str(1),strlen(line.gettoken_str(1))+1);

        int pc;
        for (pc=2; pc < line.getnumtokens(); pc ++)
        {
          if (!line.gettoken_str(pc)[0])
          {
            ERROR_MSG("!macro: macro parameter %d is empty, not valid!\n",pc-1);
            return PS_ERROR;
          }
          int a;
          for (a=2; a < pc; a ++)
          {
            if (!stricmp(line.gettoken_str(pc),line.gettoken_str(a)))
            {
              ERROR_MSG("!macro: macro parameter named %s is used multiple times!\n",
                line.gettoken_str(pc));
              return PS_ERROR;
            }
          }
          m_macros.add(line.gettoken_str(pc),strlen(line.gettoken_str(pc))+1);
        }
        m_macros.add("",1);

        for (;;)
        {
          char str[MAX_LINELENGTH];
          char *p=str;
          str[0]=0;
          fgets(str,MAX_LINELENGTH,fp);
          //SCRIPT_MSG("%s%s", str, str[strlen(str)-1]=='\n'?"":"\n");
          if (feof(fp) && !str[0])
          {
            ERROR_MSG("!macro \"%s\": unterminated (no !macroend found in file)!\n",line.gettoken_str(1));
            return PS_ERROR;
          }
          // remove trailing whitespace
          while (*p) p++;
          if (p > str) p--;
          while (p >= str && (*p == '\r' || *p == '\n' || *p == ' ' || *p == '\t')) p--;
          *++p=0;
          LineParser l2(false);
          if (!l2.parse(str))
          {
            if (!stricmp(l2.gettoken_str(0),"!macroend"))
            {
              linecnt++;
              break;
            }
            if (!stricmp(l2.gettoken_str(0),"!macro"))
            {
              ERROR_MSG("Error: can't define a macro inside a macro!\n");
              return PS_ERROR;
            }
          }
          if (str[0]) m_macros.add(str,strlen(str)+1);
          else m_macros.add(" ",2);
          linecnt++;
        }
        m_macros.add("",1);
      }
    return PS_OK;
    case TOK_P_MACROEND:
      ERROR_MSG("!macroend: no macro currently open.\n");
    return PS_ERROR;
    case TOK_P_INSERTMACRO:
      {
        if (!line.gettoken_str(1)[0]) PRINTHELP()
        char *t=(char *)m_macros.get();
        char *m=t;
        while (t && *t)
        {
          if (!stricmp(t,line.gettoken_str(1))) break;
          t+=strlen(t)+1;

          // advance over parms
          while (*t) t+=strlen(t)+1;
          t++;

          // advance over data
          while (*t) t+=strlen(t)+1;
          if (t-(char *)m_macros.get() >= m_macros.getlen()-1)
            break;
          t++;
        }
        SCRIPT_MSG("!insertmacro: %s\n",line.gettoken_str(1));
        if (!t || !*t)
        {
          ERROR_MSG("!insertmacro: macro named \"%s\" not found!\n",line.gettoken_str(1));
          return PS_ERROR;
        }
        t+=strlen(t)+1;


        GrowBuf l_define_names;
        DefineList l_define_saves;
        int npr=0;
        // advance over parms
        while (*t)
        {
          char *v=definedlist.find(t);
          if (v)
          {
            l_define_saves.add(t,v);
            definedlist.del(t);
          }
          l_define_names.add(t,strlen(t)+1);
          definedlist.add(t,line.gettoken_str(npr+2));

          npr++;
          t+=strlen(t)+1;
        }
        l_define_names.add("",1);
        t++;
        if (npr != line.getnumtokens()-2)
        {
          ERROR_MSG("!insertmacro: macro \"%s\" requires %d parameter(s), passed %d!\n",
            line.gettoken_str(1),npr,line.getnumtokens()-2);
          return PS_ERROR;
        }

        int lp=0;
        char str[1024];
        if (m_macro_entry.find(line.gettoken_str(1),0)>=0)
        {
          ERROR_MSG("!insertmacro: macro \"%s\" already being inserted!\n",line.gettoken_str(1));
          return PS_ERROR;
        }
        int npos=m_macro_entry.add(line.gettoken_str(1),0);

        wsprintf(str,"macro:%s",line.gettoken_str(1));
        while (*t)
        {
          lp++;
          if (strcmp(t," "))
          {
            int ret=process_oneline(t,str,lp);
            if (ret != PS_OK)
            {
              ERROR_MSG("Error in macro %s on macroline %d\n",line.gettoken_str(1),lp);
              return ret;
            }
          }
          {
            // fix t if process_oneline changed m_macros
            char *nm=(char *)m_macros.get();
            if (nm != m)
            {
              t += nm - m;
              m = nm;
            }
          }
          t+=strlen(t)+1;
        }
        m_macro_entry.delbypos(npos);
        {
          char *p=(char*)l_define_names.get();
          while (*p)
          {
            definedlist.del(p);
            char *v;
            if ((v=l_define_saves.find(p))) definedlist.add(p,v);
            p+=strlen(p)+1;
          }
        }
        SCRIPT_MSG("!insertmacro: end of %s\n",line.gettoken_str(1));
      }

    return PS_OK;
    // page ordering shit
    ///////////////////////////////////////////////////////////////////////////////
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
    case TOK_UNINSTPAGE:
      set_uninstall_mode(1);
    case TOK_PAGE:
      {
        if (!uninstall_mode) {
          enable_last_page_cancel = 0;
          if (!stricmp(line.gettoken_str(line.getnumtokens()-1),"/ENABLECANCEL"))
            enable_last_page_cancel = 1;
        }
        else {
          uenable_last_page_cancel = 0;
          if (!stricmp(line.gettoken_str(line.getnumtokens()-1),"/ENABLECANCEL"))
            uenable_last_page_cancel = 1;
        }

        int k = line.gettoken_enum(1,"custom\0license\0components\0directory\0instfiles\0uninstConfirm");

        if (k < 0) PRINTHELP();

        if (add_page(k) != PS_OK)
          return PS_ERROR;

#ifndef NSIS_SUPPORT_CODECALLBACKS
        if (!k) {
          ERROR_MSG("Error: custom page specified, NSIS_SUPPORT_CODECALLBACKS not defined.\n");
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
              ERROR_MSG("Error: custom page must have a creator function!\n");
              PRINTHELP();
          }
        }
#endif//NSIS_SUPPORT_CODECALLBACKS

        SCRIPT_MSG("%sPage: %s", uninstall_mode?"Uninst":"", line.gettoken_str(1));

#ifdef NSIS_SUPPORT_CODECALLBACKS
        if (cur_page->prefunc>=0)
          SCRIPT_MSG(" (%s:%s)", k?"pre":"creator", line.gettoken_str(2));
        if (cur_page->showfunc>=0 && k)
          SCRIPT_MSG(" (show:%s)", line.gettoken_str(3));
        if (cur_page->leavefunc>=0)
          SCRIPT_MSG(" (leave:%s)", line.gettoken_str(4-!k));
        else if (cur_page->caption && !k)
          SCRIPT_MSG(" (caption:%s)", line.gettoken_str(3));
#endif
        SCRIPT_MSG("\n");

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
      int k = line.gettoken_enum(1,"custom\0license\0components\0directory\0instfiles\0uninstConfirm\0");
      if (k < 0) {
        k = line.gettoken_enum(1,"un.custom\0un.license\0un.components\0un.directory\0un.instfiles\0un.uninstConfirm\0");
        if (k < 0) PRINTHELP();
        set_uninstall_mode(1);
      }

      SCRIPT_MSG("PageEx: %s\n", line.gettoken_str(1));

      if (add_page(k) != PS_OK)
        return PS_ERROR;

      cur_page->flags |= PF_PAGE_EX;
    }
    return PS_OK;

    case TOK_PAGEEXEND:
    {
      SCRIPT_MSG("PageExEnd\n");

#ifdef NSIS_SUPPORT_CODECALLBACKS
      if (cur_page_type == PAGE_CUSTOM && !cur_page->prefunc) {
        ERROR_MSG("Error: custom pages must have a creator function.\n");
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
      SCRIPT_MSG("PageCallbacks:");

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
              if (strnicmp(line.gettoken_str(2), "un.", 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
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
              if (strnicmp(line.gettoken_str(1), "un.", 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
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
              if (strnicmp(line.gettoken_str(3), "un.", 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
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
              if (strnicmp(line.gettoken_str(2), "un.", 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
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
              if (strnicmp(line.gettoken_str(1), "un.", 3))
              {
                if (uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
                  return PS_ERROR;
                }
              }
              else
              {
                if (!uninstall_mode)
                {
                  ERROR_MSG("\nError: function names must start with \"un.\" in an uninstall page.\n");
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
        SCRIPT_MSG(" %s:%s", !custom?"pre":"creator", line.gettoken_str(1));
      if (cur_page->showfunc>=0 && !custom)
        SCRIPT_MSG(" show:%s", line.gettoken_str(2));
      if (cur_page->leavefunc>=0)
        SCRIPT_MSG(" leave:%s", line.gettoken_str(3-custom));

      SCRIPT_MSG("\n");
    }
    return PS_OK;
#else
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_CODECALLBACKS not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_SUPPORT_CODECALLBACKS
#else
    case TOK_PAGE:
    case TOK_UNINSTPAGE:
    case TOK_PAGEEX:
    case TOK_PAGEEXEND:
    case TOK_PAGECALLBACKS:
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_VISIBLE_SUPPORT not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
    // header flags
    ///////////////////////////////////////////////////////////////////////////////
    case TOK_LANGSTRING:
    {
      char *name = line.gettoken_str(1);
      LANGID lang = line.gettoken_int(2);
      char *str = line.gettoken_str(3);
      int ret = SetLangString(name, lang, str);
      if (ret == PS_WARNING)
        warning_fl("LangString \"%s\" set multiple times for %d, wasting space", name, lang);
      else if (ret == PS_ERROR) {
        ERROR_MSG("Error: can't set LangString \"%s\"!\n", name);
        return PS_ERROR;
      }
      SCRIPT_MSG("LangString: \"%s\" %d \"%s\"\n", name, lang, str);
    }
    return PS_OK;
    case TOK_LANGSTRINGUP:
      SCRIPT_MSG("Error: LangStringUP is obsolete, there are no more unprocessed strings. Use LangString.\n");
    return PS_ERROR;
    case TOK_LICENSELANGSTRING:
    {
#ifdef NSIS_CONFIG_SILENT_SUPPORT
      if (build_header.flags&(CH_FLAGS_SILENT|CH_FLAGS_SILENT_LOG))
      {
        warning_fl("LicenseLangString: SilentInstall enabled, wasting space");
      }
#endif
      char *name = line.gettoken_str(1);
      LANGID lang = line.gettoken_int(2);
      char *file = line.gettoken_str(3);

      FILE *fp;
      unsigned int datalen;
      fp=FOPEN(file,"rb");
      if (!fp)
      {
        ERROR_MSG("LicenseLangString: open failed \"%s\"\n",file);
        PRINTHELP()
      }
      MANAGE_WITH(fp, fclose);
      fseek(fp,0,SEEK_END);
      datalen=ftell(fp);
      if (!datalen)
      {
        ERROR_MSG("LicenseLangString: empty license file \"%s\"\n",file);
        return PS_ERROR;
      }
      rewind(fp);
      char *data=(char*)malloc(datalen+2);
      if (!data)
      {
        ERROR_MSG("Internal compiler error #12345: LicenseData malloc(%d) failed.\n", datalen+2);
        return PS_ERROR;
      }
      MANAGE_WITH(data, free);
      char *ldata=data+1;
      if (fread(ldata,1,datalen,fp) != datalen)
      {
        ERROR_MSG("LicenseLangString: can't read file.\n");
        return PS_ERROR;
      }
      ldata[datalen]=0;
      if (!strncmp(ldata,"{\\rtf",sizeof("{\\rtf")-1))
        *data = SF_RTF;
      else
        *data = SF_TEXT;

      int ret = SetLangString(name, lang, data);
      if (ret == PS_WARNING)
        warning_fl("LicenseLangString \"%s\" set multiple times for %d, wasting space", name, lang);
      else if (ret == PS_ERROR)
      {
        ERROR_MSG("Error: can't set LicenseLangString \"%s\"!\n", name);
        return PS_ERROR;
      }

      SCRIPT_MSG("LicenseLangString: \"%s\" %d \"%s\"\n", name, lang, file);
    }
    return PS_OK;
    case TOK_NAME:
      {
        if (SetInnerString(NLF_NAME,line.gettoken_str(1)) == PS_WARNING)
          warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
        SetInnerString(NLF_NAME_DA,line.gettoken_str(2));
        SCRIPT_MSG("Name: \"%s\"",line.gettoken_str(1));
        if (*line.gettoken_str(2))
          SCRIPT_MSG(" \"%s\"",line.gettoken_str(2));
        SCRIPT_MSG("\n");
      }
    return PS_OK;
    case TOK_CAPTION:
      {
        if (!cur_page)
        {
          if (SetInnerString(NLF_CAPTION,line.gettoken_str(1)) == PS_WARNING)
            warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
        }
        else
        {
          cur_page->caption = add_string(line.gettoken_str(1));
        }
        SCRIPT_MSG("Caption: \"%s\"\n",line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_ICON:
      SCRIPT_MSG("Icon: \"%s\"\n",line.gettoken_str(1));
      try {
        init_res_editor();
        if (replace_icon(res_editor, IDI_ICON2, line.gettoken_str(1))) {
          ERROR_MSG("Error: File doesn't exist or is an invalid icon file\n");
          return PS_ERROR;
        }
      }
      catch (exception& err) {
        ERROR_MSG("Error while replacing icon: %s\n", err.what());
        return PS_ERROR;
      }
    return PS_OK;
#ifdef NSIS_CONFIG_COMPONENTPAGE
    case TOK_CHECKBITMAP:
      SCRIPT_MSG("CheckBitmap: \"%s\"\n",line.gettoken_str(1));
      try {
        init_res_editor();
        int err = update_bitmap(res_editor, IDB_BITMAP1, line.gettoken_str(1), 96, 16, 8);
        if (err) {
          switch (err) {
            case -1:
              ERROR_MSG("Error: can't find bitmap\n");
              break;
            case -2:
              ERROR_MSG("Error: invalid bitmap file - corrupted or not a bitmap\n");
              break;
            case -3:
              ERROR_MSG("Error: bitmap isn't 96x16 in size\n");
              break;
            case -4:
              ERROR_MSG("Error: bitmap has more than 8bpp\n");
              break;
          }
          return PS_ERROR;
        }
      }
      catch (exception& err) {
        ERROR_MSG("Error while replacing bitmap: %s\n", err.what());
        return PS_ERROR;
      }
    return PS_OK;
#else//NSIS_CONFIG_COMPONENTPAGE
    case TOK_CHECKBITMAP:
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_COMPONENTPAGE not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_COMPONENTPAGE
    case TOK_DIRTEXT:
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
      {
        if (!cur_page) {
          if (SetInnerString(NLF_DIR_TEXT, line.gettoken_str(1)) == PS_WARNING)
            warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
          if (line.getnumtokens() > 2)
            SetInnerString(NLF_DIR_SUBTEXT, line.gettoken_str(2));
          if (line.getnumtokens() > 3)
            SetInnerString(NLF_BTN_BROWSE, line.gettoken_str(3));
          if (line.getnumtokens() > 4)
            SetInnerString(NLF_DIR_BROWSETEXT, line.gettoken_str(4));
        }
        else {
          if (cur_page_type != PAGE_DIRECTORY) {
            ERROR_MSG("Error: DirText can only be used inside PageEx directory.\n");
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
        SCRIPT_MSG("DirText: \"%s\" \"%s\" \"%s\" \"%s\"\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
      }
    return PS_OK;
#else//NSIS_CONFIG_VISIBLE_SUPPORT
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_VISIBLE_SUPPORT not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_VISIBLE_SUPPORT
    case TOK_DIRVAR:
    {
      if (cur_page_type != PAGE_DIRECTORY && cur_page_type != PAGE_UNINSTCONFIRM) {
        ERROR_MSG("Error: can't use DirVar outside of PageEx directory|uninstConfirm.\n");
        return PS_ERROR;
      }
      cur_page->parms[4] = GetUserVarIndex(line, 1) + 1;
      if (cur_page->parms[4] <= 0) PRINTHELP();
      SCRIPT_MSG("DirVar: %s\n", line.gettoken_str(1));
    }
    return PS_OK;
    case TOK_DIRVERIFY:
    {
      if (cur_page_type != PAGE_DIRECTORY) {
        ERROR_MSG("Error: can't use DirVerify outside of PageEx directory.\n");
        return PS_ERROR;
      }
      cur_page->flags &= ~PF_DIR_NO_BTN_DISABLE;
      int k = line.gettoken_enum(1,"auto\0leave\0");
      if (k == -1)
        PRINTHELP();
      if (k)
        cur_page->flags |= PF_DIR_NO_BTN_DISABLE;
      SCRIPT_MSG("DirVerify: %s\n", line.gettoken_str(1));
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
            warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
          if (line.getnumtokens() > 2)
            SetInnerString(NLF_COMP_SUBTEXT1, line.gettoken_str(2));
          if (line.getnumtokens() > 3)
            SetInnerString(NLF_COMP_SUBTEXT2, line.gettoken_str(3));
        }
        else {
          if (cur_page_type != PAGE_COMPONENTS) {
            ERROR_MSG("Error: ComponentText can only be used inside PageEx components.\n");
            return PS_ERROR;
          }
          cur_page->parms[0] = add_string(line.gettoken_str(1));
          cur_page->parms[1] = add_string(line.gettoken_str(2));
          cur_page->parms[2] = add_string(line.gettoken_str(3));
          cur_page->parms[3] = cur_page->parms[1];
          cur_page->parms[4] = cur_page->parms[2];
        }
        SCRIPT_MSG("ComponentText: \"%s\" \"%s\" \"%s\"\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
      }
    return PS_OK;
    case TOK_INSTTYPE:
      {
        int x;

        if (!stricmp(line.gettoken_str(1),"/NOCUSTOM"))
        {
          build_header.flags|=CH_FLAGS_NO_CUSTOM;
          SCRIPT_MSG("InstType: disabling custom install type\n");
        }
        else if (!stricmp(line.gettoken_str(1),"/COMPONENTSONLYONCUSTOM"))
        {
          build_header.flags|=CH_FLAGS_COMP_ONLY_ON_CUSTOM;
          SCRIPT_MSG("InstType: making components viewable only on custom install type\n");
        }
        else if (!strnicmp(line.gettoken_str(1),"/CUSTOMSTRING=",14))
        {
          SCRIPT_MSG("InstType: setting custom text to: \"%s\"\n",line.gettoken_str(1)+14);
          if (SetInnerString(NLF_COMP_CUSTOM,line.gettoken_str(1)+14) == PS_WARNING)
            warning_fl("%s: specified multiple times, wasting space","InstType /CUSTOMSTRING");
        }
        else if (line.gettoken_str(1)[0]=='/')
        {
          PRINTHELP()
        }
        else
        {
          char *itname = line.gettoken_str(1);

          if (!strnicmp(itname, "un.", 3)) {
            set_uninstall_mode(1);
            itname += 3;
          }

          for (x = 0; x < NSIS_MAX_INST_TYPES && cur_header->install_types[x]; x ++);
          if (x == NSIS_MAX_INST_TYPES)
          {
            ERROR_MSG("InstType: no more than %d install types allowed. %d specified\n", NSIS_MAX_INST_TYPES, NSIS_MAX_INST_TYPES + 1);
            return PS_ERROR;
          }
          else
          {
            cur_header->install_types[x] = add_string(itname);
            SCRIPT_MSG("InstType: %s%d=\"%s\"\n", uninstall_mode ? "(uninstall) " : "", x+1, itname);
          }

          set_uninstall_mode(0);
        }
      }
    return PS_OK;
#else//NSIS_CONFIG_COMPONENTPAGE
    case TOK_COMPTEXT:
    case TOK_INSTTYPE:
      ERROR_MSG("Error: %s specified but NSIS_CONFIG_COMPONENTPAGE not defined\n",line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_COMPONENTPAGE
#ifdef NSIS_CONFIG_LICENSEPAGE
    case TOK_LICENSETEXT:
      {
        if (!cur_page) {
          if (SetInnerString(NLF_LICENSE_TEXT, line.gettoken_str(1)) == PS_WARNING)
            warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
          SetInnerString(NLF_LICENSE_TEXT_FSRB, line.gettoken_str(1));
          SetInnerString(NLF_LICENSE_TEXT_FSCB, line.gettoken_str(1));
          if (line.getnumtokens() > 2)
            SetInnerString(NLF_BTN_LICENSE, line.gettoken_str(2));
        }
        else {
          if (cur_page_type != PAGE_LICENSE) {
            ERROR_MSG("Error: LicenseText can only be used inside PageEx license.\n");
            return PS_ERROR;
          }
          cur_page->parms[0] = add_string(line.gettoken_str(1));
          cur_page->next = add_string(line.gettoken_str(2));
        }
        SCRIPT_MSG("LicenseText: \"%s\" \"%s\"\n",line.gettoken_str(1),line.gettoken_str(2));
      }
    return PS_OK;
    case TOK_LICENSEDATA:
      {
        int idx = 0;
        char *file = line.gettoken_str(1);
        char *data = NULL;

        if (file[0] == '$' && file[1] == '(')
        {
          char *cp = strdup(file+2);
          MANAGE_WITH(cp, free);
          char *p = strchr(cp, ')');
          if (p && p[1] == 0) { // if string is only a language str identifier
            *p = 0;
            idx = DefineLangString(cp, 0);
          }
          data = file;
        }

        if (!idx)
        {
          unsigned int datalen;
          FILE *fp=FOPEN(file,"rb");
          if (!fp)
          {
            ERROR_MSG("LicenseData: open failed \"%s\"\n",file);
            PRINTHELP()
          }
          MANAGE_WITH(fp, fclose);
          fseek(fp,0,SEEK_END);
          datalen=ftell(fp);
          if (!datalen)
          {
            ERROR_MSG("LicenseData: empty license file \"%s\"\n",file);
            return PS_ERROR;
          }
          rewind(fp);
          data=(char*)malloc(datalen+2);
          if (!data)
          {
            ERROR_MSG("Internal compiler error #12345: LicenseData malloc(%d) failed.\n", datalen+2);
            return PS_ERROR;
          }
          //MANAGE_WITH(data, free);
          char *ldata=data+1;
          if (fread(ldata,1,datalen,fp) != datalen) {
            ERROR_MSG("LicenseData: can't read file.\n");
            free(data); // TODO: fix later (orip)
            return PS_ERROR;
          }
          ldata[datalen]=0;
          if (!strncmp(ldata,"{\\rtf",sizeof("{\\rtf")-1))
            *data = SF_RTF;
          else
            *data = SF_TEXT;
        }

        if (!cur_page) {
          if (SetInnerString(NLF_LICENSE_DATA,data) == PS_WARNING)
            warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
        }
        else {
          if (cur_page_type != PAGE_LICENSE) {
            ERROR_MSG("Error: LicenseData can only be used inside PageEx license.\n");
            return PS_ERROR;
          }

          cur_page->parms[1] = add_string(data, 0);
        }

        if (!idx) free(data); // TODO: fix later (orip)

        SCRIPT_MSG("LicenseData: \"%s\"\n",file);
      }
    return PS_OK;
    case TOK_LICENSEFORCESELECTION:
    {
      int k=line.gettoken_enum(1,"off\0checkbox\0radiobuttons\0");
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
          ERROR_MSG("Error: LicenseForceSelection can only be used inside PageEx license.\n");
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

      SCRIPT_MSG("LicenseForceSelection: %s \"%s\" \"%s\"\n", line.gettoken_str(1), line.gettoken_str(2), line.gettoken_str(3));
    }
    return PS_OK;
    case TOK_LICENSEBKCOLOR:
      {
        char *p = line.gettoken_str(1);
        if (!strcmpi(p,"/windows"))
        {
          build_header.license_bg=-COLOR_WINDOW;
          SCRIPT_MSG("LicenseBkColor: /windows\n");
        }
        else if (!strcmpi(p,"/grey") || !strcmpi(p,"/gray"))
        {
          build_header.license_bg=-COLOR_BTNFACE;
          SCRIPT_MSG("LicenseBkColor: /grey\n");
        }
        else
        {
        int v=strtoul(p,&p,16);
        build_header.license_bg=((v&0xff)<<16)|(v&0xff00)|((v&0xff0000)>>16);
        build_uninst.license_bg=build_header.license_bg;
        SCRIPT_MSG("LicenseBkColor: %06X\n",v);
      }
      }
    return PS_OK;
#else//!NSIS_CONFIG_LICENSEPAGE
    case TOK_LICENSETEXT:
    case TOK_LICENSEDATA:
    case TOK_LICENSEBKCOLOR:
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_LICENSEPAGE not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_LICENSEPAGE
#ifdef NSIS_CONFIG_SILENT_SUPPORT
    case TOK_SILENTINST:
    {
      int k=line.gettoken_enum(1,"normal\0silent\0silentlog\0");
      if (k<0) PRINTHELP()
#ifndef NSIS_CONFIG_LOG
      if (k == 2)
      {
        ERROR_MSG("SilentInstall: silentlog specified, no log support compiled in (use NSIS_CONFIG_LOG)\n");
        return PS_ERROR;
      }
#endif//NSIS_CONFIG_LOG
      SCRIPT_MSG("SilentInstall: %s\n",line.gettoken_str(1));
#ifdef NSIS_CONFIG_LICENSEPAGE
      if (k && HasUserDefined(NLF_LICENSE_DATA))
      {
        warning_fl("SilentInstall: LicenseData already specified. wasting space");
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
      int k=line.gettoken_enum(1,"normal\0silent\0");
      if (k<0) PRINTHELP()
      if (k)
        build_uninst.flags|=CH_FLAGS_SILENT;
      else
        build_uninst.flags&=~CH_FLAGS_SILENT;
      SCRIPT_MSG("SilentUnInstall: %s\n",line.gettoken_str(1));
    }
    return PS_OK;
#else
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_UNINSTALL_SUPPORT not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif
    case TOK_IFSILENT:
      ent.which=EW_IFFLAG;
      if (process_jump(line,1,&ent.offsets[0]) ||
          process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=FLAG_OFFSET(silent);
      ent.offsets[3]=~0;//new value mask - keep flag
      SCRIPT_MSG("IfSilent ?%s:%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SETSILENT:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(silent);
      int k=line.gettoken_enum(1,"normal\0silent\0");
      if (k<0) PRINTHELP()
      ent.offsets[1]=add_intstring(k);
      SCRIPT_MSG("SetSilent: %s\n",line.gettoken_str(1));
    }
    return add_entry(&ent);
#else//!NSIS_CONFIG_SILENT_SUPPORT
    case TOK_SILENTINST:
    case TOK_SILENTUNINST:
    case TOK_IFSILENT:
    case TOK_SETSILENT:
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_SILENT_SUPPORT not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_CONFIG_SILENT_SUPPORT
    case TOK_OUTFILE:
      strncpy(build_output_filename,line.gettoken_str(1),1024-1);
      SCRIPT_MSG("OutFile: \"%s\"\n",build_output_filename);
    return PS_OK;
    case TOK_INSTDIR:
    {
      char *p = line.gettoken_str(1);
      if (build_header.install_directory_ptr)
      {
        warning_fl("%s: specified multiple times. wasting space",line.gettoken_str(0));
      }
      build_header.install_directory_ptr = add_string(p);
      build_header.install_directory_auto_append = 0;
      char *p2 = p + strlen(p);
      if (*p && *CharPrev(p, p2) != '\\')
      {
        // we risk hitting $\r or something like $(bla\ad) or ${bla\ad} here, but it's better
        // than hitting backslashes in processed strings
        while (p2 > p && *p2 != '\\')
          p2 = CharPrev(p, p2);
        if (*p2 == '\\')
        {
          build_header.install_directory_auto_append = add_string(p2 + 1);
        }
      }
      SCRIPT_MSG("InstallDir: \"%s\"\n",line.gettoken_str(1));
    }
    return PS_OK;
    case TOK_INSTALLDIRREGKEY: // InstallDirRegKey
      {
        if (build_header.install_reg_key_ptr)
        {
          warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
        }
        int k=line.gettoken_enum(1,rootkeys[0]);
        if (k == -1) k=line.gettoken_enum(1,rootkeys[1]);
        if (k == -1) PRINTHELP()
        build_header.install_reg_rootkey=(int)rootkey_tab[k];
        build_header.install_reg_key_ptr = add_string(line.gettoken_str(2),0);
        if (line.gettoken_str(2)[0] == '\\')
          warning_fl("%s: registry path name begins with \'\\\', may cause problems",line.gettoken_str(0));
        build_header.install_reg_value_ptr = add_string(line.gettoken_str(3),0);
        SCRIPT_MSG("InstallRegKey: \"%s\\%s\\%s\"\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
      }
    return PS_OK;
    case TOK_CRCCHECK:
      build_crcchk=line.gettoken_enum(1,"off\0on\0force\0");
      if (build_crcchk==-1) PRINTHELP()
      SCRIPT_MSG("CRCCheck: %s\n",line.gettoken_str(1));
    return PS_OK;
    case TOK_INSTPROGRESSFLAGS:
      {
        int x;
        int smooth=0;
        build_header.flags&=~CH_FLAGS_PROGRESS_COLORED;
        for (x = 1; x < line.getnumtokens(); x ++)
        {
          if (!stricmp(line.gettoken_str(x),"smooth")) smooth=1;
          else if (!stricmp(line.gettoken_str(x),"colored")) build_header.flags|=CH_FLAGS_PROGRESS_COLORED;
          else PRINTHELP()
        }
        try {
          init_res_editor();

          BYTE* dlg = res_editor->GetResource(RT_DIALOG, MAKEINTRESOURCE(IDD_INSTFILES), NSIS_DEFAULT_LANG);
          if (!dlg) throw runtime_error("IDD_INSTFILES doesn't exist!");
          CDialogTemplate dt(dlg,uDefCodePage);
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
          res_editor->UpdateResource(RT_DIALOG, MAKEINTRESOURCE(IDD_INSTFILES), NSIS_DEFAULT_LANG, dlg, dwSize);
          res_editor->FreeResource(dlg);
        }
        catch (exception& err) {
          ERROR_MSG("Error setting smooth progress bar: %s\n", err.what());
          return PS_ERROR;
        }
        SCRIPT_MSG("InstProgressFlags: smooth=%d, colored=%d\n",smooth,
          !!(build_header.flags&CH_FLAGS_PROGRESS_COLORED));
      }
    return PS_OK;
    case TOK_AUTOCLOSE:
      {
        int k=line.gettoken_enum(1,"false\0true\0");
        if (k == -1) PRINTHELP();
        if (k)
          build_header.flags|=CH_FLAGS_AUTO_CLOSE;
        else
          build_header.flags&=~CH_FLAGS_AUTO_CLOSE;
        SCRIPT_MSG("AutoCloseWindow: %s\n",k?"true":"false");
      }
    return PS_OK;
    case TOK_WINDOWICON:
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
      disable_window_icon=line.gettoken_enum(1,"on\0off\0");
      if (disable_window_icon == -1) PRINTHELP();
      SCRIPT_MSG("WindowIcon: %s\n",line.gettoken_str(1));
    return PS_OK;
#else
    ERROR_MSG("Error: %s specified, NSIS_CONFIG_VISIBLE_SUPPORT not defined.\n",line.gettoken_str(0));
    return PS_ERROR;
#endif // NSIS_CONFIG_VISIBLE_SUPPORT
    case TOK_SHOWDETAILSUNINST:
#ifndef NSIS_CONFIG_UNINSTALL_SUPPORT
      ERROR_MSG("Error: ShowUninstDetails specified but NSIS_CONFIG_UNINSTALL_SUPPORT not defined\n");
      return PS_ERROR;
#endif
    case TOK_SHOWDETAILS:
      {
        int k=line.gettoken_enum(1,"hide\0show\0nevershow\0");
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
        SCRIPT_MSG("%s: %s\n",line.gettoken_str(0),line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_DIRSHOW:
      /*{
        int k=line.gettoken_enum(1,"show\0hide\0");
        if (k == -1) PRINTHELP();
        if (k)
          build_header.flags|=CH_FLAGS_DIR_NO_SHOW;
        else
          build_header.flags&=~CH_FLAGS_DIR_NO_SHOW;
        SCRIPT_MSG("DirShow: %s\n",k?"hide":"show");
      }*/
      ERROR_MSG("Error: DirShow doesn't currently work\n");
    return PS_ERROR;
    case TOK_ROOTDIRINST:
      {
        int k=line.gettoken_enum(1,"true\0false\0");
        if (k == -1) PRINTHELP();
        if (k)
          build_header.flags|=CH_FLAGS_NO_ROOT_DIR;
        else
          build_header.flags&=~CH_FLAGS_NO_ROOT_DIR;
        SCRIPT_MSG("AllowRootDirInstall: %s\n",k?"false":"true");
      }
    return PS_OK;
    case TOK_BGFONT:
#ifndef NSIS_SUPPORT_BGBG
      ERROR_MSG("Error: BGFont specified but NSIS_SUPPORT_BGBG not defined\n");
      return PS_ERROR;
#else//NSIS_SUPPORT_BGBG
      if (line.getnumtokens()==1)
      {
        memcpy(&bg_font,&bg_default_font,sizeof(LOGFONT));
        SCRIPT_MSG("BGFont: default font\n");
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

      strncpy(newfont.lfFaceName,line.gettoken_str(1),LF_FACESIZE);

      SCRIPT_MSG("BGFont: \"%s\"",line.gettoken_str(1));
      {
        bool height=false;
        bool weight=false;
        for (int i = 2; i < line.getnumtokens(); i++) {
          char *tok=line.gettoken_str(i);
          if (tok[0]=='/') {
            if (!strcmpi(tok,"/ITALIC")) {
              SCRIPT_MSG(" /ITALIC");
              newfont.lfItalic=TRUE;
            }
            else if (!strcmpi(tok,"/UNDERLINE")) {
              SCRIPT_MSG(" /UNDERLINE");
              newfont.lfUnderline=TRUE;
            }
            else if (!strcmpi(tok,"/STRIKE")) {
              SCRIPT_MSG(" /STRIKE");
              newfont.lfStrikeOut=TRUE;
            }
            else {
              SCRIPT_MSG("\n");
              PRINTHELP();
            }
          }
          else {
            if (!height) {
              SCRIPT_MSG(" height=%s",tok);
              newfont.lfHeight=line.gettoken_int(i);
              height=true;
            }
            else if (!weight) {
              SCRIPT_MSG(" weight=%s",tok);
              newfont.lfWeight=line.gettoken_int(i);
              weight=true;
            }
            else {
              SCRIPT_MSG("\n");
              PRINTHELP();
            }
          }
        }
      }
      SCRIPT_MSG("\n");
      memcpy(&bg_font, &newfont, sizeof(LOGFONT));
    return PS_OK;
#endif//NSIS_SUPPORT_BGBG
    case TOK_BGGRADIENT:
#ifndef NSIS_SUPPORT_BGBG
      ERROR_MSG("Error: BGGradient specified but NSIS_SUPPORT_BGBG not defined\n");
      return PS_ERROR;
#else//NSIS_SUPPORT_BGBG
      if (line.getnumtokens()==1)
      {
        SCRIPT_MSG("BGGradient: default colors\n");
        build_header.bg_color1=0;
        build_header.bg_color2=RGB(0,0,255);
      }
      else if (!stricmp(line.gettoken_str(1),"off"))
      {
        build_header.bg_color1=build_header.bg_color2=build_header.bg_textcolor=-1;
        SCRIPT_MSG("BGGradient: off\n");
        if (line.getnumtokens()>2) PRINTHELP()
      }
      else
      {
        char *p = line.gettoken_str(1);
        int v1,v2,v3=-1;
        v1=strtoul(p,&p,16);
        build_header.bg_color1=((v1&0xff)<<16)|(v1&0xff00)|((v1&0xff0000)>>16);
        p=line.gettoken_str(2);
        v2=strtoul(p,&p,16);
        build_header.bg_color2=((v2&0xff)<<16)|(v2&0xff00)|((v2&0xff0000)>>16);

        p=line.gettoken_str(3);
        if (*p)
        {
          if (!stricmp(p,"notext")) build_header.bg_textcolor=-1;
          else
          {
            v3=strtoul(p,&p,16);
            build_header.bg_textcolor=((v3&0xff)<<16)|(v3&0xff00)|((v3&0xff0000)>>16);
          }
        }

        SCRIPT_MSG("BGGradient: 0x%06X->0x%06X (text=0x%06X)\n",v1,v2,v3);
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
      char *p = line.gettoken_str(1);
      if (p[0]=='/')
      {
        if (stricmp(p,"/windows") || line.getnumtokens()!=2) PRINTHELP()
        build_header.lb_fg=build_header.lb_bg=-1;
        SCRIPT_MSG("InstallColors: windows default colors\n");
      }
      else
      {
        int v1,v2;
        if (line.getnumtokens()!=3) PRINTHELP()
        v1=strtoul(p,&p,16);
        build_header.lb_fg=((v1&0xff)<<16)|(v1&0xff00)|((v1&0xff0000)>>16);
        p=line.gettoken_str(2);
        v2=strtoul(p,&p,16);
        build_header.lb_bg=((v2&0xff)<<16)|(v2&0xff00)|((v2&0xff0000)>>16);
        SCRIPT_MSG("InstallColors: fg=%06X bg=%06X\n",v1,v2);
      }

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      build_uninst.lb_fg=build_header.lb_fg;
      build_uninst.lb_bg=build_header.lb_bg;
#endif
    }
    return PS_OK;
    case TOK_XPSTYLE:
      try {
        int k=line.gettoken_enum(1,"on\0off\0");
        if (k == -1) PRINTHELP()
        SCRIPT_MSG("XPStyle: %s\n", line.gettoken_str(1));
        init_res_editor();
        const char *szXPManifest = k ? 0 : "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\"><assemblyIdentity version=\"1.0.0.0\" processorArchitecture=\"X86\" name=\"Nullsoft.NSIS.exehead\" type=\"win32\"/><description>Nullsoft Install System v2.02</description><dependency><dependentAssembly><assemblyIdentity type=\"win32\" name=\"Microsoft.Windows.Common-Controls\" version=\"6.0.0.0\" processorArchitecture=\"X86\" publicKeyToken=\"6595b64144ccf1df\" language=\"*\" /></dependentAssembly></dependency></assembly>";
        res_editor->UpdateResource(MAKEINTRESOURCE(24), MAKEINTRESOURCE(1), NSIS_DEFAULT_LANG, (unsigned char*)szXPManifest, k ? 0 : strlen(szXPManifest));
      }
      catch (exception& err) {
        ERROR_MSG("Error while adding XP style: %s\n", err.what());
        return PS_ERROR;
      }
    return PS_OK;
    case TOK_CHANGEUI:
      try {
        DWORD dwSize;
        int k=line.gettoken_enum(1, "all\0IDD_LICENSE\0IDD_DIR\0IDD_SELCOM\0IDD_INST\0IDD_INSTFILES\0IDD_UNINST\0IDD_VERIFY\0IDD_LICENSE_FSRB\0IDD_LICENSE_FSCB\0");
        if (k<0) PRINTHELP();

        FILE *fui = FOPEN(line.gettoken_str(2), "rb");
        if (!fui) {
          ERROR_MSG("Error: Can't open \"%s\"!\n", line.gettoken_str(2));
          return PS_ERROR;
        }
        MANAGE_WITH(fui, fclose);

        fseek(fui, 0, SEEK_END);
        unsigned int len = ftell(fui);
        fseek(fui, 0, SEEK_SET);
        LPBYTE ui = (LPBYTE) malloc(len);
        if (!ui) {
          ERROR_MSG("Internal compiler error #12345: malloc(%d) failed\n", len);
          extern void quit(); quit();
        }
        MANAGE_WITH(ui, free);
        if (fread(ui, 1, len, fui) != len) {
          ERROR_MSG("Error: Can't read \"%s\"!\n", line.gettoken_str(2));
          return PS_ERROR;
        }

        CResourceEditor *uire = new CResourceEditor(ui, len);

        init_res_editor();

        // Search for required items
        #define GET(x) dlg = uire->GetResource(RT_DIALOG, MAKEINTRESOURCE(x), 0); if (!dlg) return PS_ERROR; CDialogTemplate UIDlg(dlg, uDefCodePage);
        #define SEARCH(x) if (!UIDlg.GetItem(x)) {ERROR_MSG("Error: Can't find %s (%u) in the custom UI!\n", #x, x);delete [] dlg;delete uire;return PS_ERROR;}
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
            if (IS_INTRESOURCE(dlgItem->szClass)) {
              if (dlgItem->szClass == MAKEINTRESOURCE(0x0082)) {
                if ((dlgItem->dwStyle & SS_BITMAP) == SS_BITMAP) {
                  branding_image_found = true;
                  branding_image_id = dlgItem->wId;
                  break;
                }
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

        SCRIPT_MSG("ChangeUI: %s %s%s\n", line.gettoken_str(1), line.gettoken_str(2), branding_image_found?" (branding image holder found)":"");
      }
      catch (exception& err) {
        ERROR_MSG("Error while changing UI: %s\n", err.what());
        return PS_ERROR;
      }
    return PS_OK;
    case TOK_ADDBRANDINGIMAGE:
#ifdef _WIN32
      try {
        int k=line.gettoken_enum(1,"top\0left\0bottom\0right\0");
        int wh=line.gettoken_int(2);
        if (k == -1) PRINTHELP();
        int padding = 2;
        if (line.getnumtokens() == 4)
          padding = line.gettoken_int(3);

        init_res_editor();
        BYTE* dlg = res_editor->GetResource(RT_DIALOG, MAKEINTRESOURCE(IDD_INST), NSIS_DEFAULT_LANG);

        CDialogTemplate dt(dlg,uDefCodePage);
        delete [] dlg;

        DialogItemTemplate brandingCtl = {0,};

        brandingCtl.dwStyle = SS_BITMAP | WS_CHILD | WS_VISIBLE;
        brandingCtl.sX = padding;
        brandingCtl.sY = padding;
        brandingCtl.szClass = MAKEINTRESOURCE(0x0082);
        brandingCtl.szTitle = "";
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

        res_editor->FreeResource(dlg);

        dt.DlgUnitsToPixels(brandingCtl.sWidth, brandingCtl.sHeight);
        SCRIPT_MSG("AddBrandingImage: %s %ux%u\n", line.gettoken_str(1), brandingCtl.sWidth, brandingCtl.sHeight);

        branding_image_found = true;
        branding_image_id = IDC_BRANDIMAGE;
      }
      catch (exception& err) {
        ERROR_MSG("Error while adding image branding support: %s\n", err.what());
        return PS_ERROR;
      }
    return PS_OK;
#else
      ERROR_MSG("Error: AddBrandingImage is disabled for non Win32 platforms.\n");
    return PS_ERROR;
#endif
    case TOK_SETFONT:
    {
      if (!strnicmp(line.gettoken_str(1), "/LANG=", 6))
      {
        LANGID lang_id = atoi(line.gettoken_str(1) + 6);
        LanguageTable *table = GetLangTable(lang_id);
        table->nlf.m_szFont = (char*)malloc(strlen(line.gettoken_str(2))+1);
        strcpy(table->nlf.m_szFont, line.gettoken_str(2));
        table->nlf.m_iFontSize = line.gettoken_int(3);

        SCRIPT_MSG("SetFont: lang=%d \"%s\" %s\n", lang_id, line.gettoken_str(2), line.gettoken_str(3));
      }
      else
      {
        strncpy(build_font, line.gettoken_str(1), sizeof(build_font));
        build_font_size = line.gettoken_int(2);

        SCRIPT_MSG("SetFont: \"%s\" %s\n", line.gettoken_str(1), line.gettoken_str(2));
      }
    }
    return PS_OK;
#else
  case TOK_INSTCOLORS:
  case TOK_XPSTYLE:
  case TOK_CHANGEUI:
  case TOK_ADDBRANDINGIMAGE:
  case TOK_SETFONT:
    ERROR_MSG("Error: %s specified, NSIS_CONFIG_VISIBLE_SUPPORT not defined.\n",line.gettoken_str(0));
  return PS_ERROR;
#endif// NSIS_CONFIG_VISIBLE_SUPPORT
    // Ability to change compression methods from within the script
    case TOK_SETCOMPRESSOR:
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    {
      if (build_compressor_set) {
        ERROR_MSG("Error: can't change compressor after data already got compressed or header already changed!\n");
        return PS_ERROR;
      }
      if (!build_compressor_final)
      {
        int a = 1;
        if (!strcmpi(line.gettoken_str(1),"/FINAL"))
        {
          build_compressor_final = true;
          a++;
        }
        else if (line.getnumtokens() == 3)
        {
          ERROR_MSG("%s expects 2 parameters, got 3.\n",line.gettoken_str(0));
          PRINTHELP();
        }
        int k=line.gettoken_enum(a,"zlib\0bzip2\0lzma\0");
        switch (k) {
          case 0: // JF> should handle the state of going from bzip2 back to zlib:
            compressor = &zlib_compressor;
            update_exehead(zlib_exehead, zlib_exehead_size);
#ifdef NSIS_ZLIB_COMPRESS_WHOLE
            build_compress_whole=true;
#else
            build_compress_whole=false;
#endif
          break;

          case 1:
            compressor=&bzip2_compressor;
            update_exehead(bzip2_exehead, bzip2_exehead_size);
#ifdef NSIS_BZIP2_COMPRESS_WHOLE
            build_compress_whole=true;
#else
            build_compress_whole=false;
#endif
            break;

          case 2:
            compressor = &lzma_compressor;
            update_exehead(lzma_exehead, lzma_exehead_size);
#ifdef NSIS_LZMA_COMPRESS_WHOLE
            build_compress_whole=true;
#else
            build_compress_whole=false;
#endif
          break;

          default:
            PRINTHELP();
        }
        SCRIPT_MSG("SetCompressor: %s%s\n", build_compressor_final? "/FINAL " : "", line.gettoken_str(a));
      }
      else
      {
        warning_fl("SetCompressor ignored due to previous call with the /FINAL switch");
      }
    }
    return PS_OK;
#else//NSIS_CONFIG_COMPRESSION_SUPPORT
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_COMPRESSION_SUPPORT not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_CONFIG_COMPRESSION_SUPPORT
    case TOK_LOADNLF:
    {
      SCRIPT_MSG("LoadLanguageFile: %s\n", line.gettoken_str(1));

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
      char lang_id[16];
      char lang_name[1024];
      wsprintf(lang_name, "LANG_%s", table->nlf.m_szName);
      wsprintf(lang_id, "%u", table->lang_id);
      definedlist.add(lang_name, lang_id);
    }
    return PS_OK;

    // preprocessor-ish (ifdef/ifndef/else/endif are handled one step out from here)
    ///////////////////////////////////////////////////////////////////////////////
    case TOK_P_DEFINE:
      if (definedlist.add(line.gettoken_str(1),line.gettoken_str(2)))
      {
        ERROR_MSG("!define: \"%s\" already defined!\n",line.gettoken_str(1));
        return PS_ERROR;
      }
      SCRIPT_MSG("!define: \"%s\"=\"%s\"\n",line.gettoken_str(1),line.gettoken_str(2));
    return PS_OK;
    case TOK_P_UNDEF:
      if (definedlist.del(line.gettoken_str(1)))
      {
        ERROR_MSG("!undef: \"%s\" not defined!\n",line.gettoken_str(1));
        return PS_ERROR;
      }
      SCRIPT_MSG("!undef: \"%s\"\n",line.gettoken_str(1));
    return PS_OK;
    case TOK_P_PACKEXEHEADER:
      strncpy(build_packname,line.gettoken_str(1),sizeof(build_packname)-1);
      strncpy(build_packcmd,line.gettoken_str(2),sizeof(build_packcmd)-1);
      SCRIPT_MSG("!packhdr: filename=\"%s\", command=\"%s\"\n",
        build_packname, build_packcmd);
    return PS_OK;
    case TOK_P_SYSTEMEXEC:
      {
        char *exec=line.gettoken_str(1);
        int comp=line.gettoken_enum(2,"<\0>\0<>\0=\0ignore\0");
        if (line.getnumtokens() == 2) comp = 4;
        if (comp == -1 && line.getnumtokens() == 3) comp=4;
        if (comp == -1) PRINTHELP()
        int success=0;
        int cmpv=line.gettoken_int(3,&success);
        if (!success && comp != 4) PRINTHELP()
        SCRIPT_MSG("!system: \"%s\"\n",exec);
#ifdef _WIN32
        int ret=system(exec);
#else
        char *execfixed = my_convert(exec);
        int ret=system(execfixed);
        my_convert_free(execfixed);
#endif
        if (comp == 0 && ret < cmpv);
        else if (comp == 1 && ret > cmpv);
        else if (comp == 2 && ret != cmpv);
        else if (comp == 3 && ret == cmpv);
        else if (comp == 4);
        else
        {
          ERROR_MSG("!system: returned %d, aborting\n",ret);
          return PS_ERROR;
        }
        SCRIPT_MSG("!system: returned %d\n",ret);
      }
    return PS_OK;
    case TOK_P_EXECUTE:
      {
        char *exec=line.gettoken_str(1);
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
        char *execfixed = my_convert(exec);
        system(execfixed);
        my_convert_free(execfixed);
#endif
        SCRIPT_MSG("!execute: \"%s\"\n",exec);
      }
    case TOK_P_ADDINCLUDEDIR:
      include_dirs.add(line.gettoken_str(1),0);
    return PS_OK;
    case TOK_P_INCLUDE:
      {
        char *f = line.gettoken_str(1);
        int included = 0;
#ifdef _WIN32
        WIN32_FIND_DATA fd;
        unsigned int malloced = sizeof(fd.cFileName) + strlen(f) + 1;
        
        char *incfile = (char *) malloc(malloced);

        strcpy(incfile, f);
        char *slash = strrchr(incfile, PATH_SEPARATOR_C);

        HANDLE search = FindFirstFile(f, &fd);
        if (search != INVALID_HANDLE_VALUE)
        {
          do
          {
            if (slash)
              slash[1] = 0;
            else
              incfile[0] = 0;
            strcat(incfile, fd.cFileName);
            if (includeScript(incfile) != PS_OK)
#else
        unsigned int malloced = strlen(f) + 100;
        char *incfile = (char *) malloc(malloced);
        assert(incfile != 0);
        MANAGE_WITH(incfile, free);
        strcpy(incfile, f);
        glob_t globbuf;
        if (!GLOB(incfile, GLOB_NOSORT, NULL, &globbuf))
        {
          for (unsigned int i = 0; i < globbuf.gl_pathc; i++)
          {
            if (includeScript(globbuf.gl_pathv[i]) != PS_OK)
#endif
              return PS_ERROR;
            included++;
          }
#ifdef _WIN32
          while (FindNextFile(search, &fd));
          FindClose(search);
#else
          globfree(&globbuf);
#endif
        }
        else
        {
          char *dir = include_dirs.get();
          int dirs = include_dirs.getnum();

          for (int i = 0; i < dirs; i++) {
            if (malloced < strlen(f) + strlen(dir) + 1)
            {
              free(incfile);
              malloced += strlen(dir);
              incfile = (char *) malloc(malloced);
            }
            strcpy(incfile, dir);
            if (*f != PATH_SEPARATOR_C)
              strcat(incfile, PATH_SEPARATOR_STR);
            strcat(incfile, f);
#ifdef _WIN32
            slash = strrchr(incfile, PATH_SEPARATOR_C);

            search = FindFirstFile(incfile, &fd);
            if (search != INVALID_HANDLE_VALUE)
            {
              do
              {
                if (slash)
                  slash[1] = 0;
                else
                  incfile[0] = 0;
                strcat(incfile, fd.cFileName);

                if (includeScript(incfile) != PS_OK)
#else
            if (!GLOB(incfile, GLOB_NOSORT, NULL, &globbuf))
            {
              for (unsigned int i = 0; i < globbuf.gl_pathc; i++)
              {
                if (includeScript(globbuf.gl_pathv[i]) != PS_OK)
#endif
                  return PS_ERROR;
                included++;
              }
#ifdef _WIN32
              while (FindNextFile(search, &fd));
              FindClose(search);
#else
              globfree(&globbuf);
#endif
              break;
            }
            else
            {
              dir += strlen(dir) + 1;
            }
          }
        }

        if (!included)
        {
          ERROR_MSG("!include: could not find: \"%s\"\n",f);
          return PS_ERROR;
        }
      }
    return PS_OK;
    case TOK_P_CD:
      if (!line.gettoken_str(1)[0] || chdir(line.gettoken_str(1)))
      {
        ERROR_MSG("!cd: error changing to: \"%s\"\n",line.gettoken_str(1));
        return PS_ERROR;
      }
    return PS_OK;
    case TOK_P_ERROR:
      ERROR_MSG("!error: %s\n",line.gettoken_str(1));
    return PS_ERROR;
    case TOK_P_WARNING:
      warning_fl("!warning: %s",line.gettoken_str(1));
    return PS_OK;
    case TOK_P_ECHO:
      SCRIPT_MSG("%s (%s:%d)\n", line.gettoken_str(1),curfilename,linecnt);
    return PS_OK;

    case TOK_P_VERBOSE:
    {
      extern int g_display_errors;
      int k=line.gettoken_enum(1,"push\0pop\0");
      int v;
      if (k < 0)
        // just set
        v=line.gettoken_int(1);
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
            return PS_OK;
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
          return PS_OK;
        }
      }
      display_script=v>3;
      display_info=v>2;
      display_warnings=v>1;
      display_errors=v>0;
      g_display_errors=display_errors;
    }
    return PS_OK;

    case TOK_UNINSTALLEXENAME: PRINTHELP()


#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    case TOK_UNINSTCAPTION:
      {
        if (SetInnerString(NLF_UCAPTION,line.gettoken_str(1)) == PS_WARNING)
          warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
        SCRIPT_MSG("UninstCaption: \"%s\"\n",line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_UNINSTICON:
      SCRIPT_MSG("UninstallIcon: \"%s\"\n",line.gettoken_str(1));
      try {
        free(m_unicon_data);
        m_unicon_data = generate_uninstall_icon_data(line.gettoken_str(1));
        if (!m_unicon_data) {
          ERROR_MSG("Error: File doesn't exist or is an invalid icon file\n");
          return PS_ERROR;
        }
      }
      catch (exception& err) {
        ERROR_MSG("Error while replacing icon: %s\n", err.what());
        return PS_ERROR;
      }
    return PS_OK;
    case TOK_UNINSTTEXT:
      {
        if (!cur_page) {
          if (SetInnerString(NLF_UNINST_TEXT, line.gettoken_str(1)) == PS_WARNING)
            warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
          SetInnerString(NLF_UNINST_SUBTEXT, line.gettoken_str(2));
        }
        else {
          if (cur_page_type != PAGE_UNINSTCONFIRM) {
            ERROR_MSG("Error: UninstallText can only be used inside PageEx uninstConfirm.\n");
            return PS_ERROR;
          }
          cur_page->parms[0] = add_string(line.gettoken_str(1));
          cur_page->parms[1] = add_string(line.gettoken_str(2));
        }
        SCRIPT_MSG("UninstallText: \"%s\" \"%s\"\n",line.gettoken_str(1),line.gettoken_str(2));
      }
    return PS_OK;
    case TOK_UNINSTSUBCAPTION:
      {
        int s;
        int w=line.gettoken_int(1,&s);
        if (!s || w < 0 || w > 2) PRINTHELP()
        SetInnerString(NLF_USUBCAPTION_CONFIRM+w,line.gettoken_str(2));
        SCRIPT_MSG("UninstSubCaption: page:%d, text=%s\n",w,line.gettoken_str(2));
      }
    return PS_OK;
    case TOK_WRITEUNINSTALLER:
      if (uninstall_mode)
      {
        ERROR_MSG("WriteUninstaller only valid from install, not from uninstall.\n");
        PRINTHELP()
      }
      uninstaller_writes_used++;
      ent.which=EW_WRITEUNINSTALLER;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      if (!ent.offsets[0]) PRINTHELP()
      SCRIPT_MSG("WriteUninstaller: \"%s\"\n",line.gettoken_str(1));

      DefineInnerLangString(NLF_ERR_CREATING);
      DefineInnerLangString(NLF_CREATED_UNINST);
    return add_entry(&ent);
#else//!NSIS_CONFIG_UNINSTALL_SUPPORT
    case TOK_WRITEUNINSTALLER:
    case TOK_UNINSTCAPTION:
    case TOK_UNINSTICON:
    case TOK_UNINSTTEXT:
    case TOK_UNINSTSUBCAPTION:
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_UNINSTALL_SUPPORT not defined.\n", line.gettoken_str(0));
    return PS_ERROR;
#endif



    // section/function shit
    ///////////////////////////////////////////////////////////////////////////////

    case TOK_SECTION:
    {
      int a=1,unselected = 0;
      if (!strcmpi(line.gettoken_str(1),"/o"))
      {
        unselected = 1;
        a++;
      }
      else if (line.getnumtokens() > 3)
        PRINTHELP();
      SCRIPT_MSG("Section: \"%s\"",line.gettoken_str(a));
      if (line.gettoken_str(a+1)[0]) SCRIPT_MSG(" ->(%s)",line.gettoken_str(a+1));
      SCRIPT_MSG("\n");
#ifndef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (!stricmp(line.gettoken_str(a),"uninstall"))
      {
        ERROR_MSG("Error: Uninstall section declared, no NSIS_CONFIG_UNINSTALL_SUPPORT\n");
        return PS_ERROR;
      }
#endif

      int ret;

      if (line.gettoken_str(a)[0]=='-')
      {
        if (!strnicmp(line.gettoken_str(a)+1,"un.",3))
          ret=add_section("un.",line.gettoken_str(a+1));
        else
          ret=add_section("",line.gettoken_str(a+1));
      }
      else ret=add_section(line.gettoken_str(a),line.gettoken_str(a+1));
      if (ret != PS_OK) return ret;
      
      if (unselected)
        build_cursection->flags &= ~SF_SELECTED;

      return PS_OK;
    }
    case TOK_SECTIONEND:
      SCRIPT_MSG("SectionEnd\n");
    return section_end();
    case TOK_SECTIONIN:
      {
        SCRIPT_MSG("SectionIn: ");
        int wt;
        for (wt = 1; wt < line.getnumtokens(); wt ++)
        {
          char *p=line.gettoken_str(wt);
          if (p[0]=='R' && p[1]=='O')
          {
            if (section_add_flags(SF_RO) != PS_OK) return PS_ERROR;
            SCRIPT_MSG("[RO] ");
          }
          else
          {
            int x=atoi(p)-1;
            if (x >= 0 && x < NSIS_MAX_INST_TYPES)
            {
              if (section_add_install_type(1<<x) != PS_OK) return PS_ERROR;
              SCRIPT_MSG("[%d] ",x);
            }
            else if (x < 0)
            {
              PRINTHELP()
            }
            else
            {
              ERROR_MSG("Error: SectionIn section %d out of range 1-%d\n",x+1,NSIS_MAX_INST_TYPES);
              return PS_ERROR;
            }
            p++;
          }
        }
        SCRIPT_MSG("\n");
      }
    return PS_OK;
    case TOK_SUBSECTIONEND:
    case TOK_SUBSECTION:
    {
      char buf[1024];
      int a=1,ex = 0;
      if (!strcmpi(line.gettoken_str(1),"/e"))
      {
        ex = 1;
        a++;
      }
      wsprintf(buf,"-%s",line.gettoken_str(a));
      if (which_token == TOK_SUBSECTION)
      {
        char *s = line.gettoken_str(a);
        if (!s[0] || (!strcmpi(s, "un.") && !s[3]))
          PRINTHELP();
      }

      SCRIPT_MSG("%s %s",line.gettoken_str(0),line.gettoken_str(a));
      if (line.gettoken_str(a+1)[0]) SCRIPT_MSG(" ->(%s)",line.gettoken_str(a+1));
      SCRIPT_MSG("\n");
      return add_section(buf,line.gettoken_str(a+1),ex);
    }
    case TOK_FUNCTION:
      if (!line.gettoken_str(1)[0]) PRINTHELP()
      if (line.gettoken_str(1)[0]==':' || line.gettoken_str(1)[0]=='/')
      {
        ERROR_MSG("Function: function name cannot begin with : or /.\n");
        PRINTHELP()
      }
      SCRIPT_MSG("Function: \"%s\"\n",line.gettoken_str(1));
#ifndef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (!strnicmp(line.gettoken_str(1),"un.",3))
      {
        ERROR_MSG("Error: Uninstall function declared, no NSIS_CONFIG_UNINSTALL_SUPPORT\n");
        return PS_ERROR;
      }
#endif
      return add_function(line.gettoken_str(1));
    case TOK_FUNCTIONEND:
      SCRIPT_MSG("FunctionEnd\n");
    return function_end();

    // flag setters
    ///////////////////////////////////////////////////////////////////////////////

    // BEGIN - Added by ramon 23 May 2003
    case TOK_ALLOWSKIPFILES:
      build_allowskipfiles=line.gettoken_enum(1,"off\0on\0");
      if (build_allowskipfiles==-1) PRINTHELP()
      SCRIPT_MSG("AllowSkipFiles: %s\n",line.gettoken_str(1));
    return PS_OK;
    // END - Added by ramon 23 May 2003
    case TOK_SETDATESAVE:
      build_datesave=line.gettoken_enum(1,"off\0on\0");
      if (build_datesave==-1) PRINTHELP()
      SCRIPT_MSG("SetDateSave: %s\n",line.gettoken_str(1));
    return PS_OK;
    case TOK_SETOVERWRITE:
    {
      int k=line.gettoken_enum(1,"on\0off\0try\0ifnewer\0ifdiff\0lastused\0");
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
      SCRIPT_MSG("overwrite = %d, last_overwrite = %d\n", build_overwrite, build_last_overwrite);
      SCRIPT_MSG("SetOverwrite: %s\n",line.gettoken_str(1));
    }
    return PS_OK;
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    case TOK_SETPLUGINUNLOAD:
      build_plugin_unload=line.gettoken_enum(1,"manual\0alwaysoff\0");
      if (build_plugin_unload==-1) PRINTHELP()
      SCRIPT_MSG("SetPluginUnload: %s\n",line.gettoken_str(1));
    return PS_OK;
#endif //NSIS_CONFIG_PLUGIN_SUPPORT
    case TOK_SETCOMPRESS:
      build_compress=line.gettoken_enum(1,"off\0auto\0force\0");
      if (build_compress==-1) PRINTHELP()
      if (build_compress==0 && build_compress_whole)
      {
        warning_fl("'SetCompress off' encountered, and in whole compression mode. Effectively ignored.");
      }
      SCRIPT_MSG("SetCompress: %s\n",line.gettoken_str(1));
    return PS_OK;
    case TOK_DBOPTIMIZE:
      build_optimize_datablock=line.gettoken_enum(1,"off\0on\0");
      if (build_optimize_datablock==-1) PRINTHELP()
      SCRIPT_MSG("SetDatablockOptimize: %s\n",line.gettoken_str(1));
    return PS_OK;
    case TOK_FILEBUFSIZE:
      build_filebuflen=line.gettoken_int(1);
      build_filebuflen<<=20;
      if (build_filebuflen<=0)
      {
        ERROR_MSG("Error: FileBufSize: invalid buffer size -- %d\n",build_filebuflen);
        return PS_ERROR;
      }
      SCRIPT_MSG("FileBufSize: %smb (%d bytes)\n",line.gettoken_str(1),build_filebuflen);
    return PS_OK;
#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    case TOK_SETCOMPRESSIONLEVEL:
    {
      if (compressor == &lzma_compressor)
        warning_fl("SetCompressionLevel: compressor is set to LZMA. Effectively ignored.");
      if (build_compressor_set && build_compress_whole)
        warning_fl("SetCompressionLevel: data already compressed in compress whole mode. Effectively ignored.");

      int s;
      build_compress_level=line.gettoken_int(1,&s);
      if (!s || build_compress_level < 0 || build_compress_level > 9) PRINTHELP();
      SCRIPT_MSG("SetCompressionLevel: %u\n", build_compress_level);
    }
    return PS_OK;
    case TOK_SETCOMPRESSORDICTSIZE:
    {
      if (compressor != &lzma_compressor)
        warning_fl("SetCompressorDictSize: compressor is not set to LZMA. Effectively ignored.");
      if (build_compressor_set && build_compress_whole)
        warning_fl("SetCompressorDictSize: data already compressed in compress whole mode. Effectively ignored.");

      int s;
      build_compress_dict_size=line.gettoken_int(1,&s);
      if (!s) PRINTHELP();
      SCRIPT_MSG("SetCompressorDictSize: %u mb\n", build_compress_dict_size);
      build_compress_dict_size <<= 20;
    }
    return PS_OK;
#else
    case TOK_SETCOMPRESSIONLEVEL:
    case TOK_SETCOMPRESSORDICTSIZE:
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_COMPRESSION_SUPPORT not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//NSIS_CONFIG_COMPRESSION_SUPPORT
    case TOK_ADDSIZE:
      {
        int s;
        int size_kb=line.gettoken_int(1,&s);
        if (!s) PRINTHELP()
        SCRIPT_MSG("AddSize: %d kb\n",size_kb);
        section_add_size_kb(size_kb);
      }
    return PS_OK;
    case TOK_SUBCAPTION:
      {
        int s;
        int w=line.gettoken_int(1,&s);
        if (!s || w < 0 || w > 4) PRINTHELP()
        SetInnerString(NLF_SUBCAPTION_LICENSE+w,line.gettoken_str(2));
        SCRIPT_MSG("SubCaption: page:%d, text=%s\n",w,line.gettoken_str(2));
      }
    return PS_OK;
    case TOK_FILEERRORTEXT:
#ifdef NSIS_SUPPORT_FILE
      {
        SetInnerString(NLF_FILE_ERROR,line.gettoken_str(1));
        SetInnerString(NLF_FILE_ERROR_NOIGNORE,line.gettoken_str(2));
        SCRIPT_MSG("FileErrorText: \"%s\" \"%s\"\n",line.gettoken_str(1),line.gettoken_str(2));
      }
    return PS_OK;
#else
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_FILE not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif
    case TOK_BRANDINGTEXT:
      {
        int a = 1;
        int trim = 0;
        while (line.gettoken_str(a)[0] == '/') {
          if (!strnicmp(line.gettoken_str(a),"/TRIM",5)) {
            if (!stricmp(line.gettoken_str(a)+5,"LEFT")) trim = 1;
            else if (!stricmp(line.gettoken_str(a)+5,"RIGHT")) trim = 2;
            else if (!stricmp(line.gettoken_str(a)+5,"CENTER")) trim = 3;
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

          BYTE* dlg = res_editor->GetResource(RT_DIALOG, MAKEINTRESOURCE(IDD_INST), NSIS_DEFAULT_LANG);
          CDialogTemplate td(dlg,uDefCodePage);
          free(dlg);

          if (trim) {
            char str[512];
            extern const char *NSIS_VERSION;
            if (line.getnumtokens()==a+1 && line.gettoken_str(a)[0])
              strcpy(str, line.gettoken_str(a));
            else
              wsprintf(str, "Nullsoft Install System %s", NSIS_VERSION);

            switch (trim) {
              case 1: td.LTrimToString(IDC_VERSTR, str, 4); break;
              case 2: td.RTrimToString(IDC_VERSTR, str, 4); break;
              case 3: td.CTrimToString(IDC_VERSTR, str, 4); break;
            }
          }

          DWORD dwSize;
          dlg = td.Save(dwSize);
          res_editor->UpdateResource(RT_DIALOG, MAKEINTRESOURCE(IDD_INST), NSIS_DEFAULT_LANG, dlg, dwSize);
          res_editor->FreeResource(dlg);
        }
        catch (exception& err) {
          ERROR_MSG("Error while triming branding text control: %s\n", err.what());
          return PS_ERROR;
        }
#else
        if (trim)
        {
          ERROR_MSG("Error: BrandingText /TRIM* is disabled for non Win32 platforms.\n");
          return PS_ERROR;
        }
#endif
        SCRIPT_MSG("BrandingText: \"%s\"\n",line.gettoken_str(a));
      }
    return PS_OK;
    case TOK_MISCBUTTONTEXT:
      {
        SetInnerString(NLF_BTN_BACK,line.gettoken_str(1));
        SetInnerString(NLF_BTN_NEXT,line.gettoken_str(2));
        SetInnerString(NLF_BTN_CANCEL,line.gettoken_str(3));
        SetInnerString(NLF_BTN_CLOSE,line.gettoken_str(4));
        SCRIPT_MSG("MiscButtonText: back=\"%s\" next=\"%s\" cancel=\"%s\" close=\"%s\"\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
      }
    return PS_OK;
    case TOK_SPACETEXTS:
      {
        if (!strcmpi(line.gettoken_str(1), "none")) {
          no_space_texts=true;
          SCRIPT_MSG("SpaceTexts: none\n");
        }
        else {
          no_space_texts=false;
          SetInnerString(NLF_SPACE_REQ,line.gettoken_str(1));
          SetInnerString(NLF_SPACE_AVAIL,line.gettoken_str(2));
          SCRIPT_MSG("SpaceTexts: required=\"%s\" available=\"%s\"\n",line.gettoken_str(1),line.gettoken_str(2));
        }
      }
    return PS_OK;
    case TOK_INSTBUTTONTEXT:
      {
        SetInnerString(NLF_BTN_INSTALL,line.gettoken_str(1));
        SCRIPT_MSG("InstallButtonText: \"%s\"\n",line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_DETAILSBUTTONTEXT:
      {
        if (!cur_page) {
          if (SetInnerString(NLF_BTN_DETAILS,line.gettoken_str(1)) == PS_WARNING)
            warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
        }
        else {
          if (cur_page_type != PAGE_INSTFILES) {
            ERROR_MSG("Error: DetailsButtonText can only be used inside PageEx instfiles.\n");
            return PS_ERROR;
          }
          cur_page->parms[1] = add_string(line.gettoken_str(1));
        }
        SCRIPT_MSG("DetailsButtonText: \"%s\"\n",line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_COMPLETEDTEXT:
      {
        if (!cur_page) {
          if (SetInnerString(NLF_COMPLETED,line.gettoken_str(1)) == PS_WARNING)
            warning_fl("%s: specified multiple times, wasting space",line.gettoken_str(0));
        }
        else {
          if (cur_page_type != PAGE_INSTFILES) {
            ERROR_MSG("Error: CompletedText can only be used inside PageEx instfiles.\n");
            return PS_ERROR;
          }
          cur_page->parms[2] = add_string(line.gettoken_str(1));
        }
        SCRIPT_MSG("CompletedText: \"%s\"\n",line.gettoken_str(1));
      }
    return PS_OK;
    case TOK_UNINSTBUTTONTEXT:
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      {
        SetInnerString(NLF_BTN_UNINSTALL,line.gettoken_str(1));
        SCRIPT_MSG("UninstButtonText: \"%s\"\n",line.gettoken_str(1));
      }
    return PS_OK;
#else
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_UNINSTALL_SUPPORT not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif

    // instructions
    ///////////////////////////////////////////////////////////////////////////////
    case TOK_NOP:
      SCRIPT_MSG("Nop\n");
      ent.which=EW_NOP;
    return add_entry(&ent);
    case TOK_GOTO:
      ent.which=EW_NOP;
      if (process_jump(line,1,&ent.offsets[0])) PRINTHELP()
      SCRIPT_MSG("Goto: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_SETSHELLVARCONTEXT:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(all_user_var);
      int k=line.gettoken_enum(1,"current\0all\0");
      if (k<0) PRINTHELP()
      ent.offsets[1]=add_intstring(k);
      SCRIPT_MSG("SetShellVarContext: %s\n",line.gettoken_str(1));
    }
    return add_entry(&ent);
    case TOK_RET:
      SCRIPT_MSG("Return\n");
      ent.which=EW_RET;
    return add_entry(&ent);
    case TOK_CALL:
      if (!line.gettoken_str(1)[0] || (line.gettoken_str(1)[0]==':' && !line.gettoken_str(1)[1] )) PRINTHELP()
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (uninstall_mode && strnicmp(line.gettoken_str(1),"un.",3) && (GetUserVarIndex(line,1) < 0))
      {
        ERROR_MSG("Call must be used with function names starting with \"un.\" in the uninstall section.\n");
        PRINTHELP()
      }
      if (!uninstall_mode && !strnicmp(line.gettoken_str(1),"un.",3))
      {
        ERROR_MSG("Call must not be used with functions starting with \"un.\" in the non-uninstall sections.\n");
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
          if (line.gettoken_str(1)[0] == ':')
          {
            ent.offsets[1]=1;
            ent.offsets[0]=ns_label.add(line.gettoken_str(1)+1,0);
          }
          else ent.offsets[0]=ns_func.add(line.gettoken_str(1),0);
        }
      }
      SCRIPT_MSG("Call \"%s\"\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_SETOUTPATH:
      {
        char *op=line.gettoken_str(1);
        if (!strcmp(op,"-"))
        {
          op="$INSTDIR";
        }
        SCRIPT_MSG("SetOutPath: \"%s\"\n",op);
        ent.which=EW_CREATEDIR;
        ent.offsets[0]=add_string(op);
        ent.offsets[1]=1;

        DefineInnerLangString(NLF_OUTPUT_DIR);
      }
    return add_entry(&ent);
    case TOK_CREATEDIR:
      {
        char out_path[1024];
        char *p=line.gettoken_str(1);
        if (*p == '-') out_path[0]=0;
        else
        {
          if (p[0] == '\\' && p[1] != '\\') p++;
          strncpy(out_path,p,1024-1);
          if (*CharPrev(out_path,out_path+strlen(out_path))=='\\')
            *CharPrev(out_path,out_path+strlen(out_path))=0; // remove trailing slash
        }
        if (!*out_path) PRINTHELP()
        SCRIPT_MSG("CreateDirectory: \"%s\"\n",out_path);
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
      SCRIPT_MSG("%s: \"%s\" (->%s)\n",ent.offsets[2]?"ExecWait":"Exec",line.gettoken_str(1),line.gettoken_str(2));

      DefineInnerLangString(NLF_EXEC);
    return add_entry(&ent);
#else//!NSIS_SUPPORT_EXECUTE
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_EXECUTE not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_EXECUTE
    case TOK_EXECSHELL: // this uses improvements of Andras Varga
#ifdef NSIS_SUPPORT_SHELLEXECUTE
      ent.which=EW_SHELLEXEC;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=add_string(line.gettoken_str(3));
      ent.offsets[3]=SW_SHOWNORMAL;
      if (line.getnumtokens() > 4)
      {
        int tab[4]={SW_SHOWNORMAL,SW_SHOWMAXIMIZED,SW_SHOWMINIMIZED,SW_HIDE};
        int a=line.gettoken_enum(4,"SW_SHOWNORMAL\0SW_SHOWMAXIMIZED\0SW_SHOWMINIMIZED\0SW_HIDE\0");
        if (a < 0) PRINTHELP()
        ent.offsets[3]=tab[a];
      }
      SCRIPT_MSG("ExecShell: %s: \"%s\" \"%s\" %s\n",line.gettoken_str(1),line.gettoken_str(2),
                                                 line.gettoken_str(3),line.gettoken_str(4));

      DefineInnerLangString(NLF_EXEC_SHELL);
    return add_entry(&ent);
#else//!NSIS_SUPPORT_SHELLEXECUTE
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_SHELLEXECUTE not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_SHELLEXECUTE
    case TOK_CALLINSTDLL:
    case TOK_REGDLL:
    case TOK_UNREGDLL:
#ifndef NSIS_SUPPORT_ACTIVEXREG
      ERROR_MSG("%s: support not compiled in (NSIS_SUPPORT_ACTIVEXREG)\n",line.gettoken_str(0));
      return PS_ERROR;
#else//NSIS_SUPPORT_ACTIVEXREG
      ent.which=EW_REGISTERDLL;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      if (which_token == TOK_UNREGDLL)
      {
        ent.offsets[1]=add_string("DllUnregisterServer");
        ent.offsets[2]=DefineInnerLangString(NLF_UNREGISTERING);
      }
      else if (which_token == TOK_CALLINSTDLL)
      {
        int a = 2;
        if (!stricmp(line.gettoken_str(a), "/NOUNLOAD")) {
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
        if (!ent.offsets[1]) ent.offsets[1]=add_string("DllRegisterServer");
        ent.offsets[2]=DefineInnerLangString(NLF_REGISTERING);
      }

      SCRIPT_MSG("%s: \"%s\" %s\n",line.gettoken_str(0),line.gettoken_str(1), line.gettoken_str(ent.offsets[3]?3:2));

      DefineInnerLangString(NLF_SYMBOL_NOT_FOUND);
      DefineInnerLangString(NLF_COULD_NOT_LOAD);
      DefineInnerLangString(NLF_NO_OLE);
      DefineInnerLangString(NLF_ERR_REG_DLL);
    return add_entry(&ent);
#endif//NSIS_SUPPORT_ACTIVEXREG
    case TOK_RENAME:
#ifdef NSIS_SUPPORT_RENAME
      {
        int a=1;
        ent.which=EW_RENAME;
        if (!stricmp(line.gettoken_str(1),"/REBOOTOK"))
        {
          ent.offsets[2]=1;
          a++;
#ifndef NSIS_SUPPORT_MOVEONREBOOT
          ERROR_MSG("Error: /REBOOTOK specified, NSIS_SUPPORT_MOVEONREBOOT not defined\n");
          PRINTHELP()
#endif
        }
        else if (line.gettoken_str(1)[0]=='/')
        {
          a=line.getnumtokens(); // cause usage to go here:
        }
        if (line.getnumtokens()!=a+2) PRINTHELP()
        ent.offsets[0]=add_string(line.gettoken_str(a));
        ent.offsets[1]=add_string(line.gettoken_str(a+1));
        SCRIPT_MSG("Rename: %s%s->%s\n",ent.offsets[2]?"/REBOOTOK ":"",line.gettoken_str(a),line.gettoken_str(a+1));

        DefineInnerLangString(NLF_RENAME);
#ifdef NSIS_SUPPORT_MOVEONREBOOT
        DefineInnerLangString(NLF_RENAME_ON_REBOOT);
#endif
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_RENAME
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_RENAME not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_RENAME
    case TOK_MESSAGEBOX:
#ifdef NSIS_SUPPORT_MESSAGEBOX
      {
        #define MBD(x) {x,#x},
        struct
        {
          int id;
          char *str;
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
          MBD(MB_TOPMOST)
          MBD(MB_SETFOREGROUND)
          MBD(MB_RIGHT)
          MBD(MB_DEFBUTTON1)
          MBD(MB_DEFBUTTON2)
          MBD(MB_DEFBUTTON3)
          MBD(MB_DEFBUTTON4)
        };
        #undef MBD
        int r=0;
        int x;
        char *p=line.gettoken_str(1);

        while (*p)
        {
          char *np=p;
          while (*np && *np != '|') np++;
          if (*np) *np++=0;
          for (x = 0 ; (unsigned) x < sizeof(list) / sizeof(list[0]) && strcmpi(list[x].str, p); x++);
          if ((unsigned) x < sizeof(list) / sizeof(list[0]))
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
        const char *retstr="0\0IDABORT\0IDCANCEL\0IDIGNORE\0IDNO\0IDOK\0IDRETRY\0IDYES\0";
        int a=3;
        if (line.getnumtokens() > 3)
        {
          if (!strcmpi(line.gettoken_str(3),"/SD"))
          {
            int k=line.gettoken_enum(4,retstr);
            if (k <= 0) PRINTHELP();
            ent.offsets[0]|=rettab[k]<<20;
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
        SCRIPT_MSG("MessageBox: %d: \"%s\"",r,line.gettoken_str(2));
        if (line.getnumtokens()>a+1) SCRIPT_MSG(" (on %s goto %s)",line.gettoken_str(a),line.gettoken_str(a+1));
        SCRIPT_MSG("\n");
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_MESSAGEBOX
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_MESSAGEBOX not defined.\n",  line.gettoken_str(0));
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
          ERROR_MSG("CreateShortCut: cannot interpret icon index\n");
          PRINTHELP()
        }
        ent.offsets[4]=0;
      }
      if (line.getnumtokens() > 6 && *line.gettoken_str(6))
      {
        int tab[3]={SW_SHOWNORMAL,SW_SHOWMAXIMIZED,SW_SHOWMINNOACTIVE/*SW_SHOWMINIMIZED doesn't work*/};
        int a=line.gettoken_enum(6,"SW_SHOWNORMAL\0SW_SHOWMAXIMIZED\0SW_SHOWMINIMIZED\0");
        if (a < 0)
        {
          ERROR_MSG("CreateShortCut: unknown show mode \"%s\"\n",line.gettoken_str(6));
          PRINTHELP()
        }
        ent.offsets[4]|=tab[a]<<8;
      }
      if (line.getnumtokens() > 7)
      {
        char *s=(line.gettoken_str(7));

        char b[255];
        for (unsigned int spos=0; (spos <= strlen(s)) && (spos <= 255); spos++)
          b[spos]=toupper(*(s+spos));
        strcpy(s,b);

        if (*s)
        {
          int c=0;
          if (strstr(s,"ALT|")) ent.offsets[4]|=HOTKEYF_ALT << 24;
          if (strstr(s,"CONTROL|")) ent.offsets[4]|=HOTKEYF_CONTROL << 24;
          if (strstr(s,"EXT|")) ent.offsets[4]|=HOTKEYF_EXT << 24;
          if (strstr(s,"SHIFT|")) ent.offsets[4]|=HOTKEYF_SHIFT << 24;
          while (strstr(s,"|"))
          {
            s=strstr(s,"|")+1;
          }
          if ((s[0] == 'F') && (s[1] >= '1' && s[1] <= '9'))
          {
            c=VK_F1-1+atoi(s+1);
            if (atoi(s+1) < 1 || atoi(s+1) > 24)
            {
              warning_fl("CreateShortCut: F-key \"%s\" out of range",s);
            }
          }
          else if (((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= '0' && s[0] <= '9')) && !s[1])
            c=s[0];
          else
          {
            c=s[0];
            warning_fl("CreateShortCut: unrecognized hotkey \"%s\"",s);
          }
          ent.offsets[4] |= (c) << 16;
        }
      }
      SCRIPT_MSG("CreateShortCut: \"%s\"->\"%s\" %s icon:%s,%d, showmode=0x%X, hotkey=0x%X, comment=%s\n",
        line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),
        line.gettoken_str(4),ent.offsets[4]&0xff,(ent.offsets[4]>>8)&0xff,ent.offsets[4]>>16,line.gettoken_str(8));

      DefineInnerLangString(NLF_CREATE_SHORTCUT);
      DefineInnerLangString(NLF_ERR_CREATING_SHORTCUT);
    return add_entry(&ent);
#else//!NSIS_SUPPORT_CREATESHORTCUT
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_CREATESHORTCUT not defined.\n",  line.gettoken_str(0));
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
      SCRIPT_MSG("FindWindow: output=%s, class=\"%s\", text=\"%s\" hwndparent=\"%s\" hwndafter=\"%s\"\n",
        line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4),line.gettoken_str(5));
    return add_entry(&ent);
    case TOK_SENDMESSAGE:
      ent.which=EW_SENDMESSAGE;

      if (line.gettoken_str(1)[0] == '/' || line.gettoken_str(2)[0] == '/' ||
          line.gettoken_str(3)[0] == '/' || line.gettoken_str(4)[0] == '/')
      {
        PRINTHELP()
      }

      SCRIPT_MSG("SendMessage:");
      {
        int a=5;
        ent.offsets[0]=GetUserVarIndex(line, 5);
        if (ent.offsets[0]>=0)
        {
          SCRIPT_MSG("(->%s)",line.gettoken_str(5));
          a++;
        }

        if (!strncmp(line.gettoken_str(a),"/TIMEOUT=",9))
        {
          ent.offsets[5]|=atoi(line.gettoken_str(a)+9)<<2;
          SCRIPT_MSG(" (timeout=%d)",ent.offsets[5]>>2);
          a++;
        }

        if (line.getnumtokens()>a)
        {
          PRINTHELP()
        }
      }

      if (!strncmp(line.gettoken_str(3),"STR:",4))
      {
        ent.offsets[5]|=1;
        ent.offsets[3]=add_string(line.gettoken_str(3)+4);
      }
      else ent.offsets[3]=add_string(line.gettoken_str(3));
      if (!strncmp(line.gettoken_str(4),"STR:",4))
      {
        ent.offsets[5]|=2;
        ent.offsets[4]=add_string(line.gettoken_str(4)+4);
      }
      else ent.offsets[4]=add_string(line.gettoken_str(4));

      ent.offsets[1]=add_string(line.gettoken_str(1));
      ent.offsets[2]=add_string(line.gettoken_str(2));

      SCRIPT_MSG("(%s,%s,%s,%s)\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
    return add_entry(&ent);
    case TOK_ISWINDOW:
      ent.which=EW_ISWINDOW;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      if (process_jump(line,2,&ent.offsets[1])||
          process_jump(line,3,&ent.offsets[2])) PRINTHELP()
      SCRIPT_MSG("IsWindow(%s): %s:%s\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
    case TOK_GETDLGITEM:
      ent.which=EW_GETDLGITEM;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0]<0) PRINTHELP();
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=add_string(line.gettoken_str(3));
      SCRIPT_MSG("GetDlgItem: output=%s dialog=%s item=%s\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
    case TOK_SETCTLCOLORS:
    {
      ctlcolors c={0, };

      ent.which=EW_SETCTLCOLORS;
      ent.offsets[0]=add_string(line.gettoken_str(1));

      int a = 2;

      if (!strcmpi(line.gettoken_str(2),"/BRANDING"))
        a++;
      
      {
        char *p;

        if (a == 2 && line.getnumtokens() == 5) {
          ERROR_MSG("Error: SetCtlColors expected 3 parameters, got 4\n");
          return PS_ERROR;
        }

        if (!strcmpi(line.gettoken_str(a+1),"transparent")) {
          c.flags|=CC_BKB;
          c.lbStyle=BS_NULL;
          c.bkmode=TRANSPARENT;
        }
        else {
          p=line.gettoken_str(a+1);
          if (*p) {
            int v=strtoul(p,&p,16);
            c.bkc=((v&0xff)<<16)|(v&0xff00)|((v&0xff0000)>>16);
            c.flags|=CC_BK|CC_BKB;
          }

          c.lbStyle=BS_SOLID;
          c.bkmode=OPAQUE;
        }

        p=line.gettoken_str(a);
        if (*p) {
          int v=strtoul(p,&p,16);
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

      SCRIPT_MSG("SetCtlColors: hwnd=%s %stext=%s background=%s\n",line.gettoken_str(1),a==2?"":"/BRANDING ",line.gettoken_str(a),line.gettoken_str(a+1));
    }
    return add_entry(&ent);
    case TOK_CREATEFONT:
      ent.which=EW_CREATEFONT;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=add_string(line.gettoken_str(2));
      SCRIPT_MSG("CreateFont: output=%s \"%s\"",line.gettoken_str(1),line.gettoken_str(2));
      {
        int height=0;
        int weight=0;
        int flags=0;
        for (int i = 3; i < line.getnumtokens(); i++) {
          char *tok=line.gettoken_str(i);
          if (tok[0]=='/') {
            if (!strcmpi(tok,"/ITALIC")) {
              SCRIPT_MSG(" /ITALIC");
              flags|=1;
            }
            else if (!strcmpi(tok,"/UNDERLINE")) {
              SCRIPT_MSG(" /UNDERLINE");
              flags|=2;
            }
            else if (!strcmpi(tok,"/STRIKE")) {
              SCRIPT_MSG(" /STRIKE");
              flags|=4;
            }
            else {
              SCRIPT_MSG("\n");
              PRINTHELP();
            }
          }
          else {
            if (!height) {
              SCRIPT_MSG(" height=%s",tok);
              height=add_string(tok);
            }
            else if (!weight) {
              SCRIPT_MSG(" weight=%s",tok);
              weight=add_string(tok);
            }
            else {
              SCRIPT_MSG("\n");
              PRINTHELP();
            }
          }
        }
        ent.offsets[2]=height;
        ent.offsets[3]=weight;
        ent.offsets[4]=flags;
      }
      SCRIPT_MSG("\n");
    return add_entry(&ent);
    case TOK_ENABLEWINDOW:
      ent.which=EW_SHOWWINDOW;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[3]=1;
      SCRIPT_MSG("EnableWindow: handle=%s enable=%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SHOWWINDOW:
      ent.which=EW_SHOWWINDOW;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      SCRIPT_MSG("ShowWindow: handle=%s show state=%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_HIDEWINDOW:
      ent.which=EW_SHOWWINDOW;
      ent.offsets[0]=add_string("$HWNDPARENT");
      ent.offsets[1]=add_string("0"/*SW_HIDE*/);
      ent.offsets[2]=1;
      SCRIPT_MSG("HideWindow\n");
    return add_entry(&ent);
    case TOK_BRINGTOFRONT:
    {
      int ret;
      ent.which=EW_SHOWWINDOW;
      ent.offsets[0]=add_string("$HWNDPARENT");
      ent.offsets[1]=add_string("5"/*SW_SHOW*/);
      ret = add_entry(&ent);
      if (ret != PS_OK) return ret;
      ent.which=EW_BRINGTOFRONT;
      ent.offsets[0]=0;
      ent.offsets[1]=0;
      SCRIPT_MSG("BringToFront\n");
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
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_ENHANCEDUI_SUPPORT not defined.\n",  line.gettoken_str(0));
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
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_HWNDS not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_HWNDS
    case TOK_DELETE:
#ifdef NSIS_SUPPORT_DELETE
      {
        int a=1;
        ent.which=EW_DELETEFILE;
        if (!stricmp(line.gettoken_str(a),"/REBOOTOK"))
        {
          a++;
          ent.offsets[1]=DEL_REBOOT;
#ifndef NSIS_SUPPORT_MOVEONREBOOT
          ERROR_MSG("Error: /REBOOTOK specified, NSIS_SUPPORT_MOVEONREBOOT not defined\n");
          PRINTHELP()
#endif
        }
        else if (line.gettoken_str(1)[0]=='/')
        {
          a=line.getnumtokens();
        }
        if (line.getnumtokens() != a+1) PRINTHELP()
        ent.offsets[0]=add_string(line.gettoken_str(a));
        SCRIPT_MSG("Delete: %s\"%s\"\n",ent.offsets[1]?"/REBOOTOK ":"",line.gettoken_str(a));

        DefineInnerLangString(NLF_DEL_FILE);
#ifdef NSIS_SUPPORT_MOVEONREBOOT
        DefineInnerLangString(NLF_DEL_ON_REBOOT);
#endif
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_DELETE
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_DELETE not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_DELETE
    case TOK_RMDIR:
#ifdef NSIS_SUPPORT_RMDIR
      {
        int a=1;
        ent.which=EW_RMDIR;
        ent.offsets[1]=DEL_DIR;
        while (line.gettoken_str(a)[0]=='/')
        {
          if (!stricmp(line.gettoken_str(a),"/r"))
          {
            if (a == 3) PRINTHELP();
            a++;
            ent.offsets[1]|=DEL_RECURSE;
          }
          else if (!stricmp(line.gettoken_str(a),"/REBOOTOK"))
          {
            if (a == 3) PRINTHELP();
            a++;
            ent.offsets[1]|=DEL_REBOOT;
          }          
          else PRINTHELP();
        }
        if (a < line.getnumtokens() - 1) PRINTHELP();
        ent.offsets[0]=add_string(line.gettoken_str(a));
        SCRIPT_MSG("RMDir: ");
        if (a>1)
          SCRIPT_MSG("%s ",line.gettoken_str(1));
        if (a>2)
          SCRIPT_MSG("%s ",line.gettoken_str(2));
        SCRIPT_MSG("\"%s\"\n",line.gettoken_str(a));

        DefineInnerLangString(NLF_REMOVE_DIR);
        DefineInnerLangString(NLF_DEL_FILE);
#ifdef NSIS_SUPPORT_MOVEONREBOOT
        DefineInnerLangString(NLF_DEL_ON_REBOOT);
#endif
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_RMDIR
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_RMDIR not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_RMDIR
    case TOK_RESERVEFILE:
    case TOK_FILE:
#ifdef NSIS_SUPPORT_FILE
      {
        int a=1,attrib=0,rec=0,fatal=1;
        if (!stricmp(line.gettoken_str(a),"/nonfatal")) {
          fatal=0;
          a++;
        }
        if (which_token == TOK_FILE && !stricmp(line.gettoken_str(a),"/a"))
        {
          attrib=1;
          a++;
        }
        if (!stricmp(line.gettoken_str(a),"/r"))
        {
          rec=1;
          a++;
        }
        else if (which_token == TOK_FILE && !strnicmp(line.gettoken_str(a),"/oname=",7))
        {
          char *on=line.gettoken_str(a)+7;
          a++;
          if (!*on||line.getnumtokens()!=a+1||strstr(on,"*") || strstr(on,"?")) PRINTHELP()

          int tf=0;
#ifdef _WIN32
          int v=do_add_file(line.gettoken_str(a), attrib, 0, linecnt,&tf,on);
#else
          char *fn = my_convert(line.gettoken_str(a));
          int v=do_add_file(fn, attrib, 0, linecnt,&tf,on);
          my_convert_free(fn);
#endif
          if (v != PS_OK) return v;
          if (tf > 1) PRINTHELP()
          if (!tf)
          {
            if (fatal)
            {
              ERROR_MSG("%sFile: \"%s\" -> no files found.\n",(which_token == TOK_FILE)?"":"Reserve",line.gettoken_str(a));
              PRINTHELP()
            }
            else
            {
              warning_fl("%sFile: \"%s\" -> no files found",(which_token == TOK_FILE)?"":"Reserve",line.gettoken_str(a));
            }
          }

          return PS_OK;
        }
#ifdef _WIN32
        else if (line.gettoken_str(a)[0] == '/') PRINTHELP()
#endif
        if (line.getnumtokens()<a+1) PRINTHELP()
        while (a < line.getnumtokens())
        {
#ifdef _WIN32
          if (line.gettoken_str(a)[0]=='/') PRINTHELP()
#endif
          char buf[32];
          char *t=line.gettoken_str(a++);
          if (t[0] && CharNext(t)[0] == ':' && CharNext(t)[1] == '\\' && !CharNext(t)[2])
          {
            strcpy(buf,"X:\\*.*");
            buf[0]=t[0];
            t=buf;
          }
          int tf=0;
#ifdef _WIN32
          int v=do_add_file(t, attrib, rec, linecnt,&tf,NULL,which_token == TOK_FILE);
#else
          char *fn = my_convert(t);
          int v=do_add_file(fn, attrib, rec, linecnt,&tf,NULL,which_token == TOK_FILE);
          my_convert_free(fn);
#endif
          if (v != PS_OK) return v;
          if (!tf)
          {
            if (fatal)
            {
              ERROR_MSG("%sFile: \"%s\" -> no files found.\n",(which_token == TOK_FILE)?"":"Reserve",t);
              PRINTHELP();
            }
            else
            {
              warning_fl("%sFile: \"%s\" -> no files found.",(which_token == TOK_FILE)?"":"Reserve",t);
            }
          }
        }
      }
    return PS_OK;
#else//!NSIS_SUPPORT_FILE
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_FILE not defined.\n",  line.gettoken_str(0));
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
          if (!stricmp(line.gettoken_str(a),"/SILENT"))
          {
            a++;
            ent.offsets[2]&=~FOF_SIMPLEPROGRESS;
            ent.offsets[2]|=FOF_SILENT;
          }
          else if (!stricmp(line.gettoken_str(a),"/FILESONLY"))
          {
            a++;
            ent.offsets[2]|=FOF_FILESONLY;
          }
          else if (line.gettoken_str(a)[0]=='/') PRINTHELP()
          else break;
        }
        if (line.getnumtokens() < a+2) PRINTHELP()
        ent.offsets[0]=add_string(line.gettoken_str(a));
        ent.offsets[1]=add_string(line.gettoken_str(a+1));
        int s;
        int size_kb=line.gettoken_int(a+2,&s);
        if (!s && line.gettoken_str(a+2)[0]) PRINTHELP()
        section_add_size_kb(size_kb);
        SCRIPT_MSG("CopyFiles: %s\"%s\" -> \"%s\", size=%iKB\n",ent.offsets[2]&FOF_SILENT?"(silent) ":"", line.gettoken_str(a),line.gettoken_str(a+1),size_kb);

        DefineInnerLangString(NLF_COPY_FAILED);
        DefineInnerLangString(NLF_COPY_TO);
      }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_COPYFILES
    case TOK_COPYFILES:
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_COPYFILES not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_COPYFILES

    case TOK_SETFILEATTRIBUTES:
      {
        #define MBD(x) {x,#x},
        struct
        {
          int id;
          char *str;
        } list[]=
        {
          MBD(FILE_ATTRIBUTE_NORMAL)
          MBD(FILE_ATTRIBUTE_ARCHIVE)
          MBD(FILE_ATTRIBUTE_HIDDEN)
          MBD(FILE_ATTRIBUTE_OFFLINE)
          MBD(FILE_ATTRIBUTE_READONLY)
          MBD(FILE_ATTRIBUTE_SYSTEM)
          MBD(FILE_ATTRIBUTE_TEMPORARY)
          {FILE_ATTRIBUTE_NORMAL,"NORMAL"},
          {FILE_ATTRIBUTE_ARCHIVE,"ARCHIVE"},
          {FILE_ATTRIBUTE_HIDDEN,"HIDDEN"},
          {FILE_ATTRIBUTE_OFFLINE,"OFFLINE"},
          {FILE_ATTRIBUTE_READONLY,"READONLY"},
          {FILE_ATTRIBUTE_SYSTEM,"SYSTEM"},
          {FILE_ATTRIBUTE_TEMPORARY,"TEMPORARY"},
          {FILE_ATTRIBUTE_NORMAL,"0"},
        };
        #undef MBD
        int r=0;
        int x;
        char *p=line.gettoken_str(2);

        while (*p)
        {
          char *np=p;
          while (*np && *np != '|') np++;
          if (*np) *np++=0;
          for (x = 0 ; (unsigned) x < sizeof(list)/sizeof(list[0]) && stricmp(list[x].str,p); x ++);

          if ((unsigned) x < sizeof(list)/sizeof(list[0]))
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
        SCRIPT_MSG("Sleep: %s ms\n",line.gettoken_str(1));
      }
    return add_entry(&ent);
    case TOK_IFFILEEXISTS:
      ent.which=EW_IFFILEEXISTS;
      ent.offsets[0] = add_string(line.gettoken_str(1));
      if (process_jump(line,2,&ent.offsets[1]) ||
          process_jump(line,3,&ent.offsets[2])) PRINTHELP()
      SCRIPT_MSG("IfFileExists: \"%s\" ? %s : %s\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
    case TOK_QUIT:
      ent.which=EW_QUIT;
      SCRIPT_MSG("Quit\n");
    return add_entry(&ent);
    case TOK_ABORT:
      ent.which=EW_ABORT;
      ent.offsets[0] = add_string(line.gettoken_str(1));
      SCRIPT_MSG("Abort: \"%s\"\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_SETDETAILSVIEW:
      {
        int v=line.gettoken_enum(1,"hide\0show\0");
        ent.which=EW_CHDETAILSVIEW;
        if (v < 0) PRINTHELP()
        ent.offsets[0] = v?SW_SHOWNA:SW_HIDE;
        ent.offsets[1] = v?SW_HIDE:SW_SHOWNA;
        SCRIPT_MSG("SetDetailsView: %s\n",line.gettoken_str(1));
      }
    return add_entry(&ent);
    case TOK_SETDETAILSPRINT:
      ent.which=EW_UPDATETEXT;
      ent.offsets[0] = 0;
      ent.offsets[1] = line.gettoken_enum(1,"lastused\0listonly\0textonly\0both\0none\0");
      if (ent.offsets[1] < 0) PRINTHELP();
      switch (ent.offsets[1]) {
        case 0:
          ent.offsets[1]=8;
        break;
        case 1:
        case 2:
        case 3:
          ent.offsets[1]<<=1;
        break;
        case 4:
          ent.offsets[1]=16;
        break;
      }
      SCRIPT_MSG("SetDetailsPrint: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_SETAUTOCLOSE:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(autoclose);
      int k=line.gettoken_enum(1,"false\0true\0");
      if (k < 0) PRINTHELP()
      ent.offsets[1]=add_intstring(k);
      SCRIPT_MSG("SetAutoClose: %s\n",line.gettoken_str(1));
    }
    return add_entry(&ent);
    case TOK_IFERRORS:
      ent.which=EW_IFFLAG;
      if (process_jump(line,1,&ent.offsets[0]) ||
          process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=FLAG_OFFSET(exec_error);
      ent.offsets[3]=0;//new value mask - clean error
      SCRIPT_MSG("IfErrors ?%s:%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_IFABORT:
      ent.which=EW_IFFLAG;
      if (process_jump(line,1,&ent.offsets[0]) ||
          process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=FLAG_OFFSET(abort);
      ent.offsets[3]=~0;//new value mask - keep flag
      SCRIPT_MSG("IfAbort ?%s:%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_CLEARERRORS:
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(exec_error);
      ent.offsets[1]=add_intstring(0);
      SCRIPT_MSG("ClearErrors\n");
    return add_entry(&ent);
    case TOK_SETERRORS:
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(exec_error);
      ent.offsets[1]=add_intstring(1);
      SCRIPT_MSG("SetErrors\n");
    return add_entry(&ent);
    case TOK_SETERRORLEVEL:
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(errlvl);
      ent.offsets[1]=add_string(line.gettoken_str(1));
      SCRIPT_MSG("SetErrorLevel: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_GETERRORLEVEL:
      ent.which=EW_GETFLAG;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=FLAG_OFFSET(errlvl);
      if (line.gettoken_str(1)[0] && ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG("GetErrorLevel: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
#ifdef NSIS_SUPPORT_STROPTS
    case TOK_STRLEN:
      ent.which=EW_STRLEN;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=add_string(line.gettoken_str(2));
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG("StrLen %s \"%s\"\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_STRCPY:
      ent.which=EW_ASSIGNVAR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=add_string(line.gettoken_str(3));
      ent.offsets[3]=add_string(line.gettoken_str(4));

      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG("StrCpy %s \"%s\" (%s) (%s)\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
    return add_entry(&ent);
    case TOK_GETFUNCTIONADDR:
      ent.which=EW_GETFUNCTIONADDR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=ns_func.add(line.gettoken_str(2),0);
      ent.offsets[2]=0;
      ent.offsets[3]=0;
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG("GetFunctionAddress: %s %s",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_GETLABELADDR:
      ent.which=EW_GETLABELADDR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0 || process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=0;
      ent.offsets[3]=0;
      SCRIPT_MSG("GetLabelAddress: %s %s",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_GETCURRENTADDR:
      ent.which=EW_ASSIGNVAR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      {
        char buf[32];
        wsprintf(buf,"%d",1+(cur_header->blocks[NB_ENTRIES].num));
        ent.offsets[1]=add_string(buf);
      }
      if (ent.offsets[0] < 0) PRINTHELP()
      ent.offsets[2]=0;
      ent.offsets[3]=0;
      SCRIPT_MSG("GetCurrentAddress: %s",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_STRCMP:
      ent.which=EW_STRCMP;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      if (process_jump(line,3,&ent.offsets[2]) ||
          process_jump(line,4,&ent.offsets[3])) PRINTHELP()
      SCRIPT_MSG("StrCmp \"%s\" \"%s\" equal=%s, nonequal=%s\n",line.gettoken_str(1),line.gettoken_str(2), line.gettoken_str(3),line.gettoken_str(4));
    return add_entry(&ent);
    case TOK_GETDLLVERSIONLOCAL:
      {
        char buf[128];
        DWORD low=0, high=0;
        int flag=0;
#ifdef _WIN32
        DWORD s,d;
        int alloced=0;
        char *path=line.gettoken_str(1);
        if (!((*path == '\\' && path[1] == '\\') || (*path && path[1] == ':'))) {
          size_t pathlen=strlen(path)+GetCurrentDirectory(0, buf)+2;
          char *nrpath=(char *)malloc(pathlen);
          alloced=1;
          GetCurrentDirectory(pathlen, nrpath);
          if (path[0] != '\\')
            strcat(nrpath,"\\");
          else if (nrpath[1] == ':') {
            nrpath[2]=0;
          }
          else {
            char *p=nrpath+2;
            while (*p!='\\') p++;
            *p=0;
          }
          strcat(nrpath,path);
          FILE *f=FOPEN(nrpath, "r");
          if (f) {
            path=nrpath;
            fclose(f);
          }
          else {
            free(nrpath);
            alloced=0;
          }
        }
        s=GetFileVersionInfoSize(path,&d);
        if (s)
        {
          void *buf;
          buf=(void *)GlobalAlloc(GPTR,s);
          if (buf)
          {
            UINT uLen;
            VS_FIXEDFILEINFO *pvsf;
            if (GetFileVersionInfo(path,0,s,buf) && VerQueryValue(buf,"\\",(void**)&pvsf,&uLen))
            {
              low=pvsf->dwFileVersionLS;
              high=pvsf->dwFileVersionMS;
              flag=1;
            }
            GlobalFree(buf);
          }
        }
        if (alloced) free(path);
#else
        FILE *fdll = FOPEN(line.gettoken_str(1), "rb");
        if (!fdll) {
          ERROR_MSG("Error: Can't open \"%s\"!\n", line.gettoken_str(1));
          return PS_ERROR;
        }
        MANAGE_WITH(fdll, fclose);

        fseek(fdll, 0, SEEK_END);
        unsigned int len = ftell(fdll);
        fseek(fdll, 0, SEEK_SET);
        LPBYTE dll = (LPBYTE) malloc(len);
        if (!dll) {
          ERROR_MSG("Internal compiler error #12345: malloc(%d) failed\n", dll);
          extern void quit(); quit();
        }
        MANAGE_WITH(dll, free);
        if (fread(dll, 1, len, fdll) != len) {
          ERROR_MSG("Error: Can't read \"%s\"!\n", line.gettoken_str(1));
          return PS_ERROR;
        }

        try
        {
          CResourceEditor *dllre = new CResourceEditor(dll, len);
          LPBYTE ver = dllre->GetResource(VS_FILE_INFO, MAKEINTRESOURCE(VS_VERSION_INFO), 0);
          int versize = dllre->GetResourceSize(VS_FILE_INFO, MAKEINTRESOURCE(VS_VERSION_INFO), 0);

          if (ver)
          {
            if ((size_t) versize > sizeof(WORD) * 3)
            {
              // get VS_FIXEDFILEINFO from VS_VERSIONINFO
              WCHAR *szKey = (WCHAR *)(ver + sizeof(WORD) * 3);
              int len = WCStrLen(szKey) * sizeof(WCHAR) + sizeof(WORD) * 3;
              len = (len + 3) & ~3; // align on DWORD boundry
              VS_FIXEDFILEINFO *verinfo = (VS_FIXEDFILEINFO *)(ver + len);
              if (versize > len && verinfo->dwSignature == VS_FFI_SIGNATURE)
              {
                low = verinfo->dwFileVersionLS;
                high = verinfo->dwFileVersionMS;
                flag = 1;
              }
            }
            dllre->FreeResource(ver);
          }

          delete dllre;
        }
        catch (exception& err) {
          ERROR_MSG(
            "GetDLLVersionLocal: error reading version info from \"%s\": %s\n",
            line.gettoken_str(1),
            err.what()
          );
          return PS_ERROR;
        }
#endif
        if (!flag)
        {
          ERROR_MSG("GetDLLVersionLocal: error reading version info from \"%s\"\n",line.gettoken_str(1));
          return PS_ERROR;
        }
        ent.which=EW_ASSIGNVAR;
        ent.offsets[0]=GetUserVarIndex(line, 2);
        wsprintf(buf,"%u",high);
        ent.offsets[1]=add_string(buf);
        ent.offsets[2]=0;
        ent.offsets[3]=0;
        if (ent.offsets[0]<0) PRINTHELP()
        add_entry(&ent);

        ent.offsets[0]=GetUserVarIndex(line, 3);
        wsprintf(buf,"%u",low);
        ent.offsets[1]=add_string(buf);
        ent.offsets[2]=0;
        ent.offsets[3]=0;
        if (ent.offsets[0]<0) PRINTHELP()
        SCRIPT_MSG("GetDLLVersionLocal: %s (%u,%u)->(%s,%s)\n",
          line.gettoken_str(1),high,low,line.gettoken_str(2),line.gettoken_str(3));
      }
    return add_entry(&ent);
    case TOK_GETFILETIMELOCAL:
      {
        char buf[129];
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
          ERROR_MSG("GetFileTimeLocal: error reading date from \"%s\"\n",line.gettoken_str(1));
          return PS_ERROR;
        }
#else
        struct stat st;
        if (!stat(line.gettoken_str(1), &st))
        {
          union
          {
            struct
            {
              long l;
              long h;
            } words;
            long long ll;
          };
          ll = (st.st_mtime * 10000000LL) + 116444736000000000LL;
          high = words.h;
          low = words.l;
        }
        else
        {
          ERROR_MSG("GetFileTimeLocal: error reading date from \"%s\"\n",line.gettoken_str(1));
          return PS_ERROR;
        }
#endif

        ent.which=EW_ASSIGNVAR;
        ent.offsets[0]=GetUserVarIndex(line, 2);
        wsprintf(buf,"%u",high);
        ent.offsets[1]=add_string(buf);
        ent.offsets[2]=0;
        ent.offsets[3]=0;
        if (ent.offsets[0]<0) PRINTHELP()
        add_entry(&ent);

        ent.offsets[0]=GetUserVarIndex(line, 3);
        wsprintf(buf,"%u",low);
        ent.offsets[1]=add_string(buf);
        ent.offsets[2]=0;
        ent.offsets[3]=0;
        if (ent.offsets[0]<0) PRINTHELP()
        SCRIPT_MSG("GetFileTimeLocal: %s (%u,%u)->(%s,%s)\n",
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
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_STROPTS not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_STROPTS
#ifdef NSIS_SUPPORT_INIFILES
    case TOK_DELETEINISEC:
    case TOK_DELETEINISTR:
      {
        char *vname="";
        char *space="";
        ent.which=EW_WRITEINI;
        ent.offsets[0]=add_string(line.gettoken_str(2)); // section name
        if (line.getnumtokens() > 3)
        {
          vname=line.gettoken_str(3);
          ent.offsets[1]=add_string(vname); // value name
          space=" ";
        }
        else ent.offsets[1]=0;
        ent.offsets[2]=0;
        ent.offsets[3]=add_string(line.gettoken_str(1));
        SCRIPT_MSG("DeleteINI%s: [%s] %s%sin %s\n",*vname?"Str":"Sec",
          line.gettoken_str(2),vname,space,line.gettoken_str(1));
      }
    return add_entry(&ent);
    case TOK_FLUSHINI:
      ent.which=EW_WRITEINI;
      ent.offsets[3]=add_string(line.gettoken_str(1));
      SCRIPT_MSG("FlushINI: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_WRITEINISTR:
      ent.which=EW_WRITEINI;
      ent.offsets[0]=add_string(line.gettoken_str(2));
      ent.offsets[1]=add_string(line.gettoken_str(3));
      ent.offsets[2]=add_string(line.gettoken_str(4));
      ent.offsets[3]=add_string(line.gettoken_str(1));
      ent.offsets[4]=1; // write
      SCRIPT_MSG("WriteINIStr: [%s] %s=%s in %s\n",
        line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_READINISTR:
      ent.which=EW_READINISTR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0) PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(3));
      ent.offsets[2]=add_string(line.gettoken_str(4));
      ent.offsets[3]=add_string(line.gettoken_str(2));
      SCRIPT_MSG("ReadINIStr %s [%s]:%s from %s\n",line.gettoken_str(1),line.gettoken_str(3),line.gettoken_str(4),line.gettoken_str(2));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_INIFILES
    case TOK_DELETEINISEC:
    case TOK_DELETEINISTR:
    case TOK_FLUSHINI:
    case TOK_WRITEINISTR:
    case TOK_READINISTR:
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_INIFILES not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_INIFILES
    case TOK_DETAILPRINT:
      ent.which=EW_UPDATETEXT;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=0;
      SCRIPT_MSG("DetailPrint: \"%s\"\n",line.gettoken_str(1));
    return add_entry(&ent);
#ifdef NSIS_SUPPORT_FNUTIL
    case TOK_GETTEMPFILENAME:
      ent.which=EW_GETTEMPFILENAME;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (line.getnumtokens() == 3)
        ent.offsets[1]=add_string(line.gettoken_str(2));
      else
        ent.offsets[1]=add_string("$TEMP");
      if (ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG("GetTempFileName -> %s\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_GETFULLPATHNAME:
      {
        int a=0;
        ent.which=EW_GETFULLPATHNAME;
        if (line.getnumtokens()==4 && !stricmp(line.gettoken_str(1),"/SHORT")) a++;
        else if (line.getnumtokens()==4 || *line.gettoken_str(1)=='/') PRINTHELP()
        ent.offsets[0]=add_string(line.gettoken_str(2+a));
        ent.offsets[1]=GetUserVarIndex(line, 1+a);
        ent.offsets[2]=!a;
        if (ent.offsets[0]<0) PRINTHELP()
        SCRIPT_MSG("GetFullPathName: %s->%s (%d)\n",
          line.gettoken_str(2+a),line.gettoken_str(1+a),a?"sfn":"lfn");
      }
    return add_entry(&ent);
    case TOK_SEARCHPATH:
      ent.which=EW_SEARCHPATH;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0) PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(2));
      SCRIPT_MSG("SearchPath %s %s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
#else
    case TOK_SEARCHPATH:
    case TOK_GETTEMPFILENAME:
    case TOK_GETFULLPATHNAME:
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_FNUTIL not defined.\n",  line.gettoken_str(0));
      return PS_ERROR;
#endif
    case TOK_GETDLLVERSION:
#ifdef NSIS_SUPPORT_GETDLLVERSION
      ent.which=EW_GETDLLVERSION;
      ent.offsets[0]=GetUserVarIndex(line, 2);
      ent.offsets[1]=GetUserVarIndex(line, 3);
      ent.offsets[2]=add_string(line.gettoken_str(1));
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG("GetDLLVersion: %s->%s,%s\n",
        line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_GETDLLVERSION
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_GETDLLVERSION not defined.\n",  line.gettoken_str(0));
      return PS_ERROR;
#endif//!NSIS_SUPPORT_GETDLLVERSION
    case TOK_GETFILETIME:
#ifdef NSIS_SUPPORT_GETFILETIME
      ent.which=EW_GETFILETIME;
      ent.offsets[0]=GetUserVarIndex(line, 2);
      ent.offsets[1]=GetUserVarIndex(line, 3);
      ent.offsets[2]=add_string(line.gettoken_str(1));
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG("GetFileTime: %s->%s,%s\n",
        line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_GETFILETIME
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_GETFILETIME not defined.\n",  line.gettoken_str(0));
      return PS_ERROR;
#endif//!NSIS_SUPPORT_GETFILETIME
#ifdef NSIS_SUPPORT_INTOPTS
    case TOK_INTOP:
      ent.which=EW_INTOP;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[3]=line.gettoken_enum(3,"+\0-\0*\0/\0|\0&\0^\0!\0||\0&&\0%\0<<\0>>\0~\0");
      if (ent.offsets[0] < 0 || ent.offsets[3] < 0 ||
        ((ent.offsets[3] == 7 || ent.offsets[3] == 13) && line.getnumtokens() > 4))
        PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(2));
      if (ent.offsets[3] != 7 && ent.offsets[3] != 13) ent.offsets[2]=add_string(line.gettoken_str(4));
      if (ent.offsets[3] == 13) {
        ent.offsets[3]=6;
        ent.offsets[2]=add_string("0xFFFFFFFF");
      }
      SCRIPT_MSG("IntOp: %s=%s%s%s\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
    return add_entry(&ent);
    case TOK_INTFMT:
      ent.which=EW_INTFMT;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0]<0) PRINTHELP()
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=add_string(line.gettoken_str(3));
      SCRIPT_MSG("IntFmt: %s->%s (fmt:%s)\n",line.gettoken_str(3),line.gettoken_str(1),line.gettoken_str(2));
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
      SCRIPT_MSG("%s %s:%s equal=%s, < %s, > %s\n",line.gettoken_str(0),
        line.gettoken_str(1),line.gettoken_str(2), line.gettoken_str(3),line.gettoken_str(4),line.gettoken_str(5));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_INTOPTS
    case TOK_INTOP:
    case TOK_INTCMP:
    case TOK_INTFMT:
    case TOK_INTCMPU:
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_INTOPTS not defined.\n",  line.gettoken_str(0));
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
        ent.offsets[1]=(int)rootkey_tab[k];
        ent.offsets[2]=add_string(line.gettoken_str(3));
        ent.offsets[3]=add_string(line.gettoken_str(4));
        if (which_token == TOK_READREGDWORD) ent.offsets[4]=1;
        else ent.offsets[4]=0;
        if (line.gettoken_str(3)[0] == '\\')
          warning_fl("%s: registry path name begins with \'\\\', may cause problems",line.gettoken_str(0));

        SCRIPT_MSG("%s %s %s\\%s\\%s\n",line.gettoken_str(0),
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
          char *s=line.gettoken_str(a);
          if (s[0] == '/')
          {
            if (stricmp(s,"/ifempty")) PRINTHELP()
            a++;
            ent.offsets[4]=3;
          }
          if (line.gettoken_str(a+2)[0]) PRINTHELP()
        }
        int k=line.gettoken_enum(a,rootkeys[0]);
        if (k == -1) k=line.gettoken_enum(a,rootkeys[1]);
        if (k == -1) PRINTHELP()
        ent.which=EW_DELREG;
        ent.offsets[1]=(int)rootkey_tab[k];
        ent.offsets[2]=add_string(line.gettoken_str(a+1));
        ent.offsets[3]=(which_token==TOK_DELETEREGKEY)?0:add_string(line.gettoken_str(a+2));
        if (line.gettoken_str(a+1)[0] == '\\')
          warning_fl("%s: registry path name begins with \'\\\', may cause problems",line.gettoken_str(0));
        if (which_token==TOK_DELETEREGKEY)
          SCRIPT_MSG("DeleteRegKey: %s\\%s\n",line.gettoken_str(a),line.gettoken_str(a+1));
        else
          SCRIPT_MSG("DeleteRegValue: %s\\%s\\%s\n",line.gettoken_str(a),line.gettoken_str(a+1),line.gettoken_str(a+2));
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
        ent.offsets[0]=(int)rootkey_tab[k];
        ent.offsets[1]=add_string(line.gettoken_str(2));
        if (line.gettoken_str(2)[0] == '\\')
          warning_fl("%s: registry path name begins with \'\\\', may cause problems",line.gettoken_str(0));
        ent.offsets[2]=add_string(line.gettoken_str(3));
        if (which_token == TOK_WRITEREGSTR || which_token == TOK_WRITEREGEXPANDSTR)
        {
          SCRIPT_MSG("%s: %s\\%s\\%s=%s\n",
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
          char data[3*NSIS_MAX_STRLEN];
          char *p=line.gettoken_str(4);
          int data_len=0;
          while (*p)
          {
            int c;
            int a,b;
            a=*p;
            if (a >= '0' && a <= '9') a-='0';
            else if (a >= 'a' && a <= 'f') a-='a'-10;
            else if (a >= 'A' && a <= 'F') a-='A'-10;
            else break;
            b=*++p;
            if (b >= '0' && b <= '9') b-='0';
            else if (b >= 'a' && b <= 'f') b-='a'-10;
            else if (b >= 'A' && b <= 'F') b-='A'-10;
            else break;
            p++;
            c=(a<<4)|b;
            if (data_len >= 3*NSIS_MAX_STRLEN)
            {
              ERROR_MSG("WriteRegBin: %d bytes of data exceeded\n",3*NSIS_MAX_STRLEN);
              return PS_ERROR;
            }
            data[data_len++]=c;
          }
          if (*p) PRINTHELP()
          SCRIPT_MSG("WriteRegBin: %s\\%s\\%s=%s\n",
            line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(4));
          ent.offsets[3]=add_db_data(data,data_len);
          if (ent.offsets[3] < 0) return PS_ERROR;
          ent.offsets[4]=ent.offsets[5]=REG_BINARY;
        }
        if (which_token == TOK_WRITEREGDWORD)
        {
          ent.offsets[3]=add_string(line.gettoken_str(4));
          ent.offsets[4]=ent.offsets[5]=REG_DWORD;

          SCRIPT_MSG("WriteRegDWORD: %s\\%s\\%s=%s\n",
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
        ent.offsets[1]=(int)rootkey_tab[k];
        ent.offsets[2]=add_string(line.gettoken_str(3));
        ent.offsets[3]=add_string(line.gettoken_str(4));
        ent.offsets[4]=which_token == TOK_ENUMREGKEY;
        if (line.gettoken_str(3)[0] == '\\') warning_fl("%s: registry path name begins with \'\\\', may cause problems",line.gettoken_str(0));
        SCRIPT_MSG("%s %s %s\\%s\\%s\n",which_token == TOK_ENUMREGKEY ? "EnumRegKey" : "EnumRegValue",
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
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_REGISTRYFUNCTIONS not defined.\n",  line.gettoken_str(0));
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
          SCRIPT_MSG("Exch(%s,0)\n",line.gettoken_str(1));
          ent.offsets[0]=add_string(line.gettoken_str(1));
          ent.offsets[1]=0;
          ent.offsets[2]=0;
          add_entry(&ent);
        }
        else SCRIPT_MSG("Exch(st(%d),0)\n",swapitem);

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
      SCRIPT_MSG("Push: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_POP:
      ent.which=EW_PUSHPOP;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=1;
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG("Pop: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_STACK
    case TOK_POP:
    case TOK_PUSH:
    case TOK_EXCH:
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_STACK not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_STACK
#ifdef NSIS_SUPPORT_ENVIRONMENT
    case TOK_READENVSTR:
      ent.which=EW_READENVSTR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      {
        ent.offsets[1]=add_string(line.gettoken_str(2));
        if (ent.offsets[0] < 0 || strlen(line.gettoken_str(2))<1) PRINTHELP()
      }
      ent.offsets[2]=1;
      SCRIPT_MSG("ReadEnvStr: %s->%s\n",line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_EXPANDENVSTRS:
      ent.which=EW_READENVSTR;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=0;
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG("ExpandEnvStrings: %s->%s\n",line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_ENVIRONMENT
    case TOK_EXPANDENVSTRS:
    case TOK_READENVSTR:
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_ENVIRONMENT not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_ENVIRONMENT
#ifdef NSIS_SUPPORT_FINDFIRST
    case TOK_FINDFIRST:
      ent.which=EW_FINDFIRST;
      ent.offsets[0]=GetUserVarIndex(line, 2); // out
      ent.offsets[1]=GetUserVarIndex(line, 1); // handleout
      ent.offsets[2]=add_string(line.gettoken_str(3)); // filespec
      if (ent.offsets[0] < 0 || ent.offsets[1] < 0) PRINTHELP()
      SCRIPT_MSG("FindFirst: spec=\"%s\" handle=%s output=%s\n",line.gettoken_str(3),line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_FINDNEXT:
      ent.which=EW_FINDNEXT;
      ent.offsets[0]=GetUserVarIndex(line, 2);
      ent.offsets[1]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0 || ent.offsets[1] < 0) PRINTHELP()
      SCRIPT_MSG("FindNext: handle=%s output=%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_FINDCLOSE:
      ent.which=EW_FINDCLOSE;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG("FindClose: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
#else//!NSIS_SUPPORT_FINDFIRST
    case TOK_FINDCLOSE:
    case TOK_FINDNEXT:
    case TOK_FINDFIRST:
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_FINDFIRST not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;

#endif//!NSIS_SUPPORT_FINDFIRST


#ifdef NSIS_SUPPORT_FILEFUNCTIONS
    case TOK_FILEOPEN:
      {
        ent.which=EW_FOPEN;
        ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
        ent.offsets[3]=add_string(line.gettoken_str(2));
        ent.offsets[1]=0; //openmode
        if (!stricmp(line.gettoken_str(3),"r"))
        {
          ent.offsets[1]=GENERIC_READ;
          ent.offsets[2]=OPEN_EXISTING;
        }
        else if (!stricmp(line.gettoken_str(3),"w"))
        {
          ent.offsets[1]=GENERIC_WRITE;
          ent.offsets[2]=CREATE_ALWAYS;
        }
        else if (!stricmp(line.gettoken_str(3),"a"))
        {
          ent.offsets[1]=GENERIC_WRITE|GENERIC_READ;
          ent.offsets[2]=OPEN_ALWAYS;
        }

        if (ent.offsets[3] < 0 || !ent.offsets[1]) PRINTHELP()
      }
      SCRIPT_MSG("FileOpen: %s as %s -> %s\n",line.gettoken_str(2),line.gettoken_str(3),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_FILECLOSE:
      ent.which=EW_FCLOSE;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      if (ent.offsets[0] < 0) PRINTHELP()
      SCRIPT_MSG("FileClose: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_FILEREAD:
      ent.which=EW_FGETS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=GetUserVarIndex(line, 2); // output string
      ent.offsets[2]=add_string(line.gettoken_str(3)[0]?line.gettoken_str(3):"1023");
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG("FileRead: %s->%s (max:%s)\n",line.gettoken_str(1),line.gettoken_str(2),line.gettoken_str(3));
    return add_entry(&ent);
    case TOK_FILEWRITE:
      ent.which=EW_FPUTS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=add_string(line.gettoken_str(2));
      if (ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG("FileWrite: %s->%s\n",line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_FILEREADBYTE:
      ent.which=EW_FGETS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=GetUserVarIndex(line, 2); // output string
      ent.offsets[2]=add_string("1");
      ent.offsets[3]=1;
      if (ent.offsets[0]<0 || ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG("FileReadByte: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_FILEWRITEBYTE:
      ent.which=EW_FPUTS;
      ent.offsets[0]=GetUserVarIndex(line, 1); // file handle
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=1;
      if (ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG("FileWriteByte: %s->%s\n",line.gettoken_str(2),line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_FILESEEK:
      {
        char *modestr;
        int tab[3]={FILE_BEGIN,FILE_CURRENT,FILE_END};
        int mode=line.gettoken_enum(3,"SET\0CUR\0END\0");
        ent.which=EW_FSEEK;
        ent.offsets[0]=GetUserVarIndex(line, 1);
        ent.offsets[1]=GetUserVarIndex(line, 4);
        ent.offsets[2]=add_string(line.gettoken_str(2));

        if (mode<0 && !line.gettoken_str(3)[0])
        {
          mode=0;
          modestr="SET";
        }
        else modestr=line.gettoken_str(3);

        if (mode<0 || ent.offsets[0] < 0 || (ent.offsets[1]<0 && line.gettoken_str(4)[0])) PRINTHELP()
        ent.offsets[3]=tab[mode];
        SCRIPT_MSG("FileSeek: fp=%s, ofs=%s, mode=%s, output=%s\n",
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
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_FILEFUNCTIONS not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;

#endif//!NSIS_SUPPORT_FILEFUNCTIONS
#ifdef NSIS_SUPPORT_REBOOT
    case TOK_REBOOT:
    {
      int ret = add_entry_direct(EW_REBOOT, 0xbadf00d);
      if (ret != PS_OK) return ret;

      ret = add_entry_direct(EW_QUIT);
      if (ret != PS_OK) return ret;

      SCRIPT_MSG("Reboot! (WOW)\n");

      DefineInnerLangString(NLF_INST_CORRUPTED);
    }
    return PS_OK;
    case TOK_IFREBOOTFLAG:
      ent.which=EW_IFFLAG;
      if (process_jump(line,1,&ent.offsets[0]) ||
          process_jump(line,2,&ent.offsets[1])) PRINTHELP()
      ent.offsets[2]=FLAG_OFFSET(exec_reboot);
      ent.offsets[3]=~0;//new value mask - keep flag
      SCRIPT_MSG("IfRebootFlag ?%s:%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SETREBOOTFLAG:
    {
      ent.which=EW_SETFLAG;
      ent.offsets[0]=FLAG_OFFSET(exec_reboot);
      int k=line.gettoken_enum(1,"false\0true\0");
      if (k < 0) PRINTHELP()
      ent.offsets[1]=add_intstring(k);
    }
    return add_entry(&ent);
#else//!NSIS_SUPPORT_REBOOT
    case TOK_REBOOT:
    case TOK_IFREBOOTFLAG:
    case TOK_SETREBOOTFLAG:
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_REBOOT not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_SUPPORT_REBOOT
#ifdef NSIS_CONFIG_LOG
    case TOK_LOGSET:
      ent.which=EW_LOG;
      ent.offsets[0]=1;
      ent.offsets[1]=line.gettoken_enum(1,"off\0on\0");
      if (ent.offsets[1]<0) PRINTHELP()

      SCRIPT_MSG("LogSet: %s\n",line.gettoken_str(1));
    return add_entry(&ent);
    case TOK_LOGTEXT:
      ent.which=EW_LOG;
      ent.offsets[0]=0;
      ent.offsets[1]=add_string(line.gettoken_str(1));
      SCRIPT_MSG("LogText \"%s\"\n",line.gettoken_str(1));
    return add_entry(&ent);
#else//!NSIS_CONFIG_LOG

    case TOK_LOGSET:
    case TOK_LOGTEXT:
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_LOG not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_LOG
#ifdef NSIS_CONFIG_COMPONENTPAGE
    case TOK_SECTIONSETTEXT:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=SECTION_FIELD_SET(name_ptr);
      SCRIPT_MSG("SectionSetText: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONGETTEXT:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=SECTION_FIELD_GET(name_ptr);
      if (line.gettoken_str(2)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG("SectionGetText: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONSETFLAGS:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=SECTION_FIELD_SET(flags);
      SCRIPT_MSG("SectionSetFlags: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONGETFLAGS:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=SECTION_FIELD_GET(flags);
      if (line.gettoken_str(2)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG("SectionGetFlags: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_INSTTYPESETTEXT:
      ent.which=EW_INSTTYPESET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=1;
      SCRIPT_MSG("InstTypeSetText: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_INSTTYPEGETTEXT:
      ent.which=EW_INSTTYPESET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=0;
      if (line.gettoken_str(1)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG("InstTypeGetText: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONSETINSTTYPES:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=SECTION_FIELD_SET(install_types);
      SCRIPT_MSG("SectionSetInstTypes: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONGETINSTTYPES:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=SECTION_FIELD_GET(install_types);
      if (line.gettoken_str(2)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG("SectionGetInstTypes: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONSETSIZE:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=add_string(line.gettoken_str(2));
      ent.offsets[2]=SECTION_FIELD_SET(size_kb);
      SCRIPT_MSG("SectionSetSize: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SECTIONGETSIZE:
      ent.which=EW_SECTIONSET;
      ent.offsets[0]=add_string(line.gettoken_str(1));
      ent.offsets[1]=GetUserVarIndex(line, 2);
      ent.offsets[2]=SECTION_FIELD_GET(size_kb);
      if (line.gettoken_str(2)[0] && ent.offsets[1]<0) PRINTHELP()
      SCRIPT_MSG("SectionGetSize: %s->%s\n",line.gettoken_str(1),line.gettoken_str(2));
    return add_entry(&ent);
    case TOK_SETCURINSTTYPE:
    {
      int ret;
      SCRIPT_MSG("SetCurInstType: %s\n",line.gettoken_str(1));
      ret = add_entry_direct(EW_SETFLAG, FLAG_OFFSET(cur_insttype), add_string(line.gettoken_str(1)));
      if (ret != PS_OK) return ret;
      ret = add_entry_direct(EW_INSTTYPESET, 0, 0, 0, 1);
      if (ret != PS_OK) return ret;
    }
    return PS_OK;
    case TOK_GETCURINSTTYPE:
      ent.which=EW_GETFLAG;
      ent.offsets[0]=GetUserVarIndex(line, 1);
      ent.offsets[1]=FLAG_OFFSET(cur_insttype);
      if (line.gettoken_str(1)[0] && ent.offsets[0]<0) PRINTHELP()
      SCRIPT_MSG("GetCurInstType: %s\n",line.gettoken_str(1));
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
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_COMPONENTPAGE not defined.\n",  line.gettoken_str(0));
    return PS_ERROR;
#endif//!NSIS_CONFIG_COMPONENTPAGE
    // Added by Amir Szekely 29th July 2002
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
    case TOK_SETBRANDINGIMAGE:
    {
      SCRIPT_MSG("SetBrandingImage: ");
      if (!branding_image_found) {
        ERROR_MSG("\nError: no branding image found in chosen UI!\n");
        return PS_ERROR;
      }
      ent.which=EW_SETBRANDINGIMAGE;
      for (int i = 1; i < line.getnumtokens(); i++)
        if (!strnicmp(line.gettoken_str(i),"/IMGID=",7)) {
          ent.offsets[1]=atoi(line.gettoken_str(i)+7);
          SCRIPT_MSG("/IMGID=%d ",ent.offsets[1]);
        }
        else if (!stricmp(line.gettoken_str(i),"/RESIZETOFIT")) {
          ent.offsets[2]=1; // must be 1 or 0
          SCRIPT_MSG("/RESIZETOFIT ");
        }
        else if (!ent.offsets[0]) {
          ent.offsets[0]=add_string(line.gettoken_str(i));
          SCRIPT_MSG("\"%s\" ", line.gettoken_str(i));
        }
        else {
          SCRIPT_MSG("\n");
          PRINTHELP();
        }

      if (!ent.offsets[1])
        ent.offsets[1]=branding_image_id;
      SCRIPT_MSG("\n");
    }
    return add_entry(&ent);
#else//NSIS_CONFIG_ENHANCEDUI_SUPPORT
    case TOK_SETBRANDINGIMAGE:
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_ENHANCEDUI_SUPPORT not defined.\n",line.gettoken_str(0));
      return PS_ERROR;
#endif//!NSIS_SUPPORT_CREATEFONT

    // Added by ramon 3 jun 2003
    case TOK_DEFVAR:
    {
        SCRIPT_MSG("VAR \"%s\"\n",line.gettoken_str(1));
        int res = DeclaredUserVar(line.gettoken_str(1));
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
        if (!strnicmp(line.gettoken_str(a),"/LANG=",6))
          LangID=atoi(line.gettoken_str(a++)+6);
        if (line.getnumtokens()!=a+2) PRINTHELP();
        char *pKey = line.gettoken_str(a);
        char *pValue = line.gettoken_str(a+1);
        if ( !(*pKey) )
        {
           ERROR_MSG("Error: empty name for version info key!\n");
           return PS_ERROR;
        }
        else
        {
           SCRIPT_MSG("%s: \"%s\" \"%s\"\n", line.gettoken_str(0), line.gettoken_str(a), line.gettoken_str(a+1));
           LANGID lReaded = LangID;
           LanguageTable *table = GetLangTable(LangID);
           if ( a > 1 && lReaded == 0 )
             warning_fl("%s: %s language not loaded, using default \"1033-English\"", line.gettoken_str(0), line.gettoken_str(1));
           if ( rVersionInfo.SetKeyValue(LangID, table->nlf.m_bLoaded ? table->nlf.m_uCodePage : 1252 /*English US*/, pKey, pValue) )
           {
             ERROR_MSG("%s: \"%s\" \"%04d-%s\" already defined!\n",line.gettoken_str(0), line.gettoken_str(2), LangID, table->nlf.m_bLoaded ? table->nlf.m_szName : LangID == 1033 ? "English" : "???");
             return PS_ERROR;
           }

           return PS_OK;
        }
    }
    case TOK_VI_SETPRODUCTVERSION:
      if ( version_product_v[0] )
      {
           ERROR_MSG("Error: %s already defined!\n", line.gettoken_str(0));
           return PS_ERROR;
      }
      strcpy(version_product_v, line.gettoken_str(1));
      return PS_OK;

#else
    case TOK_VI_ADDKEY:
    case TOK_VI_SETPRODUCTVERSION:
      ERROR_MSG("Error: %s specified, NSIS_SUPPORT_VERSION_INFO not defined.\n",line.gettoken_str(0));
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
        SCRIPT_MSG("PluginDir: \"%s\"\n",line.gettoken_str(1));
#ifdef _WIN32
        m_plugins.FindCommands(line.gettoken_str(1),display_info?true:false);
#else
        char *converted_path = my_convert(line.gettoken_str(1));
        m_plugins.FindCommands(converted_path,display_info?true:false);
        my_convert_free(converted_path);
#endif
        return PS_OK;
      }
    }
    return PS_ERROR;
    case TOK__PLUGINCOMMAND:
    {
      int ret, data_handle;
      char* command = strdup(line.gettoken_str(0));
      assert(command != 0);
      MANAGE_WITH(command, free);

      char* dllPath = m_plugins.GetPluginDll(uninstall_mode, &command, &data_handle);
      if (dllPath)
      {
        if (uninstall_mode) uninst_plugin_used = true;
        else plugin_used = true;

        // Initialize $PLUGINSDIR
        ent.which=EW_CALL;
        ent.offsets[0]=ns_func.add(uninstall_mode?"un.Initialize_____Plugins":"Initialize_____Plugins",0);
        ret=add_entry(&ent);
        if (ret != PS_OK) {
          return ret;
        }

        // DLL name on the user machine
        char tempDLL[NSIS_MAX_STRLEN];
        char *dllName = strrchr(dllPath,PLATFORM_PATH_SEPARATOR_C);
        if (dllName && *dllName == PLATFORM_PATH_SEPARATOR_C)
          dllName++;
        wsprintf(tempDLL, "$PLUGINSDIR%c%s", PATH_SEPARATOR_C, dllName);

        // Add the DLL to the installer
        if (data_handle == -1)
        {
          int files_added;
          // BEGIN - Added by ramon 23 May 2003
          int old_build_allowskipfiles=build_allowskipfiles;
          build_allowskipfiles=1; // on
          // END - Added by ramon 23 May 2003
          int old_build_overwrite=build_overwrite;
          build_overwrite=1; // off
          int old_build_datesave=build_datesave;
          build_datesave=0; // off
          ret=do_add_file(dllPath,0,0,linecnt,&files_added,tempDLL,2,&data_handle); // 2 means no size add
          if (ret != PS_OK) {
            return ret;
          }
          m_plugins.SetDllDataHandle(uninstall_mode, line.gettoken_str(0),data_handle);
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
          ent.offsets[0]|=(MB_RETRYCANCEL|MB_ICONSTOP|(IDCANCEL<<20))<<3;
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
        ent.which=EW_UPDATETEXT;
        ent.offsets[0]=0;
        ent.offsets[1]=8; // lastused
        ent.offsets[2]=0;
        ret=add_entry(&ent);
        if (ret != PS_OK) {
          return ret;
        }

        // Call the DLL
        char* funcname = strstr(command,"::");
        if (funcname) funcname += 2;
        else          funcname  = command;
        SCRIPT_MSG("Plugin Command: %s",funcname);

        int i = 1;
        int nounload = 0;
        if (!strcmpi(line.gettoken_str(i), "/NOUNLOAD")) {
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
          if (!strcmpi(line.gettoken_str(w), "/NOUNLOAD")) nounloadmisused=1;
          ent.offsets[1]=0;
          ret=add_entry(&ent);
          if (ret != PS_OK) {
            return ret;
          }
          SCRIPT_MSG(" %s",line.gettoken_str(i));
        }
        SCRIPT_MSG("\n");
        if (nounloadmisused)
          warning_fl("/NOUNLOAD must come first before any plugin parameter. Unless the plugin you are trying to use has a parameter /NOUNLOAD, you are doing something wrong");

        // next, call it
        ent.which=EW_REGISTERDLL;
        ent.offsets[0]=add_string(tempDLL);;
        ent.offsets[1]=add_string(funcname);
        ent.offsets[2]=0;
        ent.offsets[3]=nounload|build_plugin_unload;
        ret=add_entry(&ent);
        if (ret != PS_OK) {
          return ret;
        }

        return PS_OK;
      }
      else
        ERROR_MSG("Error: Plugin dll for command \"%s\" not found.\n",line.gettoken_str(0));
    }
    return PS_ERROR;
    case TOK_INITPLUGINSDIR:
    {
      int ret;
      SCRIPT_MSG("%s\n",line.gettoken_str(0));
      if (uninstall_mode) uninst_plugin_used = true;
      else plugin_used = true;
      // Call [un.]Initialize_____Plugins
      ent.which=EW_CALL;
      ent.offsets[0]=ns_func.add(uninstall_mode?"un.Initialize_____Plugins":"Initialize_____Plugins",0);
      ret=add_entry(&ent);
      if (ret != PS_OK) return ret;
      // SetDetailsPrint lastused
      ent.which=EW_UPDATETEXT;
      ent.offsets[0]=0;
      ent.offsets[1]=8; // lastused
      ret=add_entry(&ent);
      if (ret != PS_OK) return ret;
    }
    return PS_OK;
#else
    case TOK_PLUGINDIR:
    case TOK__PLUGINCOMMAND:
    case TOK_INITPLUGINSDIR:
    {
      ERROR_MSG("Error: %s specified, NSIS_CONFIG_PLUGIN_SUPPORT not defined.\n",line.gettoken_str(0));
    }
    return PS_ERROR;
#endif// NSIS_CONFIG_PLUGIN_SUPPORT

#ifdef NSIS_LOCKWINDOW_SUPPORT
    case TOK_LOCKWINDOW:
      SCRIPT_MSG("LockWindow: lock state=%d\n",line.gettoken_str(1));
      ent.which=EW_LOCKWINDOW;
      ent.offsets[0]=line.gettoken_enum(1,"on\0off\0");
      if (ent.offsets[0] == -1)
        PRINTHELP();
    return add_entry(&ent);
#else
    case TOK_LOCKWINDOW:
    {
      ERROR_MSG("Error: %s specified, NSIS_LOCKWINDOW_SUPPORT not defined.\n",line.gettoken_str(0));
    }
    return PS_ERROR;
#endif // NSIS_LOCKWINDOW_SUPPORT

    default: break;

  }
  ERROR_MSG("Error: doCommand: Invalid token \"%s\".\n",line.gettoken_str(0));
  return PS_ERROR;
}

#ifdef NSIS_SUPPORT_FILE
int CEXEBuild::do_add_file(const char *lgss, int attrib, int recurse, int linecnt, int *total_files, const char *name_override, int generatecode, int *data_handle, int rec_depth)
{
  char dir[1024];
  char newfn[1024];
#ifdef _WIN32
  HANDLE h;
  WIN32_FIND_DATA d;
#else
  glob_t globbuf;
#endif
  strcpy(dir,lgss);
  {
    char *s=dir+strlen(dir);
    while (s > dir && *s != PLATFORM_PATH_SEPARATOR_C) s=CharPrev(dir,s);
    if (s == dir)
    {
      if (*s == PLATFORM_PATH_SEPARATOR_C)
        sprintf(dir,"%c.",PLATFORM_PATH_SEPARATOR_C);
      else
        strcpy(dir,".");
    }
    else
      s[0]=0;
  }

#ifdef _WIN32
  h = FindFirstFile(lgss,&d);
  if (h != INVALID_HANDLE_VALUE)
  {
    do
    {
      if ((d.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
      {
#else
  GLOB(lgss, GLOB_NOSORT, NULL, &globbuf);
  {
    for (unsigned int i = 0; i < globbuf.gl_pathc; i++)
    {
      struct stat s;
      if (!stat(globbuf.gl_pathv[i], &s) && S_ISREG(s.st_mode))
      {
        char *filename = strrchr(globbuf.gl_pathv[i], PLATFORM_PATH_SEPARATOR_C);
        if (filename)
          filename++;
        else
          filename = globbuf.gl_pathv[i];
#endif
        MMapFile mmap;
        DWORD len;
        (*total_files)++;

#ifdef _WIN32
        sprintf(newfn,"%s%s%s",dir,dir[0]?PLATFORM_PATH_SEPARATOR_STR:"",d.cFileName);
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
          FindClose(h);
          ERROR_MSG("%sFile: failed opening file \"%s\"\n",generatecode?"":"Reserve",newfn);
          return PS_ERROR;
        }

        // Will auto-CloseHandle hFile
        MANAGE_WITH(hFile, CloseHandle);

        len = GetFileSize(hFile, NULL);
        if (len && !mmap.setfile(hFile, len))
        {
          FindClose(h);
          ERROR_MSG("%sFile: failed creating mmap of \"%s\"\n",generatecode?"":"Reserve",newfn);
          return PS_ERROR;
        }
#else
        sprintf(newfn,"%s%s%s",dir,dir[0]?PLATFORM_PATH_SEPARATOR_STR:"",filename);
        len = (DWORD) s.st_size;

        int fd = OPEN(newfn, O_RDONLY);
        if (fd == -1)
        {
          globfree(&globbuf);
          ERROR_MSG("%sFile: failed opening file \"%s\"\n",generatecode?"":"Reserve",newfn);
          return PS_ERROR;
        }

        // Will auto-close(2) fd
        MANAGE_WITH(fd, close);

        if (len && !mmap.setfile(fd, len))
        {
          globfree(&globbuf);
          ERROR_MSG("%sFile: failed creating mmap of \"%s\"\n",generatecode?"":"Reserve",newfn);
          return PS_ERROR;
        }
#endif

        if (generatecode&1)
          section_add_size_kb((len+1023)/1024);
#ifdef _WIN32
        if (name_override) SCRIPT_MSG("%sFile: \"%s\"->\"%s\"",generatecode?"":"Reserve",d.cFileName,name_override);
        else SCRIPT_MSG("%sFile: \"%s\"",generatecode?"":"Reserve",d.cFileName);
#else
        if (name_override) SCRIPT_MSG("%sFile: \"%s\"->\"%s\"",generatecode?"":"Reserve",filename,name_override);
        else SCRIPT_MSG("%sFile: \"%s\"",generatecode?"":"Reserve",filename);
#endif
        if (!build_compress_whole)
          if (build_compress) SCRIPT_MSG(" [compress]");
        fflush(stdout);
        char buf[1024];
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
#ifdef _WIN32
            char *i=d.cFileName,*o=buf;
#else
            char *i=filename,*o=buf;
#endif
            while (*i)
            {
              char c=*i++;
              *o++=c;
              if (c == '$') *o++='$';
            }
            *o=0;
            ent.offsets[1]=add_string(buf);
          }
        }
        ent.offsets[2]=add_db_data(&mmap);

        mmap.clear();

        if (ent.offsets[2] < 0)
        {
#ifdef _WIN32
          FindClose(h);
#else
          globfree(&globbuf);
#endif
          return PS_ERROR;
        }

        if (data_handle)
        {
          *data_handle=ent.offsets[2];
        }

        {
          DWORD s=getcurdbsize()-last_build_datablock_used;
          if (s) s-=4;
          if (s != len) SCRIPT_MSG(" %d/%d bytes\n",s,len);
          else SCRIPT_MSG(" %d bytes\n",len);
        }

        if (generatecode)
        {
          if (build_datesave || build_overwrite>=0x3 /*ifnewer or ifdiff*/)
          {
#ifdef _WIN32
            FILETIME ft;
            if (GetFileTime(hFile,NULL,NULL,&ft))
            {
              ent.offsets[3]=ft.dwLowDateTime;
              ent.offsets[4]=ft.dwHighDateTime;
            }
            else
            {
              FindClose(h);
#else
            struct stat st;
            if (!fstat(fd, &st))
            {
              union
              {
                struct
                {
                  long l;
                  long h;
                } words;
                long long ll;
              };
              ll = (st.st_mtime * 10000000LL) + 116444736000000000LL;
              ent.offsets[3] = words.l;
              ent.offsets[4] = words.h;
            }
            else
            {
              globfree(&globbuf);
#endif
              ERROR_MSG("%sFile: failed getting file date from \"%s\"\n",generatecode?"":"Reserve",newfn);
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
            mb |= IDIGNORE << 20;
          }
          else
          {
            mb = MB_RETRYCANCEL | MB_ICONSTOP;
            // default for silent installers
            mb |= IDCANCEL << 20;
          }
          ent.offsets[0] |= mb << 3;

          ent.offsets[5] = DefineInnerLangString(build_allowskipfiles ? NLF_FILE_ERROR : NLF_FILE_ERROR_NOIGNORE);
        }

        if (generatecode)
        {
          int a=add_entry(&ent);
          if (a != PS_OK)
          {
#ifdef _WIN32
            FindClose(h);
#else
            globfree(&globbuf);
#endif
            return a;
          }
          if (attrib)
          {
#ifdef _WIN32
            ent.which=EW_SETFILEATTRIBUTES;
            // $OUTDIR is the working directory
            ent.offsets[0]=add_string(name_override?name_override:buf);
            ent.offsets[1]=d.dwFileAttributes;
            ent.offsets[2]=0;
            ent.offsets[3]=0;
            ent.offsets[4]=0;
            ent.offsets[5]=0;

            a=add_entry(&ent);
            if (a != PS_OK)
            {
              FindClose(h);
              return a;
            }
#else
            warning_fl("File /a is disabled for non Win32 platforms.");
#endif
          }
        }
      }
    }
#ifdef _WIN32
    while (FindNextFile(h,&d));
    FindClose(h);
#else
    globfree(&globbuf);
#endif
  }

  if (recurse)
  {
#ifdef NSIS_SUPPORT_STACK
#ifdef _WIN32
    WIN32_FIND_DATA temp;
#endif

    const char *fspec;
    if (!strcmp(dir,".") && strncmp(lgss,".",1))
      fspec=lgss;
    else
      fspec=lgss+strlen(dir)+!!dir[0];

    strcpy(newfn,lgss);
#ifdef _WIN32
    DWORD a=GetFileAttributes(lgss);
    if (a==INVALID_FILE_ATTRIBUTES)
    {
      a=GetFileAttributes(dir);
      sprintf(newfn,"%s%s*.*",dir,dir[0]?PLATFORM_PATH_SEPARATOR_STR:"");
    }
#else
    int a;
    struct stat st;
    if (stat(lgss, &st))
    {
      stat(dir, &st);
      sprintf(newfn,"%s%s*",dir,dir[0]?PLATFORM_PATH_SEPARATOR_STR:"");
    }
#endif
    else
    {
      // we don't want to include a whole directory if it's not the first call
      if (rec_depth) return PS_OK;
#ifdef _WIN32
      fspec="*.*";
#else
      fspec="*";
#endif
    }
#ifdef _WIN32
    if (a&FILE_ATTRIBUTE_DIRECTORY)
    {
      h=FindFirstFile(newfn,&d);
      if (h != INVALID_HANDLE_VALUE)
      {
        do
        {
          if (d.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
          {
            if (strcmp(d.cFileName,"..") && strcmp(d.cFileName,"."))
            {
#else
    if (S_ISDIR(st.st_mode))
    {
      if (!GLOB(newfn, GLOB_NOSORT, NULL, &globbuf))
      {
        for (unsigned int i = 0; i < globbuf.gl_pathc; i++)
        {
          struct stat s;
          if (!stat(globbuf.gl_pathv[i], &s) && S_ISDIR(s.st_mode))
          {
            char *dirname = strrchr(globbuf.gl_pathv[i], PLATFORM_PATH_SEPARATOR_C);
            if (dirname)
              dirname++;
            else
              dirname = globbuf.gl_pathv[i];
            if (strcmp(dirname, "..") && strcmp(dirname, "."))
            {
#endif
              char out_path[1024] = "$OUTDIR\\";

              {
#ifdef _WIN32
                char *i = d.cFileName;
#else
                char *i = dirname;
#endif
                char *o=out_path+strlen(out_path);

                while (*i)
                {
                  char *ni=CharNext(i);
                  if (ni-i > 1)
                  {
                    int l=ni-i;
                    while (l--)
                    {
                      *o++=*i++;
                    }
                  }
                  else
                  {
                    char c=*i++;
                    *o++=c;
                    if (c == '$') *o++='$';
                  }
                }
                *o=0;
              }

              char spec[1024];
#ifdef _WIN32
              wsprintf(spec,"%s%s%s",dir,dir[0]?PLATFORM_PATH_SEPARATOR_STR:"",d.cFileName);
#else
              wsprintf(spec,"%s%s%s",dir,dir[0]?PLATFORM_PATH_SEPARATOR_STR:"",dirname);
#endif
              SCRIPT_MSG("%sFile: Descending to: \"%s\"\n",generatecode?"":"Reserve",spec);
              strcat(spec,PLATFORM_PATH_SEPARATOR_STR);
              strcat(spec,fspec);
              if (generatecode)
              {
                a=add_entry_direct(EW_PUSHPOP, add_string("$OUTDIR"));
                if (a != PS_OK)
                {
#ifdef _WIN32
                  FindClose(h);
#else
                  globfree(&globbuf);
#endif
                  return a;
                }

                a=add_entry_direct(EW_ASSIGNVAR, m_UserVarNames.get("OUTDIR"), add_string(out_path));
                if (a != PS_OK)
                {
#ifdef _WIN32
                  FindClose(h);
#else
                  globfree(&globbuf);
#endif
                  return a;
                }

#ifdef _WIN32
                HANDLE htemp = FindFirstFile(spec,&temp);
                if (htemp != INVALID_HANDLE_VALUE)
                {
                  FindClose(htemp);
#else
                glob_t globbuf2;
                GLOB(spec, GLOB_NOSORT, NULL, &globbuf2);
                if (globbuf2.gl_pathc)
                {
#endif

                  a=add_entry_direct(EW_CREATEDIR, add_string("$OUTDIR"), 1);
                  if (a != PS_OK)
                  {
#ifdef _WIN32
                    FindClose(h);
#else
                    globfree(&globbuf2);
#endif
                    return a;
                  }
                }
              }
              a=do_add_file(spec,attrib,recurse,linecnt,total_files,NULL,generatecode,data_handle,rec_depth+1);
              if (a != PS_OK)
              {
#ifdef _WIN32
                FindClose(h);
#else
                globfree(&globbuf);
#endif
                return a;
              }

              if (generatecode)
              {
                a=add_entry_direct(EW_PUSHPOP, m_UserVarNames.get("OUTDIR"), 1);
                if (a != PS_OK)
                {
#ifdef _WIN32
                  FindClose(h);
#else
                  globfree(&globbuf);
#endif
                  return a;
                }

                if (attrib)
                {
#ifdef _WIN32
                  a=add_entry_direct(EW_SETFILEATTRIBUTES, add_string(out_path), d.dwFileAttributes);
                  if (a != PS_OK)
                  {
                    FindClose(h);
                    return a;
                  }
#else
                  warning_fl("File /a is disabled for non Win32 platforms.");
#endif
                }
              }
              SCRIPT_MSG("%sFile: Returning to: \"%s\"\n",generatecode?"":"Reserve",dir);
            }
          }
        }
#ifdef _WIN32
        while (FindNextFile(h,&d));
        FindClose(h);
#else
        globfree(&globbuf);
#endif

        if (!rec_depth)
        {
          // return to the original $OUTDIR
          a=add_entry_direct(EW_CREATEDIR, add_string("$OUTDIR"), 1);
          if (a != PS_OK)
          {
            return a;
          }
        }
      }
    }
#else
    ERROR_MSG("Error: recursive [Reserve]File requires NSIS_SUPPORT_STACK\n");
    return PS_ERROR;
#endif
  }

  return PS_OK;
}
#endif
