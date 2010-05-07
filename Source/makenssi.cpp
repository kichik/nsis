/*
 * makenssi.cpp
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
#include <signal.h>
#ifdef _WIN32
#  include <direct.h>
#else
#  include <unistd.h>
#endif
#include "tstring.h"

#include "build.h"
#include "util.h"

#include <nsis-version.h>
#include <fcntl.h>
#include <io.h>

using namespace std;

int g_noconfig=0;
int g_display_errors=1;
FILE *g_output=stdout;
#ifdef _UNICODE
UINT g_initialCodepage;
#endif

void quit()
{
  if (g_display_errors) 
  {
    _ftprintf(g_output,_T("\nNote: you may have one or two (large) stale temporary file(s)\n")
         _T("left in your temporary directory (Generally this only happens on Windows 9x).\n"));
    fflush(g_output);
  }
  exit(1);
}

static void myatexit()
{
  dopause();
  if (g_output != stdout && g_output) fclose(g_output);
#ifdef _UNICODE
  SetConsoleOutputCP(g_initialCodepage);
#endif
}

static void sigint(int sig)
{
  if (g_display_errors) 
  {
    _ftprintf(g_output,_T("\n\nAborting on Ctrl+C...\n"));
    fflush(g_output);
  }
  quit();
}

#ifdef _WIN32
static DWORD WINAPI sigint_event_msg_handler(LPVOID)
{
  HANDLE hEvent = OpenEvent(EVENT_ALL_ACCESS, FALSE, _T("makensis win32 signint event"));

  if (hEvent)
  {
    if (WaitForSingleObject(hEvent, INFINITE) == WAIT_OBJECT_0)
      raise(SIGINT);
    CloseHandle(hEvent);
  }

  return 0;
}
#endif

static void init_signals()
{
  atexit(myatexit);
  signal(SIGINT,sigint);

#ifdef _WIN32
  DWORD id;
  HANDLE hThread = CreateThread(NULL, 0, sigint_event_msg_handler, NULL, 0, &id);
  if (hThread) CloseHandle(hThread);
#endif
}

static void print_logo()
{
  _ftprintf(g_output,_T("MakeNSIS %s - Copyright 1995-2009 Contributors\n")
         _T("See the file COPYING for license details.\n")
         _T("Credits can be found in the Users Manual.\n\n"), NSIS_VERSION);
  fflush(g_output);
}

static void print_license()
{
  _ftprintf(g_output,_T("Copyright (C) 1999-2009 Nullsoft and Contributors\n\n")
       _T("This license applies to everything in the NSIS package, except where otherwise\n")
       _T("noted.\n\n")
       _T("This software is provided 'as-is', without any express or implied warranty.\n")
       _T("In no event will the authors be held liable for any damages arising from the\n")
       _T("use of this software.\n\n")
       _T("Permission is granted to anyone to use this software for any purpose, including\n")
       _T("commercial applications, and to alter it and redistribute it freely, subject to\n")
       _T("the following restrictions:\n")
       _T("  1. The origin of this software must not be misrepresented; you must not claim\n")
       _T("     that you wrote the original software. If you use this software in a\n")
       _T("     product, an acknowledgment in the product documentation would be\n")
       _T("     appreciated but is not required.\n")
       _T("  2. Altered source versions must be plainly marked as such, and must not be\n")
       _T("     misrepresented as being the original software.\n")
       _T("  3. This notice may not be removed or altered from any source distribution.\n\n")
       _T("In addition to this license, different licenses apply to the included\n")
       _T("compression modules. See the file COPYING for details.\n"));
  fflush(g_output);
}

static void print_usage()
{
  _ftprintf(g_output,_T("Usage:\n")
         _T("  makensis [option | script.nsi | - [...]]\n")
         _T("   options are:\n")
         _T("    ") OPT_STR _T("CMDHELP item prints out help for 'item', or lists all commands\n")
         _T("    ") OPT_STR _T("HDRINFO prints information about what options makensis was compiled with\n")
         _T("    ") OPT_STR _T("LICENSE prints the makensis software license\n")
         _T("    ") OPT_STR _T("VERSION prints the makensis version and exits\n")
         _T("    ") OPT_STR _T("Px sets the compiler process priority, where x is 5=realtime,4=high,\n")
         _T("    ")         _T("    3=above normal,2=normal,1=below normal,0=idle\n")
         _T("    ") OPT_STR _T("Vx verbosity where x is 4=all,3=no script,2=no info,1=no warnings,0=none\n")
         _T("    ") OPT_STR _T("Ofile specifies a text file to log compiler output (default is stdout)\n")
         _T("    ") OPT_STR _T("PAUSE pauses after execution\n")
         _T("    ") OPT_STR _T("NOCONFIG disables inclusion of <path to makensis.exe>") PLATFORM_PATH_SEPARATOR_STR _T("nsisconf.nsh\n")
         _T("    ") OPT_STR _T("NOCD disabled the current directory change to that of the .nsi file\n")
         _T("    ") OPT_STR _T("Ddefine[=value] defines the symbol \"define\" for the script [to value]\n")
         _T("    ") OPT_STR _T("Xscriptcmd executes scriptcmd in script (i.e. \"") OPT_STR _T("XOutFile poop.exe\")\n")
         _T("   parameters are processed by order (") OPT_STR _T("Ddef ins.nsi != ins.nsi ") OPT_STR _T("Ddef)\n")
         _T("   for script file name, you can use - to read from the standard input\n")
#ifdef _WIN32	
         _T("   you can also use - as an option character: -PAUSE as well as /PAUSE\n")
#endif
         _T("   you can use a double-dash to end options processing: makensis -- -ins.nsi\n"));
  fflush(g_output);
}

static void print_stub_info(CEXEBuild& build)
{
  if (build.display_info)
  {
    _ftprintf(g_output,_T("Size of first header is %lu bytes.\n"),(unsigned long)sizeof(firstheader));
    _ftprintf(g_output,_T("Size of main header is %lu bytes.\n"),(unsigned long)sizeof(header));
    _ftprintf(g_output,_T("Size of each section is %lu bytes.\n"),(unsigned long)sizeof(section));
    _ftprintf(g_output,_T("Size of each page is %lu bytes.\n"),(unsigned long)sizeof(page));
    _ftprintf(g_output,_T("Size of each instruction is %lu bytes.\n"),(unsigned long)sizeof(entry));
    int x=build.definedlist.getnum();
    _ftprintf(g_output,_T("\nDefined symbols: "));
    for (int i=0; i<x; i++)
    {
      _ftprintf(g_output,_T("%s"),build.definedlist.getname(i));
      TCHAR *p=build.definedlist.getvalue(i);
      if (*p) _ftprintf(g_output,_T("=%s"),p);
      if (i<x-1) _ftprintf(g_output,_T(","));
    }
    if (!x) _ftprintf(g_output,_T("none"));
    _ftprintf(g_output,_T("\n"));
    fflush(g_output);
  }
}

static tstring get_home()
{
  TCHAR *home = _tgetenv(
#ifdef _WIN32
    _T("APPDATA")
#else
    _T("HOME")
#endif
  );

  return home ? home : _T("");
}

static int process_config(CEXEBuild& build, tstring& conf)
{
  FILE *cfg=FOPENTEXT(conf.c_str(),"rt");
  if (cfg)
  {
    if (build.display_script) 
    {
      _ftprintf(g_output,_T("Processing config: \n"));
      fflush(g_output);
    }
    int ret=build.process_script(cfg,(TCHAR*)conf.c_str());
    fclose(cfg);
    if (ret != PS_OK && ret != PS_EOF)
    {
      if (build.display_errors) 
      {
        _ftprintf(g_output,_T("Error in config on line %d -- aborting creation process\n"),build.linecnt);
        fflush(g_output);
      }
      return 1;
    }
    if (build.display_script) 
    {
      _ftprintf(g_output,_T("\n"));
      fflush(g_output);
    }
  }
  return 0;
}

static int change_to_script_dir(CEXEBuild& build, tstring& script)
{
  tstring dir = get_dir_name(get_full_path(script));
  if (!dir.empty()) 
  {
    if (build.display_script) 
    {
      _ftprintf(g_output,_T("Changing directory to: \"%s\"\n"),dir.c_str());
      fflush(g_output);
    }
    if (_tchdir(dir.c_str()))
    {
      if (build.display_errors)
      {
        _ftprintf(g_output,_T("Error changing directory to \"%s\"\n"),dir.c_str());
        fflush(g_output);
      }
      return 1;
    }
    if (build.display_script) 
    {
      _ftprintf(g_output,_T("\n"));
      fflush(g_output);
    }
  }
  return 0;
}

#ifdef NSIS_HPUX_ALLOW_UNALIGNED_DATA_ACCESS
extern "C" void allow_unaligned_data_access();
#endif

int _tmain(int argc, TCHAR **argv)
{

#ifdef NSIS_HPUX_ALLOW_UNALIGNED_DATA_ACCESS
  allow_unaligned_data_access();
#endif

  CEXEBuild build;
  int do_cd=1;
  int outputtried=0;
  int argpos=1;
  int nousage=0;
  int files_processed=0;
  int cmds_processed=0;
  FILE *fp;
  int tmpargpos=1;
  int no_logo=0;
  int in_files=0;

#ifdef _UNICODE
  _setmode(_fileno(stdout), _O_U8TEXT); // set stdout to UTF-8
  g_initialCodepage = GetConsoleOutputCP();
  SetConsoleOutputCP(CP_UTF8); // set console output to UTF-8 (especially useful for subprocesses like !system)
#endif
  try
  {
    build.initialize(argv[0]);
  }
  catch (exception& err)
  {
    _ftprintf(g_output, _T("Error initalizing CEXEBuild: %s\n"), CtoTString(err.what()));
    fflush(g_output);
    return 1;
  }

  if (argc > 1 && IS_OPT(argv[tmpargpos]) && !_tcsicmp(&argv[tmpargpos][1],_T("VERSION")))
  {
    _ftprintf(g_output,NSIS_VERSION);
    fflush(g_output);
    return 0;
  }
  if (argc > 1 && IS_OPT(argv[tmpargpos]) && (argv[tmpargpos][1]==_T('v') || argv[tmpargpos][1]==_T('V')))
  {
    if (argv[tmpargpos][2] <= _T('2') && argv[tmpargpos][2] >= _T('0'))
    {
      no_logo=1;
    }
   tmpargpos++;
  }
  
  if (!no_logo)
  {
    if (argc > tmpargpos && IS_OPT(argv[tmpargpos]) && (argv[tmpargpos][1]==_T('o') || argv[tmpargpos][1]==_T('O')) && argv[tmpargpos][2])
    {
      g_output=FOPENTEXT(argv[tmpargpos]+2,"w");
      if (!g_output) 
      {
        _tprintf(_T("Error opening output log for writing. Using stdout.\n"));
        g_output=stdout;
      }
      outputtried=1;
    }
    print_logo();
  }

  init_signals();

  if (!g_output) g_output=stdout;
  while (argpos < argc)
  {
    if (!_tcscmp(argv[argpos], _T("--")))
      in_files=1;
    else if (IS_OPT(argv[argpos]) && _tcscmp(argv[argpos], _T("-")) && !in_files)
    {
      if ((argv[argpos][1]==_T('D') || argv[argpos][1]==_T('d')) && argv[argpos][2])
      {
        TCHAR *p=argv[argpos]+2;
        TCHAR *s=_tcsdup(p),*v;
        if (build.display_script) 
        {
          _ftprintf(g_output,_T("Command line defined: \"%s\"\n"),p);
          fflush(g_output);
        }
        v=_tcsstr(s,_T("="));
        if (v) *v++=0;
        build.define(s,v?v:_T(""));
        free(s);
      }
      else if ((argv[argpos][1]==_T('X') || argv[argpos][1]==_T('x')) && argv[argpos][2])
      {
        if (build.process_oneline(argv[argpos]+2,_T("command line"),argpos+1) != PS_OK)
        {
          return 1;
        }
        cmds_processed++;
      }
      else if ((argv[argpos][1]==_T('O') || argv[argpos][1]==_T('o')) && argv[argpos][2])
      {
        if (!outputtried)
        {
          g_output=FOPENTEXT(argv[argpos]+2,"w");
          if (!g_output) 
          {
            if (build.display_errors) _tprintf(_T("Error opening output log for writing. Using stdout.\n"));
            g_output=stdout;
          }
          outputtried=1;
        }
      }
      else if (!_tcsicmp(&argv[argpos][1],_T("NOCD"))) do_cd=0;
      else if ((argv[argpos][1] == _T('V') || argv[argpos][1] == _T('v')) && 
               argv[argpos][2] >= _T('0') && argv[argpos][2] <= _T('4') && !argv[argpos][3])
      {
        int v=argv[argpos][2]-_T('0');
        build.display_script=v>3;
        build.display_info=v>2;
        build.display_warnings=v>1;
        build.display_errors=v>0;
        g_display_errors=build.display_errors;
      }
      else if (!_tcsicmp(&argv[argpos][1],_T("NOCONFIG"))) g_noconfig=1;
      else if (!_tcsicmp(&argv[argpos][1],_T("PAUSE"))) g_dopause=1;
      else if (!_tcsicmp(&argv[argpos][1],_T("LICENSE"))) 
      {
        if (build.display_info) 
        {
          print_license();
        }
        nousage++;
      }
      else if (!_tcsicmp(&argv[argpos][1],_T("CMDHELP")))
      {
        if (argpos < argc-1)
          build.print_help(argv[++argpos]);
        else
          build.print_help(NULL);
        nousage++;
      }
      else if (!_tcsicmp(&argv[argpos][1],_T("NOTIFYHWND")))
      {
#ifdef _WIN32
        build.notify_hwnd=(HWND)_ttol(argv[++argpos]);
        if (!IsWindow(build.notify_hwnd))
          build.notify_hwnd=0;
#else
        argpos++;
        build.warning(OPT_STR _T("NOTIFYHWND is disabled for non Win32 platforms."));
#endif
      }
      else if (!_tcsicmp(&argv[argpos][1],_T("HDRINFO")))
      {
        print_stub_info(build);
        nousage++;
      }
      else if ((argv[argpos][1]==_T('P') || argv[argpos][1]==_T('p')) &&
               argv[argpos][2] >= _T('0') && argv[argpos][2] <= _T('5') && !argv[argpos][3])
      {
#ifdef _WIN32
        // priority setting added 01-2007 by Comm@nder21
        int p=argv[argpos][2]-_T('0');
        HANDLE hProc = GetCurrentProcess();
        
        struct
        {
          DWORD priority;
          DWORD fallback;
        } classes[] = {
          {IDLE_PRIORITY_CLASS,         IDLE_PRIORITY_CLASS},
          {BELOW_NORMAL_PRIORITY_CLASS, IDLE_PRIORITY_CLASS},
          {NORMAL_PRIORITY_CLASS,       NORMAL_PRIORITY_CLASS},
          {ABOVE_NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS},
          {HIGH_PRIORITY_CLASS,         HIGH_PRIORITY_CLASS},
          {REALTIME_PRIORITY_CLASS,     REALTIME_PRIORITY_CLASS}
        };

        if (SetPriorityClass(hProc, classes[p].priority) == FALSE)
        {
          SetPriorityClass(hProc, classes[p].fallback);
        }

        if (p == 5)
          build.warning(_T("makensis is running in REALTIME priority mode!"));

#else
        build.warning(OPT_STR _T("Px is disabled for non Win32 platforms."));
#endif
      }
      else break;
    }
    else
    {
      files_processed++;
      if (!_tcscmp(argv[argpos],_T("-")) && !in_files)
        g_dopause=0;
      if (!g_noconfig)
      {
        g_noconfig=1;
        tstring main_conf;
        TCHAR* env_var = _tgetenv(_T("NSISCONFDIR"));
        if(env_var == NULL)
#ifndef NSIS_CONFIG_CONST_DATA_PATH
          main_conf = get_dir_name(get_executable_dir(argv[0]));
#else
          main_conf = PREFIX_CONF;
#endif
        else main_conf = env_var;
        main_conf += PLATFORM_PATH_SEPARATOR_STR;
        main_conf += _T("nsisconf.nsh");
        if (process_config(build, main_conf))
          return 1;

        tstring home_conf = get_home();
        if (home_conf != _T(""))
        {
          home_conf += PLATFORM_PATH_SEPARATOR_STR;
#ifdef _WIN32
          home_conf += _T("nsisconf.nsh");
#else
          home_conf += _T(".nsisconf.nsh");
#endif
          if (process_config(build, home_conf))
            return 1;
        }
      }

      {
        TCHAR sfile[1024];
        if (!_tcscmp(argv[argpos],_T("-")) && !in_files)
        {
          fp=stdin;
          _tcscpy(sfile,_T("stdin"));
        }
        else
        {
          _tcscpy(sfile,argv[argpos]);
          fp=FOPENTEXT(sfile,"rt");
          if (!fp)
          {
            _stprintf(sfile,_T("%s.nsi"),argv[argpos]);
            fp=FOPENTEXT(sfile,"rt");
            if (!fp)
            {
              if (build.display_errors) 
              {
                sfile[_tcslen(sfile)-4]=0;
                _ftprintf(g_output,_T("Can't open script \"%s\"\n"),sfile);
                fflush(g_output);
              }
              return 1;
            }
          }
          if (do_cd)
          {
            tstring script_file = tstring(sfile);
            if (change_to_script_dir(build, script_file))
              return 1;
          }
        }

        if (build.display_script) 
        {
          build.notify(MAKENSIS_NOTIFY_SCRIPT,sfile);
          _ftprintf(g_output,_T("Processing script file: \"%s\"\n"),sfile);
          fflush(g_output);
        }
        int ret=build.process_script(fp,sfile);
        if (fp != stdin) fclose(fp);

        if (ret != PS_EOF && ret != PS_OK)
        {
          if (build.display_errors) 
          {
            _ftprintf(g_output,_T("Error in script \"%s\" on line %d -- aborting creation process\n"),sfile,build.linecnt);
            fflush(g_output);
          }
          return 1;
        }
      }
    }
    argpos++;
  }

  if (argpos<argc || (!files_processed && !cmds_processed))
  {
    if (build.display_errors && !nousage)
    {
      print_usage();
    }
    return 1;
  }

  if (build.display_info) 
  {
    _ftprintf(g_output,_T("\nProcessed "));
    if (files_processed) _ftprintf(g_output,_T("%d file%s, "),files_processed,files_processed==1?_T(""):_T("s"));
    if (cmds_processed) _ftprintf(g_output,_T("%d command line command%s, "),cmds_processed,cmds_processed==1?_T(""):_T("s"));
    _ftprintf(g_output,_T("writing output:\n"));
    fflush(g_output);
  }
  
  if (build.write_output())
  { 
    if (build.display_errors) 
    {
      _ftprintf(g_output,_T("Error - aborting creation process\n"));
      fflush(g_output);
    }
    return 1;
  }
  return 0; 
}
