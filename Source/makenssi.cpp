/*
 * makenssi.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2006 Nullsoft and Contributors
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
#include <signal.h>
#ifdef _WIN32
#  include <direct.h>
#else
#  include <unistd.h>
#endif
#include <string>

#include "build.h"
#include "util.h"

#include "version.h"

using namespace std;

int g_noconfig=0;
int g_display_errors=1;
FILE *g_output=stdout;

void quit()
{
  if (g_display_errors) 
  {
    fprintf(g_output,"\nNote: you may have one or two (large) stale temporary file(s)\n"
         "left in your temporary directory (Generally this only happens on Windows 9x).\n");
    fflush(g_output);
  }
  exit(1);
}

static void myatexit()
{
  dopause();
  if (g_output != stdout && g_output) fclose(g_output);
}

static void sigint(int sig)
{
  if (g_display_errors) 
  {
    fprintf(g_output,"\n\nAborting on Ctrl+C...\n");
    fflush(g_output);
  }
  quit();
}

#ifdef _WIN32
static DWORD WINAPI sigint_event_msg_handler(LPVOID)
{
  HANDLE hEvent = OpenEvent(EVENT_ALL_ACCESS, FALSE, "makensis win32 signint event");

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
  fprintf(g_output,"MakeNSIS %s - Copyright 1995-2006 Contributors\n"
         "See the file COPYING for license details.\n"
         "Credits can be found in the Users Manual.\n\n", NSIS_VERSION);
  fflush(g_output);
}

static void print_license()
{
  fprintf(g_output,"Copyright (C) 1999-2006 Nullsoft and Contributors\n\n"
       "This license applies to everything in the NSIS package, except where otherwise\n"
       "noted.\n\n"
       "This software is provided 'as-is', without any express or implied warranty.\n"
       "In no event will the authors be held liable for any damages arising from the\n"
       "use of this software.\n\n"
       "Permission is granted to anyone to use this software for any purpose, including\n"
       "commercial applications, and to alter it and redistribute it freely, subject to\n"
       "the following restrictions:\n"
       "  1. The origin of this software must not be misrepresented; you must not claim\n"
       "     that you wrote the original software. If you use this software in a\n"
       "     product, an acknowledgment in the product documentation would be\n"
       "     appreciated but is not required.\n"
       "  2. Altered source versions must be plainly marked as such, and must not be\n"
       "     misrepresented as being the original software.\n"
       "  3. This notice may not be removed or altered from any source distribution.\n\n"
       "In addition to this license, different licenses apply to the included\n"
       "compression modules. See the file COPYING for details.\n");
  fflush(g_output);
}

static void print_usage()
{
  fprintf(g_output,"Usage:\n"
         "  makensis [option | script.nsi | - [...]]\n"
         "   options are:\n"
         "    " OPT_STR "CMDHELP item prints out help for 'item', or lists all commands\n"
         "    " OPT_STR "HDRINFO prints information about what options makensis was compiled with\n"
         "    " OPT_STR "LICENSE prints the makensis software license\n"
         "    " OPT_STR "VERSION prints the makensis version and exits\n"
         "    " OPT_STR "Vx verbosity where x is 4=all,3=no script,2=no info,1=no warnings,0=none\n"
         "    " OPT_STR "Ofile specifies a text file to log compiler output (default is stdout)\n"
         "    " OPT_STR "PAUSE pauses after execution\n"
         "    " OPT_STR "NOCONFIG disables inclusion of <path to makensis.exe>" PLATFORM_PATH_SEPARATOR_STR "nsisconf.nsh\n"
         "    " OPT_STR "NOCD disabled the current directory change to that of the .nsi file\n"
         "    " OPT_STR "Ddefine[=value] defines the symbol \"define\" for the script [to value]\n"
         "    " OPT_STR "Xscriptcmd executes scriptcmd in script (i.e. \"" OPT_STR "XOutFile poop.exe\")\n"
         "   parameters are processed by order (" OPT_STR "Ddef ins.nsi != ins.nsi " OPT_STR "Ddef)\n"
         "   for script file name, you can use - to read from the standard input\n");
  fflush(g_output);
}

static void print_stub_info(CEXEBuild& build)
{
  if (build.display_info)
  {
    fprintf(g_output,"Size of first header is %d bytes.\n",sizeof(firstheader));
    fprintf(g_output,"Size of main header is %d bytes.\n",sizeof(header));
    fprintf(g_output,"Size of each section is %d bytes.\n",sizeof(section));
    fprintf(g_output,"Size of each page is %d bytes.\n",sizeof(page));
    fprintf(g_output,"Size of each instruction is %d bytes.\n",sizeof(entry));
    int x=build.definedlist.getnum();
    fprintf(g_output,"\nDefined symbols: ");
    for (int i=0; i<x; i++)
    {
      fprintf(g_output,"%s",build.definedlist.getname(i));
      char *p=build.definedlist.getvalue(i);
      if (*p) fprintf(g_output,"=%s",p);
      if (i<x-1) fprintf(g_output,",");
    }
    if (!x) fprintf(g_output,"none");
    fprintf(g_output,"\n");
    fflush(g_output);
  }
}

static string get_home()
{
  char *home = getenv(
#ifdef _WIN32
    "APPDATA"
#else
    "HOME"
#endif
  );

  return home ? home : "";
}

static int process_config(CEXEBuild& build, string& conf)
{
  FILE *cfg=fopen(conf.c_str(),"rt");
  if (cfg)
  {
    if (build.display_script) 
    {
      fprintf(g_output,"Processing config: \n");
      fflush(g_output);
    }
    int ret=build.process_script(cfg,(char*)conf.c_str());
    fclose(cfg);
    if (ret != PS_OK && ret != PS_EOF)
    {
      if (build.display_errors) 
      {
        fprintf(g_output,"Error in config on line %d -- aborting creation process\n",build.linecnt);
        fflush(g_output);
      }
      return 1;
    }
    if (build.display_script) 
    {
      fprintf(g_output,"\n");
      fflush(g_output);
    }
  }
  return 0;
}

static int change_to_script_dir(CEXEBuild& build, string& script)
{
  string dir = get_dir_name(get_full_path(script));
  if (!dir.empty()) 
  {
    if (build.display_script) 
    {
      fprintf(g_output,"Changing directory to: \"%s\"\n",dir.c_str());
      fflush(g_output);
    }
    if (chdir(dir.c_str()))
    {
      if (build.display_errors)
      {
        fprintf(g_output,"Error changing directory to \"%s\"\n",dir.c_str());
        fflush(g_output);
      }
      return 1;
    }
    if (build.display_script) 
    {
      fprintf(g_output,"\n");
      fflush(g_output);
    }
  }
  return 0;
}

int main(int argc, char **argv)
{
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
#ifndef _WIN32
  int in_files=0;
#endif

  try
  {
    build.initialize(argv[0]);
  }
  catch (exception& err)
  {
    fprintf(g_output, "Error initalizing CEXEBuild: %s\n", err.what());
    fflush(g_output);
    return 1;
  }

  if (argc > 1 && !stricmp(argv[1], OPT_STR "VERSION"))
  {
    fprintf(g_output,NSIS_VERSION);
    fflush(g_output);
    return 0;
  }
  if (argc > 1 && argv[1][0]==OPT_C && (argv[1][1]=='v' || argv[1][1]=='V'))
  {
    tmpargpos++;
    if (argv[1][2] <= '2' && argv[1][2] >= '0')
    {
      no_logo=1;
    }
  }
  
  if (!no_logo)
  {
    if (argc > tmpargpos && argv[tmpargpos][0]==OPT_C && (argv[tmpargpos][1]=='o' || argv[tmpargpos][1]=='O') && argv[tmpargpos][2])
    {
      g_output=fopen(argv[tmpargpos]+2,"w");
      if (!g_output) 
      {
        printf("Error opening output log for writing. Using stdout.\n");
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
#ifndef _WIN32
    if (!strcmp(argv[argpos], "--"))
      in_files=1;
    else if (argv[argpos][0] == OPT_C && strcmp(argv[argpos], "-") && !in_files)
#else
    if (argv[argpos][0] == OPT_C && strcmp(argv[argpos], "-"))
#endif
    {
      if ((argv[argpos][1]=='D' || argv[argpos][1]=='d') && argv[argpos][2])
      {
        char *p=argv[argpos]+2;
        char *s=strdup(p),*v;
        if (build.display_script) 
        {
          fprintf(g_output,"Command line defined: \"%s\"\n",p);
          fflush(g_output);
        }
        v=strstr(s,"=");
        if (v) *v++=0;
        build.define(s,v?v:"");
        free(s);
      }
      else if ((argv[argpos][1]=='X' || argv[argpos][1]=='x') && argv[argpos][2])
      {
        if (build.process_oneline(argv[argpos]+2,"command line",argpos+1) != PS_OK)
        {
          return 1;
        }
        cmds_processed++;
      }
      else if ((argv[argpos][1]=='O' || argv[argpos][1]=='o') && argv[argpos][2])
      {
        if (!outputtried)
        {
          g_output=fopen(argv[argpos]+2,"w");
          if (!g_output) 
          {
            if (build.display_errors) printf("Error opening output log for writing. Using stdout.\n");
            g_output=stdout;
          }
          outputtried=1;
        }
      }
      else if (!stricmp(&argv[argpos][1],"NOCD")) do_cd=0;
      else if ((argv[argpos][1] == 'V' || argv[argpos][1] == 'v') && 
               argv[argpos][2] >= '0' && argv[argpos][2] <= '4' && !argv[argpos][3])
      {
        int v=argv[argpos][2]-'0';
        build.display_script=v>3;
        build.display_info=v>2;
        build.display_warnings=v>1;
        build.display_errors=v>0;
        g_display_errors=build.display_errors;
      }
      else if (!stricmp(&argv[argpos][1],"NOCONFIG")) g_noconfig=1;
      else if (!stricmp(&argv[argpos][1],"PAUSE")) g_dopause=1;
      else if (!stricmp(&argv[argpos][1],"LICENSE")) 
      {
        if (build.display_info) 
        {
          print_license();
        }
        nousage++;
      }
      else if (!stricmp(&argv[argpos][1],"CMDHELP"))
      {
        if (argpos < argc-1)
          build.print_help(argv[++argpos]);
        else
          build.print_help(NULL);
        nousage++;
      }
      else if (!stricmp(&argv[argpos][1],"NOTIFYHWND"))
      {
#ifdef _WIN32
        build.notify_hwnd=(HWND)atol(argv[++argpos]);
        if (!IsWindow(build.notify_hwnd))
          build.notify_hwnd=0;
#else
        argpos++;
        build.warning(OPT_STR "NOTIFYHWND is disabled for non Win32 platforms.");
#endif
      }
      else if (!stricmp(&argv[argpos][1],"HDRINFO"))
      {
        print_stub_info(build);
        nousage++;
      }
      else break;
    }
    else
    {
      files_processed++;
#ifndef _WIN32
      if (!strcmp(argv[argpos],"-") && !in_files)
#else
      if (!strcmp(argv[argpos],"-"))
#endif
        g_dopause=0;
      if (!g_noconfig)
      {
        g_noconfig=1;
        string main_conf;
        char* env_var = getenv("NSISCONFDIR");
        if(env_var == NULL)
#ifndef NSIS_CONFIG_CONST_DATA_PATH
          main_conf = get_executable_dir(argv[0]);
#else
          main_conf = PREFIX_CONF;
#endif
        else main_conf = env_var;
        main_conf += PLATFORM_PATH_SEPARATOR_STR;
        main_conf += "nsisconf.nsh";
        if (process_config(build, main_conf))
          return 1;

        string home_conf = get_home();
        if (home_conf != "")
        {
          home_conf += PLATFORM_PATH_SEPARATOR_STR;
#ifdef _WIN32
          home_conf += "nsisconf.nsh";
#else
          home_conf += ".nsisconf.nsh";
#endif
          if (process_config(build, home_conf))
            return 1;
        }
      }

      {
        char sfile[1024];
#ifndef _WIN32
        if (!strcmp(argv[argpos],"-") && !in_files)
#else
        if (!strcmp(argv[argpos],"-"))
#endif
        {
          fp=stdin;
          strcpy(sfile,"stdin");
        }
        else
        {
          strcpy(sfile,argv[argpos]);
          fp=fopen(sfile,"rt");
          if (!fp)
          {
            sprintf(sfile,"%s.nsi",argv[argpos]);
            fp=fopen(sfile,"rt");
            if (!fp)
            {
              if (build.display_errors) 
              {
                sfile[strlen(sfile)-4]=0;
                fprintf(g_output,"Can't open script \"%s\"\n",sfile);
                fflush(g_output);
              }
              return 1;
            }
          }
          if (do_cd)
          {
            string script_file = string(sfile);
            if (change_to_script_dir(build, script_file))
              return 1;
          }
        }

        if (build.display_script) 
        {
          build.notify(MAKENSIS_NOTIFY_SCRIPT,sfile);
          fprintf(g_output,"Processing script file: \"%s\"\n",sfile);
          fflush(g_output);
        }
        int ret=build.process_script(fp,sfile);
        if (fp != stdin) fclose(fp);

        if (ret != PS_EOF && ret != PS_OK)
        {
          if (build.display_errors) 
          {
            fprintf(g_output,"Error in script \"%s\" on line %d -- aborting creation process\n",sfile,build.linecnt);
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
    fprintf(g_output,"\nProcessed ");
    if (files_processed) fprintf(g_output,"%d file%s, ",files_processed,files_processed==1?"":"s");
    if (cmds_processed) fprintf(g_output,"%d command line command%s, ",cmds_processed,cmds_processed==1?"":"s");
    fprintf(g_output,"writing output:\n");
    fflush(g_output);
  }
  
  if (build.write_output())
  { 
    if (build.display_errors) 
    {
      fprintf(g_output,"Error - aborting creation process\n");
      fflush(g_output);
    }
    return 1;
  }
  return 0; 
}
