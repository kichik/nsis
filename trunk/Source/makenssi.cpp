const char *NSIS_VERSION="v2.02";

/* 

  Nullsoft Scriptable Install System (NSIS)
  makensis.cpp - installer compiler code

  Copyright (C) 1999-2004 Nullsoft, Inc.
  
  This license applies to everything in the NSIS package, except where otherwise noted.

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  This is the zlib/libpng license, which is approved by opensource.org.

  Portions Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler (zlib).
  Portions Copyright (C) 1996-2002 Julian R Seward (bzip2).
  Portions Copyright (C) 1999-2003 Igor Pavlov (lzma).

*/

#include "Platform.h"
#include <stdio.h>
#include <signal.h>
#ifdef _WIN32
#  include <direct.h>
#else
#  include <unistd.h>
#endif

#include "build.h"
#include "util.h"
#include "exedata.h"


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

  build.setdirs(argv[0]);

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
    fprintf(g_output,"MakeNSIS %s - Copyright 1999-2004 Nullsoft, Inc.\n"
           "\n"
           "Portions Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler (zlib).\n"
           "Portions Copyright (C) 1996-2002 Julian R Seward (bzip2).\n"
           "Portions Copyright (C) 1999-2003 Igor Pavlov (lzma).\n"
           "\n"
           "Contributors: nnop@newmail.ru, Ryan Geiss, Andras Varga, Drew Davidson, Peter Windridge, Dave Laundon, Robert Rainwater, Yaroslav Faybishenko, Jeff Doozan, Amir Szekely, Ximon Eighteen, et al.\n\n",NSIS_VERSION);
    fflush(g_output);
  }

  atexit(myatexit);
  signal(SIGINT,sigint);

  if (!g_output) g_output=stdout;
  while (argpos < argc)
  {
    if (argv[argpos][0] == OPT_C && strcmp(argv[argpos], "-"))
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
          fprintf(g_output,"Copyright (C) 1999-2004 Nullsoft, Inc.\n\n"
               "This license applies to everything in the NSIS package, except where otherwise\nnoted.\n\n"
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
               "  3. This notice may not be removed or altered from any source distribution.\n\n");
          fflush(g_output);
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
        if (build.display_info)
        {
          fprintf(g_output,"Size of zlib EXE header is %d bytes.\n",zlib_exehead_size);
          fprintf(g_output,"Size of bzip2 EXE header is %d bytes.\n",bzip2_exehead_size);
          fprintf(g_output,"Size of lzma EXE header is %d bytes.\n",lzma_exehead_size);
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
        nousage++;
      }
      else break;
    }
    else
    {
      files_processed++;
      if (!strcmp(argv[argpos],"-")) g_dopause=0;
      if (!g_noconfig)
      {
        g_noconfig=1;
        char exepath[1024];
        strncpy(exepath,argv[0],sizeof(exepath)-1);
        exepath[1023]=0;
        char *p=exepath;
        while (*p) p++;
        while (p > exepath && *p != PLATFORM_PATH_SEPARATOR_C) p=CharPrev(exepath,p);
        if (p>exepath) p++;
        strcpy(p,"nsisconf.nsh");
        FILE *cfg=fopen(exepath,"rt");
        if (cfg)
        {
          if (build.display_script) 
          {
            fprintf(g_output,"Processing config: \n");
            fflush(g_output);
          }
          int ret=build.process_script(cfg,exepath);
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
      }

      {
        char sfile[1024];
        if (!strcmp(argv[argpos],"-"))
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
            string dir = get_dir_name(get_full_path(sfile));
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
      fprintf(g_output,"Usage:\n"
             "  makensis [options] [script.nsi | - [...]]\n"
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
             "   for script file name, you can use - to read from the standard input\n");
      fflush(g_output);
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
