const char *NSIS_VERSION="v2.0b3";

/* 
  Nullsoft "SuperPimp" Installation System - makensis.cpp - installer compiler code

  Copyright (C) 1999-2003 Nullsoft, Inc.

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

  This source distribution includes portions of zlib. see zlib/zlib.h for 
  its license and so forth. Note that this license is also borrowed from zlib.
*/

#include <windows.h>
#include <stdio.h>
#include <signal.h>

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
  int plugins_processed=0;
  FILE *fp;
  int tmpargpos=1;
  int no_logo=0;

  if (argc > 1 && !stricmp(argv[1], "/VERSION"))
  {
    fprintf(g_output,NSIS_VERSION);
    fflush(g_output);
    return 0;
  }
  if (argc > 1 && argv[1][0]=='/' && (argv[1][1]=='v' || argv[1][1]=='V'))
  {
    tmpargpos++;
    if (argv[1][2] <= '2' && argv[1][2] >= '0')
    {
      no_logo=1;
    }
  }
  
  if (!no_logo)
  {
    if (argc > tmpargpos && argv[tmpargpos][0]=='/' && (argv[tmpargpos][1]=='o' || argv[tmpargpos][1]=='O') && argv[tmpargpos][2])
    {
      g_output=fopen(argv[tmpargpos]+2,"w");
      if (!g_output) 
      {
        printf("Error opening output log for writing. Using stdout.\n");
        g_output=stdout;
      }
      outputtried=1;
    }
    fprintf(g_output,"MakeNSIS %s - Copyright 1999-2003 Nullsoft, Inc.\n"
           "\n"
           "Portions Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler (zlib).\n"
           "Includes portions derived from bzip2 (see documentation for details).\n"
           "Contributors: nnop@newmail.ru, Ryan Geiss, Andras Varga, Drew Davidson, Peter Windridge, Dave Laundon, Robert Rainwater, Yaroslav Faybishenko, Jeff Doozan, Amir Szekely, Ximon Eighteen, et al.\n\n",NSIS_VERSION);
    fflush(g_output);
  }

  atexit(myatexit);
  signal(SIGINT,sigint);

  if (!g_output) g_output=stdout;
  while (argpos < argc)
  {
    if (argv[argpos][0]=='/' && (argv[argpos][1]=='D' || argv[argpos][1]=='d') && argv[argpos][2])
    {
      char *p=argv[argpos]+2;
      if (p[0])
      {
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
      else
      {
        build.warning("command line /D requires argument (i.e. \"/Ddefine\"). ignored.");
      }
    }
    else if (argv[argpos][0]=='/' && (argv[argpos][1]=='X' || argv[argpos][1]=='x') && argv[argpos][2])
    {
      if (build.process_oneline(argv[argpos]+2,"command line",argpos+1) != PS_OK)
      {
        return 1;
      }
      cmds_processed++;
    }
    else if (argv[argpos][0]=='/' && (argv[argpos][1]=='O' || argv[argpos][1]=='o') && argv[argpos][2])
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
    else if (!stricmp(argv[argpos],"/NOCD")) do_cd=0;
    else if (argv[argpos][0] == '/' && (argv[argpos][1] == 'V' || argv[argpos][1] == 'v') && 
             argv[argpos][2] >= '0' && argv[argpos][2] <= '4' && !argv[argpos][3])
    {
      int v=argv[argpos][2]-'0';
      build.display_script=v>3;
      build.display_info=v>2;
      build.display_warnings=v>1;
      build.display_errors=v>0;
      g_display_errors=build.display_errors;
    }
    else if (!stricmp(argv[argpos],"/NOCONFIG")) g_noconfig=1;
    else if (!stricmp(argv[argpos],"/PAUSE")) g_dopause=1;
    else if (!stricmp(argv[argpos],"/LICENSE")) 
    {
      if (build.display_info) 
      {
        fprintf(g_output,"This software is provided 'as-is', without any express or implied warranty.  In\n"
             "no event will the authors be held liable for any damages arising from the use\n"
             "of this software.\n\n"
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
    else if (!stricmp(argv[argpos],"/CMDHELP"))
    {
      if (argpos < argc-1)
        build.print_help(argv[++argpos]);
      else 
        build.print_help(NULL);
      nousage++;
    }
    else if (!stricmp(argv[argpos],"/NOTIFYHWND")) build.notify_hwnd=(HWND)atol(argv[++argpos]);
    else if (!stricmp(argv[argpos],"/HDRINFO"))
    {
      if (build.display_info) 
      {
        fprintf(g_output,"Size of EXE header is %d bytes for zlib, %d bytes for bzip2.\n", zlib_exeheader_size,bzip2_exeheader_size);
        fprintf(g_output,"Size of info header is %d bytes.\n",sizeof(firstheader));
        fprintf(g_output,"Size of install header is %d bytes, uninstall header is %d bytes.\n",sizeof(header),sizeof(uninstall_header));
        fprintf(g_output,"Size of each section is %d bytes.\n",sizeof(section));
        fprintf(g_output,"Size of each page is %d bytes.\n",sizeof(page));
        fprintf(g_output,"Size of each instruction is %d bytes.\n",sizeof(entry));
        char *p=build.definedlist.defines.get();
        char *p2=build.definedlist.values.get();
        int x=0;
        fprintf(g_output,"\nDefined symbols: ");
        while (x < build.definedlist.defines.getlen())
        {
          if (x) fprintf(g_output,",");;
          fprintf(g_output,"%s",p+x);
          if (*p2) fprintf(g_output,"=%s",p2);

          x+=strlen(p+x)+1;
          p2+=strlen(p2)+1;
        }
        if (!x) fprintf(g_output,"none");
        fprintf(g_output,"\n");
        fflush(g_output);
      }
      nousage++;
    }
    else 
    {
      if (argv[argpos][0]=='/') break;
      files_processed++;
      if (!strcmp(argv[argpos],"-")) g_dopause=0;
      if (!g_noconfig)
      {
        g_noconfig=1;
        char exepath[1024];
        GetModuleFileName(NULL,exepath,sizeof(exepath)-1);
        //strncpy(exepath,argv[0],1023);
        exepath[1023]=0;
        char *p=exepath;
        while (*p) p++;
        while (p > exepath && *p != '\\') p=CharPrev(exepath,p);
        if (p>exepath) p++;
        strcpy(p,"nsisconf.nsh");
        FILE *cfg=fopen(exepath,"rt");
        if (cfg)
        {
          if (build.display_script) 
          {
            fprintf(g_output,"\n\nProcessing config: \n");
            fflush(g_output);
          }
          int lc=0;
          int ret=build.process_script(cfg,exepath,&lc);
          fclose(cfg);
          if (ret != PS_OK && ret != PS_EOF)
          {
            if (build.display_errors) 
            {
              fprintf(g_output,"Error in config on line %d -- aborting creation process\n",lc);
              fflush(g_output);
            }
            return 1;
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
            char dirbuf[1024],*p;
            GetFullPathName(sfile,sizeof(dirbuf),dirbuf,&p);
            p=dirbuf;
            while (*p) p++;
            while (p > dirbuf && *p != '\\') p=CharPrev(dirbuf,p);
            *p=0;
            if (dirbuf[0]) 
            {
              if (build.display_script) 
              {
                fprintf(g_output,"Changing directory to: \"%s\"\n",dirbuf);
                fflush(g_output);
              }
              if (!SetCurrentDirectory(dirbuf))
              {
                if (build.display_errors)
                {
                  fprintf(g_output,"Error changing directory to \"%s\"\n",dirbuf);
                  fflush(g_output);
                }
                return 1;
              }
            }
          }
        }

        #ifdef NSIS_CONFIG_PLUGIN_SUPPORT
        // Added by Ximon Eighteen 5th August 2002
        if (!plugins_processed) {
          build.build_plugin_table();
          plugins_processed=1;
        }
        #endif //NSIS_CONFIG_PLUGIN_SUPPORT

        if (build.display_script) 
        {
          build.notify(MAKENSIS_NOTIFY_SCRIPT,sfile);
          fprintf(g_output,"\n\nProcessing script file: \"%s\"\n",sfile);
          fflush(g_output);
        }
        int lc=0;
        int ret=build.process_script(fp,sfile,&lc);
        if (fp != stdin) fclose(fp);

        if (ret != PS_EOF && ret != PS_OK)
        {
          if (build.display_errors) 
          {
            fprintf(g_output,"Error in script \"%s\" on line %d -- aborting creation process\n",sfile,lc);
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
             "    /CMDHELP item prints out help for 'item', or lists all commands\n"
             "    /HDRINFO prints information about what options makensis was compiled with\n"
             "    /LICENSE prints the makensis software license\n");
      fprintf(g_output,
             "    /Vx verbosity where x is 4=all,3=no script,2=no info,1=no warnings,0=none\n"
             "    /Ofile specifies a text file to log compiler output (default is stdout)\n"
             "    /PAUSE pauses after execution\n"
             "    /NOCONFIG disables inclusion of <path to makensis.exe>\\nsisconf.nsh\n"
             "    /NOCD disabled the current directory change to that of the .nsi file\n"
             "    /Ddefine[=value] defines the symbol \"define\" for the script [to value]\n"
             "    /Xscriptcmd executes scriptcmd in script (i.e. \"/XOutFile poop.exe\")\n"
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
