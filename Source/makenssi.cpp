/*
 * makenssi.cpp
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
#include "utf.h"
#include "winchar.h" // assert(sizeof(WINWCHAR)...)

#include <nsis-version.h>
#define NSIS_COPYYEARS _T("1999-2021")

using namespace std;

NSISRT_DEFINEGLOBALS();
bool g_dopause=false;
NStreamEncoding g_outputenc;
#ifdef _WIN32
UINT g_wincon_orgoutcp;
#ifdef _UNICODE
WINSIO_OSDATA g_osdata_stdout, g_osdata_stderr;
#endif
#endif
const TCHAR *g_argv0=0;

static void dopause(void)
{
  if (!g_dopause) return;
  if (g_display_errors) _ftprintf(g_output,_T("MakeNSIS done - hit enter to close..."));
  fflush(stdout);
  int a;
  while ((a=_gettchar()) != _T('\r') && a != _T('\n') && a != 27/*esc*/);
}

void quit()
{
  if (g_display_errors) 
  {
    PrintColorFmtMsg_WARN(_T("\nNote: you may have one or two (large) stale temporary file(s) ")
         _T("left in your temporary directory (Generally this only happens on Windows 9x).\n"));
  }
  exit(1);
}

static void myatexit()
{
  dopause();
  ResetPrintColor();
  bool oneoutputstream = g_output == g_errout;
  if (g_output != stdout && g_output                    ) fclose(g_output);
  if (g_errout != stderr && g_errout && !oneoutputstream) fclose(g_errout);
  g_output = g_errout = 0;
#ifdef _WIN32
  SetConsoleOutputCP(g_wincon_orgoutcp);
#endif
}

static void sigint(int sig)
{
  if (g_display_errors) 
  {
    PrintColorFmtMsg_WARN(_T("\n\nAborting on Ctrl+C...\n"));
  }
  quit();
}

#ifdef _WIN32
static DWORD WINAPI sigint_event_msg_handler(LPVOID ThreadParam)
{
  using namespace MakensisAPI;
  HANDLE hEvent = 0;
  if (ThreadParam)
  {
    TCHAR eventnamebuf[100];
    wsprintf(eventnamebuf, SigintEventNameFmt, (HWND)ThreadParam);
    hEvent = OpenEvent(SYNCHRONIZE, FALSE, eventnamebuf);
  }
  if (!hEvent) hEvent = OpenEvent(SYNCHRONIZE, FALSE, SigintEventNameLegacy);

  if (hEvent)
  {
    if (WaitForSingleObject(hEvent, INFINITE) == WAIT_OBJECT_0)
      raise(SIGINT);
    CloseHandle(hEvent);
  }

  return 0;
}

static UINT_PTR QueryHost(HWND hHost, UINT_PTR wp, UINT_PTR lp=0, UINT_PTR def=0) { return hHost ? SendMessage(hHost, MakensisAPI::QUERYHOST, wp, lp) : def; }
#else //! _WIN32
static inline UINT_PTR QueryHost(HWND hHost, UINT_PTR wp, UINT_PTR lp=0, UINT_PTR def=0) { return def; }
#endif //~ _WIN32

static void init_signals(HWND notify_hwnd)
{
  atexit(myatexit);
  signal(SIGINT,sigint);

#ifdef _WIN32
  DWORD id;
  HANDLE hThread = CreateThread(NULL, 0, sigint_event_msg_handler, (LPVOID)notify_hwnd, 0, &id);
  if (hThread)
  {
    SetThreadPriority(hThread, THREAD_PRIORITY_HIGHEST);
    CloseHandle(hThread);
  }
#endif
}

static void print_logo()
{
  _ftprintf(g_output,_T("MakeNSIS %") NPRIs _T(" - Copyright ") NSIS_COPYYEARS _T(" Contributors\n")
         _T("See the file COPYING for license details.\n")
         _T("Credits can be found in the Users Manual.\n\n"), NSIS_VERSION);
  fflush(g_output);
}

static void print_license()
{
  _ftprintf(g_output,_T("Copyright (C) ") NSIS_COPYYEARS _T(" Nullsoft and Contributors\n\n")
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
         _T("  ")         _T("makensis [ option | script.nsi | - ] [...]\n")
         _T("\n")
         _T("Options:\n")
         _T("  ") OPT_STR _T("CMDHELP [item] prints out help for 'item', or lists all commands\n")
         _T("  ") OPT_STR _T("HDRINFO prints information about what options makensis was compiled with\n")
         _T("  ") OPT_STR _T("LICENSE prints the makensis software license\n")
         _T("  ") OPT_STR _T("VERSION prints the makensis version and exits\n")
         //_T("  ") OPT_STR _T("HELP this usage info\n")
#ifdef _WIN32
         _T("  ") OPT_STR _T("Px sets the compiler process priority, where x is 5=realtime,4=high,\n")
         _T("  ")         _T("  3=above normal,2=normal,1=below normal,0=idle\n")
#endif
         _T("  ") OPT_STR _T("Vx verbosity where x is 4=all,3=no script,2=no info,1=no warnings,0=none\n")
         _T("  ") OPT_STR _T("WX treat warnings as errors\n")
         _T("  ") OPT_STR _T("Ofile specifies a text file to log compiler output (default is stdout)\n")
         _T("  ") OPT_STR _T("PAUSE pauses after execution\n")
         _T("  ") OPT_STR _T("NOCONFIG disables inclusion of <path to makensis.exe>") PLATFORM_PATH_SEPARATOR_STR _T("nsisconf.nsh\n")
         _T("  ") OPT_STR _T("NOCD disables the current directory change to that of the .nsi file\n")
         _T("  ") OPT_STR _T("INPUTCHARSET <") TSTR_INPUTCHARSET _T(">\n")
#ifdef _WIN32
         _T("  ") OPT_STR _T("OUTPUTCHARSET <") TSTR_OUTPUTCHARSET _T(">\n")
#endif
         _T("  ") OPT_STR _T("[SAFE]PPO preprocess to stdout/file\n")
         _T("  ") OPT_STR _T("Ddefine[=value] defines the symbol \"define\" for the script [to value]\n")
         _T("  ") OPT_STR _T("Xscriptcmd executes scriptcmd in script (i.e. \"") OPT_STR _T("XOutFile inst.exe\")\n")
         _T("  ")         _T("  parameters are processed by order (") OPT_STR _T("Ddef ins.nsi != ins.nsi ") OPT_STR _T("Ddef)\n")
         _T("\n")
         _T("For script file name, you can use - to read from the standard input\n")
#ifdef _WIN32
         _T("You can also use - as an option character: -PAUSE as well as /PAUSE\n")
#endif
         _T("You can use a double-dash to end options processing: makensis -- -ins.nsi\n"));
  fflush(g_output);
}

static void print_stub_info(CEXEBuild& build)
{
  if (build.display_info)
  {
    _ftprintf(g_output,_T("Size of first header is %lu bytes.\n"),(unsigned long)sizeof(firstheader));
    _ftprintf(g_output,_T("Size of main header is %lu bytes.\n"),(unsigned long)build.get_header_size());
    _ftprintf(g_output,_T("Size of each section is %lu bytes.\n"),(unsigned long)sizeof(section));
    _ftprintf(g_output,_T("Size of each page is %lu bytes.\n"),(unsigned long)sizeof(page));
    _ftprintf(g_output,_T("Size of each instruction is %lu bytes.\n"),(unsigned long)sizeof(entry));
    int x=build.definedlist.getnum();
    _ftprintf(g_output,_T("\nDefined symbols: "));
    for (int i=0; i<x; i++)
    {
      _ftprintf(g_output,_T("%")NPRIs,build.definedlist.getname(i));
      TCHAR *p=build.definedlist.getvalue(i);
      if (*p) _ftprintf(g_output,_T("=%")NPRIs,p);
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
  NIStream strm;
  if (strm.OpenFileForReading(conf.c_str()))
  {
    build.INFO_MSG(_T("Processing config: %") NPRIs _T("\n"),conf.c_str());
    int ret=build.process_script(strm,conf.c_str());
    if (ret != PS_OK && ret != PS_EOF)
    {
      build.ERROR_MSG(_T("Error in config on line %d -- aborting creation process\n"),build.linecnt); 
      return 1;
    }
    build.SCRIPT_MSG(_T("\n")); // Extra newline to separate the config from the real script
  }
  return 0;
}

static int change_to_script_dir(CEXEBuild& build, tstring& script)
{
  tstring dir = get_dir_name(get_full_path(script));
  if (!dir.empty()) 
  {
    build.SCRIPT_MSG(_T("Changing directory to: \"%") NPRIs _T("\"\n"),dir.c_str());
    if (_tchdir(dir.c_str()))
    {
      build.ERROR_MSG(_T("Error changing directory to \"%") NPRIs _T("\"\n"),dir.c_str());
      return 1;
    }
    build.SCRIPT_MSG(_T("\n"));
  }
  return 0;
}

static inline bool HasReqParam(TCHAR**argv,int argi,int argc,bool silent=false)
{
  if (argi>=argc || !*argv[argi])
  {
    if (!silent) PrintColorFmtMsg_ERR(_T("Error: Missing required parameter!\n"));
    return false;
  }
  return true;
}

#ifdef NSIS_HPUX_ALLOW_UNALIGNED_DATA_ACCESS
extern "C" void allow_unaligned_data_access();
#endif

static inline int makensismain(int argc, TCHAR **argv)
{

#ifdef NSIS_HPUX_ALLOW_UNALIGNED_DATA_ACCESS
  allow_unaligned_data_access();
#endif
  assert(sizeof(UINT_PTR) == sizeof(void*));
  assert('a' + 25 == 'z' && '0' < 'A' && 'A' < 'a'); // ASCII, do you speak it?
  assert(sizeof(wchar_t) > 1 && sizeof(wchar_t) <= 4);
  assert(sizeof(WINWCHAR) == 2 && sizeof(WORD) == 2);
  assert(sizeof(WINWCHAR) == sizeof(WCHAR)); // Not really required but if WCHAR changes we need to know

  g_argv0=argv[0];

  if (!NSISRT_Initialize())
  {
    _ftprintf(stderr,_T("NSISRT_Initialize failed!\n"));
    return 1;
  }

  HWND hostnotifyhandle=0;
  const TCHAR*stdoutredirname=0;
  NStreamEncoding inputenc, &outputenc = g_outputenc;
  int argpos=0;
  bool do_cd=true, noconfig=false;
  bool no_logo=true, warnaserror=false;
  bool initialparsefail=false, in_files=false;
  bool oneoutputstream=false;
  signed char pponly=0;
#ifdef _WIN32
  signed char outputbom=1;

#ifdef DEBUG
  assert(CP_ACP == outputenc.GetCodepage()); // Required by CEXEBuild::notify() char* legacy handling.
#endif
#endif //~ _WIN32

  // Some parameters have to be parsed early so we can initialize stdout and the "host API".
  while (++argpos < argc && !initialparsefail)
  {
    if (!IS_OPT(argv[argpos])) break; // must be a filename, stop parsing
    if (!_tcscmp(argv[argpos], _T("--"))) break; // stop parsing
    if (_T('-') == argv[argpos][0] && !argv[argpos][1]) continue; // stdin

    const TCHAR *swname = &argv[argpos][1];
    if (!_tcsicmp(swname,_T("INPUTCHARSET")) || !_tcsicmp(swname,_T("ICS"))) ++argpos; // Skip
    else if (!_tcsicmp(swname,_T("VERSION"))) argc=0;
    else if (!_tcsicmp(swname,_T("NOTIFYHWND")))
    {
      initialparsefail=!HasReqParam(argv,++argpos,argc,true);
      if (initialparsefail) break;
      hostnotifyhandle=(HWND)(INT_PTR) _ttol(argv[argpos]); // MSDN says we should sign extend HWNDs: msdn.microsoft.com/en-us/library/aa384203
#ifdef _WIN32
      if (!IsWindow(hostnotifyhandle)) hostnotifyhandle=0;
#endif
    }
    else if (!_tcsicmp(swname,_T("OUTPUTCHARSET")) || !_tcsicmp(swname,_T("OCS")))
    {
      initialparsefail=!HasReqParam(argv,++argpos,argc,true);
      if (initialparsefail) break;
#ifdef _WIN32
      bool bom;
      WORD cp=GetEncodingFromString(argv[argpos],bom);
      if (NStreamEncoding::UNKNOWN == cp)
      {
        ++initialparsefail;
      }
      else
      {
        outputbom=bom ? 1 : -1;
        outputenc.SetCodepage(cp);
      }
#else
      outputenc.SetCodepage(NStreamEncoding::UNKNOWN);
#endif
    }
#ifdef _WIN32
    else if (!_tcsicmp(swname,_T("RAW")))
    {
      // Emulate the scratchpaper.com fork and its /RAW switch.
      // NOTE: Unlike the fork, we print \r\n and not just \n.
      outputbom=0;
      outputenc.SetCodepage(NStreamEncoding::UTF16LE);
    }
#endif
    else if (!_tcsicmp(swname,_T("PPO")) || !_tcsicmp(swname,_T("SafePPO")))
    {
      pponly = S7IsChEqualI('s',swname[0]) ? 1 : -1;
    }
    else if (S7IsChEqualI('v',swname[0]) && swname[1] && !swname[2])
    {
      no_logo=swname[1] >= _T('0') && swname[1] <= _T('2');
    }
    else if (!_tcsicmp(swname,_T("WX")))
    {
      warnaserror = true;
    }
    // This must be parsed last because it will eat other switches
    else if (S7IsChEqualI('o',swname[0]) && swname[1]) stdoutredirname=swname+1;
  }


#ifdef _WIN32
  g_wincon_orgoutcp = GetConsoleOutputCP();
#endif

  init_signals(hostnotifyhandle);


  FILE*stdoutredir=stdout;
  if (stdoutredirname) stdoutredir=my_fopen(stdoutredirname,"w");
  g_output=stdoutredir;
  if (!g_output)
    g_output=stdout; // We could not open stdoutredirname, fall back to stdout
  else if (stdoutredirname)
    oneoutputstream=true; // -O used, put all output in the same file
  if (oneoutputstream || !(1 & QueryHost(hostnotifyhandle,MakensisAPI::QH_ENABLESTDERR,0,1)))
    g_errout=g_output;
#if defined(_WIN32) && defined(_UNICODE)
  if (hostnotifyhandle)
  {
    // The host can override the output format if they want to
    LPARAM lp=MAKELONG(outputenc.GetCodepage(),outputbom);
    LRESULT mr=QueryHost(hostnotifyhandle,MakensisAPI::QH_OUTPUTCHARSET,lp);
    if (mr) outputenc.SetCodepage((WORD)(--mr)), outputbom = -1;
  }

  if ((                        !WinStdIO_OStreamInit(g_osdata_stdout,g_output,outputenc.GetCodepage(),outputbom))
   || (g_errout != g_output && !WinStdIO_OStreamInit(g_osdata_stderr,g_errout,outputenc.GetCodepage(),outputbom)))
  {
    assert(!"StdIO init failed");
    return 1;
  }
#endif
  // g_output is now initialized and Print*/_[f]tprintf can be used
  if (!stdoutredir) PrintColorFmtMsg_WARN(_T("Error opening output log for writing! Using stdout.\n"));

  unsigned int nousage=0, performed=0;
  unsigned int files_processed=0;
  unsigned int cmds_processed=0;

  CEXEBuild build(pponly, warnaserror);
  try
  {
    build.initialize(argv[0]);
  }
  catch (exception& err)
  {
    PrintColorFmtMsg_ERR(_T("Error initalizing CEXEBuild: %") NPRIs _T("\n"), CtoTStrParam(err.what()));
    return 1;
  }

#ifdef _WIN32
  build.notify_hwnd=hostnotifyhandle;
#else
  const TCHAR*const badnonwinswitchfmt=OPT_STR _T("%") NPRIs _T(" is disabled for non Win32 platforms.");
  if (hostnotifyhandle)
    build.warning(DW_CMDLINE_UNSUPP_NIX,badnonwinswitchfmt,_T("NOTIFYHWND"));
  if (NStreamEncoding::UNKNOWN==outputenc.GetCodepage())
    build.warning(DW_CMDLINE_UNSUPP_NIX,badnonwinswitchfmt,_T("OUTPUTCHARSET"));
#endif // ~_WIN32

  if (!argc)
  {
    _ftprintf(g_output,NSIS_VERSION);
    fflush(g_output);
    return 0;
  }
  if (!no_logo && !pponly) print_logo();


  argpos=initialparsefail ? argc : 1;
  while (argpos < argc)
  {
    if (!_tcscmp(argv[argpos], _T("--")))
      in_files=1;
    else if (IS_OPT(argv[argpos]) && _tcscmp(argv[argpos], _T("-")) && !in_files)
    {
      const TCHAR* const swname = &argv[argpos][1];
      if (!_tcsicmp(swname,_T("PPO")) || !_tcsicmp(swname,_T("SafePPO"))) {} // Already parsed
      else if (!_tcsicmp(swname,_T("WX"))) {} // Already parsed
      else if (!_tcsicmp(swname,_T("NOCD"))) do_cd=false;
      else if (!_tcsicmp(swname,_T("NOCONFIG"))) noconfig=true;
      else if (!_tcsicmp(swname,_T("PAUSE"))) g_dopause=true;
      else if (!_tcsicmp(swname,_T("HELP")))
      {
        print_usage();
        performed |= ++nousage;
      }
      else if (!_tcsicmp(swname,_T("LICENSE"))) 
      {
        if (build.display_info) print_license();
        performed |= ++nousage;
      }
      else if (!_tcsicmp(swname,_T("CMDHELP")))
      {
        if (build.print_cmdhelp(argpos < argc-1 ? argv[++argpos] : NULL, true))
          performed |= ++nousage;
      }
      else if (!_tcsicmp(swname,_T("HDRINFO")))
      {
        print_stub_info(build);
        performed |= ++nousage;
      }
      else if (!_tcsicmp(swname,_T("INPUTCHARSET")) || !_tcsicmp(swname,_T("ICS")))
      {
        if (!HasReqParam(argv, ++argpos, argc)) break;
        WORD cp = GetEncodingFromString(argv[argpos]);
        if (NStreamEncoding::UNKNOWN == cp)
        {
          if (_tcsicmp(argv[argpos], _T("AUTO")))
            build.warning(DW_CMDLINE_BAD_INPUTENC, OPT_STR _T("INPUTCHARSET: Ignoring invalid charset %") NPRIs , argv[argpos]);
          cp = NStreamEncoding::AUTO;
        }
        inputenc.SafeSetCodepage(cp);
      }
      else if (S7IsChEqualI('v',*swname) && 
               argv[argpos][2] >= _T('0') && argv[argpos][2] <= _T('4') && !argv[argpos][3])
      {
        int v=argv[argpos][2]-_T('0');
        build.set_verbosity(v);
      }
      else if (S7IsChEqualI('p',*swname) &&
               argv[argpos][2] >= _T('0') && argv[argpos][2] <= _T('5') && !argv[argpos][3])
      {
#ifdef _WIN32
        // priority setting added 01-2007 by Comm@nder21
        int p=argv[argpos][2]-_T('0');
        HANDLE hProc = GetCurrentProcess();
        struct
        {
          DWORD priority, fallback;
        } static const classes[] = {
          {IDLE_PRIORITY_CLASS,         IDLE_PRIORITY_CLASS},
          {BELOW_NORMAL_PRIORITY_CLASS, IDLE_PRIORITY_CLASS},
          {NORMAL_PRIORITY_CLASS,       NORMAL_PRIORITY_CLASS},
          {ABOVE_NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS},
          {HIGH_PRIORITY_CLASS,         HIGH_PRIORITY_CLASS},
          {REALTIME_PRIORITY_CLASS,     REALTIME_PRIORITY_CLASS}
        };
        if (!SetPriorityClass(hProc, classes[p].priority))
          SetPriorityClass(hProc, classes[p].fallback);
        if (p == 5) build.warning(DW_CMDLINE_HIGHPRIORITY,_T("makensis is running in REALTIME priority mode!"));
#else
        build.warning(DW_CMDLINE_UNSUPP_NIX,badnonwinswitchfmt,_T("Px"));
#endif
      }
      // Already parsed these (must adjust argpos)
      else if (!_tcsicmp(swname,_T("NOTIFYHWND"))) ++argpos;
      else if (!_tcsicmp(swname,_T("OUTPUTCHARSET")) || !_tcsicmp(swname,_T("OCS"))) ++argpos;
      // These must be parsed last because they will eat other switches
      else if (S7IsChEqualI('d',swname[0]) && swname[1])
      {
        TCHAR *p=argv[argpos]+2;
        TCHAR *s=_tcsdup(p),*v;
        build.INFO_MSG(_T("Command line defined: \"%") NPRIs _T("\"\n"),p);
        v=_tcsstr(s,_T("="));
        if (v) *v++=0;
        build.define(s,v?v:_T(""));
        free(s);
      }
      else if (S7IsChEqualI('x',swname[0]) && swname[1])
      {
        if (build.process_oneline(swname+1,build.get_commandlinecode_filename(),argpos+1) != PS_OK)
        {
          return 1;
        }
        cmds_processed++;
      }
      // Already parsed these ("VERSION" never gets this far)
#ifdef _WIN32
      else if (!_tcsicmp(swname,_T("RAW"))) {}
#endif
      else if (S7IsChEqualI('o',swname[0]) && swname[1]) {} 
      else
        break;
    }
    else
    {
      files_processed++;
      if (!_tcscmp(argv[argpos],_T("-")) && !in_files)
        g_dopause=false;
      if (!noconfig)
      {
        noconfig=true;
        tstring main_conf;
        TCHAR* env_var = _tgetenv(_T("NSISCONFDIR"));
        if (env_var == NULL)
#ifndef NSIS_CONFIG_CONST_DATA_PATH
          main_conf = get_dir_name(get_executable_dir(argv[0]));
#else
          main_conf = _T(PREFIX_CONF);
#endif
        else
          main_conf = env_var;
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
        tstring nsifile;
        NIStream strm;
        if (!_tcscmp(argv[argpos],_T("-")) && !in_files)
        {
          strm.OpenStdIn(inputenc);
          nsifile = _T("<stdin>");
        }
        else
        {
          nsifile = argv[argpos];
          if (!strm.OpenFileForReading(nsifile.c_str(),inputenc))
          {
            nsifile += _T(".nsi");
            if (!strm.OpenFileForReading(nsifile.c_str(),inputenc))
            {
              nsifile = argv[argpos];
              build.ERROR_MSG(_T("Can't open script \"%") NPRIs _T("\"\n"),nsifile.c_str());
              return 1;
            }
          }
          if (do_cd)
          {
            if (change_to_script_dir(build, nsifile))
              return 1;
          }
          build.set_default_output_filename(remove_file_extension(nsifile)+_T(".exe"));
        }

        build.notify(MakensisAPI::NOTIFY_SCRIPT,nsifile.c_str());
        TCHAR bufcpdisp[20];
        strm.StreamEncoding().GetCPDisplayName(bufcpdisp);
        build.INFO_MSG(_T("Processing script file: \"%") NPRIs _T("\" (%") NPRIs _T(")\n"),nsifile.c_str(),bufcpdisp);
        int ret=build.process_script(strm,nsifile.c_str());

        if (ret != PS_EOF && ret != PS_OK)
        {
          build.ERROR_MSG(_T("Error in script \"%") NPRIs _T("\" on line %d -- aborting creation process\n"),nsifile.c_str(),build.linecnt);
          return 1;
        }
      }
    }
    argpos++;
  }

  bool parsed_all_params = argpos >= argc, processed_any = files_processed || cmds_processed;
  if (!parsed_all_params || !processed_any)
  {
    if (build.display_errors && !nousage)
    {
      print_usage();
    }
    return performed && parsed_all_params ? 0 : 1;
  }

  if (build.preprocessonly) return 0;

  if (build.display_info) 
  {
    _ftprintf(g_output,_T("\nProcessed "));
    if (files_processed) _ftprintf(g_output,_T("%d file%") NPRIs _T(", "),files_processed,files_processed==1?_T(""):_T("s"));
    if (cmds_processed) _ftprintf(g_output,_T("%d command line command%") NPRIs _T(", "),cmds_processed,cmds_processed==1?_T(""):_T("s"));
    _ftprintf(g_output,_T("writing output (%") NPRIs _T("):\n"),build.get_target_suffix());
    fflush(g_output);
  }
  
  if (build.write_output())
  { 
    build.ERROR_MSG(_T("Error - aborting creation process\n"));
    return 1;
  }
  return 0;
}


#ifndef NDEBUG
#  ifdef _MSC_VER
#    include <crtdbg.h>
#  endif
#endif

NSIS_ENTRYPOINT_TMAIN
int _tmain(int argc, TCHAR **argv)
{
#ifndef NDEBUG
#ifdef _MSC_VER
  const int dbgchkthorough = true, checkeveryallocfree = dbgchkthorough ? _CRTDBG_CHECK_ALWAYS_DF : 0;
  _CrtSetDbgFlag(_CrtSetDbgFlag(_CRTDBG_REPORT_FLAG) | _CRTDBG_ALLOC_MEM_DF | checkeveryallocfree | _CRTDBG_LEAK_CHECK_DF);
  _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE), _CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDOUT);
  _CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_FILE), _CrtSetReportFile(_CRT_ERROR, _CRTDBG_FILE_STDOUT);
  //_CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE), _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDOUT);
#endif
#endif
  int retval = makensismain(argc,argv);
#ifndef NDEBUG
#ifdef _MSC_VER
  assert(_CrtCheckMemory());
#endif
#endif
  return retval;
}


#if !defined(_WIN32) && defined(_UNICODE)
#include <errno.h>
int main(int argc, char **argv)
{
  errno = ENOMEM;
  int wargc = 0;
  wchar_t term[1], *p, **wargv = (wchar_t **) malloc((argc+1) * sizeof(void*));
  if (wargv) 
  {
    for ( ; wargc < argc; ++wargc )
      if ((p = NSISRT_mbtowc(argv[wargc]))) wargv[wargc] = p; else break;
  }
  if (wargc == argc)
    *term = L'\0', wargv[wargc] = term, errno = _tmain(wargc,wargv);
  else
    wprintf(L"FATAL: main argv conversion failed!\n");
#ifndef NDEBUG // Normally we just leak
  if (wargv) for ( int i = 0; i < wargc; ++i ) NSISRT_free(wargv[i]);
  free(wargv);
#endif
  return errno;
}
#endif

