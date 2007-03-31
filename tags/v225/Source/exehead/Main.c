/*
 * main.c: executable header main code
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "../Platform.h"
#include <shlobj.h>
#include <shellapi.h>
#include "resource.h"
#include "util.h"
#include "fileform.h"
#include "state.h"
#include "ui.h"
#include "lang.h"
#include "state.h"
#include "exec.h"

#if !defined(NSIS_CONFIG_VISIBLE_SUPPORT) && !defined(NSIS_CONFIG_SILENT_SUPPORT)
#error One of NSIS_CONFIG_SILENT_SUPPORT or NSIS_CONFIG_VISIBLE_SUPPORT must be defined.
#endif
#ifdef NSIS_COMPRESS_WHOLE
extern HANDLE dbd_hFile;
#endif

char g_caption[NSIS_MAX_STRLEN*2];
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
HWND g_hwnd;
HANDLE g_hInstance;
#endif

void NSISCALL CleanUp();

char *ValidateTempDir()
{
  validate_filename(state_temp_dir);
  if (!validpathspec(state_temp_dir))
    return NULL;
  addtrailingslash(state_temp_dir);
  CreateDirectory(state_temp_dir, NULL);
  // state_language is used as a temp var here
  return my_GetTempFileName(state_language, state_temp_dir);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInst,LPSTR lpszCmdParam, int nCmdShow)
{
  int ret = 0;
  const char *m_Err = _LANG_ERRORWRITINGTEMP;

  int cl_flags = 0;

  char *realcmds;
  char seekchar=' ';
  char *cmdline;

  InitCommonControls();

#if defined(NSIS_SUPPORT_ACTIVEXREG) || defined(NSIS_SUPPORT_CREATESHORTCUT)
  {
    extern HRESULT g_hres;
    g_hres=OleInitialize(NULL);
  }
#endif

  {
    // workaround for bug #1008632
    // http://sourceforge.net/tracker/index.php?func=detail&aid=1008632&group_id=22049&atid=373085
    //
    // without this, SHGetSpecialFolderLocation doesn't always recognize
    // some special folders, like the desktop folder for all users, on
    // Windows 9x. unlike SHGetSpecialFolderPath, which is not available
    // on all versions of Windows, SHGetSpecialFolderLocation doesn't try
    // too hard to make sure the caller gets what he asked for. so we give
    // it a little push in the right direction by doing part of the work
    // for it.
    //
    // part of what SHGetFileInfo does, is to convert a path into an idl.
    // to do this conversion, it first needs to initialize the list of 
    // special idls, which are exactly the idls we use to get the paths
    // of special folders (CSIDL_*).

    SHFILEINFO shfi;
    SHGetFileInfo("", 0, &shfi, sizeof(SHFILEINFO), 0);
  }

  mystrcpy(g_caption,_LANG_GENERIC_ERROR);

  mystrcpy(state_command_line, GetCommandLine());

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  g_hInstance = GetModuleHandle(NULL);
#endif//NSIS_CONFIG_VISIBLE_SUPPORT

  cmdline = state_command_line;
  if (*cmdline == '\"') seekchar = *cmdline++;

  cmdline=findchar(cmdline, seekchar);
  cmdline=CharNext(cmdline);
  realcmds=cmdline;

  while (*cmdline)
  {
    // skip over any spaces
    while (*cmdline == ' ') cmdline++;
    
    // get char we should look for to get the next parm
    seekchar = ' ';
    if (cmdline[0] == '\"')
    {
      cmdline++;
      seekchar = '\"';
    }

    // is it a switch?
    if (cmdline[0] == '/')
    {
      cmdline++;

// this only works with spaces because they have just one bit on
#define END_OF_ARG(c) (((c)|' ')==' ')

#if defined(NSIS_CONFIG_VISIBLE_SUPPORT) && defined(NSIS_CONFIG_SILENT_SUPPORT)
      if (cmdline[0] == 'S' && END_OF_ARG(cmdline[1]))
        cl_flags |= FH_FLAGS_SILENT;
#endif//NSIS_CONFIG_SILENT_SUPPORT && NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_CRC_SUPPORT
      if (*(LPDWORD)cmdline == CHAR4_TO_DWORD('N','C','R','C') && END_OF_ARG(cmdline[4]))
        cl_flags |= FH_FLAGS_NO_CRC;
#endif//NSIS_CONFIG_CRC_SUPPORT

      if (*(LPDWORD)(cmdline-2) == CHAR4_TO_DWORD(' ', '/', 'D','='))
      {
        *(LPDWORD)(cmdline-2)=0; // keep this from being passed to uninstaller if necessary
        mystrcpy(state_install_directory,cmdline+2);
        break; // /D= must always be last
      }
    }

    // skip over our parm
    cmdline = findchar(cmdline, seekchar);
    // skip the quote
    if (*cmdline == '\"')
      cmdline++;
  }

  GetTempPath(NSIS_MAX_STRLEN, state_temp_dir);
  if (!ValidateTempDir())
  {
    GetWindowsDirectory(state_temp_dir, NSIS_MAX_STRLEN - 5); // leave space for \Temp
    mystrcat(state_temp_dir, "\\Temp");
    if (!ValidateTempDir())
    {
      goto end;
    }
  }
  DeleteFile(state_language);

  m_Err = loadHeaders(cl_flags);
  if (m_Err) goto end;

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (g_is_uninstaller)
  {
    char *p = findchar(state_command_line, 0);

    // state_command_line has state_install_directory right after it in memory, so reading
    // a bit over state_command_line won't do any harm
    while (p >= state_command_line && *(LPDWORD)p != CHAR4_TO_DWORD(' ', '_', '?', '=')) p--;

    m_Err = _LANG_UNINSTINITERROR;

    if (p >= state_command_line)
    {
      *p=0; // terminate before "_?="
      p+=4; // skip over " _?="
      if (is_valid_instpath(p))
      {
        mystrcpy(state_install_directory, p);
        mystrcpy(state_output_directory, p);
        m_Err = 0;
      }
      else
      {
        goto end;
      }
    }
    else
    {
      int x;
      char s[] = "Au_.exe";

      mystrcat(state_temp_dir,"~nsu.tmp");
      CreateDirectory(state_temp_dir,NULL);

      if (!state_install_directory[0])
        mystrcpy(state_install_directory,state_exe_directory);

      mystrcpy(g_usrvars[0], realcmds);
      mystrcpy(g_usrvars[1], s);

      for (x = 0; x < 26; x ++)
      {
        static char buf2[NSIS_MAX_STRLEN];
        static char ibuf[NSIS_MAX_STRLEN];

        GetNSISString(buf2,g_header->str_uninstchild); // $TEMP\$1

        DeleteFile(buf2); // clean up after all the other ones if they are there

        if (m_Err) // not done yet
        {
          // get current name
          int l=GetModuleFileName(NULL,ibuf,sizeof(ibuf));
          // check if it is ?u_.exe - if so, fuck it
          if (!lstrcmpi(ibuf+l-(sizeof(s)-2),s+1)) break;

          // copy file
          if (CopyFile(ibuf,buf2,TRUE))
          {
            HANDLE hProc;
#ifdef NSIS_SUPPORT_MOVEONREBOOT
            MoveFileOnReboot(buf2,NULL);
            MoveFileOnReboot(state_temp_dir,NULL);
#endif
            GetNSISString(buf2,g_header->str_uninstcmd); // '"$TEMP\$1" $0 _?=$INSTDIR\'
            hProc=myCreateProcess(buf2,state_temp_dir);
            if (hProc)
            {
              CloseHandle(hProc);
              // success
              m_Err = 0;
            }
          }
        }
        g_usrvars[1][0]++;
      }
      goto end;
    }
  }
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT

  g_exec_flags.errlvl = -1;
  ret = ui_doinstall();

#ifdef NSIS_CONFIG_LOG
#if !defined(NSIS_CONFIG_LOG_ODS) && !defined(NSIS_CONFIG_LOG_STDOUT)
  log_write(1);
#endif//!NSIS_CONFIG_LOG_ODS && !NSIS_CONFIG_LOG_STDOUT
#endif//NSIS_CONFIG_LOG
end:

  CleanUp();

#if defined(NSIS_SUPPORT_ACTIVEXREG) || defined(NSIS_SUPPORT_CREATESHORTCUT)
  OleUninitialize();
#endif

  if (m_Err)
  {
    my_MessageBox(m_Err, MB_OK | MB_ICONSTOP | (IDOK << 21));
    ExitProcess(2);
    return 0;
  }

#ifdef NSIS_SUPPORT_REBOOT
  if (g_exec_flags.reboot_called)
  {
    BOOL (WINAPI *OPT)(HANDLE, DWORD,PHANDLE);
    BOOL (WINAPI *LPV)(LPCTSTR,LPCTSTR,PLUID);
    BOOL (WINAPI *ATP)(HANDLE,BOOL,PTOKEN_PRIVILEGES,DWORD,PTOKEN_PRIVILEGES,PDWORD);
    OPT=myGetProcAddress("ADVAPI32.dll","OpenProcessToken");
    LPV=myGetProcAddress("ADVAPI32.dll","LookupPrivilegeValueA");
    ATP=myGetProcAddress("ADVAPI32.dll","AdjustTokenPrivileges");
    if (OPT && LPV && ATP)
    {
      HANDLE hToken;
      TOKEN_PRIVILEGES tkp;
      if (OPT(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken))
      {
        LPV(NULL, SE_SHUTDOWN_NAME, &tkp.Privileges[0].Luid);
        tkp.PrivilegeCount = 1;
        tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
        ATP(hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES)NULL, 0);
      }
    }

    if (!ExitWindowsEx(EWX_REBOOT,0))
      ExecuteCallbackFunction(CB_ONREBOOTFAILED);
  }
#endif//NSIS_SUPPORT_REBOOT

  if (g_exec_flags.errlvl != -1)
    ret = g_exec_flags.errlvl;

  ExitProcess(ret);
  return 0;
}

void NSISCALL CleanUp()
{
  if (g_db_hFile != INVALID_HANDLE_VALUE)
  {
    CloseHandle(g_db_hFile);
    g_db_hFile = INVALID_HANDLE_VALUE;
  }
#ifdef NSIS_COMPRESS_WHOLE
  if (dbd_hFile != INVALID_HANDLE_VALUE)
  {
    CloseHandle(dbd_hFile);
    dbd_hFile = INVALID_HANDLE_VALUE;
  }
#endif
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  // Clean up after plug-ins
  myDelete(state_plugins_dir, DEL_DIR | DEL_RECURSE | DEL_REBOOT);
#endif // NSIS_CONFIG_PLUGIN_SUPPORT
}
