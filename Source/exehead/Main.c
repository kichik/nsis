/* 

  Nullsoft Scriptable Install System (NSIS)
  main.c - executable header main code

  Copyright (C) 1999-2005 Nullsoft, Inc.
  
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

#include "../Platform.h"
#include <shlobj.h>
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
  // state_command_line is used as a temp var here
  return my_GetTempFileName(state_command_line, state_temp_dir);
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

  mystrcpy(g_caption,_LANG_GENERIC_ERROR);

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
  DeleteFile(state_command_line);

  lstrcpyn(state_command_line, GetCommandLine(), NSIS_MAX_STRLEN);

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
        cmdline[-2]=0; // keep this from being passed to uninstaller if necessary
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

      mystrcat(state_temp_dir,"~nsu.tmp\\");
      CreateDirectory(state_temp_dir,NULL);

      for (x = 0; x < 26; x ++)
      {
        static char s[]="Au_.exe";
        static char buf2[NSIS_MAX_STRLEN*2];
        static char ibuf[NSIS_MAX_STRLEN];

        *(LPWORD)buf2=CHAR2_TO_WORD('\"',0);
        mystrcat(buf2,state_temp_dir);
        mystrcat(buf2,s);

        DeleteFile(buf2+1); // clean up after all the other ones if they are there

        if (m_Err) // not done yet
        {
          // get current name
          int l=GetModuleFileName(g_hInstance,ibuf,sizeof(ibuf));
          // check if it is ?Au_.exe - if so, fuck it
          if (!lstrcmpi(ibuf+l-(sizeof(s)-2),s+1)) break;

          // copy file
          if (CopyFile(ibuf,buf2+1,FALSE))
          {
            HANDLE hProc;
#ifdef NSIS_SUPPORT_MOVEONREBOOT
            MoveFileOnReboot(buf2+1,NULL);
#endif
            if (state_install_directory[0]) mystrcpy(ibuf,state_install_directory);
            else trimslashtoend(ibuf);
            mystrcat(buf2,"\" ");
            mystrcat(buf2,realcmds);
            mystrcat(buf2," _?=");
            mystrcat(buf2,ibuf);
            // add a trailing backslash to make sure is_valid_instpath will not fail when it shouldn't
            addtrailingslash(buf2);
            hProc=myCreateProcess(buf2,state_temp_dir);
            if (hProc)
            {
              CloseHandle(hProc);
              // success
              m_Err = 0;
            }
          }
        }
        s[0]++;
      }
      goto end;
    }
  }
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT

  g_exec_flags.errlvl = -1;
  ret = ui_doinstall();

#ifdef NSIS_CONFIG_LOG
#ifndef NSIS_CONFIG_LOG_ODS
  log_write(1);
#endif//!NSIS_CONFIG_LOG_ODS
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
