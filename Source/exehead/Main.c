/*
  Nullsoft "SuperPimp" Installation System - main.c - executable header main code

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
#include <commctrl.h>
#include <shlobj.h>
#include "resource.h"
#include "util.h"
#include "fileform.h"
#include "state.h"
#include "ui.h"
#include "lang.h"
#include "state.h"

#if !defined(NSIS_CONFIG_VISIBLE_SUPPORT) && !defined(NSIS_CONFIG_SILENT_SUPPORT)
#error One of NSIS_CONFIG_SILENT_SUPPORT or NSIS_CONFIG_VISIBLE_SUPPORT must be defined.
#endif
#ifdef NSIS_COMPRESS_WHOLE
extern HANDLE dbd_hFile;
#endif

char g_caption[NSIS_MAX_STRLEN*2];
int g_filehdrsize;
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
HWND g_hwnd;
HANDLE g_hInstance;
#endif

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInst,LPSTR lpszCmdParam, int nCmdShow)
{
  int ret;
  const char *m_Err = 0;

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

  GetTempPath(sizeof(state_temp_dir), state_temp_dir);
  validate_filename(state_temp_dir);
  CreateDirectory(state_temp_dir, NULL);

  // g_caption is used as a temp var here
  if (!my_GetTempFileName(g_caption, state_temp_dir))
  {
    GetWindowsDirectory(state_temp_dir, sizeof(state_temp_dir));
    lstrcat(state_temp_dir, "\\Temp");
    validate_filename(state_temp_dir);
    CreateDirectory(state_temp_dir, NULL);
    if (!my_GetTempFileName(g_caption, state_temp_dir))
    {
      m_Err = _LANG_ERRORWRITINGTEMP;
      goto end;
    }
  }
  DeleteFile(g_caption);

  mystrcpy(g_caption,_LANG_GENERIC_ERROR);

  lstrcpyn(state_command_line, GetCommandLine(), NSIS_MAX_STRLEN);

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  g_hInstance = GetModuleHandle(NULL);
#endif//NSIS_CONFIG_VISIBLE_SUPPORT

  cmdline = state_command_line;
  if (*cmdline == '\"') seekchar = *cmdline++;

  while (*cmdline && *cmdline != seekchar) cmdline=CharNext(cmdline);
  cmdline=CharNext(cmdline);
  realcmds=cmdline;

  for (;;)
  {
    // skip over any spaces
    while (*cmdline == ' ') cmdline=CharNext(cmdline);
    if (cmdline[0] != '/') break;
    cmdline++;

#define END_OF_ARG(c) (((c)|' ')==' ')

#if defined(NSIS_CONFIG_VISIBLE_SUPPORT) && defined(NSIS_CONFIG_SILENT_SUPPORT)
    if (cmdline[0] == 'S' && END_OF_ARG(cmdline[1]))
      cl_flags |= FH_FLAGS_SILENT;
#endif//NSIS_CONFIG_SILENT_SUPPORT && NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_CRC_SUPPORT
    if (*(DWORD*)cmdline == CHAR4_TO_DWORD('N','C','R','C') && END_OF_ARG(cmdline[4]))
      cl_flags |= FH_FLAGS_NO_CRC;
#endif//NSIS_CONFIG_CRC_SUPPORT

    if (*(WORD*)cmdline == CHAR2_TO_WORD('D','='))
    {
      cmdline[-2]=0; // keep this from being passed to uninstaller if necessary
      mystrcpy(state_install_directory,cmdline+2);
      cmdline=""; // prevent further processing of cmdline
      break; // not necessary, but for some reason makes smaller exe :)
    }

    // skip over our parm
    while (!END_OF_ARG(*cmdline)) cmdline=CharNext(cmdline);
  }

  m_Err = loadHeaders(cl_flags);
  if (m_Err) goto end;

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (g_is_uninstaller)
  {
    char *p=cmdline;
    while (*p) p++;

    while (p >= cmdline && (p[0] != '_' || p[1] != '?' || p[2] != '=')) p--;

    m_Err = _LANG_UNINSTINITERROR;

    if (p >= cmdline)
    {
      *(p-1)=0; // terminate before the " _?="
      p+=3; // skip over _?=
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

      for (x = 0; x < 26; x ++)
      {
        // File name need slash before because temp dir was changed by validate_filename
        static char s[]="\\A~NSISu_.exe";
        static char buf2[NSIS_MAX_STRLEN*2];
        static char ibuf[NSIS_MAX_STRLEN];

        buf2[0]='\"';
        mystrcpy(buf2+1,state_temp_dir);
        lstrcat(buf2,s);

        DeleteFile(buf2+1); // clean up after all the other ones if they are there

        if (m_Err) // not done yet
        {
          // get current name
          int l=GetModuleFileName(g_hInstance,ibuf,sizeof(ibuf));
          // check if it is ?~NSISu_.exe - if so, fuck it
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
            lstrcat(buf2,"\" ");
            lstrcat(buf2,realcmds);
            lstrcat(buf2," _?=");
            lstrcat(buf2,ibuf);
            // add a trailing backslash to make sure is_valid_instpath will not fail when it shouldn't
            lstrcat(buf2,"\\");
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

  ret = ui_doinstall();

#ifdef NSIS_CONFIG_LOG
  log_write(1);
#endif//NSIS_CONFIG_LOG
end:

  if (g_db_hFile != INVALID_HANDLE_VALUE) CloseHandle(g_db_hFile);
#ifdef NSIS_COMPRESS_WHOLE
  if (dbd_hFile != INVALID_HANDLE_VALUE) CloseHandle(dbd_hFile);
#endif
  if (m_Err) my_MessageBox(m_Err, MB_OK | MB_ICONSTOP);

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  // Clean up after plug-ins
  if (state_plugins_dir[0]) doRMDir(state_plugins_dir, 1);
#endif // NSIS_CONFIG_PLUGIN_SUPPORT
  if (g_hIcon) DeleteObject(g_hIcon);

#if defined(NSIS_SUPPORT_ACTIVEXREG) || defined(NSIS_SUPPORT_CREATESHORTCUT)
  OleUninitialize();
#endif

  ExitProcess(ret);
}