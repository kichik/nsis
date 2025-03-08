/*
 * main.c: executable header main code
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2025 Nullsoft and Contributors
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
#include "resource.h"
#include "util.h"
#include "fileform.h"
#include "state.h"
#include "ui.h"
#include "lang.h"
#include "exec.h"
#include "plugin.h"

#ifndef LOAD_LIBRARY_SEARCH_USER_DIRS
#define LOAD_LIBRARY_SEARCH_USER_DIRS 0x00000400
#define LOAD_LIBRARY_SEARCH_SYSTEM32  0x00000800
#endif
#ifndef SHTDN_REASON_FLAG_PLANNED
#define SHTDN_REASON_FLAG_PLANNED 0x80000000
#endif
#ifndef SHTDN_REASON_MAJOR_APPLICATION
#define SHTDN_REASON_MAJOR_APPLICATION 0x00040000
#endif
#ifndef SHTDN_REASON_MINOR_INSTALLATION
#define SHTDN_REASON_MINOR_INSTALLATION 0x0002
#endif
#ifndef SHUTDOWN_RESTART
#define SHUTDOWN_RESTART 0x00000004
#endif
#ifndef SHUTDOWN_FORCE_OTHERS
#define SHUTDOWN_FORCE_OTHERS 0x00000001
#endif
#ifndef SHUTDOWN_GRACE_OVERRIDE
#define SHUTDOWN_GRACE_OVERRIDE 0x00000020
#endif

#if defined(_MSC_VER) && _MSC_VER >= 1200
EXTERN_C IMAGE_DOS_HEADER __ImageBase;
#define HINST_THISCOMPONENT ( (HINSTANCE) &__ImageBase )
#define HINST_APPLICATION HINST_THISCOMPONENT
#else
#define HINST_APPLICATION ( (HINSTANCE) GetModuleHandle(NULL) )
#endif

#if !defined(NSIS_CONFIG_VISIBLE_SUPPORT) && !defined(NSIS_CONFIG_SILENT_SUPPORT)
#error One of NSIS_CONFIG_SILENT_SUPPORT or NSIS_CONFIG_VISIBLE_SUPPORT must be defined.
#endif
#ifdef NSIS_COMPRESS_WHOLE
extern HANDLE dbd_hFile;
#endif

TCHAR g_caption[NSIS_MAX_STRLEN*2]; // Why does this have to be NSIS_MAX_STRLEN*2?
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
HWND g_hwnd;
HANDLE g_hInstance;
#endif
void *g_SHGetFolderPath;
DWORD g_WinVer;

void NSISCALL CleanUp();

TCHAR *ValidateTempDir()
{
  validate_filename(state_temp_dir);
  if (!validpathspec(state_temp_dir))
    return NULL;
  addtrailingslash(state_temp_dir);
  CreateNormalDirectory(state_temp_dir);
  // state_language is used as a temp var here
  return my_GetTempFileName(state_language, state_temp_dir);
}


NSIS_ENTRYPOINT_GUINOCRT
EXTERN_C void NSISWinMainNOCRT()
{
  int ret = 0;
  const TCHAR *m_Err = _LANG_ERRORWRITINGTEMP;
  int cl_flags = 0;

  TCHAR *realcmds;
  TCHAR seekchar=_T(' ');
  TCHAR *cmdline;
  OSVERSIONINFOEX ovi;

  SetErrorMode(SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS);

  // Get the version as reported by Windows
  if (sizeof(void*) < 8)
  {
    *((UINT32*)&ovi.szCSDVersion[0]) = 0; // Zero out SP
    *((UINT64*)&ovi.wServicePackMajor) = 0; // wServicePackMajor, wSuiteMask and wProductType
  }
  ovi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
  if (!GetVersionEx((OSVERSIONINFO*) &ovi) && sizeof(void*) < 8)
  {
    ovi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx((OSVERSIONINFO*) &ovi);
    if (sizeof(TCHAR) == 2 || ovi.dwPlatformId == VER_PLATFORM_WIN32_NT)
    {
      ovi.wProductType = 4; // TODO: For < NT4SP6, look it up in the registry. 4 means not W9x and not VER_NT_*
      ovi.wServicePackMajor = ovi.szCSDVersion[0] == 'S' ? ovi.szCSDVersion[13] - '0' : 0;
    }
  }
  if (sizeof(TCHAR) == 1 && ovi.dwPlatformId < VER_PLATFORM_WIN32_NT)
  {
    ovi.wProductType = 0;
    ovi.wServicePackMajor = ovi.szCSDVersion[1] >= 'A' ? ovi.szCSDVersion[1] - ('A'-1) : 0; // A, B or C
  }
  if (sizeof(void*) < 8 && ovi.dwMajorVersion < 10) // Ideally (sizeof(TCHAR) == 1 && ovi.dwMajorVersion < 5) but the compatibility tab emulates this bug
  {
    ovi.dwBuildNumber &= 0xffff; // Remove W9x garbage
  }
  // Save the packed version information
  {
    UINT32 *p = &g_osinfo.WVBuild;
    p[0] = ovi.dwBuildNumber;
    p[1] = MAKELONG(MAKEWORD(ovi.wProductType, ovi.wServicePackMajor), MAKEWORD(ovi.dwMinorVersion, ovi.dwMajorVersion));
  }

  {
    // bug #1125: Don't load modules from the application nor current directory.
    // SetDefaultDllDirectories() allows us to restrict implicitly loaded and 
    // dynamically loaded modules to just %windir%\System32 and directories 
    // added with AddDllDirectory(). This prevents DLL search order attacks (CAPEC-471).
    // CoCreateInstance(CLSID_ShellLink, ...) fails on Vista if SetDefaultDllDirectories is called
    BOOL avoidwinbug = IsWinVista();
    if (!avoidwinbug)
    {
      FARPROC fp = myGetProcAddress(MGA_SetDefaultDllDirectories);
      if (fp) ((BOOL(WINAPI*)(DWORD))fp)(LOAD_LIBRARY_SEARCH_SYSTEM32|LOAD_LIBRARY_SEARCH_USER_DIRS);
    }
    // SetDefaultDllDirectories might not be available so we try to preload various libraries as 
    // best we can before Windows gets a chance to mess things up by loading from the wrong directory.
    {
      static const char preload[] = 
        "UXTHEME\0" // Vista: OleInitialize calls NtUserCreateWindowEx and that pulls in UXTheme.dll
        "USERENV\0" // Vista: SHGetFileInfo ends up in SHELL32.kfapi::GetUserProfileDir and that pulls in UserEnv.dll
        "SETUPAPI\0" // XP: SHGetFileInfo ends up in CMountPoint::_InitLocalDriveHelper and that pulls in SetupAPI.dll
        "APPHELP\0" // Vista: SHGetFileInfo ... SHELL32.SHILAliasTranslate ... SHELL32.ApphelpCheckShellObject
        "PROPSYS\0" // Vista: SHGetFileInfo ... SHELL32.SHILAliasTranslate ... SHLWAPI.#187 ... SHLWAPI.#505/SHPropertyBag_ReadGUID
        "DWMAPI\0" // Win7 without KB2533623: UXTheme pulls in DWMAPI.dll
        "CRYPTBASE\0" // Win7 without KB2533623: OleInitialize ... RPCRT4.UuidCreate ... RPCRT4.GenerateRandomNumber
        "OLEACC\0" // Vista: SHFileOperation ... SHELL32.CProgressDialogUI::_Setup ... SHELL32.GetRoleTextW
        "CLBCATQ\0" // XP.SP2&SP3: SHAutoComplete ... OLE32!InitializeCatalogIfNecessary ... OLE32!CComCatalog::TryToLoadCLB
        "NTMARTA\0" // Win7 without KB2533623 (Bug #1204): SHGetFileInfo ... SetEntriesInAcl ... ADVAPI32!AccProvpLoadMartaFunctions
#ifndef NSIS_SUPPORT_GETDLLVERSION
        "VERSION\0"
#endif
      ;
      const char *dll;
      for (dll = preload; dll[0]; dll += lstrlenA(dll) + 1)
        LoadSystemLibrary(dll);
    }
  }

  // Because myGetProcAddress now loads dlls with a full path 
  // under GetSystemDirectory() the previous issues in <= v3.0b2 with 
  // 'SetOutPath' and/or 'File "shfolder.dll"' no longer apply.
  // All MGA dlls still need to be loaded early here because installers 
  // running under WoW64 might disable WoW64 FS redirection in .onInit and 
  // because GetSystemDirectory() can return the native system32 path we need
  // the redirection to be turned off so LoadLibrary uses the correct folder.
  // Note: We also import directly from KERNEL32, ADVAPI32 and SHELL32 so they 
  // are exempt from this requirement and SHELL32 imports from SHLWAPI on 
  // WoW64 systems and it is also on the KnownDLLs list so 
  // SHLWAPI also gets a pass and that just leaves 
#ifdef NSIS_SUPPORT_GETDLLVERSION
  myGetProcAddress(MGA_GetFileVersionInfo); // VERSION
#endif
  g_SHGetFolderPath = myGetProcAddress(MGA_SHGetFolderPath); // and SHFOLDER

#ifndef _WIN64
  {
    // KEY_WOW64_xxKEY flags causes registry functions to fail on WinNT4 & Win2000.
    // We don't filter them out because all registry instructions are supposed to fail when 
    // accessing a unsupported view and RegKey* takes care of that by looking at the WOW64 flag.
    FARPROC fp = myGetProcAddress(MGA_IsOS);
    enum { os_wow6432 = 30 };
    if (fp && ((BOOL(WINAPI*)(UINT))fp)(os_wow6432)) g_osinfo.WVProd |= NSIS_OSINFO_PROD_WOW64FLAG;
  }
#endif

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
    SHGetFileInfo(_T(""), 0, &shfi, sizeof(SHFILEINFO), 0);
  }

  mystrcpy(g_caption,_LANG_GENERIC_ERROR);

  mystrcpy(state_command_line, GetCommandLine());

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  g_hInstance = HINST_APPLICATION;
#endif//NSIS_CONFIG_VISIBLE_SUPPORT

  cmdline = state_command_line;
  if (*cmdline == _T('\"')) seekchar = *cmdline++;

  cmdline=findchar(cmdline, seekchar);
  cmdline=CharNext(cmdline);
  realcmds=cmdline;

  while (*cmdline)
  {
    // skip over any spaces
    while (*cmdline == _T(' ')) cmdline++;
    
    // get char we should look for to get the next parm
    seekchar = _T(' ');
    if (cmdline[0] == _T('\"'))
    {
      cmdline++;
      seekchar = _T('\"');
    }

    // is it a switch?
    if (cmdline[0] == _T('/'))
    {
      cmdline++;

#define END_OF_ARG(c) (c == _T(' ') || c == _T('\0'))

#if defined(NSIS_CONFIG_VISIBLE_SUPPORT) && defined(NSIS_CONFIG_SILENT_SUPPORT)
      if (cmdline[0] == _T('S') && END_OF_ARG(cmdline[1]))
        g_exec_flags.silent = 1; // bug #1076 - just set the silent flag. the user really wants it silent.
                                 // loadHeaders() will not reset this as it uses |= to apply the script flags.
                                 // there is also no option to force non-silent like `CRCCheck force`
#endif//NSIS_CONFIG_SILENT_SUPPORT && NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_CRC_SUPPORT
      if (CMP4CHAR(cmdline, _T("NCRC")) && END_OF_ARG(cmdline[4]))
        cl_flags |= FH_FLAGS_NO_CRC;
#endif//NSIS_CONFIG_CRC_SUPPORT

      if (CMP4CHAR(cmdline-2, _T(" /D=")))
      {
        *(cmdline-2)=_T('\0'); // keep this from being passed to uninstaller if necessary
        mystrcpy(state_install_directory,cmdline+2);
        break; // /D= must always be last
      }
    }

    // skip over our parm
    cmdline = findchar(cmdline, seekchar);
    // skip the quote
    if (*cmdline == _T('\"'))
      cmdline++;
  }

  GetTempPath(NSIS_MAX_STRLEN, state_temp_dir);
  if (!ValidateTempDir())
  {
    GetWindowsDirectory(state_temp_dir, NSIS_MAX_STRLEN - 5); // leave space for \Temp
    mystrcat(state_temp_dir, _T("\\Temp"));
    if (!ValidateTempDir())
    {
      // Bug #2909242:
      // When running at <= Low IL we cannot write to %Temp% but we can try the temp folder used by IE.
      // There does not seem to be a API to get the low temp dir directly, so we build the path on our own

      GetTempPath(NSIS_MAX_STRLEN - 4, state_temp_dir); // leave space for \Low
      mystrcat(state_temp_dir, _T("Low"));

      // If we don't call SetEnvironmentVariable 
      // child processes will use %temp% and not %temp%\Low
      // and some apps probably can't handle a read only %temp%
      // Do it before ValidateTempDir() because it appends a backslash.
      // TODO: Should this be moved to ValidateTempDir() so it also updates for %windir%\Temp?
      SetEnvironmentVariable(_T("TEMP"), state_temp_dir);
      SetEnvironmentVariable(_T("TMP"), state_temp_dir);

      if (!ValidateTempDir())
      {
        goto end;
      }
    }
  }
  DeleteFile(state_language);

  m_Err = loadHeaders(cl_flags);
  if (m_Err) goto end;

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (g_is_uninstaller)
  {
    TCHAR *p = findchar(state_command_line, 0);

    // state_command_line has state_install_directory right after it in memory, so reading
    // a bit over state_command_line won't do any harm
    while (p >= state_command_line && !CMP4CHAR(p, _T(" _?="))) p--;

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
      int admin = UserIsAdminGrpMember();
      WORD tries; // 0xfffe attempts or bust
      size_t cchtmpslash = mystrlen(state_temp_dir);
      LPTSTR unexe = g_usrvars[5], unexecmd = g_usrvars[6];

      mystrcpy(g_usrvars[0], realcmds);
      if (!(*state_install_directory))
      {
        mystrcpy(state_install_directory, state_exe_directory);
      }

      for (tries = 0; ++tries != 0;)
      {
        DWORD retry = 0, ec;
retry_un_dir:
        wsprintf(state_temp_dir + cchtmpslash, _T("~nsu%X.tmp"), tries);
        GetNSISString(unexe, g_header->str_uninstchild); // '$TEMP\Un.exe'
        if (admin)
        {
          ec = CreateRestrictedDirectory(state_temp_dir);
        }
        else
        {
          ec = CreateNormalDirectory(state_temp_dir);
        }

        if (ec)
        {
          // Delete previous uninstaller (if it is safe to do so) (Bug #1296)
          if (!(GetFileAttributes(unexe) & FILE_ATTRIBUTE_REPARSE_POINT) && DeleteFile(unexe))
          {
            myDelete(state_temp_dir, DEL_DIR);
            if (!retry++) goto retry_un_dir;
          }
        }
        else
        {
          HANDLE hProc;
          SetCurrentDirectory(state_temp_dir);
#ifdef NSIS_SUPPORT_MOVEONREBOOT
          MoveFileOnReboot(state_temp_dir, NULL);
#endif
          if (CopyFile(state_exe_path, unexe, TRUE))
          {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
            MoveFileOnReboot(unexe, NULL);
#endif
            GetNSISString(unexecmd, g_header->str_uninstcmd); // '"$TEMP\Un.exe" $0 _?=$INSTDIR\'
            hProc = myCreateProcess(unexecmd);
            if (hProc)
            {
              CloseHandle(hProc);
              m_Err = 0; // Success
            }
            else if (!retry++ && !file_exists(unexe))
            {
              // Another instance deleted us between CopyFile and CreateProcess
              goto retry_un_dir;
            }
          }
          break; // We called CreateProcess; success or failure, we are done.
        }
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
  }

#ifdef NSIS_SUPPORT_REBOOT
  if (g_exec_flags.reboot_called)
  {
    const DWORD reason = SHTDN_REASON_FLAG_PLANNED | SHTDN_REASON_MAJOR_APPLICATION | SHTDN_REASON_MINOR_INSTALLATION;
    BOOL (WINAPI *IS)(LPTSTR,LPTSTR,DWORD,DWORD,DWORD);
    HANDLE hToken;
    TOKEN_PRIVILEGES tkp;
    if (OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken))
    {
      LookupPrivilegeValue(NULL, SE_SHUTDOWN_NAME, &tkp.Privileges[0].Luid);
      tkp.PrivilegeCount = 1;
      tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
      AdjustTokenPrivileges(hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES)NULL, 0);
    }

    IS=myGetProcAddress(MGA_InitiateShutdown);
    if ( (IS && !IS(NULL, NULL, 0, SHUTDOWN_RESTART | SHUTDOWN_FORCE_OTHERS | SHUTDOWN_GRACE_OVERRIDE, reason))
      || (!ExitWindowsEx(EWX_REBOOT, reason))
      )
      ExecuteCallbackFunction(CB_ONREBOOTFAILED);
  }
#endif//NSIS_SUPPORT_REBOOT

  if (g_exec_flags.errlvl != -1)
    ret = g_exec_flags.errlvl;

  ExitProcess(ret);
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
  // Notify plugins that we are about to unload
  Plugins_UnloadAll();

  // Clean up after plug-ins
  myDelete(state_plugins_dir, DEL_DIR | DEL_RECURSE | DEL_REBOOT);
#endif // NSIS_CONFIG_PLUGIN_SUPPORT
#ifdef DEBUG
  // GlobalFree(g_header); ?
#endif
}
