#ifndef NSIS_CONFIG_H
#define NSIS_CONFIG_H

#ifndef APSTUDIO_INVOKED // keep msdev's resource editor from mangling the .rc file

// Added by Dave Laundon 19th August 2002
// For all internal functions, use of stdcall calling convention moves the
// responsibility for tidying the stack to callee from caller, reducing the code
// involved considerably.  Gives an instant saving of 0.5K.
// NB - the zlib and bzip2 portions have been given the same treatment, but with
// project compiler-options settings and/or project-wide defines.
// NB - safer for NSIS's routines to be defined explicitly to avoid problems
// calling DLL functions.
#if defined(_WIN32) && ((_MSC_VER >= 800) || defined(_STDCALL_SUPPORTED))
#define NSISCALL  __stdcall   // Ordinary functions
#define NSISCALLV __cdecl     // Variable-argument-list functions
#else
#define NSISCALL
#define NSISCALLV
#endif

// NSIS_MAX_STRLEN defines the maximum string length for internal variables
// and stack entries. 1024 should be plenty, but if you are doing crazy registry
// shit, you might want to bump it up. Generally it adds about 16-32x the memory,
// so setting this to 4096 from 1024 will add around 64k of memory usage (not
// really a big deal, but not usually needed).
#define NSIS_MAX_STRLEN 1024


// NSIS_MAX_INST_TYPES specified the  maximum install types.
// note that this should not exceed 30, ever.
#define NSIS_MAX_INST_TYPES 8

// NSIS_CONFIG_UNINSTALL_SUPPORT enables the uninstaller
// support. Comment it out if your installers don't need
// uninstallers
// adds approximately 2kb.
#define NSIS_CONFIG_UNINSTALL_SUPPORT

// NSIS_CONFIG_LICENSEPAGE enables support for the installer to
// present a license page.
#define NSIS_CONFIG_LICENSEPAGE

// NSIS_CONFIG_LICENSEPAGE enables support for the installer to
// present a page.where you can select what sections are installed.
// with this disabled, all sections are installed.
#define NSIS_CONFIG_COMPONENTPAGE

// NSIS_CONFIG_SILENT_SUPPORT enables support for making installers
// that are completely silent.
#define NSIS_CONFIG_SILENT_SUPPORT

// NSIS_CONFIG_VISIBLE_SUPPORT enables support for making installers
// that are visible.
#define NSIS_CONFIG_VISIBLE_SUPPORT

// NSIS_CONFIG_ENHANCEDUI_SUPPORT enables support for CreateFont, SetStaticBkColor (used by some UIs),etc
#define NSIS_CONFIG_ENHANCEDUI_SUPPORT


// Changed by Amir Szekely 31st July 2002
// Now supports runtime choice of compression method

// NSIS_CONFIG_COMPRESSION_SUPPORT enables support for making installers
// that use compression (recommended).
#define NSIS_CONFIG_COMPRESSION_SUPPORT
  // compression specific options

  // NSIS_ZLIB_COMPRESS_WHOLE makes all install data in zlib installers
  // compressed together. Runtime requirements are increased, but potential
  // for compression is as well. Adds approximately 1kb of disk footprint,
  // and requires that the installer create a (potentially large) temporary
  // file in the temp directory.
  // #define NSIS_ZLIB_COMPRESS_WHOLE

  // NSIS_BZIP2_COMPRESS_WHOLE makes all install data in bzip2 installers
  // compressed together. Runtime requirements are increased, but potential
  // for compression is as well. Adds approximately 1kb of disk footprint,
  // and requires that the installer create a (potentially large) temporary
  // file in the temp directory.
  #define NSIS_BZIP2_COMPRESS_WHOLE

  // if NSIS_COMPRESS_BZIP2_SMALLMODE is defined, bzip2's decompressor uses
  // bzip2's alternative decompression method that uses a lot less memory, at
  // the expense of speed. not recommended.
  // #define NSIS_COMPRESS_BZIP2_SMALLMODE

  // if NSIS_COMPRESS_BZIP2_LEVEL is defined, it overrides the default bzip2
  // compression window size of 9 (1-9 is valid)
  // 9 uses the most memory, but typically compresses best (recommended).
  // 1 uses the least memory, but typically compresses the worst.
  #define NSIS_COMPRESS_BZIP2_LEVEL 9


// NSIS_CONFIG_CRC_SUPPORT enables support for installer verification.
// HIGHLY recommended.
#define NSIS_CONFIG_CRC_SUPPORT

// NSIS_CONFIG_CRC_ANAL makes the CRC verification extremely careful, meaning
// extra bytes on the end of file, or the first 512 bytes changing, will give
// error. Enable this if you are paranoid, otherwise leaving it off seems safe
// (and is less prone to reporting virii). If you will be digitally signing your
// installers, leave this off (the default).
// #define NSIS_CONFIG_CRC_ANAL


// NSIS_CONFIG_LOG enables the logging facility.
// turning this on (by uncommenting it) adds about
// 3kb, but can be useful in debugging your installers.
// NOT ENABLED BY DEFAULT.
// #define NSIS_CONFIG_LOG

// NSIS_SUPPORT_BGBG enables support for the blue (well, whatever
// color you want) gradient background window.
#define NSIS_SUPPORT_BGBG


// NSIS_SUPPORT_CODECALLBACKS enables support for installer code callbacks.
// recommended, as it uses a minimum of space and allows for neat functionality.
#define NSIS_SUPPORT_CODECALLBACKS


// NSIS_SUPPORT_MOVEONREBOOT enables support for uninstallers that automatically
// delete themselves from the temp directory, as well as the reboot moving/deleting
// modes of Delete and Rename. Adds about 512 gay bytes..
#define NSIS_SUPPORT_MOVEONREBOOT

/////////////// the following are instruction enabling defines ///////////////

// NSIS_SUPPORT_ACTIVEXREG enables activeX plug-in registration
// and deregistration, as well as CallInstDLL
#define NSIS_SUPPORT_ACTIVEXREG

// NSIS_SUPPORT_INTOPTS enables support for IntCmp, IntCmpU, IntOp, and IntFmt.
#define NSIS_SUPPORT_INTOPTS

// NSIS_SUPPORT_STROPTS enables support for StrCmp, StrCpy, and StrLen, as well as Get*Local.
#define NSIS_SUPPORT_STROPTS

// NSIS_SUPPORT_STACK enables support for the stack (Push, Pop, Exch)
#define NSIS_SUPPORT_STACK

// NSIS_SUPPORT_FILEFUNCTIONS enables support for FileOpen,FileClose, FileSeek, FileRead, and FileWrite.
#define NSIS_SUPPORT_FILEFUNCTIONS

// NSIS_SUPPORT_FINDFIRST enables support for FindFirst, FindNext, and FindClose.
#define NSIS_SUPPORT_FINDFIRST

// NSIS_SUPPORT_CREATESHORTCUT enables support for CreateShortCut.
#define NSIS_SUPPORT_CREATESHORTCUT

// NSIS_SUPPORT_INIFILES enables support for ReadINIStr and WriteINIStr.
#define NSIS_SUPPORT_INIFILES

// NSIS_SUPPORT_REGISTRYFUNCTIONS enables support for ReadRegStr, ReadRegDWORD, WriteRegStr, etc etc etc.
#define NSIS_SUPPORT_REGISTRYFUNCTIONS

// NSIS_SUPPORT_COPYFILES enables support for CopyFiles
#define NSIS_SUPPORT_COPYFILES

// NSIS_SUPPORT_REBOOT enables support for Reboot, IfRebootFlag, SetRebootFlag
#define NSIS_SUPPORT_REBOOT

// NSIS_SUPPORT_FNUTIL enables support for GetFullPathName, GetTempFileName, and SearchPath
#define NSIS_SUPPORT_FNUTIL

// NSIS_SUPPORT_EXECUTE enables support for Exec and ExecWait
#define NSIS_SUPPORT_EXECUTE

// NSIS_SUPPORT_SHELLEXECUTE enables support for ExecShell
#define NSIS_SUPPORT_SHELLEXECUTE

// NSIS_SUPPORT_GETDLLVERSION enables support for GetDLLVersion
#define NSIS_SUPPORT_GETDLLVERSION

// NSIS_SUPPORT_GETFILETIME enables support for GetFileTime
#define NSIS_SUPPORT_GETFILETIME

// NSIS_SUPPORT_HWNDS enables support for FindWindow, SendMessage, and IsWindow
#define NSIS_SUPPORT_HWNDS

// NSIS_SUPPORT_ENVIRONMENT enables support for ReadEnvStr and ExpandEnvStrings
#define NSIS_SUPPORT_ENVIRONMENT

// NSIS_SUPPORT_RMDIR enables support for RMDir
#define NSIS_SUPPORT_RMDIR

// NSIS_SUPPORT_FILE enables support for File (extracting files)
#define NSIS_SUPPORT_FILE

// NSIS_SUPPORT_DELETE enables support for Delete (delete files)
#define NSIS_SUPPORT_DELETE

// NSIS_SUPPORT_RENAME enables support for Rename (rename files)
#define NSIS_SUPPORT_RENAME

// NSIS_SUPPORT_MESSAGEBOX enables support for MessageBox
#define NSIS_SUPPORT_MESSAGEBOX


// Added by Ximon Eighteen 5th August 2002
// If this is uncommented the following changes/new features are
// turned on :-
//   - At the start of compilation a directory called dlls in
//     the directory where makensis.exe is running from will be
//     scanned for .dll files.
//   - Any functions in the detected dll files that are exported
//     by name will be remembered. These names are then legal
//     command keywords in an NSIS script.
//   - Any command that is unrecognised is checked against the
//     list of external dll command names. If matched the dll will
//     be packed into the installer.
//   - On the installer machine (rather than the build machine)
//     on first use of a command that requires a plugin dll that
//     dll will be extracted to the temporary directory with a
//     temporary file name.
//   - Any parameters following the command will be pushed onto
//     the stack in left to right order.
//   - The command will then be invoked in the dll as if
//     "CallInstDLL dll command" had been invoked.
//   - When the installer exits any extracted temporary dlls will
//     be deleted.
#define NSIS_CONFIG_PLUGIN_SUPPORT


// fixes
#ifndef NSIS_CONFIG_VISIBLE_SUPPORT
  #ifdef NSIS_CONFIG_LICENSEPAGE
    #undef NSIS_CONFIG_LICENSEPAGE
  #endif
  #ifdef NSIS_CONFIG_COMPONENTPAGE
    #undef NSIS_CONFIG_COMPONENTPAGE
  #endif
  #ifdef NSIS_SUPPORT_BGBG
    #undef NSIS_SUPPORT_BGBG
  #endif
#endif

#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
  #ifndef NSIS_SUPPORT_HWNDS
    #define NSIS_SUPPORT_HWNDS
  #endif
#endif


#if defined(NSIS_CONFIG_CRC_SUPPORT) && defined(NSIS_CONFIG_VISIBLE_SUPPORT)
  #define _NSIS_CONFIG_VERIFYDIALOG
#endif

#if defined(NSIS_CONFIG_UNINSTALL_SUPPORT) && defined(NSIS_CONFIG_VISIBLE_SUPPORT)
  #define _NSIS_CONFIG_UNINSTDLG
#endif

#if defined(NSIS_CONFIG_UNINSTALL_SUPPORT) && defined(NSIS_CONFIG_VISIBLE_SUPPORT)
  #define _NSIS_CONFIG_UNINSTDLG
#endif

#ifdef EXEHEAD
  #ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
    #ifndef NSIS_COMPRESS_USE_ZLIB
      #ifndef NSIS_COMPRESS_USE_BZIP2
        #error compression is enabled but both zlib and bzip2 are disabled.
      #endif
    #endif
  #endif

  #ifdef NSIS_COMPRESS_USE_ZLIB
    #ifdef NSIS_COMPRESS_USE_BZIP2
      #error both zlib and bzip2 are enabled.
    #endif
  #endif

  #ifdef NSIS_COMPRESS_USE_ZLIB
    #ifdef NSIS_ZLIB_COMPRESS_WHOLE
      #define NSIS_COMPRESS_WHOLE
      #ifdef NSIS_CONFIG_VISIBLE_SUPPORT
        #ifndef _NSIS_CONFIG_VERIFYDIALOG
          #define _NSIS_CONFIG_VERIFYDIALOG
        #endif
      #endif
    #endif
  #endif

  #ifdef NSIS_COMPRESS_USE_BZIP2
    #ifdef NSIS_BZIP2_COMPRESS_WHOLE
      #define NSIS_COMPRESS_WHOLE
      #ifdef NSIS_CONFIG_VISIBLE_SUPPORT
        #ifndef _NSIS_CONFIG_VERIFYDIALOG
          #define _NSIS_CONFIG_VERIFYDIALOG
        #endif
      #endif
    #endif
  #endif
#endif // EXEHEAD

#ifdef NSIS_COMPRESS_WHOLE
  #ifndef NSIS_CONFIG_COMPRESSION_SUPPORT
    #error NSIS_COMPRESS_WHOLE defined, NSIS_CONFIG_COMPRESSION_SUPPORT not
  #endif
#endif

#ifdef NSIS_CONFIG_CRC_ANAL
  #ifndef NSIS_CONFIG_CRC_SUPPORT
    #error NSIS_CONFIG_CRC_ANAL defined but NSIS_CONFIG_CRC_SUPPORT not
  #endif
#endif

#ifndef NSIS_COMPRESS_BZIP2_LEVEL
  #define NSIS_COMPRESS_BZIP2_LEVEL 9
#endif

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
  #ifndef NSIS_SUPPORT_RMDIR
    #error NSIS_CONFIG_PLUGIN_SUPPORT relies on NSIS_SUPPORT_RMDIR, but NSIS_SUPPORT_RMDIR is not defined
  #endif
  #ifndef NSIS_SUPPORT_FILE
    #error NSIS_CONFIG_PLUGIN_SUPPORT relies on NSIS_SUPPORT_FILE, but NSIS_SUPPORT_FILE is not defined
  #endif
  #ifndef NSIS_SUPPORT_ACTIVEXREG
    #error NSIS_CONFIG_PLUGIN_SUPPORT relies on NSIS_SUPPORT_ACTIVEXREG, but NSIS_SUPPORT_ACTIVEXREG is not defined
  #endif
  #ifndef NSIS_SUPPORT_STACK
    #error NSIS_CONFIG_PLUGIN_SUPPORT relies on NSIS_SUPPORT_STACK, but NSIS_SUPPORT_STACK is not defined
  #endif
#endif

#if NSIS_MAX_INST_TYPES > 30
  #error NSIS_MAX_INST_TYPES > 30
#endif

#endif//!APSTUDIO_INVOKED

#endif // NSIS_CONFIG_H
