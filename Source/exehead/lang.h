#ifndef _NSIS_LANG_H_
#define _NSIS_LANG_H_


// generic startup strings (these will never be overridable)
#ifdef NSIS_CONFIG_CRC_SUPPORT
#define _LANG_INVALIDCRC "Installer corrupted or incomplete.\r\n\r\n"  \
                        "This could be the result of a failed download or corruption from a virus."  \
                        "\r\n\r\nIf desperate, try the /NCRC command line switch (NOT recommended)"
#else
#define _LANG_INVALIDCRC  "Installer corrupted.\r\n\r\n" \
                          "This could be the result of an incomplete download"
#endif

#define _LANG_ERRORWRITINGTEMP "Error writing temp file. Make sure your temp directory is valid."

#define _LANG_UNINSTINITERROR "Error launching installer"

#define _LANG_VERIFYINGINST "verifying installer: %d%%"

#define _LANG_UNPACKING "unpacking data: %d%%"

#define _LANG_CANTOPENSELF "Error launching installer" // same as uninstiniterror for size

#define _LANG_GENERIC_ERROR "NSIS ERROR"

#define LANG_STR_TAB(x)             cur_langtable[-((int)x+1)]

#define LANG_BRANDING               -1
#define LANG_CAPTION                -2
#define LANG_NAME                   -3
#define LANG_SPACE_AVAIL            -4
#define LANG_SPACE_REQ              -5
#define LANG_CANTWRITE              -6
#define LANG_COPYFAILED             -7
#define LANG_COPYTO                 -8
#define LANG_CANNOTFINDSYMBOL       -9
#define LANG_COULDNOTLOAD           -10
#define LANG_CREATEDIR              -11
#define LANG_CREATESHORTCUT         -12
#define LANG_CREATEDUNINST          -13
#define LANG_DELETEFILE             -14
#define LANG_DELETEONREBOOT         -15
#define LANG_ERRORCREATINGSHORTCUT  -16
#define LANG_ERRORCREATING          -17
#define LANG_ERRORDECOMPRESSING     -18
#define LANG_DLLREGERROR            -19
#define LANG_EXECSHELL              -20
#define LANG_EXECUTE                -21
#define LANG_EXTRACT                -22
#define LANG_ERRORWRITING           -23
#define LANG_INSTCORRUPTED          -24
#define LANG_NOOLE                  -25
#define LANG_OUTPUTDIR              -26
#define LANG_REMOVEDIR              -27
#define LANG_RENAMEONREBOOT         -28
#define LANG_RENAME                 -29
#define LANG_SKIPPED                -30
#define LANG_COPYDETAILS            -31
#define LANG_LOG_INSTALL_PROCESS    -32
#define LANG_BYTE                   -33
#define LANG_KILO                   -34
#define LANG_MEGA                   -35
#define LANG_GIGA                   -36

#endif//_NSIS_LANG_H_
