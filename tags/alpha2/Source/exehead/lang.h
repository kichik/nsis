#ifndef _NSIS_LANG_H_
#define _NSIS_LANG_H_

// generic startup strings (these will never be overridable)
#define _LANG_INVALIDCRC "Installer verification failed.\r\n\r\n"  \
                        "This could be the result of an incomplete download,\r\n" \
                        "a failing disk, or (possibly) corruption from a virus."  \
                        "\r\n\r\nYou can try to force an install using the /NCRC\r\n" \
                        "command line switch (but it is NOT recommended)"

#define _LANG_INVALIDINST  "Installer corrupted.\r\n\r\n" \
                          "This could be the result of an incomplete download"


#define _LANG_UNINSTINITERROR "Error initializing uninstaller"

#define _LANG_VERIFYINGINST "verifying installer: %d%%"

#define _LANG_CANTOPENSELF "Can't open self"

#define _LANG_GENERIC_ERROR "NSIS ERROR"


#define LANG_STR(x) (x)

// instruction strings (these may someday be stored in the datablock or string table, and accessed
// via LANG_STR()
#define LANG_DELETEFILE "Delete file: "
#define LANG_DLLREGERROR "Error registering DLL"
#define LANG_REMOVEDIR "Remove directory: "
#define LANG_OUTPUTDIR "Output directory: "
#define LANG_CREATEDIR "Create directory: "
#define LANG_RENAME "Rename: "
#define LANG_RENAMEONREBOOT "Rename on reboot: "
#define LANG_SKIPPED "Skipped: "
#define LANG_CANTWRITE "Can't write: "
#define LANG_EXTRACT "Extract: "
#define LANG_ERRORWRITING "Extract: error writing to file "
#define LANG_ERRORDECOMPRESSING "Error decompressing data! Corrupted installer?"
#define LANG_DELETEONREBOOT "Delete on reboot: "
#define LANG_EXECSHELL "ExecShell: "
#define LANG_EXECUTE "Execute: "
#define LANG_CANNOTFINDSYMBOL "Could not find symbol: "
#define LANG_COULDNOTLOAD "Could not load: "
#define LANG_NOOLE "No OLE for: "
#define LANG_ERRORCREATINGSHORTCUT "Error creating shortcut: "
#define LANG_CREATESHORTCUT "Create shortcut: "
#define LANG_COPYTO "Copy to "
#define LANG_COPYFAILED "Copy failed"
#define LANG_ERRORCREATING "Error creating: "
#define LANG_CREATEDUNINST "Created uninstaller: "
#define LANG_INSTCORRUPTED "Install corrupted: invalid opcode"




#endif//_NSIS_LANG_H_