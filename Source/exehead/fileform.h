#include "config.h"

#ifndef _FILEFORM_H_
#define _FILEFORM_H_


// stored in file:
// exehead (~34k)
// firstheader (~28 bytes)
// hdrinfo (4 bytes describing length/compression)::
//   (if install)
//     header (~228 bytes)
//     sections (20 bytes each)
//   (if uninstall)
//     uninstall_header (~116 bytes)
//   pages (12 bytes each)
//   entries (24 bytes each)
//   string table
//   language tables
// datablock
// (hdrinfo+datablock is at least 512 bytes if CRC enabled)
// CRC (optional - 4 bytes)


#define MAX_ENTRY_OFFSETS 6


// if you want people to not be able to decompile your installers as easily,
// reorder the lines following EW_INVALID_OPCODE randomly.

enum
{
  EW_INVALID_OPCODE,    // zero is invalid. useful for catching errors. (otherwise an all zeroes instruction
                        // does nothing, which is easily ignored but means something is wrong.
  EW_RET,               // return from function call
  EW_NOP,               // Nop/Jump, do nothing: 1, [?new address+1:advance one]
  EW_ABORT,             // Abort: 1 [status]
  EW_QUIT,              // Quit: 0
  EW_CALL,              // Call: 1 [new address+1]
  EW_UPDATETEXT,        // Update status text: 2 [update str, ui_st_updateflag=?ui_st_updateflag:this]
  EW_SLEEP,             // Sleep: 1 [sleep time in milliseconds]
  EW_BRINGTOFRONT,      // BringToFront: 0
  EW_CHDETAILSVIEW,     // SetDetailsView: 2 [listaction,buttonaction]
  EW_SETFILEATTRIBUTES, // SetFileAttributes: 2 [filename, attributes]
  EW_CREATEDIR,         // Create directory: 2, [path, ?update$INSTDIR]
  EW_IFFILEEXISTS,      // IfFileExists: 3, [file name, jump amount if exists, jump amount if not exists]
  EW_SETFLAG,           // Sets a flag: 2 [id, data]
  EW_IFFLAG,            // If a flag: 4 [on, off, id, new value mask]
  EW_GETFLAG,           // Gets a flag: 2 [output, id]
#ifdef NSIS_SUPPORT_RENAME
  EW_RENAME,            // Rename: 3 [old, new, rebootok]
#endif
#ifdef NSIS_SUPPORT_FNUTIL
  EW_GETFULLPATHNAME,   // GetFullPathName: 2 [output, input, ?lfn:sfn]
  EW_SEARCHPATH,        // SearchPath: 2 [output, filename]
  EW_GETTEMPFILENAME,   // GetTempFileName: 2 [output, base_dir]
#endif
#ifdef NSIS_SUPPORT_FILE
  EW_EXTRACTFILE,       // File to extract: 6 [overwriteflag, output filename, compressed filedata, filedatetimelow, filedatetimehigh, allow ignore]
                        //  overwriteflag: 0x1 = no. 0x0=force, 0x2=try, 0x3=if date is newer
#endif
#ifdef NSIS_SUPPORT_DELETE
  EW_DELETEFILE,        // Delete File: 2, [filename, rebootok]
#endif
#ifdef NSIS_SUPPORT_MESSAGEBOX
  EW_MESSAGEBOX,        // MessageBox: 5,[MB_flags,text,retv1:retv2,moveonretv1:moveonretv2]
#endif
#ifdef NSIS_SUPPORT_RMDIR
  EW_RMDIR,             // RMDir: 2 [path, recursiveflag]
#endif
#ifdef NSIS_SUPPORT_STROPTS
  EW_STRLEN,            // StrLen: 2 [output, input]
  EW_ASSIGNVAR,         // Assign: 4 [variable (0-9) to assign, string to assign, maxlen, startpos]
  EW_STRCMP,            // StrCmp: 4 [str1, str2, jump_if_equal, jump_if_not_equal] (case-insensitive)
#endif
#ifdef NSIS_SUPPORT_ENVIRONMENT
  EW_READENVSTR,        // ReadEnvStr/ExpandEnvStrings: 3 [output, string_with_env_variables, IsRead]
#endif
#ifdef NSIS_SUPPORT_INTOPTS
  EW_INTCMP,            // IntCmp: 6 [val1, val2, equal, val1<val2, val1>val2, unsigned?]
  EW_INTOP,             // IntOp: 4 [output, input1, input2, op] where op: 0=add, 1=sub, 2=mul, 3=div, 4=bor, 5=band, 6=bxor, 7=bnot input1, 8=lnot input1, 9=lor, 10=land], 11=1%2
  EW_INTFMT,            // IntFmt: [output, format, input]
#endif
#ifdef NSIS_SUPPORT_STACK
  EW_PUSHPOP,           // Push/Pop/Exchange: 3 [variable/string, ?pop:push, ?exch]
#endif
#ifdef NSIS_SUPPORT_HWNDS
  EW_FINDWINDOW,        // FindWindow: 5, [outputvar, window class,window name, window_parent, window_after]
  EW_SENDMESSAGE,       // SendMessage: 6 [output, hwnd, msg, wparam, lparam, [wparamstring?1:0 | lparamstring?2:0 | timeout<<2]
  EW_ISWINDOW,          // IsWindow: 3 [hwnd, jump_if_window, jump_if_notwindow]
#endif

#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
  EW_GETDLGITEM,        // GetDlgItem:        3: [outputvar, dialog, item_id]
  EW_GETWINTEXT,        // GetWindowText:     2: [outputvar, hwnd]
  EW_SETBKCOLOR,        // SerBkColor:        2: [hwnd, color]
  EW_SETBRANDINGIMAGE,  // SetBrandingImage:  1: [Bitmap file]
  EW_CREATEFONT,        // CreateFont:        5: [handle output, face name, height, weight, flags]
  EW_SHOWWINDOW,        // ShowWindow:        2: [hwnd, show state]
#endif

#ifdef NSIS_SUPPORT_SHELLEXECUTE
  EW_SHELLEXEC,         // ShellExecute program: 4, [shell action, complete commandline, parameters, showwindow]
#endif

#ifdef NSIS_SUPPORT_EXECUTE
  EW_EXECUTE,           // Execute program: 3,[complete command line,waitflag,>=0?output errorcode]
#endif

#ifdef NSIS_SUPPORT_GETFILETIME
  EW_GETFILETIME,       // GetFileTime; 3 [file highout lowout]
#endif

#ifdef NSIS_SUPPORT_GETDLLVERSION
  EW_GETDLLVERSION,     // GetDLLVersion: 3 [file highout lowout]
#endif

#ifdef NSIS_SUPPORT_ACTIVEXREG
  EW_REGISTERDLL,       // Register DLL: 3,[DLL file name, string ptr of function to call, text to put in display (<0 if none/pass parms), 1 - no unload, 0 - unload]
#endif

#ifdef NSIS_SUPPORT_CREATESHORTCUT
  EW_CREATESHORTCUT,    // Make Shortcut: 5, [link file, target file, parameters, icon file, iconindex|show mode<<8|hotkey<<16]
#endif

#ifdef NSIS_SUPPORT_COPYFILES
  EW_COPYFILES,         // CopyFiles: 3 [source mask, destination location, flags]
#endif

#ifdef NSIS_SUPPORT_REBOOT
  EW_REBOOT,            // Reboot: 0
#endif

#ifdef NSIS_SUPPORT_INIFILES
  EW_WRITEINI,          // Write INI String: 4, [Section, Name, Value, INI File]
  EW_READINISTR,        // ReadINIStr: 4 [output, section, name, ini_file]
#endif

#ifdef NSIS_SUPPORT_REGISTRYFUNCTIONS
  EW_DELREG,            // DeleteRegValue/DeleteRegKey: 4, [root key(int), KeyName, ValueName, delkeyonlyifempty]. ValueName is -1 if delete key
  EW_WRITEREG,          // Write Registry value: 5, [RootKey(int),KeyName,ItemName,ItemData,typelen]
                        //  typelen=1 for str, 2 for dword, 3 for binary, 0 for expanded str
  EW_READREGSTR,        // ReadRegStr: 5 [output, rootkey(int), keyname, itemname, ==1?int::str]
  EW_REGENUM,           // RegEnum: 5 [output, rootkey, keyname, index, ?key:value]
#endif

#ifdef NSIS_SUPPORT_FILEFUNCTIONS
  EW_FCLOSE,            // FileClose: 1 [handle]
  EW_FOPEN,             // FileOpen: 4  [name, openmode, createmode, outputhandle]
  EW_FPUTS,             // FileWrite: 3 [handle, string, ?int:string]
  EW_FGETS,             // FileRead: 4  [handle, output, maxlen, ?getchar:gets]
  EW_FSEEK,             // FileSeek: 4  [handle, offset, mode, >=0?positionoutput]
#endif//NSIS_SUPPORT_FILEFUNCTIONS

#ifdef NSIS_SUPPORT_FINDFIRST
  EW_FINDCLOSE,         // FindClose: 1 [handle]
  EW_FINDNEXT,          // FindNext: 2  [output, handle]
  EW_FINDFIRST,         // FindFirst: 2 [filespec, output, handleoutput]
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  EW_WRITEUNINSTALLER,  // WriteUninstaller: 1 [name]
#endif

#ifdef NSIS_CONFIG_LOG
  EW_LOG,               // LogText: 2 [0, text] / LogSet: [1, logstate]
#endif

#ifdef NSIS_CONFIG_COMPONENTPAGE
  EW_SECTIONSET,        // SectionSetText:    3: [idx, 0, text]
                        // SectionGetText:    3: [idx, 1, output]
                        // SectionSetFlags:   3: [idx, 2, flags]
                        // SectionGetFlags:   3: [idx, 3, output]
  EW_INSTTYPESET,       // InstTypeSetFlags:  3: [idx, 0, flags]
                        // InstTypeGetFlags:  3: [idx, 1, output]
#endif

  // instructions not actually implemented in exehead, but used in compiler.
  EW_GETLABELADDR,      // both of these get converted to EW_ASSIGNVAR
  EW_GETFUNCTIONADDR,
};

#define FH_FLAGS_MASK 15
#define FH_FLAGS_UNINSTALL 1
#ifdef NSIS_CONFIG_SILENT_SUPPORT
#  define FH_FLAGS_SILENT 2
#endif
#ifdef NSIS_CONFIG_CRC_SUPPORT
#  define FH_FLAGS_NO_CRC 4
#  define FH_FLAGS_FORCE_CRC 8
#endif

#define FH_SIG 0xDEADBEEF

// neato surprise signature that goes in firstheader. :)
#define FH_INT1 0x6C6C754E
#define FH_INT2 0x74666F73
#define FH_INT3 0x74736E49

typedef struct
{
  int flags; // FH_FLAGS_*
  int siginfo;  // FH_SIG

  int nsinst[3]; // FH_INT1,FH_INT2,FH_INT3

  // these point to the header+sections+entries+stringtable in the datablock
  int length_of_header;

  // this specifies the length of all the data (including the firstheader and CRC)
  int length_of_all_following_data;
} firstheader;

// Strings common to both installers and uninstallers
typedef struct
{
  int name; // name of installer

  // unprocessed strings
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  int branding;
  int backbutton;
  int nextbutton;
  int cancelbutton;
  int showdetailsbutton;
  int closebutton;   // "Close"
  int completed;

  int copy_details;

  int byte;
  int kilo;
  int mega;
  int giga;

  // processed strings
  int subcaptions[5];
#endif
  int caption; // name of installer + " Setup" or whatever.

#ifdef NSIS_SUPPORT_FILE
  int fileerrtext;
  int fileerrtext_noignore;
#endif

#if defined(NSIS_SUPPORT_DELETE) || defined(NSIS_SUPPORT_RMDIR) || defined(NSIS_SUPPORT_FILE)
  int cant_write;
#endif
#ifdef NSIS_SUPPORT_RMDIR
  int remove_dir;
#endif
#ifdef NSIS_SUPPORT_COPYFILES
  int copy_failed;
  int copy_to;
#endif
#ifdef NSIS_SUPPORT_ACTIVEXREG
  int registering;
  int unregistering;
  int symbol_not_found;
  int could_not_load;
  int no_ole;
  // not used anywhere - int err_reg_dll;
#endif
#ifdef NSIS_SUPPORT_CREATESHORTCUT
  int create_shortcut;
  int err_creating_shortcut;
#endif
#ifdef NSIS_SUPPORT_DELETE
  int del_file;
#ifdef NSIS_SUPPORT_MOVEONREBOOT
  int del_on_reboot;
#endif
#endif
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  int created_uninst;
  int err_creating;
#endif
#ifdef NSIS_SUPPORT_SHELLEXECUTE
  int exec_shell;
#endif
#ifdef NSIS_SUPPORT_EXECUTE
  int exec;
#endif
#ifdef NSIS_SUPPORT_MOVEONREBOOT
  int rename_on_reboot;
#endif
#ifdef NSIS_SUPPORT_RENAME
  int rename;
#endif
#ifdef NSIS_SUPPORT_FILE
  int extract;
  int err_writing;
  int err_decompressing;
  int skipped;
#endif
  int inst_corrupted;
  int output_dir;
  int create_dir;
#ifdef NSIS_CONFIG_LOG
  int log_install_process;
#endif
} common_strings;

// Flags for common_header.flags
#define CH_FLAGS_DETAILS_SHOWDETAILS 1
#define CH_FLAGS_DETAILS_NEVERSHOW 2
#define CH_FLAGS_PROGRESS_COLORED 4
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  #define CH_FLAGS_SILENT 8
  #define CH_FLAGS_SILENT_LOG 16
#endif
#define CH_FLAGS_AUTO_CLOSE 32
#define CH_FLAGS_DIR_NO_SHOW 64
#define CH_FLAGS_NO_ROOT_DIR 128
#ifdef NSIS_CONFIG_COMPONENTPAGE
  #define CH_FLAGS_COMP_ONLY_ON_CUSTOM 256
  #define CH_FLAGS_NO_CUSTOM 512
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
  #define CH_FLAGS_LICENSE_FORCE_SELECTION 1024
#endif

// Settings common to both installers and uninstallers
typedef struct
{
  int language_tables_num; // number of strings tables in array
  int language_table_size; // size of each language table

  int num_entries; // total number of entries
  int num_string_bytes; // total number of bytes taken by strings

  int num_pages; // number of used pages (including custom pages)

#ifdef NSIS_SUPPORT_BGBG
  int bg_color1, bg_color2, bg_textcolor;
#endif
  int lb_bg, lb_fg, license_bg;

#ifdef NSIS_SUPPORT_CODECALLBACKS
  // .on* calls
  int code_onInit;
  int code_onInstSuccess;
  int code_onInstFailed;
  int code_onUserAbort;
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
  int code_onGUIInit;
  int code_onGUIEnd;
#endif
#endif//NSIS_SUPPORT_CODECALLBACKS

  int flags; // CH_FLAGS_*
} common_header;

// Strings specific to installers
typedef struct
{
  // these first strings are literals (should not be encoded)
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  int browse; // "Browse..."
  int installbutton; // "Install"
  int spacerequired; // "Space required: "
  int spaceavailable; // "Space available: "
  int custom;  // Custom
  int text; // directory page text
  int dirsubtext; // directory text2
#ifdef NSIS_CONFIG_COMPONENTPAGE
  int componenttext; // component page text
  int componentsubtext[4];
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
  int licensetext; // license page text
  int licensedata; // license text
  int licensebutton; // license button text
  int licensebuttonagree; // agree check box/radio button
  int licensebuttondisagree; // disagree check box/radio button
#endif//NSIS_CONFIG_LICENSEPAGE
#else
  int foo;
#endif
} installer_strings;

// Settings specific to installers
typedef struct
{
  // common settings
  common_header common;

  int install_reg_rootkey;
  // these two are not processed!
  int install_reg_key_ptr, install_reg_value_ptr;

#ifdef NSIS_CONFIG_COMPONENTPAGE
  int install_types[NSIS_MAX_INST_TYPES+1];
#endif

#ifdef NSIS_CONFIG_LICENSEPAGE
  int license_bg; // license background color
#endif//NSIS_CONFIG_LICENSEPAGE

  int install_directory_ptr; // default install dir.
  int install_directory_auto_append; // auto append part

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  int uninstdata_offset; // -1 if no uninst data.
  int uninsticon_size;
#endif

  int num_sections; // total number of sections

#ifdef NSIS_SUPPORT_CODECALLBACKS
  // .on* calls
  int code_onVerifyInstDir;
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
  int code_onMouseOverSection;
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
  int code_onSelChange;
#endif//NSIS_CONFIG_COMPONENTPAGE
#endif//NSIS_SUPPORT_CODECALLBACKS
} header;

// Strings specific to uninstallers
typedef struct
{
  int uninstbutton;
  int uninstalltext;
  int uninstalltext2;
} uninstall_strings;

// Settings specific to uninstallers
typedef struct
{
  // common settings
  common_header common;

  int code;
  int code_size;
} uninstall_header;

// used for section->flags
#define SF_SELECTED   1
#define SF_SUBSEC     2
#define SF_SUBSECEND  4
#define SF_BOLD       8
#define SF_RO         16
#define SF_EXPAND     32
#define SF_PSELECTED  64

typedef struct
{
  int name_ptr; // '' for non-optional components
  int install_types; // bits set for each of the different install_types, if any.
  int flags; // SF_* - defined above
  int code;
  int code_size;
  int size_kb;
} section;

typedef struct
{
  int which;
  int offsets[MAX_ENTRY_OFFSETS]; // count and meaning of offsets depend on 'which'
} entry;

enum
{
  NSIS_PAGE_CUSTOM = -1,
#ifdef NSIS_CONFIG_LICENSEPAGE
  NSIS_PAGE_LICENSE,
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
  NSIS_PAGE_SELCOM,
#endif
  NSIS_PAGE_DIR,
  NSIS_PAGE_INSTFILES,
  NSIS_PAGE_COMPLETED,
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  NSIS_PAGE_UNINST
#endif
};

typedef struct
{
  int id; // index in the pages array
#ifdef NSIS_SUPPORT_CODECALLBACKS
  int prefunc; // called before the page is created, or if custom to show the it. Allows to skip the page using Abort.
  int showfunc; // function to do stuff right before page is shown
  int leavefunc; // function to do stuff after the page is shown
#endif //NSIS_SUPPORT_CODECALLBACKS
  int caption; // caption tab
  int next;
  int button_states;
} page;

// the following are only used/implemented in exehead, not makensis.

int NSISCALL isheader(firstheader *h); // returns 0 on not header, length_of_datablock on success

// returns nonzero on error
// returns 0 on success
// on success, m_header will be set to a pointer that should eventually be GlobalFree()'d.
// (or m_uninstheader)
const char * NSISCALL loadHeaders(int cl_flags);

int NSISCALL _dodecomp(int offset, HANDLE hFileOut, char *outbuf, int outbuflen);

#define GetCompressedDataFromDataBlock(offset, hFileOut) _dodecomp(offset,hFileOut,NULL,0)
#define GetCompressedDataFromDataBlockToMemory(offset, out, out_len) _dodecomp(offset,NULL,out,out_len)

extern HANDLE g_db_hFile;
extern int g_quit_flag;

const char * NSISCALL GetStringFromStringTab(int offs);
BOOL NSISCALL ReadSelfFile(LPVOID lpBuffer, DWORD nNumberOfBytesToRead);
DWORD NSISCALL SetSelfFilePointer(LONG lDistanceToMove, DWORD dwMoveMethod);

// $0..$9, $INSTDIR, etc are encoded as ASCII bytes starting from this value.
// Added by ramon 3 jun 2003
#ifdef NSIS_SUPPORT_NAMED_USERVARS
  #ifdef NSIS_SUPPORT_LANG_IN_STRINGS
    #define VAR_CODES_START 253
  #else
    #define VAR_CODES_START 254
  #endif
  #ifdef NSIS_SUPPORT_LANG_IN_STRINGS
    #define LANG_CODES_START 254
  #endif
#else
  #ifdef NSIS_SUPPORT_LANG_IN_STRINGS
    #define LANG_CODES_START 254
  #else
    #define LANG_CODES_START 255
  #endif

  #ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    #define VAR_CODES_START (LANG_CODES_START - 37)
  #else
    #define VAR_CODES_START (LANG_CODES_START - 36)
  #endif
#endif

union installer_flags {
  struct {
    int autoclose;
    int all_user_var;
    int exec_error;
    int abort;
#ifdef NSIS_SUPPORT_REBOOT
    int exec_reboot;
#endif
    int cur_insttype;
    int insttype_changed;
  };
  int flags[1];
};

#endif //_FILEFORM_H_
