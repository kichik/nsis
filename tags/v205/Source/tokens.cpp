#include "Platform.h"
#include <stdlib.h>
#include <stdio.h>

#include "build.h"
#include "tokens.h"

// token placement
#define TP_SEC    1
#define TP_FUNC   2
#define TP_CODE   (TP_SEC | TP_FUNC)
#define TP_GLOBAL 4
#define TP_PAGEEX 8
#define TP_PG     (TP_GLOBAL | TP_PAGEEX)
#define TP_ALL    (TP_CODE | TP_PG)

typedef struct 
{
  int id;
  char *name;
  int num_parms; // minimum number of parameters
  int opt_parms; // optional parmaters, usually 0, can be -1 for unlimited.
  char *usage_str;
  int placement; // where the token can be placed
} tokenType;


static tokenType tokenlist[TOK__LAST] =
{
{TOK_ABORT,"Abort",0,1,"[message]",TP_CODE},
{TOK_ADDBRANDINGIMAGE,"AddBrandingImage",2,1,"(top|left|bottom|right) (height|width) [padding]",TP_GLOBAL},
{TOK_ADDSIZE,"AddSize",1,0,"size_to_add_to_section_in_kb",TP_SEC},
{TOK_AUTOCLOSE,"AutoCloseWindow",1,0,"(false|true)",TP_GLOBAL},
{TOK_BGFONT,"BGFont",0,6,"[font_face [height [wieght] [/ITALIC] [/UNDERLINE] [/STRIKE]]]",TP_GLOBAL},
{TOK_BGGRADIENT,"BGGradient",0,3,"(off | [top_color [bottom_color [text_color]]])",TP_GLOBAL},
{TOK_BRANDINGTEXT,"BrandingText",1,1,"[/TRIM(LEFT|RIGHT|CENTER)] installer_text",TP_GLOBAL},
{TOK_BRINGTOFRONT,"BringToFront",0,0,"",TP_CODE},
{TOK_CALL,"Call",1,0,"function_name | [:label_name]",TP_CODE},
{TOK_CALLINSTDLL,"CallInstDLL",2,1,"dll_path_on_target.dll [/NOUNLOAD] function",TP_CODE},
{TOK_CAPTION,"Caption",1,0,"installer_caption",TP_GLOBAL|TP_PAGEEX},
{TOK_CHANGEUI,"ChangeUI",1,1,"[/RTL] (all|dlg_id) ui_file.exe",TP_GLOBAL},
{TOK_CLEARERRORS,"ClearErrors",0,0,"",TP_CODE},
{TOK_COMPTEXT,"ComponentText",0,3,"[component_page_description] [component_subtext1] [component_subtext2]",TP_PG},
{TOK_GETDLLVERSION,"GetDLLVersion",3,0,"filename $(user_var: high output) $(user_var: low output)",TP_CODE},
{TOK_GETDLLVERSIONLOCAL,"GetDLLVersionLocal",3,0,"localfilename $(user_var: high output) $(user_var: low output)",TP_CODE},
{TOK_GETFILETIME,"GetFileTime",3,0,"file $(user_var: high output) $(user_var: low output)",TP_CODE},
{TOK_GETFILETIMELOCAL,"GetFileTimeLocal",3,0,"localfile $(user_var: high output) $(user_var: low output)",TP_CODE},
{TOK_COPYFILES,"CopyFiles",2,3,"[/SILENT] [/FILESONLY] source_path destination_path [total_size_in_kb]",TP_CODE},
{TOK_CRCCHECK,"CRCCheck",1,0,"(on|force|off)",TP_GLOBAL},
{TOK_CREATEDIR,"CreateDirectory",1,0,"directory_name",TP_CODE},
{TOK_CREATEFONT,"CreateFont",2,5,"$(user_var: handle output) face_name [height wieght /ITALIC /UNDERLINE /STRIKE]",TP_CODE},
{TOK_CREATESHORTCUT,"CreateShortCut",2,6,"shortcut_name.lnk shortcut_target [parameters [icon_file [icon index [showmode [hotkey [comment]]]]]]\n    showmode=(SW_SHOWNORMAL|SW_SHOWMAXIMIZED|SW_SHOWMINIMIZED)\n    hotkey=(ALT|CONTROL|EXT|SHIFT)|(F1-F24|A-Z)",TP_CODE},
{TOK_DBOPTIMIZE,"SetDatablockOptimize",1,0,"(off|on)",TP_ALL},
{TOK_DELETEINISEC,"DeleteINISec",2,0,"ini_file section_name",TP_CODE},
{TOK_DELETEINISTR,"DeleteINIStr",3,0,"ini_file section_name entry_name",TP_CODE},
{TOK_DELETEREGKEY,"DeleteRegKey",2,1,"[/ifempty] root_key subkey\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_DELETEREGVALUE,"DeleteRegValue",3,0,"root_key subkey entry_name\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_DELETE,"Delete",1,1,"[/REBOOTOK] filespec",TP_CODE},
{TOK_DETAILPRINT,"DetailPrint",1,0,"message",TP_CODE},
{TOK_DIRTEXT,"DirText",0,4,"[directory_page_description] [directory_page_subtext] [browse_button_text] [browse_dlg_text]",TP_PG},
//{TOK_DIRSHOW,"DirShow",1,0,"(show|hide)",TP_PG},
{TOK_DIRSHOW,"DirShow",0,0,"doesn't currently work",TP_ALL},
{TOK_DIRVAR,"DirVar",1,0,"$(user_var: dir in/out))",TP_PAGEEX},
{TOK_DIRVERIFY,"DirVerify",1,0,"auto|leave",TP_PAGEEX},
{TOK_GETINSTDIRERROR,"GetInstDirError",1,0,"$(user_var: error output)",TP_CODE},
{TOK_ROOTDIRINST,"AllowRootDirInstall",1,0,"(true|false)",TP_GLOBAL},
{TOK_CHECKBITMAP,"CheckBitmap",1,0,"local_bitmap.bmp",TP_GLOBAL},
{TOK_ENABLEWINDOW,"EnableWindow",2,0,"hwnd (1|0)",TP_CODE},
{TOK_ENUMREGKEY,"EnumRegKey",4,0,"$(user_var: output) rootkey subkey index\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_ENUMREGVAL,"EnumRegValue",4,0,"$(user_var: output) rootkey subkey index\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_EXCH,"Exch",0,1,"[$(user_var)] | [stack_item_index]",TP_CODE},
{TOK_EXEC,"Exec",1,0,"command_line",TP_CODE},
{TOK_EXECWAIT,"ExecWait",1,1,"command_line [$(user_var: return value)]",TP_CODE},
{TOK_EXECSHELL,"ExecShell",2,2,"(open|print|etc) command_line [parameters [showmode]]\n   showmode=(SW_SHOWNORMAL|SW_SHOWMAXIMIZED|SW_SHOWMINIMIZED)",TP_CODE},
{TOK_EXPANDENVSTRS,"ExpandEnvStrings",2,0,"$(user_var: output) string",TP_CODE},
{TOK_FINDWINDOW,"FindWindow",2,3,"$(user_var: handle output) WindowClass [WindowTitle] [Window_Parent] [Child_After]",TP_CODE},
{TOK_FINDCLOSE,"FindClose",1,0,"$(user_var: handle input)",TP_CODE},
{TOK_FINDFIRST,"FindFirst",3,0,"$(user_var: handle output) $(user_var: filename output) filespec",TP_CODE},
{TOK_FINDNEXT,"FindNext",2,0,"$(user_var: handle input) $(user_var: filename output)",TP_CODE},
{TOK_FILE,"File",1,-1,"[/nonfatal] [/a] ([/r] [/x filespec [...]] filespec [...] |\n   /oname=outfile one_file_only)",TP_CODE},
{TOK_FILEBUFSIZE,"FileBufSize",1,0,"buf_size_mb",TP_ALL},
{TOK_FLUSHINI,"FlushINI",1,0,"ini_file",TP_CODE},
{TOK_RESERVEFILE,"ReserveFile",1,-1,"[/nonfatal] [/r] [/x filespec [...]] file [file...]",TP_ALL},
{TOK_FILECLOSE,"FileClose",1,0,"$(user_var: handle input)",TP_CODE},
{TOK_FILEERRORTEXT,"FileErrorText",0,2,"[text (can contain $0)] [text without ignore (can contain $0)]",TP_GLOBAL},
{TOK_FILEOPEN,"FileOpen",3,0,"$(user_var: handle output) filename openmode\n   openmode=r|w|a",TP_CODE},
{TOK_FILEREAD,"FileRead",2,1,"$(user_var: handle input) $(user_var: text output) [maxlen]",TP_CODE},
{TOK_FILEWRITE,"FileWrite",2,0,"$(user_var: handle input) text",TP_CODE},
{TOK_FILEREADBYTE,"FileReadByte",2,0,"$(user_var: handle input) $(user_var: bytevalue output)",TP_CODE},
{TOK_FILEWRITEBYTE,"FileWriteByte",2,0,"$(user_var: handle input) bytevalue",TP_CODE},
{TOK_FILESEEK,"FileSeek",2,2,"$(user_var: handle input) offset [mode] [$(user_var: new position output)]\n    mode=SET|CUR|END",TP_CODE},
{TOK_FUNCTION,"Function",1,0,"function_name",TP_GLOBAL},
{TOK_FUNCTIONEND,"FunctionEnd",0,0,"",TP_FUNC},
{TOK_GETDLGITEM,"GetDlgItem",3,0,"$(user_var: handle output) dialog item_id",TP_CODE},
{TOK_GETFULLPATHNAME,"GetFullPathName",2,1,"[/SHORT] $(user_var: result) path_or_file",TP_CODE},
{TOK_GETTEMPFILENAME,"GetTempFileName",1,1,"$(user_var: name output) [base_dir]",TP_CODE},
{TOK_HIDEWINDOW,"HideWindow",0,0,"",TP_CODE},
{TOK_ICON,"Icon",1,0,"local_icon.ico",TP_GLOBAL},
{TOK_IFABORT,"IfAbort",1,1,"label_to_goto_if_abort [label_to_goto_if_no_abort]",TP_CODE},
{TOK_IFERRORS,"IfErrors",1,1,"label_to_goto_if_errors [label_to_goto_if_no_errors]",TP_CODE},
{TOK_IFFILEEXISTS,"IfFileExists",2,1,"filename label_to_goto_if_file_exists [label_to_goto_otherwise]",TP_CODE},
{TOK_IFREBOOTFLAG,"IfRebootFlag",1,1,"jump_if_set [jump_if_not_set]",TP_CODE},
{TOK_IFSILENT,"IfSilent",1,1,"jump_if_silent [jump_if_not_silent]",TP_CODE},
{TOK_INSTALLDIRREGKEY,"InstallDirRegKey",3,0,"root_key subkey entry_name\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_GLOBAL},
{TOK_INSTCOLORS,"InstallColors",1,1,"(/windows | (foreground_color background_color))",TP_GLOBAL},
{TOK_INSTDIR,"InstallDir",1,0,"default_install_directory",TP_GLOBAL},
{TOK_INSTPROGRESSFLAGS,"InstProgressFlags",0,-1,"[flag [...]]\n    flag={smooth|colored}",TP_GLOBAL},
{TOK_INSTTYPE,"InstType",1,0,"[un.]install_type_name | /NOCUSTOM | /CUSTOMSTRING=str | /COMPONENTSONLYONCUSTOM",TP_GLOBAL},
{TOK_INTOP,"IntOp",3,1,"$(user_var: result) val1 OP [val2]\n    OP=(+ - * / % | & ^ ~ ! || && << >>)",TP_CODE},
{TOK_INTCMP,"IntCmp",3,2,"val1 val2 jump_if_equal [jump_if_val1_less] [jump_if_val1_more]",TP_CODE},
{TOK_INTCMPU,"IntCmpU",3,2,"val1 val2 jump_if_equal [jump_if_val1_less] [jump_if_val1_more]",TP_CODE},
{TOK_INTFMT,"IntFmt",3,0,"$(user_var: output) format_string input",TP_CODE},
{TOK_ISWINDOW,"IsWindow",2,1,"hwnd jump_if_window [jump_if_not_window]",TP_CODE},
{TOK_GOTO,"Goto",1,0,"label",TP_CODE},
{TOK_LANGSTRING,"LangString",3,0,"[un.]name lang_id string",TP_GLOBAL},
{TOK_LANGSTRINGUP,"LangStringUP",0,0,"obsolete, use LangString.",TP_ALL},
{TOK_LICENSEDATA,"LicenseData",1,0,"local_file_that_has_license_text | license_lang_string",TP_PG},
{TOK_LICENSEFORCESELECTION,"LicenseForceSelection",1,2,"(checkbox [accept_text] | radiobuttons [accept_text] [decline_text] | off)",TP_PG},
{TOK_LICENSELANGSTRING,"LicenseLangString",3,0,"name lang_id license_path",TP_GLOBAL},
{TOK_LICENSETEXT,"LicenseText",1,1,"license_page_description [license_button_text]",TP_PG},
{TOK_LICENSEBKCOLOR,"LicenseBkColor",1,0,"background_color",TP_GLOBAL},
{TOK_LOADNLF,"LoadLanguageFile",1,0,"language.nlf",TP_GLOBAL},
{TOK_LOGSET,"LogSet",1,0,"on|off",TP_CODE},
{TOK_LOGTEXT,"LogText",1,0,"text",TP_CODE},
{TOK_MESSAGEBOX,"MessageBox",2,6,"mode messagebox_text [/SD return] [return_check label_to_goto_if_equal [return_check2 label2]]\n    mode=modeflag[|modeflag[|modeflag[...]]]\n    "
                                "modeflag=(MB_ABORTRETRYIGNORE|MB_OK|MB_OKCANCEL|MB_RETRYCANCEL|MB_YESNO|MB_YESNOCANCEL|MB_ICONEXCLAMATION|MB_ICONINFORMATION|MB_ICONQUESTION|MB_ICONSTOP|MB_TOPMOST|MB_SETFOREGROUND|MB_RIGHT",TP_CODE},
{TOK_NOP,"Nop",0,0,"",TP_CODE},
{TOK_NAME,"Name",1,1,"installer_name installer_name_doubled_ampersands",TP_GLOBAL},
{TOK_OUTFILE,"OutFile",1,0,"install_output.exe",TP_GLOBAL},
#ifdef NSIS_SUPPORT_CODECALLBACKS
{TOK_PAGE,"Page",1,4,"((custom [creator_function] [leave_function] [caption]) | ((license|components|directory|instfiles|uninstConfirm) [pre_function] [show_function] [leave_function]))",TP_GLOBAL},
#else
{TOK_PAGE,"Page",1,1,"license|components|directory|instfiles|uninstConfirm",TP_GLOBAL},
#endif
{TOK_PAGECALLBACKS,"PageCallbacks",0,3,"([creator_function] [leave_function]) | ([pre_function] [show_function] [leave_function])",TP_PAGEEX},
{TOK_PAGEEX,"PageEx",1,0,"[un.](custom|uninstConfirm|license|components|directory|instfiles)",TP_GLOBAL},
{TOK_PAGEEXEND,"PageExEnd",0,0,"",TP_PAGEEX},
{TOK_POP,"Pop",1,0,"$(user_var: output)",TP_CODE},
{TOK_PUSH,"Push",1,0,"string",TP_CODE},
{TOK_QUIT,"Quit",0,0,"",TP_CODE},
{TOK_READINISTR,"ReadINIStr",4,0,"$(user_var: output) ini_file section entry_name",TP_CODE},
{TOK_READREGDWORD,"ReadRegDWORD",4,0,"$(user_var: output) rootkey subkey entry\n   root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_READREGSTR,"ReadRegStr",4,0,"$(user_var: output) rootkey subkey entry\n   root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_READENVSTR,"ReadEnvStr",2,0,"$(user_var: output) name",TP_CODE},
{TOK_REBOOT,"Reboot",0,0,"",TP_CODE},
{TOK_REGDLL,"RegDLL",1,1,"dll_path_on_target.dll [entrypoint_symbol]",TP_CODE},
{TOK_RENAME,"Rename",2,1,"[/REBOOTOK] source_file destination_file",TP_CODE},
{TOK_RET,"Return",0,0,"",TP_CODE},
{TOK_RMDIR,"RMDir",1,2,"[/r] [/REBOOTOK] directory_name",TP_CODE},
{TOK_SECTION,"Section",0,3,"[/0] [-][un.][section_name] [section index output]",TP_GLOBAL},
{TOK_SECTIONEND,"SectionEnd",0,0,"",TP_SEC},
{TOK_SECTIONIN,"SectionIn",1,-1,"InstTypeIdx [InstTypeIdx [...]]",TP_SEC},
{TOK_SUBSECTION,"SubSection",1,2,"deprecated - use SectionGroup",TP_GLOBAL},
{TOK_SECTIONGROUP,"SectionGroup",1,2,"[/e] [un.]section_group_name [section index output]",TP_GLOBAL},
{TOK_SUBSECTIONEND,"SubSectionEnd",0,0,"deprecated - use SectionGroupEnd",TP_GLOBAL},
{TOK_SECTIONGROUPEND,"SectionGroupEnd",0,0,"",TP_GLOBAL},
{TOK_SEARCHPATH,"SearchPath",2,0,"$(user_var: result) filename",TP_CODE},
{TOK_SECTIONSETFLAGS,"SectionSetFlags",2,0,"section_index flags",TP_CODE},
{TOK_SECTIONGETFLAGS,"SectionGetFlags",2,0,"section_index $(user_var: output flags)",TP_CODE},
{TOK_SECTIONSETINSTTYPES,"SectionSetInstTypes",2,0,"section_index inst_types",TP_CODE},
{TOK_SECTIONGETINSTTYPES,"SectionGetInstTypes",2,0,"section_index $(user_var: output inst_types)",TP_CODE},
{TOK_SECTIONGETTEXT,"SectionGetText",2,0,"section_index $(user_var: output text)",TP_CODE},
{TOK_SECTIONSETTEXT,"SectionSetText",2,0,"section_index text_string",TP_CODE},
{TOK_SECTIONGETSIZE,"SectionGetSize",2,0,"section_index $(user_var: output size)",TP_CODE},
{TOK_SECTIONSETSIZE,"SectionSetSize",2,0,"section_index new_size",TP_CODE},
{TOK_GETCURINSTTYPE,"GetCurInstType",1,0,"$(user_var: output inst_type_idx)",TP_CODE},
{TOK_SETCURINSTTYPE,"SetCurInstType",1,0,"inst_type_idx",TP_CODE},
{TOK_INSTTYPESETTEXT,"InstTypeSetText",2,0,"insttype_index flags",TP_CODE},
{TOK_INSTTYPEGETTEXT,"InstTypeGetText",2,0,"insttype_index $(user_var: output flags)",TP_CODE},
{TOK_SENDMESSAGE,"SendMessage",4,2,"hwnd message [wparam|STR:wParam] [lparam|STR:lParam] [$(user_var: return value)] [/TIMEOUT=X]",TP_CODE},
{TOK_SETAUTOCLOSE,"SetAutoClose",1,0,"(false|true)",TP_CODE},
{TOK_SETCTLCOLORS,"SetCtlColors",2,2,"hwnd [/BRANDING] [text_color] [transparent|bg_color]",TP_CODE},
{TOK_SETBRANDINGIMAGE,"SetBrandingImage",1,2,"[/IMGID=image_item_id_in_dialog] [/RESIZETOFIT] bitmap.bmp",TP_CODE},
{TOK_SETCOMPRESS,"SetCompress",1,0,"(off|auto|force)",TP_ALL},
{TOK_SETCOMPRESSOR,"SetCompressor",1,1,"[/FINAL] (zlib|bzip2|lzma)",TP_GLOBAL},
{TOK_SETCOMPRESSORDICTSIZE,"SetCompressorDictSize",1,0,"dict_size_mb",TP_ALL},
{TOK_SETCOMPRESSIONLEVEL,"SetCompressionLevel",1,0,"level_0-9",TP_ALL},
{TOK_SETDATESAVE,"SetDateSave",1,0,"(off|on)",TP_ALL},
{TOK_SETDETAILSVIEW,"SetDetailsView",1,0,"(hide|show)",TP_CODE},
{TOK_SETDETAILSPRINT,"SetDetailsPrint",1,0,"(none|listonly|textonly|both)",TP_CODE},
{TOK_SETERRORS,"SetErrors",0,0,"",TP_CODE},
{TOK_SETERRORLEVEL,"SetErrorLevel",1,0,"error_level",TP_CODE},
{TOK_GETERRORLEVEL,"GetErrorLevel",1,0,"$(user_var: output)",TP_CODE},
{TOK_SETFILEATTRIBUTES,"SetFileAttributes",2,0,"file attribute[|attribute[...]]\n    attribute=(NORMAL|ARCHIVE|HIDDEN|OFFLINE|READONLY|SYSTEM|TEMPORARY|0)",TP_CODE},
{TOK_SETFONT,"SetFont",2,1,"[/LANG=lang_id] font_face_name font_size",TP_GLOBAL},
{TOK_SETOUTPATH,"SetOutPath",1,0,"output_path",TP_CODE},
{TOK_SETOVERWRITE,"SetOverwrite",1,0,"on|off|try|ifnewer|ifdiff",TP_ALL},
{TOK_SETPLUGINUNLOAD,"SetPluginUnload",1,0,"(manual|alwaysoff)",TP_ALL},
{TOK_SETREBOOTFLAG,"SetRebootFlag",1,0,"true|false",TP_CODE},
{TOK_SETSHELLVARCONTEXT,"SetShellVarContext",1,0,"all|current",TP_CODE},
{TOK_SETSILENT,"SetSilent",1,0,"silent|normal",TP_CODE},
{TOK_SHOWDETAILS,"ShowInstDetails",1,0,"(hide|show|nevershow)",TP_GLOBAL},
{TOK_SHOWDETAILSUNINST,"ShowUninstDetails",1,0,"(hide|show|nevershow)",TP_GLOBAL},
{TOK_SHOWWINDOW,"ShowWindow",2,0,"hwnd show_state",TP_CODE},
{TOK_SILENTINST,"SilentInstall",1,0,"(normal|silent|silentlog)",TP_GLOBAL},
{TOK_SILENTUNINST,"SilentUnInstall",1,0,"(normal|silent)",TP_GLOBAL},
{TOK_SLEEP,"Sleep",1,0,"sleep_time_in_ms",TP_CODE},
{TOK_STRCMP,"StrCmp",3,1,"str1 str2 label_to_goto_if_equal [label_to_goto_if_not]",TP_CODE},
{TOK_STRCPY,"StrCpy",2,2,"$(user_var: output) str [maxlen] [startoffset]",TP_CODE},
{TOK_STRLEN,"StrLen",2,0,"$(user_var: length output) str",TP_CODE},
{TOK_SUBCAPTION,"SubCaption",2,0,"page_number(0-4) new_subcaption",TP_GLOBAL},
{TOK_UNINSTALLEXENAME,"UninstallExeName",0,0,"no longer supported, use WriteUninstaller from section.",TP_ALL},
{TOK_UNINSTCAPTION,"UninstallCaption",1,0,"uninstaller_caption",TP_GLOBAL},
{TOK_UNINSTICON,"UninstallIcon",1,0,"icon_on_local_system.ico",TP_GLOBAL},
#ifdef NSIS_SUPPORT_CODECALLBACKS
{TOK_UNINSTPAGE,"UninstPage",1,4,"((custom [creator_function] [leave_function] [caption]) | ((license|components|directory|instfiles|uninstConfirm) [pre_function] [show_function] [leave_function]))",TP_GLOBAL},
#else
{TOK_UNINSTPAGE,"UninstPage",1,1,"license|components|directory|instfiles|uninstConfirm",TP_GLOBAL},
#endif
{TOK_UNINSTTEXT,"UninstallText",1,1,"Text_to_go_on_uninstall_page [subtext]",TP_PG},
{TOK_UNINSTSUBCAPTION,"UninstallSubCaption",2,0,"page_number(0-2) new_subcaption",TP_GLOBAL},
{TOK_UNREGDLL,"UnRegDLL",1,0,"dll_path_on_target.dll",TP_CODE},
{TOK_WINDOWICON,"WindowIcon",1,0,"on|off",TP_GLOBAL},
{TOK_WRITEINISTR,"WriteINIStr",4,0,"ini_file section_name entry_name new_value",TP_CODE},
{TOK_WRITEREGBIN,"WriteRegBin",4,0,"rootkey subkey entry_name hex_string_like_12848412AB\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_WRITEREGDWORD,"WriteRegDWORD",4,0,"rootkey subkey entry_name new_value_dword\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_WRITEREGSTR,"WriteRegStr",4,0,"rootkey subkey entry_name new_value_string\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_WRITEREGEXPANDSTR,"WriteRegExpandStr",4,0,"rootkey subkey entry_name new_value_string\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)",TP_CODE},
{TOK_WRITEUNINSTALLER,"WriteUninstaller",1,0,"uninstall_exe_name",TP_CODE},
{TOK_XPSTYLE, "XPStyle",1,0,"(on|off)",TP_GLOBAL},
{TOK_P_PACKEXEHEADER,"!packhdr",2,0,"temp_file_name command_line_to_compress_that_temp_file",TP_ALL},
{TOK_P_SYSTEMEXEC,"!system",1,2,"command [<|>|<>|=) retval]",TP_ALL},
{TOK_P_EXECUTE,"!execute",1,0,"command",TP_ALL},
{TOK_P_ADDINCLUDEDIR,"!AddIncludeDir",1,0,"dir",TP_ALL},
{TOK_P_INCLUDE,"!include",1,0,"filename.nsi",TP_ALL},
{TOK_P_CD,"!cd",1,0,"absolute_or_relative_new_directory",TP_ALL},
{TOK_P_IFDEF,"!ifdef",1,-1,"symbol [| symbol2 [& symbol3 [...]]]",TP_ALL},
{TOK_P_IFNDEF,"!ifndef",1,-1,"symbol [| symbol2 [& symbol3 [...]]]",TP_ALL},
{TOK_P_ENDIF,"!endif",0,0,"",TP_ALL},
{TOK_P_DEFINE,"!define",1,2,"[/date] symbol [value]",TP_ALL},
{TOK_P_UNDEF,"!undef",1,1,"symbol [value]",TP_ALL},
{TOK_P_ELSE,"!else",0,-1,"[ifdef|ifndef symbol [|symbol2 [& symbol3 [...]]]]",TP_ALL},
{TOK_P_ECHO,"!echo",1,0,"message",TP_ALL},
{TOK_P_WARNING,"!warning",0,1,"[warning_message]",TP_ALL},
{TOK_P_ERROR,"!error",0,1,"[error_message]",TP_ALL},

{TOK_P_VERBOSE,"!verbose",1,0,"verbose_level | push | pop",TP_ALL},

{TOK_P_MACRO,"!macro",1,-1,"macroname [parms ...]",TP_ALL},
{TOK_P_MACROEND,"!macroend",0,0,"",TP_ALL},
{TOK_P_INSERTMACRO,"!insertmacro",1,-1,"macroname [parms ...]",TP_ALL},
{TOK_P_IFMACRODEF,"!ifmacrodef",1,-1,"macro [| macro2 [& macro3 [...]]]",TP_ALL},
{TOK_P_IFMACRONDEF,"!ifmacrondef",1,-1,"macro [| macro2 [& macro3 [...]]]",TP_ALL},

{TOK_MISCBUTTONTEXT,"MiscButtonText",0,4,"[back button text] [next button text] [cancel button text] [close button text]",TP_GLOBAL},
{TOK_DETAILSBUTTONTEXT,"DetailsButtonText",0,1,"[details button text]",TP_PG},
{TOK_UNINSTBUTTONTEXT,"UninstallButtonText",0,1,"[uninstall button text]",TP_GLOBAL},
{TOK_INSTBUTTONTEXT,"InstallButtonText",0,1,"[install button text]",TP_GLOBAL},
{TOK_SPACETEXTS,"SpaceTexts",0,2,"none | ([space required text] [space available text])",TP_GLOBAL},
{TOK_COMPLETEDTEXT,"CompletedText",0,2,"[completed text]",TP_PG},

{TOK_GETFUNCTIONADDR,"GetFunctionAddress",2,0,"output function",TP_CODE},
{TOK_GETLABELADDR,"GetLabelAddress",2,0,"output label",TP_CODE},
{TOK_GETCURRENTADDR,"GetCurrentAddress",1,0,"output",TP_CODE},

{TOK_PLUGINDIR,"!AddPluginDir",1,0,"new_plugin_directory",TP_ALL},
{TOK_INITPLUGINSDIR,"InitPluginsDir",0,0,"",TP_CODE},
// Added by ramon 23 May 2003
{TOK_ALLOWSKIPFILES,"AllowSkipFiles",1,0,"(off|on)",TP_ALL},
// Added by ramon 3 jun 2003
{TOK_DEFVAR,"Var",1,0,"VarName",TP_GLOBAL},
// Added by ramon 6 jun 2003
{TOK_VI_ADDKEY,"VIAddVersionKey",2,1,"/LANG=lang_id keyname value",TP_GLOBAL},
{TOK_VI_SETPRODUCTVERSION,"VIProductVersion",1,0,"[version_string_X.X.X.X]",TP_GLOBAL},
{TOK_LOCKWINDOW,"LockWindow",1,0,"(on|off)",TP_CODE},
};

void CEXEBuild::print_help(char *commandname)
{
  int x;
  for (x = 0; x < TOK__LAST; x ++)
  {
    if (!commandname || !stricmp(tokenlist[x].name,commandname))
    {
      ERROR_MSG("%s%s %s\n",commandname?"Usage: ":"",tokenlist[x].name,tokenlist[x].usage_str);
      if (commandname) break;
    }
  }
  if (x == TOK__LAST && commandname)
  {
    ERROR_MSG("Invalid command \"%s\"\n",commandname);
  }

}

int CEXEBuild::get_commandtoken(char *s, int *np, int *op, int *pos)
{
  int x;
  for (x = 0; x < TOK__LAST; x ++)
    if (!stricmp(tokenlist[x].name,s)) 
    {
      *np=tokenlist[x].num_parms;
      *op=tokenlist[x].opt_parms;
      *pos=x;
      return tokenlist[x].id;
    }
  return -1;
}

int CEXEBuild::IsTokenPlacedRight(int pos, char *tok)
{
  if ((unsigned int) pos > (sizeof(tokenlist) / sizeof(tokenType)))
    return PS_OK;

  int tp = tokenlist[pos].placement;
  if (build_cursection && !build_cursection_isfunc)
  {
    // section
    if (tp & TP_SEC)
      return PS_OK;
    ERROR_MSG("Error: command %s not valid in section\n", tok);
    return PS_ERROR;
  }
  else if (build_cursection && build_cursection_isfunc)
  {
    // function
    if (tp & TP_FUNC)
      return PS_OK;
    ERROR_MSG("Error: command %s not valid in function\n", tok);
    return PS_ERROR;
  }
  else if (cur_page)
  {
    // pageex
    if (tp & TP_PAGEEX)
      return PS_OK;
    ERROR_MSG("Error: command %s not valid in PageEx\n", tok);
    return PS_ERROR;
  }
  else
  {
    // global
    if (tp & TP_GLOBAL)
      return PS_OK;
    char err[1024];
    strcpy(err, "Error: command %s not valid outside ");
    if (tp & TP_SEC)
      strcat(err, "section");
    if (tp & TP_FUNC)
    {
      if (tp & TP_SEC)
      {
        if (tp & TP_PAGEEX)
        {
          strcat(err, ", ");
        }
        else
        {
          strcat(err, " or ");
        }
      }
      strcat(err, "function");
    }
    if (tp & TP_PAGEEX)
    {
      if (tp & TP_CODE)
      {
        strcat(err, " or ");
      }
      strcat(err, "PageEx");
    }
    strcat(err, "\n");
    ERROR_MSG(err, tok);
    return PS_ERROR;
  }
}
