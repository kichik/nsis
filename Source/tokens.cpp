#include <windows.h>
#include <stdlib.h>
#include <stdio.h>

#include "build.h"
#include "tokens.h"

typedef struct 
{
  int id;
  char *name;
  int num_parms; // minimum number of parameters
  int opt_parms; // optional parmaters, usually 0, can be -1 for unlimited.
  char *usage_str;
} tokenType;


static tokenType tokenlist[TOK__LAST] =
{
{TOK_ABORT,"Abort",0,1,"[message]"},
{TOK_ADDBRANDINGIMAGE,"AddBrandingImage",2,1,"(top|left|bottom|right) (height|width) [padding]"},
{TOK_ADDSIZE,"AddSize",1,0,"size_to_add_to_section_in_kb"},
{TOK_AUTOCLOSE,"AutoCloseWindow",1,0,"(false|true)"},
{TOK_BGGRADIENT,"BGGradient",0,3,"(off | [top_color [bottom_color [text_color]]])"},
{TOK_BRANDINGTEXT,"BrandingText",1,2,"[/LANG=lang_id] [/TRIM(LEFT|RIGHT|CENTER)] installer_text"},
{TOK_BRINGTOFRONT,"BringToFront",0,0,""},
{TOK_CALL,"Call",1,0,"function_name | [:label_name]"},
{TOK_CALLINSTDLL,"CallInstDLL",2,1,"dll_path_on_target.dll [/NOUNLOAD] function"},
{TOK_CAPTION,"Caption",1,1,"[/LANG=lang_id] installer_caption"},
{TOK_CHANGEUI,"ChangeUI",2,1,"/RTL (all|dlg_id) ui_file.exe"},
{TOK_CLEARERRORS,"ClearErrors",0,0,""},
{TOK_COMPTEXT,"ComponentText",0,4,"[/LANG=lang_id] [component_page_description] [component_subtext1] [component_subtext2]"},
{TOK_GETDLLVERSION,"GetDLLVersion",3,0,"filename $(user_var: high output) $(user_var: low output)"},
{TOK_GETDLLVERSIONLOCAL,"GetDLLVersionLocal",3,0,"localfilename $(user_var: high output) $(user_var: low output)"},
{TOK_GETFILETIME,"GetFileTime",3,0,"file $(user_var: high output) $(user_var: low output)"},
{TOK_GETFILETIMELOCAL,"GetFileTimeLocal",3,0,"localfile $(user_var: high output) $(user_var: low output)"},
{TOK_COPYFILES,"CopyFiles",2,3,"[/SILENT] [/FILESONLY] source_path destination_path [total_size_in_kb]"},
{TOK_CRCCHECK,"CRCCheck",1,0,"(on|force|off)"},
{TOK_CREATEDIR,"CreateDirectory",1,0,"directory_name"},
{TOK_CREATEFONT,"CreateFont",2,5,"$(user_var: handle output) face_name [height wieght /ITALIC /UNDERLINE /STRIKE]"},
{TOK_CREATESHORTCUT,"CreateShortCut",2,6,"shortcut_name.lnk shortcut_target [parameters [icon_file [icon index [showmode [hotkey [comment]]]]]]\n    showmode=(SW_SHOWNORMAL|SW_SHOWMAXIMIZED|SW_SHOWMINIMIZED)\n    hotkey=(ALT|CONTROL|EXT|SHIFT)|(F1-F24|A-Z)"},
{TOK_DBOPTIMIZE,"SetDatablockOptimize",1,0,"(off|on)"},
{TOK_DELETEINISEC,"DeleteINISec",2,0,"ini_file section_name"},
{TOK_DELETEINISTR,"DeleteINIStr",3,0,"ini_file section_name entry_name"},
{TOK_DELETEREGKEY,"DeleteRegKey",2,1,"[/ifempty] root_key subkey\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_DELETEREGVALUE,"DeleteRegValue",3,0,"root_key subkey entry_name\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_DELETE,"Delete",1,1,"[/REBOOTOK] filespec"},
{TOK_DETAILPRINT,"DetailPrint",1,0,"message"},
{TOK_DIRTEXT,"DirText",0,4,"[/LANG=lang_id] [directory_page_description] [directory_page_subtext] [browse button text]"},
{TOK_DIRSHOW,"DirShow",1,0,"(show|hide)"},
{TOK_ROOTDIRINST,"AllowRootDirInstall",1,0,"(true|false)"},
{TOK_CHECKBITMAP,"CheckBitmap",1,0,"local_bitmap.bmp"},
{TOK_ENABLEWINDOW,"EnableWindow",2,0,"hwnd (1|0)"},
{TOK_ENUMREGKEY,"EnumRegKey",4,0,"$(user_var: output) rootkey subkey index\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_ENUMREGVAL,"EnumRegValue",4,0,"$(user_var: output) rootkey subkey index\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_EXCH,"Exch",0,1,"[$(user_var)] | [stack_item_index]"},
{TOK_EXEC,"Exec",1,0,"command_line"},
{TOK_EXECWAIT,"ExecWait",1,1,"command_line [$(user_var: return value)]"},
{TOK_EXECSHELL,"ExecShell",2,2,"(open|print|etc) command_line [parameters [showmode]]\n   showmode=(SW_SHOWNORMAL|SW_SHOWMAXIMIZED|SW_SHOWMINIMIZED)"},
{TOK_EXPANDENVSTRS,"ExpandEnvStrings",2,0,"$(user_var: output) string"},
{TOK_FINDWINDOW,"FindWindow",2,3,"$(user_var: handle output) WindowClass [WindowTitle] [Window_Parent] [Child_After]"},
{TOK_FINDCLOSE,"FindClose",1,0,"$(user_var: handle input)"},
{TOK_FINDFIRST,"FindFirst",3,0,"$(user_var: handle output) $(user_var: filename output) filespec"},
{TOK_FINDNEXT,"FindNext",2,0,"$(user_var: handle input) $(user_var: filename output)"},
{TOK_FILE,"File",1,-1,"[/nonfatal] [/a] ([/r] filespec [...]|/oname=outfile one_file_only)"},
{TOK_FLUSHINI,"FlushINI",1,0,"ini_file"},
{TOK_RESERVEFILE,"ReserveFile",1,-1,"[/nonfatal] [/r] file [file...]"},
{TOK_FILECLOSE,"FileClose",1,0,"$(user_var: handle input)"},
{TOK_FILEERRORTEXT,"FileErrorText",0,3,"[/LANG=lang_id] [text (can contain $0)] [text without ignore (can contain $0)]"},
{TOK_FILEOPEN,"FileOpen",3,0,"$(user_var: handle output) filename openmode\n   openmode=r|w|a"},
{TOK_FILEREAD,"FileRead",2,1,"$(user_var: handle input) $(user_var: text output) [maxlen]"},
{TOK_FILEWRITE,"FileWrite",2,0,"$(user_var: handle input) text"},
{TOK_FILEREADBYTE,"FileReadByte",2,0,"$(user_var: handle input) $(user_var: bytevalue output)"},
{TOK_FILEWRITEBYTE,"FileWriteByte",2,0,"$(user_var: handle input) bytevalue"},
{TOK_FILESEEK,"FileSeek",2,2,"$(user_var: handle input) offset [mode] [$(user_var: new position output)]\n    mode=SET|CUR|END"},
{TOK_FUNCTION,"Function",1,0,"function_name"},
{TOK_FUNCTIONEND,"FunctionEnd",0,0,""},
{TOK_GETDLGITEM,"GetDlgItem",3,0,"$(user_var: handle output) dialog item_id"},
{TOK_GETFULLPATHNAME,"GetFullPathName",2,1,"[/SHORT] $(user_var: result) path_or_file"},
{TOK_GETTEMPFILENAME,"GetTempFileName",1,1,"$(user_var: name output) [base_dir]"},
{TOK_GETWINTEXT,"GetWindowText",2,0,"$(user_var: handle output) hwnd"},
{TOK_HIDEWINDOW,"HideWindow",0,0,""},
{TOK_ICON,"Icon",1,0,"local_icon.ico"},
{TOK_IFABORT,"IfAbort",1,1,"label_to_goto_if_abort [label_to_goto_if_no_abort]"},
{TOK_IFERRORS,"IfErrors",1,1,"label_to_goto_if_errors [label_to_goto_if_no_errors]"},
{TOK_IFFILEEXISTS,"IfFileExists",2,1,"filename label_to_goto_if_file_exists [label_to_goto_otherwise]"},
{TOK_IFREBOOTFLAG,"IfRebootFlag",1,1,"jump_if_set [jump_if_not_set]"},
{TOK_INSTALLDIRREGKEY,"InstallDirRegKey",3,0,"root_key subkey entry_name\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_INSTCOLORS,"InstallColors",1,1,"(/windows | (foreground_color background_color))"},
{TOK_INSTDIR,"InstallDir",1,0,"default_install_directory"},
{TOK_INSTPROGRESSFLAGS,"InstProgressFlags",0,-1,"[flag [...]]\n    flag={smooth|colored}"},
{TOK_INSTTYPE,"InstType",1,1,"install_type_name | /NOCUSTOM | ([/LANG=lang_id] /CUSTOMSTRING=str) | /COMPONENTSONLYONCUSTOM"},
{TOK_INTOP,"IntOp",3,1,"$(user_var: result) val1 OP [val2]\n    OP=(+ - * / % | & ^ ~ ! || &&)"},
{TOK_INTCMP,"IntCmp",3,2,"val1 val2 jump_if_equal [jump_if_val1_less] [jump_if_val1_more]"},
{TOK_INTCMPU,"IntCmpU",3,2,"val1 val2 jump_if_equal [jump_if_val1_less] [jump_if_val1_more]"},
{TOK_INTFMT,"IntFmt",3,0,"$(user_var: output) format_string input"},
{TOK_ISWINDOW,"IsWindow",2,1,"hwnd jump_if_window [jump_if_not_window]"},
{TOK_GOTO,"Goto",1,0,"label"},
{TOK_LANGSTRING,"LangString",3,0,"[un.]name lang_id string"},
{TOK_LANGSTRINGUP,"LangStringUP",3,0,"[un.]name lang_id string"},
{TOK_LICENSEDATA,"LicenseData",1,1,"[/LANG=lang_id] local_file_that_has_license_text.txt"},
{TOK_LICENSEFORCESELECTION,"LicenseForceSelection",1,3,"[/LANG=lang_id] (checkbox [accept_text] | radiobuttons [accept_text] [decline_text] | off)"},
{TOK_LICENSETEXT,"LicenseText",1,2,"[/LANG=lang_id] license_page_description [license_button_text]"},
{TOK_LICENSEBKCOLOR,"LicenseBkColor",1,0,"background_color"},
{TOK_LOADNLF,"LoadLanguageFile",1,0,"language.nlf"},
{TOK_LOGSET,"LogSet",1,0,"on|off"},
{TOK_LOGTEXT,"LogText",1,0,"text"},
{TOK_MESSAGEBOX,"MessageBox",2,4,"mode messagebox_text [return_check label_to_goto_if_equal [return_check2 label2]]\n    mode=modeflag[|modeflag[|modeflag[...]]]\n    "
                                "modeflag=(MB_ABORTRETRYIGNORE|MB_OK|MB_OKCANCEL|MB_RETRYCANCEL|MB_YESNO|MB_YESNOCANCEL|MB_ICONEXCLAMATION|MB_ICONINFORMATION|MB_ICONQUESTION|MB_ICONSTOP|MB_TOPMOST|MB_SETFOREGROUND|MB_RIGHT"},
{TOK_NOP,"Nop",0,0,""},
{TOK_NAME,"Name",1,1,"[/LANG=lang_id] installer_name"},
{TOK_OUTFILE,"OutFile",1,0,"install_output.exe"},
#ifdef NSIS_SUPPORT_CODECALLBACKS
{TOK_PAGE,"Page",1,5,"((custom [creator_function] [leave_function] [caption]) | ((license|components|directory|instfiles) [pre_function] [show_function] [leave_function])) [define_if_last]"},
#else
{TOK_PAGE,"Page",1,1,"license|components|directory|instfiles"},
#endif
{TOK_POP,"Pop",1,0,"$(user_var: output)"},
{TOK_PUSH,"Push",1,0,"string"},
{TOK_QUIT,"Quit",0,0,""},
{TOK_READINISTR,"ReadINIStr",4,0,"$(user_var: output) ini_file section entry_name"},
{TOK_READREGDWORD,"ReadRegDWORD",4,0,"$(user_var: output) rootkey subkey entry\n   root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_READREGSTR,"ReadRegStr",4,0,"$(user_var: output) rootkey subkey entry\n   root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_READENVSTR,"ReadEnvStr",2,0,"$(user_var: output) name"},
{TOK_REBOOT,"Reboot",0,0,""},
{TOK_REGDLL,"RegDLL",1,1,"dll_path_on_target.dll [entrypoint_symbol]"},
{TOK_RENAME,"Rename",2,1,"[/REBOOTOK] source_file destination_file"},
{TOK_RET,"Return",0,0,""},
{TOK_RMDIR,"RMDir",1,1,"[/r|/REBOOTOK] directory_name"},
{TOK_SECTION,"Section",0,3,"[/0] [section_name|-section_name] [section index output]"},
{TOK_SECTIONEND,"SectionEnd",0,0,""},
{TOK_SECTIONIN,"SectionIn",1,-1,"InstTypeIdx [InstTypeIdx [...]]"},
{TOK_SUBSECTION,"SubSection",1,2,"[/e] subsection_name [section index output]"},
{TOK_SUBSECTIONEND,"SubSectionEnd",0,0,""},
{TOK_SEARCHPATH,"SearchPath",2,0,"$(user_var: result) filename"},
{TOK_SECTIONSETFLAGS,"SectionSetFlags",2,0,"section_index flags"},
{TOK_SECTIONGETFLAGS,"SectionGetFlags",2,0,"section_index $(user_var: output flags)"},
{TOK_SECTIONSETINSTTYPES,"SectionSetInstTypes",2,0,"section_index inst_types"},
{TOK_SECTIONGETINSTTYPES,"SectionGetInstTypes",2,0,"section_index $(user_var: output inst_types)"},
{TOK_SECTIONGETTEXT,"SectionGetText",2,0,"section_index $(user_var: output text)"},
{TOK_SECTIONSETTEXT,"SectionSetText",2,0,"section_index text_string"},
{TOK_SECTIONGETSIZE,"SectionGetSize",2,0,"section_index $(user_var: output size)"},
{TOK_SECTIONSETSIZE,"SectionSetSize",2,0,"section_index new_size"},
{TOK_GETCURINSTTYPE,"GetCurInstType",1,0,"$(user_var: output inst_type_idx)"},
{TOK_SETCURINSTTYPE,"SetCurInstType",1,0,"inst_type_idx"},
{TOK_INSTTYPESETTEXT,"InstTypeSetText",2,0,"insttype_index flags"},
{TOK_INSTTYPEGETTEXT,"InstTypeGetText",2,0,"insttype_index $(user_var: output flags)"},
{TOK_SENDMESSAGE,"SendMessage",4,2,"hwnd message [wparam|STR:wParam] [lparam|STR:lParam] [$(user_var: return value)] [/TIMEOUT=X]"},
{TOK_SETAUTOCLOSE,"SetAutoClose",1,0,"(false|true)"},
{TOK_SETBKCOLOR,"SetBkColor",2,0,"hwnd color"},
{TOK_SETBRANDINGIMAGE,"SetBrandingImage",1,2,"[/IMGID=image_item_id_in_dialog] [/RESIZETOFIT] bitmap.bmp"},
{TOK_SETCOMPRESS,"SetCompress",1,0,"(off|auto|force)"},
{TOK_SETCOMPRESSOR,"SetCompressor",1,0,"(zlib|bzip2)"},
{TOK_SETDATESAVE,"SetDateSave",1,0,"(off|on)"},
{TOK_SETDETAILSVIEW,"SetDetailsView",1,0,"(hide|show)"},
{TOK_SETDETAILSPRINT,"SetDetailsPrint",1,0,"(none|listonly|textonly|both)"},
{TOK_SETERRORS,"SetErrors",0,0,""},
{TOK_SETFILEATTRIBUTES,"SetFileAttributes",2,0,"file attribute[|attribute[...]]\n    attribute=(NORMAL|ARCHIVE|HIDDEN|OFFLINE|READONLY|SYSTEM|TEMPORARY|0)"},
{TOK_SETFONT,"SetFont",2,0,"font_face_name font_size"},
{TOK_SETOUTPATH,"SetOutPath",1,0,"output_path"},
{TOK_SETOVERWRITE,"SetOverwrite",1,0,"(on|off|try|ifnewer)"},
{TOK_SETPLUGINUNLOAD,"SetPluginUnload",1,0,"(manual|alwaysoff)"},
{TOK_SETREBOOTFLAG,"SetRebootFlag",1,0,"true|false"},
{TOK_SETSHELLVARCONTEXT,"SetShellVarContext",1,0,"all|current"},
{TOK_SHOWDETAILS,"ShowInstDetails",1,0,"(hide|show|nevershow)"},
{TOK_SHOWDETAILSUNINST,"ShowUninstDetails",1,0,"(hide|show|nevershow)"},
{TOK_SHOWWINDOW,"ShowWindow",2,0,"hwnd show_state"},
{TOK_SILENTINST,"SilentInstall",1,0,"(normal|silent|silentlog)"},
{TOK_SILENTUNINST,"SilentUnInstall",1,0,"(normal|silent)"},
{TOK_SLEEP,"Sleep",1,0,"sleep_time_in_ms"},
{TOK_STRCMP,"StrCmp",3,1,"str1 str2 label_to_goto_if_equal [label_to_goto_if_not]"},
{TOK_STRCPY,"StrCpy",2,2,"$(user_var: output) str [maxlen] [startoffset]"},
{TOK_STRLEN,"StrLen",2,0,"$(user_var: length output) str"},
{TOK_SUBCAPTION,"SubCaption",2,1,"[/LANG=lang_id] page_number(0-4) new_subcaption"},
{TOK_UNINSTALLEXENAME,"UninstallExeName",0,0,"no longer supported, use WriteUninstaller from section."},
{TOK_UNINSTCAPTION,"UninstallCaption",1,1,"[/LANG=lang_id] uninstaller_caption"},
{TOK_UNINSTICON,"UninstallIcon",1,0,"icon_on_local_system.ico"},
#ifdef NSIS_SUPPORT_CODECALLBACKS
{TOK_UNINSTPAGE,"UninstPage",1,5,"((custom [creator_function] [leave_function] [caption]) | ((uninstConfirm|instfiles) [pre_function] [show_function] [leave_function])) [define_if_last]"},
#else
{TOK_UNINSTPAGE,"UninstPage",1,1,"uninstConfirm|instfiles"},
#endif
{TOK_UNINSTTEXT,"UninstallText",1,2,"[/LANG=lang_id] Text_to_go_on_uninstall page [subtext]"},
{TOK_UNINSTSUBCAPTION,"UninstallSubCaption",2,1,"[/LANG=lang_id] page_number(0-2) new_subcaption"},
{TOK_UNREGDLL,"UnRegDLL",1,0,"dll_path_on_target.dll"},
// useless - {TOK_USEOUTERUIITEM,"UseOuterUIItem",2,0,"item id"},
{TOK_WINDOWICON,"WindowIcon",1,0,"on|off"},
{TOK_WRITEINISTR,"WriteINIStr",4,0,"ini_file section_name entry_name new_value"},
{TOK_WRITEREGBIN,"WriteRegBin",4,0,"rootkey subkey entry_name hex_string_like_12848412AB\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_WRITEREGDWORD,"WriteRegDWORD",4,0,"rootkey subkey entry_name new_value_dword\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_WRITEREGSTR,"WriteRegStr",4,0,"rootkey subkey entry_name new_value_string\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_WRITEREGEXPANDSTR,"WriteRegExpandStr",4,0,"rootkey subkey entry_name new_value_string\n    root_key=(HKCR|HKLM|HKCU|HKU|HKCC|HKDD|HKPD)"},
{TOK_WRITEUNINSTALLER,"WriteUninstaller",1,0,"uninstall_exe_name"},
{TOK_XPSTYLE, "XPStyle",1,0,"(on|off)"},
{TOK_P_PACKEXEHEADER,"!packhdr",2,0,"temp_file_name command_line_to_compress_that_temp_file"},
{TOK_P_SYSTEMEXEC,"!system",1,2,"command [<|>|<>|=) retval]"},
{TOK_P_ADDINCLUDEDIR,"!AddIncludeDir",1,0,"dir"},
{TOK_P_INCLUDE,"!include",1,0,"filename.nsi"},
{TOK_P_CD,"!cd",1,0,"absolute_or_relative_new_directory"},
{TOK_P_IFDEF,"!ifdef",1,-1,"symbol [| symbol2 [& symbol3 [...]]]"},
{TOK_P_IFNDEF,"!ifndef",1,-1,"symbol [| symbol2 [& symbol3 [...]]]"},
{TOK_P_ENDIF,"!endif",0,0,""},
{TOK_P_DEFINE,"!define",1,1,"symbol [value]"},
{TOK_P_UNDEF,"!undef",1,1,"symbol [value]"},
{TOK_P_ELSE,"!else",0,-1,"[ifdef|ifndef symbol [|symbol2 [& symbol3 [...]]]]"},
{TOK_P_ECHO,"!echo",1,0,"message"},
{TOK_P_WARNING,"!warning",0,1,"[warning_message]"},
{TOK_P_ERROR,"!error",0,1,"[error_message]"},

{TOK_P_VERBOSE,"!verbose",1,0,"verbose_level"},

{TOK_P_MACRO,"!macro",1,-1,"macroname [parms ...]"},
{TOK_P_MACROEND,"!macroend",0,0,""},
{TOK_P_INSERTMACRO,"!insertmacro",1,-1,"macroname [parms ...]"},

{TOK_MISCBUTTONTEXT,"MiscButtonText",0,5,"[/LANG=lang_id] [back button text] [next button text] [cancel button text] [close button text]"},
{TOK_DETAILSBUTTONTEXT,"DetailsButtonText",0,2,"[/LANG=lang_id] [details button text]"},
{TOK_UNINSTBUTTONTEXT,"UninstallButtonText",0,2,"[/LANG=lang_id] [uninstall button text]"},
{TOK_INSTBUTTONTEXT,"InstallButtonText",0,2,"[/LANG=lang_id] [install button text]"},
{TOK_SPACETEXTS,"SpaceTexts",0,3,"[/LANG=lang_id] [space required text] [space available text]"},
{TOK_COMPLETEDTEXT,"CompletedText",0,2,"[/LANG=lang_id] [completed text]"},

{TOK_GETFUNCTIONADDR,"GetFunctionAddress",2,0,"output function"},
{TOK_GETLABELADDR,"GetLabelAddress",2,0,"output label"},
{TOK_GETCURRENTADDR,"GetCurrentAddress",1,0,"output"},

{TOK_PLUGINDIR,"!AddPluginDir",1,0,"new_plugin_directory"},
{TOK_INITPLUGINSDIR,"InitPluginsDir",0,0,""},
// Added by ramon 23 May 2003
{TOK_ALLOWSKIPFILES,"AllowSkipFiles",1,0,"(off|on)"},
// Added by ramon 3 jun 2003
{TOK_DEFVAR,"Var",1,0,"VarName"},
// Added by ramon 6 jun 2003
{TOK_VI_ADDKEY,"VIAddVersionKey", 2, 1, "[/LANG=lang_id] keyname value"},
{TOK_VI_SETPRODUCTVERSION,"VIProductVersion", 1, 0, "[version_string_X.X.X.X]"},
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
  if (x == TOK__LAST && commandname)\
  {
    ERROR_MSG("Invalid command \"%s\"\n",commandname);
  }

}

int CEXEBuild::get_commandtoken(char *s, int *np, int *op)
{
  int x;
  for (x = 0; x < TOK__LAST; x ++)
    if (!stricmp(tokenlist[x].name,s)) 
    {
      *np=tokenlist[x].num_parms;
      *op=tokenlist[x].opt_parms;
      return tokenlist[x].id;
    }
  return -1;
}
