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


// Changed by Amir Szekely 3rd August 2002
// Now supports more than one language in each installer

// Please note that all of these define the offset not the string itself.
// To get the string it self use process_string_fromtab or GetStringFromStringTab.

#define INSTALL_STR(x) (install_strings_tables[current_lang].x)

// Installer specific strings
#define LANG_BTN_BACK GetStringFromStringTab(INSTALL_STR(backbutton))
#define LANG_BTN_NEXT GetStringFromStringTab(INSTALL_STR(nextbutton))
#define LANG_BTN_BROWSE GetStringFromStringTab(INSTALL_STR(browse))
#define LANG_BTN_INSTALL GetStringFromStringTab(INSTALL_STR(installbutton))
#define LANG_SPACE_REQ GetStringFromStringTab(INSTALL_STR(spacerequired))
#define LANG_SPACE_AVAIL GetStringFromStringTab(INSTALL_STR(spaceavailable))
#define LANG_COMP_CUSTOM GetStringFromStringTab(INSTALL_STR(custom))
#define LANG_DIR_TEXT GetStringFromStringTab(INSTALL_STR(text))
#define LANG_DIR_SUBTEXT GetStringFromStringTab(INSTALL_STR(dirsubtext))
#define LANG_COMP_TEXT GetStringFromStringTab(INSTALL_STR(componenttext))
#define LANG_COMP_SUBTEXT(x) GetStringFromStringTab(INSTALL_STR(componentsubtext[x]))
#define LANG_LICENSE_TEXT GetStringFromStringTab(INSTALL_STR(licensetext))
#define LANG_LICENSE_DATA GetStringFromStringTab(INSTALL_STR(licensedata))
#define LANG_BTN_LICENSE GetStringFromStringTab(INSTALL_STR(licensebutton))

#define UNINSTALL_STR(x) (uninstall_strings_tables[current_lang].x)

// Uninstall specific strings
#define LANG_BTN_UNINST GetStringFromStringTab(UNINSTALL_STR(uninstbutton))
#define LANG_UNINST_TEXT GetStringFromStringTab(UNINSTALL_STR(uninstalltext))
#define LANG_UNINST_SUBTEXT GetStringFromStringTab(UNINSTALL_STR(uninstalltext2))

#define COMMON_STR(x) (common_strings_tables[current_lang].x)

// Common strings
#define LANG_BRANDING GetStringFromStringTab(COMMON_STR(branding))
#define LANG_BTN_CANCEL GetStringFromStringTab(COMMON_STR(cancelbutton))
#define LANG_BTN_DETAILS GetStringFromStringTab(COMMON_STR(showdetailsbutton))
#define LANG_COMPLETED GetStringFromStringTab(COMMON_STR(completed))
#define LANG_BTN_CLOSE GetStringFromStringTab(COMMON_STR(closebutton))
#define LANG_NAME GetStringFromStringTab(COMMON_STR(name))
#define LANG_CAPTION GetStringFromStringTab(COMMON_STR(caption))
#define LANG_SUBCAPTION(x) GetStringFromStringTab(COMMON_STR(subcaptions[x]))

// instruction strings
#define LANG_FILEERR GetStringFromStringTab(COMMON_STR(fileerrtext))
#define LANG_DELETEFILE GetStringFromStringTab(COMMON_STR(del_file))
#define LANG_DLLREGERROR GetStringFromStringTab(COMMON_STR(err_reg_dll))
#define LANG_REMOVEDIR GetStringFromStringTab(COMMON_STR(remove_dir))
#define LANG_OUTPUTDIR GetStringFromStringTab(COMMON_STR(output_dir))
#define LANG_CREATEDIR GetStringFromStringTab(COMMON_STR(create_dir))
#define LANG_RENAME GetStringFromStringTab(COMMON_STR(rename))
#define LANG_RENAMEONREBOOT GetStringFromStringTab(COMMON_STR(rename_on_reboot))
#define LANG_SKIPPED GetStringFromStringTab(COMMON_STR(skipped))
#define LANG_CANTWRITE GetStringFromStringTab(COMMON_STR(cant_write))
#define LANG_EXTRACT GetStringFromStringTab(COMMON_STR(extract))
#define LANG_ERRORWRITING GetStringFromStringTab(COMMON_STR(err_writing))
#define LANG_ERRORDECOMPRESSING GetStringFromStringTab(COMMON_STR(err_decompressing))
#define LANG_DELETEONREBOOT GetStringFromStringTab(COMMON_STR(del_on_reboot))
#define LANG_EXECSHELL GetStringFromStringTab(COMMON_STR(exec_shell))
#define LANG_EXECUTE GetStringFromStringTab(COMMON_STR(exec))
#define LANG_CANNOTFINDSYMBOL GetStringFromStringTab(COMMON_STR(symbol_not_found))
#define LANG_COULDNOTLOAD GetStringFromStringTab(COMMON_STR(could_not_load))
#define LANG_NOOLE GetStringFromStringTab(COMMON_STR(no_ole))
#define LANG_ERRORCREATINGSHORTCUT GetStringFromStringTab(COMMON_STR(err_creating_shortcut))
#define LANG_CREATESHORTCUT GetStringFromStringTab(COMMON_STR(create_shortcut))
#define LANG_COPYTO GetStringFromStringTab(COMMON_STR(copy_to))
#define LANG_COPYFAILED GetStringFromStringTab(COMMON_STR(copy_failed))
#define LANG_ERRORCREATING GetStringFromStringTab(COMMON_STR(err_creating))
#define LANG_CREATEDUNINST GetStringFromStringTab(COMMON_STR(created_uninst))
#define LANG_INSTCORRUPTED GetStringFromStringTab(COMMON_STR(inst_corrupted))

#endif//_NSIS_LANG_H_