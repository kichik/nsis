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

#define _LANG_UNPACKING "unpacking data: %d%%"

#define _LANG_CANTOPENSELF "Can't open self"

#define _LANG_GENERIC_ERROR "NSIS ERROR"


// Changed by Amir Szekely 3rd August 2002
// Now supports more than one language in each installer

// Modified by Dave Laundon 10th August 2002
// In many places, these strings are now referenced by an ID (just their offsets
// into the (common|installer|uninstall)_strings structures) through *_from_lang
// and *FromLang functions - removing code-costly references to the
// cur_(common|install|uninstall)_strings_table globals.  Common strings are
// identified by IDs >=0 and install/uninstall strings by IDs <0.  What's more,
// these IDs fall between -128 to +127 and compile to tiny 2-byte PUSH <8-bit>
// instructions when being passed to the functions.

typedef void *langid_t; // Just so compiler can warn if an ID is given to a
                        // function expecting a string offset and vice-versa.

// Please note that all LANG_* define the offset not the string itself.
// To get the string itself use process_string_fromtab, GetStringFromStringTab or STR().

#define STR(x)                        GetStringFromStringTab(x)

#define INSTALL_STR(x)                (((installer_strings *)cur_install_strings_table)->x)
#define INSTALL_ID(x)                 ((langid_t)((~(UINT)0) - FIELD_OFFSET(installer_strings, x) / sizeof(int)))

// Installer specific strings
#define LANG_BTN_BACK                 (INSTALL_STR(backbutton))
#define LANGID_BTN_BACK               (INSTALL_ID(backbutton))
#define LANG_BTN_NEXT                 (INSTALL_STR(nextbutton))
#define LANGID_BTN_NEXT               (INSTALL_ID(nextbutton))
#define LANG_BTN_BROWSE               (INSTALL_STR(browse))
#define LANGID_BTN_BROWSE             (INSTALL_ID(browse))
#define LANG_BTN_INSTALL              (INSTALL_STR(installbutton))
#define LANGID_BTN_INSTALL            (INSTALL_ID(installbutton))
#define LANG_SPACE_REQ                (INSTALL_STR(spacerequired))
#define LANGID_SPACE_REQ              (INSTALL_ID(spacerequired))
#define LANG_SPACE_AVAIL              (INSTALL_STR(spaceavailable))
#define LANGID_SPACE_AVAIL            (INSTALL_ID(spaceavailable))
#define LANG_COMP_CUSTOM              (INSTALL_STR(custom))
#define LANGID_COMP_CUSTOM            (INSTALL_ID(custom))
#define LANG_DIR_TEXT                 (INSTALL_STR(text))
#define LANGID_DIR_TEXT               (INSTALL_ID(text))
#define LANG_DIR_SUBTEXT              (INSTALL_STR(dirsubtext))
#define LANGID_DIR_SUBTEXT            (INSTALL_ID(dirsubtext))
#define LANG_COMP_TEXT                (INSTALL_STR(componenttext))
#define LANGID_COMP_TEXT              (INSTALL_ID(componenttext))
#define LANG_COMP_SUBTEXT(x)          (INSTALL_STR(componentsubtext[x]))
#define LANGID_COMP_SUBTEXT(x)        (INSTALL_ID(componentsubtext[x]))
#define LANG_LICENSE_TEXT             (INSTALL_STR(licensetext))
#define LANGID_LICENSE_TEXT           (INSTALL_ID(licensetext))
#define LANG_LICENSE_DATA             (INSTALL_STR(licensedata))
#define LANGID_LICENSE_DATA           (INSTALL_ID(licensedata))
#define LANG_BTN_LICENSE              (INSTALL_STR(licensebutton))
#define LANGID_BTN_LICENSE            (INSTALL_ID(licensebutton))

#define UNINSTALL_STR(x)              (((uninstall_strings *)cur_install_strings_table)->x)
#define UNINSTALL_ID(x)               ((langid_t)((~(UINT)0) - FIELD_OFFSET(uninstall_strings, x) / sizeof(int)))

// Uninstall specific strings
#define LANG_BTN_UNINST               (UNINSTALL_STR(uninstbutton))
#define LANGID_BTN_UNINST             (UNINSTALL_ID(uninstbutton))
#define LANG_UNINST_TEXT              (UNINSTALL_STR(uninstalltext))
#define LANGID_UNINST_TEXT            (UNINSTALL_ID(uninstalltext))
#define LANG_UNINST_SUBTEXT           (UNINSTALL_STR(uninstalltext2))
#define LANGID_UNINST_SUBTEXT         (UNINSTALL_ID(uninstalltext2))

#define COMMON_STR(x)                 (cur_common_strings_table->x)
#define COMMON_ID(x)                  ((langid_t)(FIELD_OFFSET(common_strings, x) / sizeof(int)))

// Common strings
#define LANG_BRANDING                 (COMMON_STR(branding))
#define LANGID_BRANDING               (COMMON_ID(branding))
#define LANG_BTN_CANCEL               (COMMON_STR(cancelbutton))
#define LANGID_BTN_CANCEL             (COMMON_ID(cancelbutton))
#define LANG_BTN_DETAILS              (COMMON_STR(showdetailsbutton))
#define LANGID_BTN_DETAILS            (COMMON_ID(showdetailsbutton))
#define LANG_COMPLETED                (COMMON_STR(completed))
#define LANGID_COMPLETED              (COMMON_ID(completed))
#define LANG_BTN_CLOSE                (COMMON_STR(closebutton))
#define LANGID_BTN_CLOSE              (COMMON_ID(closebutton))
#define LANG_NAME                     (COMMON_STR(name))
#define LANGID_NAME                   (COMMON_ID(name))
#define LANG_CAPTION                  (COMMON_STR(caption))
#define LANGID_CAPTION                (COMMON_ID(caption))
#define LANG_SUBCAPTION(x)            (COMMON_STR(subcaptions[x]))
#define LANGID_SUBCAPTION(x)          (COMMON_ID(subcaptions[x]))

// instruction strings
#define LANG_FILEERR                  (COMMON_STR(fileerrtext))
#define LANGID_FILEERR                (COMMON_ID(fileerrtext))
#define LANG_DELETEFILE               (COMMON_STR(del_file))
#define LANGID_DELETEFILE             (COMMON_ID(del_file))
#define LANG_DLLREGERROR              (COMMON_STR(err_reg_dll))
#define LANGID_DLLREGERROR            (COMMON_ID(err_reg_dll))
#define LANG_REMOVEDIR                (COMMON_STR(remove_dir))
#define LANGID_REMOVEDIR              (COMMON_ID(remove_dir))
#define LANG_OUTPUTDIR                (COMMON_STR(output_dir))
#define LANGID_OUTPUTDIR              (COMMON_ID(output_dir))
#define LANG_CREATEDIR                (COMMON_STR(create_dir))
#define LANGID_CREATEDIR              (COMMON_ID(create_dir))
#define LANG_RENAME                   (COMMON_STR(rename))
#define LANGID_RENAME                 (COMMON_ID(rename))
#define LANG_RENAMEONREBOOT           (COMMON_STR(rename_on_reboot))
#define LANGID_RENAMEONREBOOT         (COMMON_ID(rename_on_reboot))
#define LANG_SKIPPED                  (COMMON_STR(skipped))
#define LANGID_SKIPPED                (COMMON_ID(skipped))
#define LANG_CANTWRITE                (COMMON_STR(cant_write))
#define LANGID_CANTWRITE              (COMMON_ID(cant_write))
#define LANG_EXTRACT                  (COMMON_STR(extract))
#define LANGID_EXTRACT                (COMMON_ID(extract))
#define LANG_ERRORWRITING             (COMMON_STR(err_writing))
#define LANGID_ERRORWRITING           (COMMON_ID(err_writing))
#define LANG_ERRORDECOMPRESSING       (COMMON_STR(err_decompressing))
#define LANGID_ERRORDECOMPRESSING     (COMMON_ID(err_decompressing))
#define LANG_DELETEONREBOOT           (COMMON_STR(del_on_reboot))
#define LANGID_DELETEONREBOOT         (COMMON_ID(del_on_reboot))
#define LANG_EXECSHELL                (COMMON_STR(exec_shell))
#define LANGID_EXECSHELL              (COMMON_ID(exec_shell))
#define LANG_EXECUTE                  (COMMON_STR(exec))
#define LANGID_EXECUTE                (COMMON_ID(exec))
#define LANG_CANNOTFINDSYMBOL         (COMMON_STR(symbol_not_found))
#define LANGID_CANNOTFINDSYMBOL       (COMMON_ID(symbol_not_found))
#define LANG_COULDNOTLOAD             (COMMON_STR(could_not_load))
#define LANGID_COULDNOTLOAD           (COMMON_ID(could_not_load))
#define LANG_NOOLE                    (COMMON_STR(no_ole))
#define LANGID_NOOLE                  (COMMON_ID(no_ole))
#define LANG_ERRORCREATINGSHORTCUT    (COMMON_STR(err_creating_shortcut))
#define LANGID_ERRORCREATINGSHORTCUT  (COMMON_ID(err_creating_shortcut))
#define LANG_CREATESHORTCUT           (COMMON_STR(create_shortcut))
#define LANGID_CREATESHORTCUT         (COMMON_ID(create_shortcut))
#define LANG_COPYTO                   (COMMON_STR(copy_to))
#define LANGID_COPYTO                 (COMMON_ID(copy_to))
#define LANG_COPYFAILED               (COMMON_STR(copy_failed))
#define LANGID_COPYFAILED             (COMMON_ID(copy_failed))
#define LANG_ERRORCREATING            (COMMON_STR(err_creating))
#define LANGID_ERRORCREATING          (COMMON_ID(err_creating))
#define LANG_CREATEDUNINST            (COMMON_STR(created_uninst))
#define LANGID_CREATEDUNINST          (COMMON_ID(created_uninst))
#define LANG_INSTCORRUPTED            (COMMON_STR(inst_corrupted))
#define LANGID_INSTCORRUPTED          (COMMON_ID(inst_corrupted))

#endif//_NSIS_LANG_H_
