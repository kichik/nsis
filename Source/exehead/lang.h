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

#define _LANG_ERRORWRITINGTEMP "Error writing temp file"

#define _LANG_UNINSTINITERROR "Error launching installer"

#define _LANG_VERIFYINGINST "verifying installer: %d%%"

#define _LANG_UNPACKING "unpacking data: %d%%"

#define _LANG_CANTOPENSELF "Error launching installer" // same as uninstiniterror for size

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

// Please note that all LANG_* define the offset not the string itself.
// To get the string itself use process_string_fromtab, GetStringFromStringTab or LANG_STR().

#define LANG_STR(x)                   GetStringFromStringTab(x)
#define LANG_STR_TAB(x)               cur_language_table[-((int)x+1)]

#define INSTALL_STR(x)                (~((sizeof(common_strings) + FIELD_OFFSET(installer_strings, x)) / sizeof(int)))

// Installer specific strings
#define LANG_BTN_BROWSE               (INSTALL_STR(browse))
#define LANG_BTN_INSTALL              (INSTALL_STR(installbutton))
#define LANG_SPACE_REQ                (INSTALL_STR(spacerequired))
#define LANG_SPACE_AVAIL              (INSTALL_STR(spaceavailable))
#define LANG_COMP_CUSTOM              (INSTALL_STR(custom))
#define LANG_DIR_TEXT                 (INSTALL_STR(text))
#define LANG_DIR_SUBTEXT              (INSTALL_STR(dirsubtext))
#define LANG_COMP_TEXT                (INSTALL_STR(componenttext))
#define LANG_COMP_SUBTEXT(x)          (INSTALL_STR(componentsubtext[x]))
#define LANG_LICENSE_TEXT             (INSTALL_STR(licensetext))
#define LANG_LICENSE_DATA             (INSTALL_STR(licensedata))
#define LANG_BTN_LICENSE              (INSTALL_STR(licensebutton))

#define UNINSTALL_STR(x)              (~((sizeof(common_strings) + FIELD_OFFSET(uninstall_strings, x)) / sizeof(int)))

// Uninstall specific strings
#define LANG_BTN_UNINST               (UNINSTALL_STR(uninstbutton))
#define LANG_UNINST_TEXT              (UNINSTALL_STR(uninstalltext))
#define LANG_UNINST_SUBTEXT           (UNINSTALL_STR(uninstalltext2))

#define COMMON_STR(x)                 (~(FIELD_OFFSET(common_strings, x) / sizeof(int)))

// Common strings
#define LANG_BTN_NEXT                 (COMMON_STR(nextbutton))
#define LANG_BTN_BACK                 (COMMON_STR(backbutton))
#define LANG_BRANDING                 (COMMON_STR(branding))
#define LANG_BTN_CANCEL               (COMMON_STR(cancelbutton))
#define LANG_BTN_DETAILS              (COMMON_STR(showdetailsbutton))
#define LANG_COMPLETED                (COMMON_STR(completed))
#define LANG_BTN_CLOSE                (COMMON_STR(closebutton))
#define LANG_NAME                     (COMMON_STR(name))
#define LANG_CAPTION                  (COMMON_STR(caption))
#define LANG_SUBCAPTION(x)            (COMMON_STR(subcaptions[x]))

// instruction strings
#define LANG_FILEERR                  (COMMON_STR(fileerrtext))
#define LANG_DELETEFILE               (COMMON_STR(del_file))
#define LANG_DLLREGERROR              (COMMON_STR(err_reg_dll))
#define LANG_REMOVEDIR                (COMMON_STR(remove_dir))
#define LANG_OUTPUTDIR                (COMMON_STR(output_dir))
#define LANG_CREATEDIR                (COMMON_STR(create_dir))
#define LANG_RENAME                   (COMMON_STR(rename))
#define LANG_RENAMEONREBOOT           (COMMON_STR(rename_on_reboot))
#define LANG_SKIPPED                  (COMMON_STR(skipped))
#define LANG_CANTWRITE                (COMMON_STR(cant_write))
#define LANG_EXTRACT                  (COMMON_STR(extract))
#define LANG_ERRORWRITING             (COMMON_STR(err_writing))
#define LANG_ERRORDECOMPRESSING       (COMMON_STR(err_decompressing))
#define LANG_DELETEONREBOOT           (COMMON_STR(del_on_reboot))
#define LANG_EXECSHELL                (COMMON_STR(exec_shell))
#define LANG_EXECUTE                  (COMMON_STR(exec))
#define LANG_CANNOTFINDSYMBOL         (COMMON_STR(symbol_not_found))
#define LANG_COULDNOTLOAD             (COMMON_STR(could_not_load))
#define LANG_NOOLE                    (COMMON_STR(no_ole))
#define LANG_ERRORCREATINGSHORTCUT    (COMMON_STR(err_creating_shortcut))
#define LANG_CREATESHORTCUT           (COMMON_STR(create_shortcut))
#define LANG_COPYTO                   (COMMON_STR(copy_to))
#define LANG_COPYFAILED               (COMMON_STR(copy_failed))
#define LANG_ERRORCREATING            (COMMON_STR(err_creating))
#define LANG_CREATEDUNINST            (COMMON_STR(created_uninst))
#define LANG_INSTCORRUPTED            (COMMON_STR(inst_corrupted))
#define LANG_COPYDETAILS              (COMMON_STR(copy_details))

#endif//_NSIS_LANG_H_
