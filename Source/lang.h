// Lang.h by Amir Szekely 3rd August 2002

#ifndef ___NLF___H_____
#define ___NLF___H_____

#include "exehead/fileform.h"
#include <StdExcept>
using namespace std;

struct StringTable {
  common_strings common;
  common_strings ucommon;
  installer_strings installer;
  uninstall_strings uninstall;
};

#define NLF_VERSION 1
#define NLF_STRINGS 56

#define NLF_BRANDING 0
#define NLF_CAPTION 1
#define NLF_UCAPTION 2
#define NLF_SUBCAPTION_LICENSE 3
#define NLF_SUBCAPTION_OPTIONS 4
#define NLF_SUBCAPTION_DIR 5
#define NLF_SUBCAPTION_INSTFILES 6
#define NLF_SUBCAPTION_COMPLETED 7
#define NLF_USUBCAPTION_CONFIRM 8
#define NLF_USUBCAPTION_INSTFILES 9
#define NLF_USUBCAPTION_COMPLETED 10
#define NLF_BTN_BACK 11
#define NLF_BTN_NEXT 12
#define NLF_BTN_LICENSE 13
#define NLF_BTN_INSTALL 14
#define NLF_BTN_UNINSTALL 15
#define NLF_BTN_CANCEL 16
#define NLF_BTN_CLOSE 17
#define NLF_BTN_BROWSE 18
#define NLF_BTN_DETAILS 19
#define NLF_DEF_NAME 20
#define NLF_COMPLETED 21
#define NLF_COMP_CUSTOM 22
#define NLF_COMP_SUBTEXT1 23
#define NLF_COMP_SUBTEXT1_NO_INST_TYPES 24
#define NLF_COMP_SUBTEXT2 25
#define NLF_DIR_SUBTEXT 26
#define NLF_SPACE_AVAIL 27
#define NLF_SPACE_REQ 28
#define NLF_UNINST_SUBTEXT 29
#define NLF_FILE_ERROR 30
#define NLF_CANT_WRITE 31
#define NLF_COPY_FAILED 32
#define NLF_COPY_TO 33
#define NLF_SYMBOL_NOT_FOUND 34
#define NLF_COULD_NOT_LOAD 35
#define NLF_CREATE_DIR 36
#define NLF_CREATE_SHORTCUT 37
#define NLF_CREATED_UNINST 38
#define NLF_DEL_FILE 39
#define NLF_DEL_ON_REBOOT 40
#define NLF_ERR_CREATING_SHORTCUT 41
#define NLF_ERR_CREATING 42
#define NLF_ERR_DECOMPRESSING 43
#define NLF_ERR_REG_DLL 44
#define NLF_EXEC_SHELL 45
#define NLF_EXEC 46
#define NLF_EXTRACT 47
#define NLF_ERR_WRITING 48
#define NLF_INST_CORRUPTED 49
#define NLF_NO_OLE 50
#define NLF_OUTPUT_DIR 51
#define NLF_REMOVE_DIR 52
#define NLF_RENAME_ON_REBOOT 53
#define NLF_RENAME 54
#define NLF_SKIPPED 55

#define LANG_NAME 56
#define LANG_COMP_TEXT 57
#define LANG_LICENSE_TEXT 58
#define LANG_LICENSE_DATA 59
#define LANG_DIR_TEXT 60
#define LANG_UNINST_TEXT 61

extern char *english_strings[NLF_STRINGS];

// NSIS Language File parser
class NLF {
  public:
    NLF(char *filename);
    ~NLF();

    WORD GetLang();
    char* GetString(int idx);

  private:
    WORD m_wLangId;
    char *m_szStrings[NLF_STRINGS];
};

#endif
