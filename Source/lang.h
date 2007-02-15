/*
 * lang.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef ___NLF___H_____
#define ___NLF___H_____

#include "strlist.h"
#include "growbuf.h"
#include "exehead/fileform.h"

struct NLFRef {
  int iRef;
  int iUnRef;
};

struct langstring {
  int name;
  int sn;
  int index;
  int uindex;
  int process;
};

class LangStringList : public SortedStringListND<struct langstring>
{
  public:
    LangStringList();

    int add(const char *name, int *sn=0);
    int get(char *name, int *sn=0, int *index=0, int *uindex=0, int *process=0);
    void set(int pos, int index=-1, int uindex=-1, int process=-1);
    void set(char *name, int index, int uindex=-1, int process=-1);
    const char *pos2name(int pos);
    const char *offset2name(int name);
    int getnum();
    static int compare_index(const void *item1, const void *item2);
    langstring *sort_index(int *num);
    static int compare_uindex(const void *item1, const void *item2);
    langstring *sort_uindex(int *num);

  private:
    int count;
    TinyGrowBuf sortbuf;
};

class StringsArray
{
  public:
    StringsArray();

    void resize(int num);
    int set(int idx, char *str);
    const char *get(int idx);

  private:
    TinyGrowBuf offsets;
    GrowBuf strings;
};

#define NLF_VERSION 6

enum {
  NLF_BRANDING,
  NLF_CAPTION,
  NLF_UCAPTION,
  NLF_SUBCAPTION_LICENSE,
  NLF_SUBCAPTION_OPTIONS,
  NLF_SUBCAPTION_DIR,
  NLF_SUBCAPTION_INSTFILES,
  NLF_SUBCAPTION_COMPLETED,
  NLF_USUBCAPTION_OPTIONS,
  NLF_USUBCAPTION_DIR,
  NLF_USUBCAPTION_CONFIRM,
  NLF_USUBCAPTION_INSTFILES,
  NLF_USUBCAPTION_COMPLETED,
  NLF_BTN_BACK,
  NLF_BTN_NEXT,
  NLF_BTN_LICENSE,
  NLF_BTN_LICENSE_AGREE,
  NLF_BTN_LICENSE_DISAGREE,
  NLF_BTN_INSTALL,
  NLF_BTN_UNINSTALL,
  NLF_BTN_CANCEL,
  NLF_BTN_CLOSE,
  NLF_BTN_BROWSE,
  NLF_BTN_DETAILS,
  NLF_CLICK_NEXT,
  NLF_CLICK_INSTALL,
  NLF_CLICK_UNINSTALL,
  NLF_NAME,
  NLF_NAME_DA, // name with doubled ampersands - virtual
  NLF_COMPLETED,
  NLF_LICENSE_TEXT,
  NLF_LICENSE_TEXT_FSCB,
  NLF_LICENSE_TEXT_FSRB,
  NLF_ULICENSE_TEXT,
  NLF_ULICENSE_TEXT_FSCB,
  NLF_ULICENSE_TEXT_FSRB,
  NLF_LICENSE_DATA, // virtual
  NLF_COMP_CUSTOM,
  NLF_COMP_TEXT,
  NLF_COMP_SUBTEXT1,
  NLF_COMP_SUBTEXT1_NO_INST_TYPES,
  NLF_COMP_SUBTEXT2,
  NLF_UCOMP_TEXT,
  NLF_UCOMP_SUBTEXT1,
  NLF_UCOMP_SUBTEXT1_NO_INST_TYPES,
  NLF_UCOMP_SUBTEXT2,
  NLF_DIR_TEXT,
  NLF_DIR_SUBTEXT,
  NLF_DIR_BROWSETEXT,
  NLF_UDIR_TEXT,
  NLF_UDIR_SUBTEXT,
  NLF_UDIR_BROWSETEXT,
  NLF_SPACE_AVAIL,
  NLF_SPACE_REQ,
  NLF_UNINST_TEXT,
  NLF_UNINST_SUBTEXT,
  NLF_FILE_ERROR,
  NLF_FILE_ERROR_NOIGNORE,
  NLF_CANT_WRITE,
  NLF_COPY_FAILED,
  NLF_COPY_TO,
  NLF_REGISTERING,
  NLF_UNREGISTERING,
  NLF_SYMBOL_NOT_FOUND,
  NLF_COULD_NOT_LOAD,
  NLF_CREATE_DIR,
  NLF_CREATE_SHORTCUT,
  NLF_CREATED_UNINST,
  NLF_DEL_FILE,
  NLF_DEL_ON_REBOOT,
  NLF_ERR_CREATING_SHORTCUT,
  NLF_ERR_CREATING,
  NLF_ERR_DECOMPRESSING,
  NLF_ERR_REG_DLL,
  NLF_EXEC_SHELL,
  NLF_EXEC,
  NLF_EXTRACT,
  NLF_ERR_WRITING,
  NLF_INST_CORRUPTED,
  NLF_NO_OLE,
  NLF_OUTPUT_DIR,
  NLF_REMOVE_DIR,
  NLF_RENAME_ON_REBOOT,
  NLF_RENAME,
  NLF_SKIPPED,
  NLF_COPY_DETAILS,
  NLF_LOG_INSTALL_PROCESS,
  NLF_BYTE,
  NLF_KILO,
  NLF_MEGA,
  NLF_GIGA,

  NLF_STRINGS_NO_SPECIAL,

  NLF_FONT = NLF_STRINGS_NO_SPECIAL,
  NLF_FONTSIZE,
  NLF_RTL,

  NLF_STRINGS
};

struct NLF {
    bool          m_bLoaded;
    char         *m_szName;
    char         *m_szFont;
    int           m_iFontSize;
    unsigned int  m_uCodePage;
    bool          m_bRTL;

    char         *m_szStrings[NLF_STRINGS];
};

struct LanguageTable {
  LANGID lang_id;

  int dlg_offset;

  StringsArray *lang_strings;

  NLF nlf;
};

#endif
