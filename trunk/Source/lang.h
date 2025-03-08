/*
 * lang.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2025 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 * 
 * Unicode support and Doxygen comments by Jim Park -- 07/30/2007
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
    /* Default constructor */
    LangStringList() : m_count(0) {}

    /**
     * Adds a langstring struct with the string name of 'name' into this
     * structure.
     *
     * @param name The string to use as key.
     * @param sn [out] The string number.
     * @return Returns the position where T was stored.
     */
    int add(const TCHAR *name, int *sn=0);

    /**
     * Gets the values in the langstring struct that is mapped to the string
     * 'name'.  Sets sn, index, and uindex to -1 before looking for the
     * 'name'.  If not found, -1 is returned.  If found, then the values
     * associated with 'name' are set to the sn, index, uindex, process
     * variables.
     * 
     * TODO: Need better documentation here.
     * @param sn [out] Set to string ID number.
     * @param index [out] Set to index value in langstring.
     * @param uindex [out] Set to uindex value in langstring.
     * @param process [out] Set to process value in langstring.
     * @return The index into langstring array.  -1 if not found.
     */
    int get(const TCHAR *name, int *sn=0, int *index=0, int *uindex=0, int *process=0);

    /**
     * Sets the values in the langstring struct that is in the position 'pos'.
     *
     * @param pos The langstring index into m_gr.
     * @param index Value to set langstring[pos].index.
     * @param uindex Value to set langstring[pos].uindex.
     * @param process Value to set langstring[pos].process.
     */ 
    void set(int pos, int index=-1, int uindex=-1, int process=-1);

    /**
     * Sets the values in the langstring struct that is mapped to the string
     * 'name'.
     *
     * @param name The string key to lookup langstring.
     * @param index Value to set langstring[pos].index.
     * @param uindex Value to set langstring[pos].uindex.
     * @param process Value to set langstring[pos].process.
     */ 
    void set(const TCHAR *name, int index, int uindex=-1, int process=-1);

    /**
     * From the position index, get the pointer to the key string.
     * Basically, get the string referenced by langstring[pos].name.
     *
     * @param pos The position index.
     * @return The TCHAR* to the string referenced by pos.
     */
    const TCHAR *pos2name(int pos);

    /**
     * From the index into the strings, get the pointer to the
     * key string.  Note: the positional index into the storage of
     * key strings probably should not be exposed to the outside.
     *
     * @param name Index into the m_strings array.
     * @return The TCHAR* to the string referenced by name.
     */
    const TCHAR *offset2name(int name);

    /**
     * Get the number of entries.
     *
     * @return The number of langstring entries.
     */
    int getnum();

    /**
     * Compare two langstring structs pointed by item1 and item2 by looking at
     * their .index values via their difference (item1->index - item2->index).
     *
     * @return 0 if equal, negative value if item1 is smaller, positive value
     * if item1 is bigger.
     */
    static int compare_index(const void *item1, const void *item2);

    /**
     * Sorts the langstrings by their index.  Then return the sorted array
     * via m_sortbuf.  Warning: This function is not thread-safe!
     *
     * @param num [out] Set to the size of langstring items in the array.
     * @return The sorted langstring array via m_sortbuf.
     */
    langstring *sort_index(int *num);

    /**
     * Compare two langstring structs pointed by item1 and item2 by looking at
     * their .uindex values via their difference (item1->uindex - item2->uindex).
     *
     * @return 0 if equal, negative value if item1 is smaller, positive value
     * if item1 is bigger.
     */
    static int compare_uindex(const void *item1, const void *item2);

    /**
     * Sorts the langstrings by their index.  Then return the sorted array
     * via m_sortbuf.  Warning: This function is not thread-safe!
     *
     * @param num [out] Set to the size of langstring items in the array.
     * @return The sorted langstring array via m_sortbuf.
     */
    langstring *sort_uindex(int *num);

  private:
    int m_count;           // Used to set string number (sn)
    TinyGrowBuf m_sortbuf; // Used only to sort.
};

/**
 * This class implements an array of C-style strings in a flat buffer.
 *
 * Implementation: Resetting the string at a particular index does not delete
 * the old string.  Instead a new string is added to the end of m_strings and
 * the old string can no longer be looked up.
 */
class StringsArray
{
  public:
    StringsArray();

    /**
     * Resizes the m_offsets so that the index num is valid.
     *
     * @param num New size.
     */
    void resize(int num);

    /**
     * Set the string 'str' at index idx.  This class cannot really delete
     * strings.  It can "overwrite" them in the sense that the string is no
     * longer referenceable via the index but they are never gone.
     *
     * @param idx The index position to set the string to.
     * @param str The string value to set.
     * @return If overwriting, the position in m_strings of the old string.
     */
    int set(int idx, const TCHAR *str);

    /**
     * Get the string at index 'idx'.
     *
     * @param idx The logical index to the string.
     * @return Returns the TCHAR* to the string.
     */
    const TCHAR *get(int idx);

  private:
    TinyGrowBuf m_offsets; /* Positional offsets of the stored string. */
    GrowBuf     m_strings; /* Storage of the actual strings. */
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
  NLF_LANGUAGE,

  NLF_STRINGS
};

struct NLF {
    bool          m_bLoaded; /* Is the table loaded? */
    TCHAR         *m_szName; /* The language name */
    TCHAR         *m_szFont;
    int           m_iFontSize;
    unsigned int  m_uCodePage; /* Code page associated with language.  When
                                * using Unicode, this value will be 1200.
                                */
    bool          m_bRTL; /* Is this a right-to-left language like Hebrew? */
    TCHAR         *m_szStrings[NLF_STRINGS];
};

/**
 * LanguageTable stores within the lang_strings, all the user strings and
 * variables for that specific language.
 */
struct LanguageTable {
  LANGID lang_id; /* Windows Language ID identifier */

  int dlg_offset;

  StringsArray *lang_strings;

  NLF nlf;
};

#endif
