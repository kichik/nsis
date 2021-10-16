/*
 * lang.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 * 
 * Unicode support and Doxygen comments by Jim Park -- 07/25/2007
 */

#include "Platform.h"
#include <stdio.h>
#include <stdlib.h>
#include "tchar.h"
#include "build.h"
#include "util.h"
#include "DialogTemplate.h"
#include "exehead/resource.h"
#include <nsis-version.h>
#include "tstring.h"
#include "utf.h"

using namespace std;

// Default English strings. Should match NSIS_DEFAULT_LANG
// Do not change the first string in every item, it's the LangString
// name for usage in scripts.

typedef enum {
  NONE_STATIC = 0,
  INSTALL_STATIC = 1,
  UNINSTALL_STATIC = 2,
  BOTH_STATIC = 3
} STATICID;

struct NLFString {
  const TCHAR *szLangStringName;
  const TCHAR *szDefault;
  STATICID eStaticID;
};

NLFString NLFStrings[NLF_STRINGS] = {
  {_T("^Branding"), _T("Nullsoft Install System %s"), BOTH_STATIC},
  {_T("^SetupCaption"), _T("$(^Name) Setup"), INSTALL_STATIC},
  {_T("^UninstallCaption"), _T("$(^Name) Uninstall"), UNINSTALL_STATIC},
  {_T("^LicenseSubCaption"), _T(": License Agreement"), NONE_STATIC},
  {_T("^ComponentsSubCaption"), _T(": Installation Options"), NONE_STATIC},
  {_T("^DirSubCaption"), _T(": Installation Folder"), NONE_STATIC},
  {_T("^InstallingSubCaption"), _T(": Installing"), NONE_STATIC},
  {_T("^CompletedSubCaption"), _T(": Completed"), NONE_STATIC},
  {_T("^UnComponentsSubCaption"), _T(": Uninstallation Options"), NONE_STATIC},
  {_T("^UnDirSubCaption"), _T(": Uninstallation Folder"), NONE_STATIC},
  {_T("^ConfirmSubCaption"), _T(": Confirmation"), NONE_STATIC},
  {_T("^UninstallingSubCaption"), _T(": Uninstalling"), NONE_STATIC},
  {_T("^UnCompletedSubCaption"), _T(": Completed"), NONE_STATIC},
  {_T("^BackBtn"), _T("< &Back"), NONE_STATIC},
  {_T("^NextBtn"), _T("&Next >"), NONE_STATIC},
  {_T("^AgreeBtn"), _T("I &Agree"), NONE_STATIC},
  {_T("^AcceptBtn"), _T("I &accept the terms of the License Agreement"), NONE_STATIC},
  {_T("^DontAcceptBtn"), _T("I &do not accept the terms of the License Agreement"), NONE_STATIC},
  {_T("^InstallBtn"), _T("&Install"), NONE_STATIC},
  {_T("^UninstallBtn"), _T("&Uninstall"), NONE_STATIC},
  {_T("^CancelBtn"), _T("Cancel"), NONE_STATIC},
  {_T("^CloseBtn"), _T("&Close"), NONE_STATIC},
  {_T("^BrowseBtn"), _T("B&rowse..."), NONE_STATIC},
  {_T("^ShowDetailsBtn"), _T("Show &details"), NONE_STATIC},
  {_T("^ClickNext"), _T("Click Next to continue."), NONE_STATIC},
  {_T("^ClickInstall"), _T("Click Install to start the installation."), NONE_STATIC},
  {_T("^ClickUninstall"), _T("Click Uninstall to start the uninstallation."), NONE_STATIC},
  {_T("^Name"), _T("Name"), BOTH_STATIC},
  {_T("^NameDA"), 0, NONE_STATIC}, // virtual
  {_T("^Completed"), _T("Completed"), NONE_STATIC},
  {_T("^LicenseText"), _T("Please review the license agreement before installing $(^NameDA). If you accept all terms of the agreement, click I Agree."), NONE_STATIC},
  {_T("^LicenseTextCB"), _T("Please review the license agreement before installing $(^NameDA). If you accept all terms of the agreement, click the check box below. $_CLICK"), NONE_STATIC},
  {_T("^LicenseTextRB"), _T("Please review the license agreement before installing $(^NameDA). If you accept all terms of the agreement, select the first option below. $_CLICK"), NONE_STATIC},
  {_T("^UnLicenseText"), _T("Please review the license agreement before uninstalling $(^NameDA). If you accept all terms of the agreement, click I Agree."), NONE_STATIC},
  {_T("^UnLicenseTextCB"), _T("Please review the license agreement before uninstalling $(^NameDA). If you accept all terms of the agreement, click the check box below. $_CLICK"), NONE_STATIC},
  {_T("^UnLicenseTextRB"), _T("Please review the license agreement before uninstalling $(^NameDA). If you accept all terms of the agreement, select the first option below. $_CLICK"), NONE_STATIC},
  {_T("^LicenseData"), 0, NONE_STATIC}, // virtual - not processed
  {_T("^Custom"), _T("Custom"), NONE_STATIC},
  {_T("^ComponentsText"), _T("Check the components you want to install and uncheck the components you don't want to install. $_CLICK"), NONE_STATIC},
  {_T("^ComponentsSubText1"), _T("Select the type of install:"), NONE_STATIC},
  {_T("^ComponentsSubText2_NoInstTypes"), _T("Select components to install:"), NONE_STATIC},
  {_T("^ComponentsSubText2"), _T("Or, select the optional components you wish to install:"), NONE_STATIC},
  {_T("^UnComponentsText"), _T("Check the components you want to uninstall and uncheck the components you don't want to uninstall. $_CLICK"), NONE_STATIC},
  {_T("^UnComponentsSubText1"), _T("Select the type of uninstall:"), NONE_STATIC},
  {_T("^UnComponentsSubText2_NoInstTypes"), _T("Select components to uninstall:"), NONE_STATIC},
  {_T("^UnComponentsSubText2"), _T("Or, select the optional components you wish to uninstall:"), NONE_STATIC},
  {_T("^DirText"), _T("Setup will install $(^NameDA) in the following folder. To install in a different folder, click Browse and select another folder. $_CLICK"), NONE_STATIC},
  {_T("^DirSubText"), _T("Destination Folder"), NONE_STATIC},
  {_T("^DirBrowseText"), _T("Select the folder to install $(^NameDA) in:"), NONE_STATIC},
  {_T("^UnDirText"), _T("Setup will uninstall $(^NameDA) from the following folder. To uninstall from a different folder, click Browse and select another folder. $_CLICK"), NONE_STATIC},
  {_T("^UnDirSubText"), _T(""), NONE_STATIC},
  {_T("^UnDirBrowseText"), _T("Select the folder to uninstall $(^NameDA) from:"), NONE_STATIC},
  {_T("^SpaceAvailable"), _T("Space available: "), BOTH_STATIC},
  {_T("^SpaceRequired"), _T("Space required: "), BOTH_STATIC},
  {_T("^UninstallingText"), _T("This wizard will uninstall $(^NameDA) from your computer. $_CLICK"), NONE_STATIC},
  {_T("^UninstallingSubText"), _T("Uninstalling from:"), NONE_STATIC},
  {_T("^FileError"), _T("Error opening file for writing: \r\n\r\n$0\r\n\r\nClick Abort to stop the installation,\r\nRetry to try again, or\r\nIgnore to skip this file."), NONE_STATIC},
  {_T("^FileError_NoIgnore"), _T("Error opening file for writing: \r\n\r\n$0\r\n\r\nClick Retry to try again, or\r\nCancel to stop the installation."), NONE_STATIC},
  {_T("^CantWrite"), _T("Can't write: "), BOTH_STATIC},
  {_T("^CopyFailed"), _T("Copy failed"), BOTH_STATIC},
  {_T("^CopyTo"), _T("Copy to "), BOTH_STATIC},
  {_T("^Registering"), _T("Registering: "), NONE_STATIC},
  {_T("^Unregistering"), _T("Unregistering: "), NONE_STATIC},
  {_T("^SymbolNotFound"), _T("Could not find symbol: "), BOTH_STATIC},
  {_T("^CouldNotLoad"), _T("Could not load: "), BOTH_STATIC},
  {_T("^CreateFolder"), _T("Create folder: "), BOTH_STATIC},
  {_T("^CreateShortcut"), _T("Create shortcut: "), BOTH_STATIC},
  {_T("^CreatedUninstaller"), _T("Created uninstaller: "), BOTH_STATIC},
  {_T("^Delete"), _T("Delete file: "), BOTH_STATIC},
  {_T("^DeleteOnReboot"), _T("Delete on reboot: "), BOTH_STATIC},
  {_T("^ErrorCreatingShortcut"), _T("Error creating shortcut: "), BOTH_STATIC},
  {_T("^ErrorCreating"), _T("Error creating: "), BOTH_STATIC},
  {_T("^ErrorDecompressing"), _T("Error decompressing data! Corrupted installer?"), BOTH_STATIC},
  {_T("^ErrorRegistering"), _T("Error registering DLL"), BOTH_STATIC},
  {_T("^ExecShell"), _T("ExecShell: "), BOTH_STATIC},
  {_T("^Exec"), _T("Execute: "), BOTH_STATIC},
  {_T("^Extract"), _T("Extract: "), BOTH_STATIC},
  {_T("^ErrorWriting"), _T("Extract: error writing to file "), BOTH_STATIC},
  {_T("^InvalidOpcode"), _T("Installer corrupted: invalid opcode"), BOTH_STATIC},
  {_T("^NoOLE"), _T("No OLE for: "), BOTH_STATIC},
  {_T("^OutputFolder"), _T("Output folder: "), BOTH_STATIC},
  {_T("^RemoveFolder"), _T("Remove folder: "), BOTH_STATIC},
  {_T("^RenameOnReboot"), _T("Rename on reboot: "), BOTH_STATIC},
  {_T("^Rename"), _T("Rename: "), BOTH_STATIC},
  {_T("^Skipped"), _T("Skipped: "), BOTH_STATIC},
  {_T("^CopyDetails"), _T("Copy Details To Clipboard"), BOTH_STATIC},
  {_T("^LogInstall"), _T("Log install process"), BOTH_STATIC},
  {_T("^Byte"), _T("B"), BOTH_STATIC},
  {_T("^Kilo"), _T(" K"), BOTH_STATIC},
  {_T("^Mega"), _T(" M"), BOTH_STATIC},
  {_T("^Giga"), _T(" G"), BOTH_STATIC},
  {_T("^Font"), _T("MS Shell Dlg"), NONE_STATIC},
  {_T("^FontSize"), _T("8"), NONE_STATIC},
  {_T("^RTL"), _T("0"), NONE_STATIC},
  {_T("^Language"), _T("English"), NONE_STATIC}
};

// ==============
// LangStringList
// ==============


int LangStringList::add(const TCHAR *name, int *sn/*=0*/)
{
  int pos = SortedStringListND<struct langstring>::add(name);
  if (pos == -1) return -1;

  langstring* lstrPtr = (langstring*)(m_gr.get()) + pos;
  lstrPtr->sn = m_count;
  if (sn) *sn = m_count;
  m_count++;
  lstrPtr->index   = -1;
  lstrPtr->uindex  = -1;
  lstrPtr->process = 1;

  return pos;
}

int LangStringList::get(const TCHAR *name, int *sn/*=0*/, int *index/*=0*/, int *uindex/*=0*/, int *process/*=0*/)
{
  if (index)  *index  = -1;
  if (uindex) *uindex = -1;
  if (sn) *sn = -1;
  int v=find(name);
  if (v==-1) return -1;
  langstring* lstrPtr = (langstring*)(m_gr.get()) + v;
  if (index) *index     = lstrPtr->index;
  if (uindex) *uindex   = lstrPtr->uindex;
  if (sn) *sn           = lstrPtr->sn;
  if (process) *process = lstrPtr->process;
  return v;
}

void LangStringList::set(int pos, int index/*=-1*/, int uindex/*=-1*/, int process/*=-1*/)
{
  if ((unsigned int)pos > (m_gr.getlen() / sizeof(struct langstring)))
    return;

  struct langstring *data=((struct langstring *) m_gr.get()) + pos;

  if (index >= 0)   data->index   = index;
  if (uindex >= 0)  data->uindex  = uindex;
  if (process >= 0) data->process = process;
}

void LangStringList::set(const TCHAR *name, int index, int uindex/*=-1*/, int process/*=-1*/)
{
  set(find(name), index, uindex, process);
}

const TCHAR* LangStringList::pos2name(int pos)
{
  struct langstring *data=(struct langstring *) m_gr.get();

  if (pos < 0 || (unsigned int)pos > (m_gr.getlen() / sizeof(struct langstring)))
    return 0;

  return ((const TCHAR*) m_strings.get() + data[pos].name);
}

const TCHAR* LangStringList::offset2name(int name)
{
  if (name < 0 || (unsigned int)name > m_strings.getlen()/sizeof(TCHAR))
    return 0;

  return (const TCHAR*) m_strings.get() + name;
}

int LangStringList::getnum()
{
  return m_gr.getlen() / sizeof(struct langstring);
}

int LangStringList::compare_index(const void *item1, const void *item2)
{
  struct langstring *ls1 = (struct langstring *)item1;
  struct langstring *ls2 = (struct langstring *)item2;

  return ls1->index - ls2->index;
}

langstring* LangStringList::sort_index(int *num)
{
  if (!num) return 0;
  m_sortbuf.resize(0);
  m_sortbuf.add(m_gr.get(), m_gr.getlen());
  *num = m_sortbuf.getlen() / sizeof(struct langstring);
  qsort(m_sortbuf.get(), *num, sizeof(struct langstring), compare_index);
  return (struct langstring*) m_sortbuf.get();
}

int LangStringList::compare_uindex(const void *item1, const void *item2)
{
  struct langstring *ls1 = (struct langstring *)item1;
  struct langstring *ls2 = (struct langstring *)item2;

  return ls1->uindex - ls2->uindex;
}

langstring* LangStringList::sort_uindex(int *num)
{
  if (!num) return 0;
  m_sortbuf.resize(0);
  m_sortbuf.add(m_gr.get(), m_gr.getlen());
  *num = m_sortbuf.getlen() / sizeof(struct langstring);
  qsort(m_sortbuf.get(), *num, sizeof(struct langstring), compare_uindex);
  return (struct langstring*) m_sortbuf.get();
}

// ============
// StringsArray
// ============

StringsArray::StringsArray()
{
  // We make zero an invalid index.  Using 0 will get back an empty string.
  m_offsets.set_zeroing(1);

  m_strings.add(_T(""), sizeof(_T("")));
}

void StringsArray::resize(int num)
{
  m_offsets.resize(num * sizeof(int));
}

int StringsArray::set(int idx, const TCHAR *str)
{
  if (idx < 0)
    return 0;

  if (idx >= (int)(m_offsets.getlen() / sizeof(int)))
    resize(idx+1);

  int old = ((int*) m_offsets.get())[idx];

  // Need to store the TCHAR index so we divide the return value of add by sizeof(TCHAR).
  ((int*)m_offsets.get())[idx] = m_strings.add(str, (DWORD)(_tcslen(str)+1)*sizeof(TCHAR))/sizeof(TCHAR);

  return old;
}

const TCHAR* StringsArray::get(int idx)
{
  if (idx < 0 || (unsigned int)idx >= (m_offsets.getlen() / sizeof(int)))
    return 0;

  return (const TCHAR *) m_strings.get() + ((int*) m_offsets.get())[idx];
}

// =========
// CEXEBuild
// =========

void CEXEBuild::InitLangTables() {
  keep_ref = false;

  for (int i = 0; i < NLF_STRINGS; i++) {
    NLFRefs[i].iRef = 0;
    NLFRefs[i].iUnRef = 0;

#ifdef NSIS_CONFIG_LOG
    if (i == NLF_NAME) {
      NLFRefs[i].iRef++;
      NLFRefs[i].iUnRef++;
    }
#endif

    if (NLFStrings[i].eStaticID & INSTALL_STATIC) {
      set_uninstall_mode(0);
      DefineLangString(NLFStrings[i].szLangStringName);
    }

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (NLFStrings[i].eStaticID & UNINSTALL_STATIC) {
      set_uninstall_mode(1);
      DefineLangString(NLFStrings[i].szLangStringName);
    }
#endif
  }

  set_uninstall_mode(0);

  keep_ref = true;
}

//////////////////////////////////////////////////////////////////////////////
//                           class CEXEBuild
//
// Note: The functions below refer to the methods related to Languages.
//////////////////////////////////////////////////////////////////////////////

LanguageTable* CEXEBuild::GetLangTable(LANGID &lang, bool create/*=true*/) {
  int nlt = lang_tables.getlen() / sizeof(LanguageTable);
  LanguageTable *nla = (LanguageTable*)lang_tables.get();

  lang = lang ? lang : last_used_lang;
  LanguageTable *table = NULL;

  for (int i = 0; i < nlt; i++) {
    if (lang == nla[i].lang_id) {
      table = &nla[i];
      break;
    }
  }
  if (!table && create) {
    LanguageTable newtable;

    newtable.lang_id = lang;
    newtable.dlg_offset = 0;
    memset(&newtable.nlf, 0, sizeof(NLF));

    newtable.lang_strings = new StringsArray;

    lang_tables.add(&newtable, sizeof(LanguageTable));
    table = (LanguageTable*)lang_tables.get() + nlt;
  }

  if (table) // update last used language if a table was loaded
    last_used_lang = lang;

  return table;
}

const TCHAR *CEXEBuild::GetLangNameAndCP(LANGID lang, unsigned int *codepage/*=NULL*/) {
  LanguageTable *table = GetLangTable(lang, false);

  if (table && table->nlf.m_bLoaded) {
    if (codepage)
      *codepage = table->nlf.m_uCodePage;

    return table->nlf.m_szName;
  }
  else {
    // If the language table does not exist, then we default to Unicode or ANSI
    // depending on the target installer type
    if (codepage)
      *codepage = build_unicode ? 1200 : 1252; // Unicode or CP1252

    if (lang == 1033)
      return _T("English");
    else
      return _T("???");
  }
}

const TCHAR *CEXEBuild::GetLangNameAndCPForVersionResource(LANGID &lang, unsigned int *codepage/*=NULL*/, bool deflangfallback/*=true*/) {
  const TCHAR *langname = GetLangNameAndCP(lang, codepage);
  if (0 == lang) {
    if (deflangfallback)
      lang = last_used_lang;
    else
      langname = _T("Neutral");
  }
  return langname;
}

int CEXEBuild::DefineLangString(const TCHAR *name, int process/*=-1*/) {
  int index, uindex, pos, ret, sn;

  /* If not exist, index and uindex will get -1. */
  pos = build_langstrings.get(name, &sn, &index, &uindex);
  if (pos < 0) {
    pos = build_langstrings.add(name);
  }

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (!this->uninstall_mode) {
#endif
    if (index < 0) {
      // Did not exist.  Increment.
      index = this->build_langstring_num++;
    }
    ret = -index - 1;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  }
  else {
    if (uindex < 0) {
      // Did not exist.  Increment.
      uindex = this->ubuild_langstring_num++;
    }
    ret = -uindex - 1;
  }
#endif

  // Now set the new index and uindex values with the
  // passed in process value.
  build_langstrings.set(pos, index, uindex, process);

  // set reference count for NLF strings
  if (this->keep_ref && name[0] == _T('^')) {
    for (int i = 0; i < NLF_STRINGS; i++) {
      if (!_tcscmp(name, NLFStrings[i].szLangStringName)) {
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
        if (this->uninstall_mode)
          this->NLFRefs[i].iUnRef++;
        else
#endif
          this->NLFRefs[i].iRef++;

        break;
      }
    }
  }

  return ret;
}

int CEXEBuild::DefineInnerLangString(int id, int process/*=-1*/) {
  bool old_keep_ref = keep_ref;

  // set reference count for NLF strings
  if (keep_ref) {
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (uninstall_mode)
      NLFRefs[id].iUnRef++;
    else
#endif
      NLFRefs[id].iRef++;

    keep_ref = false;
  }

  int ret = DefineLangString(NLFStrings[id].szLangStringName, process);

  keep_ref = old_keep_ref;

  return ret;
}

// A LangString is a string variable that varies in value depending on what
// language is being used.  This function sets the string value for the
// variable 'name' for a given language ID.
// 
// @return If the language id, the variable name or string is invalid, it will
// return a PS_ERROR.  If this function call is overwriting a set user string,
// this will return a PS_WARNING.
int CEXEBuild::SetLangString(const TCHAR *name, LANGID lang, const TCHAR *str, BOOL LicenseData) {
  if (!str || !name) return PS_ERROR;

  LanguageTable *table = GetLangTable(lang);
  if (!table) return PS_ERROR;

  int sn;

  if (!LicenseData && _tcsclen(str) > NSIS_MAX_STRLEN-1)
    warning_fl(DW_LANGSTRING_OVERLONGLENGTH, _T("LangString \"%") NPRIs _T("\" longer than NSIS_MAX_STRLEN!"), name);

  int pos = build_langstrings.get(name, &sn);
  if (pos < 0)
    pos = build_langstrings.add(name, &sn);

  if (table->lang_strings->set(sn, str))
    return PS_WARNING;

  return PS_OK;
}

int CEXEBuild::SetLangString(const TCHAR *name, LANGID lang, const TCHAR *str)
{
  return SetLangString(name, lang, str, false);
}


// Sets the user string to the specific NLF_STRINGS id.
//
// @return If the id is invalid or the string is not valid, it will return a
// PS_ERROR.  If this function call is overwriting a set user string, this
// will return a PS_WARNING.
int CEXEBuild::SetInnerString(int id, const TCHAR *str) {
  if ((unsigned int)id >= NLF_STRINGS || !str) return PS_ERROR;

  int ret = PS_OK;

  const TCHAR *ps = UserInnerStrings.get(id);
  if (ps && *ps)
    ret = PS_WARNING;

  UserInnerStrings.set(id, str);

  return ret;
}

int CEXEBuild::GenerateLangTable(LanguageTable *lt, int num_lang_tables) {
  // Add all installer language strings
  int i, j, l, tabsset;
  struct langstring* lang_strings = NULL;
  TinyGrowBuf *string_ptrs = new TinyGrowBuf[num_lang_tables];

  tabsset = 1;
  while (tabsset)
  {
    tabsset = 0;
    for (i = num_lang_tables; i--; )
    {
      // Fill in default values for all used language strings that we can
      FillLanguageTable(&lt[i]);
      // Make sure the string lists are large enough
      string_ptrs[i].set_zeroing(1);
      if (!uninstall_mode)
        string_ptrs[i].resize(build_langstring_num * sizeof(int));
      else
        string_ptrs[i].resize(ubuild_langstring_num * sizeof(int));
    }

    // For all current language strings
    if (!uninstall_mode)
      lang_strings = build_langstrings.sort_index(&l);
    else
      lang_strings = build_langstrings.sort_uindex(&l);

    for (j = 0; j < l; j++)
    {
      int lang_string_index;

      if (!uninstall_mode)
        lang_string_index = lang_strings[j].index;
      else
        lang_string_index = lang_strings[j].uindex;

      // Is this language string used (in the installer)?
      if (lang_string_index >= 0)
      {
        // For each language
        for (i = num_lang_tables; i--; )
        {
          // Get the current string pointer
          int *ptr = (int *)string_ptrs[i].get() + lang_string_index;
          // Not already set?
          if (!*ptr)
          {
            // Get the language string and its name
            const TCHAR *str = lt[i].lang_strings->get(lang_strings[j].sn);
            const TCHAR *lsn = build_langstrings.offset2name(lang_strings[j].name);
            // lsn = variable name, str = value
            if (!str || !*str)
            {
              // No string is defined; give a warning (for user strings only)
              if (lsn[0] != _T('^'))
              {
                if (lt[i].nlf.m_bLoaded)
                  warning(DW_LANGSTRING_NOTSETINLANG, _T("LangString \"%") NPRIs _T("\" is not set in language table of language %") NPRIs, lsn, lt[i].nlf.m_szName);
                else
                  warning(DW_LANGSTRING_NOTSETINLANG, _T("LangString \"%") NPRIs _T("\" is not set in language table of language %d"), lsn, lt[i].lang_id);
              }
            }
            else
            {
              // Add the language string to the string data block
              TCHAR fn[1024];
              _stprintf(fn, _T("LangString %") NPRIs, lsn);
              curfilename = fn;
              linecnt = lt[i].lang_id;
              *ptr = add_string(str, lang_strings[j].process, (WORD) lt[i].nlf.m_uCodePage);
              curfilename = 0;
              // Indicate that we should check again for any newly referenced language strings
              tabsset++;
            }
          }
        }
      }
    }
  }

  // Optimize langstrings and check for recursion
  for (i = num_lang_tables; i--; )
  {
    TinyGrowBuf rec;
    int *lst = (int *)string_ptrs[i].get();
    int langstring_num = uninstall_mode ? ubuild_langstring_num : build_langstring_num;

    for (j = 0; j < langstring_num; j++)
    {
      // Does this string reference another language string directly?
      while (lst[j] < 0)
      {
        // Search through list of language string references
        for (l = 0; (unsigned int)l < rec.getlen() / sizeof(int); l++)
        {
          if (((int*)rec.get())[l] == lst[j])
          {
            // We have the index of a recursive language string; now find the name
            const TCHAR *name = _T("(unnamed)");
            for (l = 0; l < langstring_num; l++)
            {
              int recidx = uninstall_mode ? lang_strings[l].uindex : lang_strings[l].index;
              if (recidx == j)
              {
                name = build_langstrings.offset2name(lang_strings[l].name);
              }
            }
            ERROR_MSG(_T("Error: LangString %") NPRIs _T(" in language %u is recursive!\n"), name, lt->lang_id);
            delete [] string_ptrs;
            return PS_ERROR;
          }
        }
        // Add this reference to the list
        rec.add(&lst[j], sizeof(int));
        // and dereference it
        lst[j] = lst[-lst[j] - 1];
      }
      rec.resize(0);
    }
  }

  // Add language tables into their datablock
  for (i = num_lang_tables; i--; )
  {
    cur_langtables->add(&lt[i].lang_id, sizeof(LANGID));
    cur_langtables->add(&lt[i].dlg_offset, sizeof(int));
    int rtl = lt[i].nlf.m_bRTL ? 1 : 0;
    cur_langtables->add(&rtl, sizeof(int));
    cur_langtables->add(string_ptrs[i].get(), string_ptrs[i].getlen());
    string_ptrs[i].resize(0);
  }

  cur_header->blocks[NB_LANGTABLES].num = num_lang_tables;
  cur_header->langtable_size = cur_langtables->getlen() / num_lang_tables;

  delete [] string_ptrs;

  return PS_OK;
}

int CEXEBuild::GenerateLangTables() {
  int i;
  LanguageTable *lt = (LanguageTable*)lang_tables.get();

  SCRIPT_MSG(_T("Generating language tables... "));

  if (
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      ubuild_langstring_num > MAX_CODED ||
#endif
      build_langstring_num > MAX_CODED
     )
  {
    ERROR_MSG(_T("\nError: too many LangStrings. Maximum allowed is %u.\n"), MAX_CODED);
    return PS_ERROR;
  }

  // If we have no tables (user didn't set any string and didn't load any NLF) create the default one
  if (!lang_tables.getlen()) {
    LANGID lang = NSIS_DEFAULT_LANG;
    LanguageTable *table = GetLangTable(lang);
    if (!table) return PS_ERROR;

    lt = (LanguageTable*)lang_tables.get();
  }

  // Apply default font
  if (*build_font)
  {
    try {
      init_res_editor();

#define ADD_FONT(id) { \
        BYTE* dlg = res_editor->GetResource(RT_DIALOG, id, NSIS_DEFAULT_LANG); \
        if (dlg) { \
          CDialogTemplate td(dlg,build_unicode); \
          res_editor->FreeResource(dlg); \
          td.SetFont(build_font, (WORD) build_font_size); \
          DWORD dwSize; \
          dlg = td.Save(dwSize); \
          res_editor->UpdateResource(RT_DIALOG, id, NSIS_DEFAULT_LANG, dlg, dwSize); \
          td.FreeSavedTemplate(dlg); \
        } \
      }

#ifdef NSIS_CONFIG_LICENSEPAGE
      ADD_FONT(IDD_LICENSE);
      ADD_FONT(IDD_LICENSE_FSRB);
      ADD_FONT(IDD_LICENSE_FSCB);
#endif
      ADD_FONT(IDD_DIR);
#ifdef NSIS_CONFIG_COMPONENTPAGE
      ADD_FONT(IDD_SELCOM);
#endif
      ADD_FONT(IDD_INST);
      ADD_FONT(IDD_INSTFILES);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      ADD_FONT(IDD_UNINST);
#endif
#ifdef NSIS_CONFIG_CRC_SUPPORT
      ADD_FONT(IDD_VERIFY);
#endif
#undef ADD_FONT
    }
    catch (exception& err) {
      ERROR_MSG(_T("\nError while applying font: %") NPRIs _T("\n"), CtoTStrParam(err.what()));
      return PS_ERROR;
    }
  }

  // Fill tables with defaults (if needed) and with instruction strings
  // Create language specific resources (currently only dialogs with different fonts)
  int num_lang_tables = lang_tables.getlen() / sizeof(LanguageTable);
  // if there is one string table then there is no need for two sets of dialogs
  int cur_offset = num_lang_tables == 1 ? 0 : 100;
  for (i = 0; i < num_lang_tables; i++)
  {
    // A Unicode-only language is never displayed correctly by ANSI exehead
    if (!build_unicode && 1200 == lt[i].nlf.m_uCodePage)
    {
      ERROR_MSG(_T("\nError: Unicode-only language %") NPRIs _T(" cannot be used in ANSI installer!\n"), lt[i].nlf.m_szName);
      return PS_ERROR;
    }

    if ((lt[i].nlf.m_szFont && !*build_font) || lt[i].nlf.m_bRTL)
    {
      lt[i].dlg_offset = cur_offset;

      TCHAR *font = lt[i].nlf.m_szFont;
      if (*build_font) font = 0;

      try {
        init_res_editor();

#define ADD_FONT(id) { \
          BYTE* dlg = res_editor->GetResource(RT_DIALOG, id, NSIS_DEFAULT_LANG); \
          if (dlg) { \
            CDialogTemplate td(dlg,build_unicode,lt[i].nlf.m_uCodePage); \
            res_editor->FreeResource(dlg); \
            if (font) td.SetFont(font, (WORD) lt[i].nlf.m_iFontSize); \
            if (lt[i].nlf.m_bRTL) { \
              td.ConvertToRTL(); \
              DialogItemTemplate* dir = td.GetItem(IDC_DIR); \
              if (id == IDD_DIR && dir) { \
                if ((dir->dwStyle & ES_CENTER) == 0) dir->dwStyle ^= ES_RIGHT; \
                dir->dwExtStyle &= ~(WS_EX_RTLREADING | WS_EX_LEFTSCROLLBAR); \
              } \
            } \
            DWORD dwSize; \
            dlg = td.Save(dwSize); \
            res_editor->UpdateResource(RT_DIALOG, id+cur_offset, NSIS_DEFAULT_LANG, dlg, dwSize); \
            td.FreeSavedTemplate(dlg); \
          } \
        }

#ifdef NSIS_CONFIG_LICENSEPAGE
        ADD_FONT(IDD_LICENSE);
        ADD_FONT(IDD_LICENSE_FSRB);
        ADD_FONT(IDD_LICENSE_FSCB);
#endif
        ADD_FONT(IDD_DIR);
#ifdef NSIS_CONFIG_COMPONENTPAGE
        ADD_FONT(IDD_SELCOM);
#endif
        ADD_FONT(IDD_INST);
        ADD_FONT(IDD_INSTFILES);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
        ADD_FONT(IDD_UNINST);
#endif
#ifdef NSIS_CONFIG_CRC_SUPPORT
        ADD_FONT(IDD_VERIFY);
#endif
#undef ADD_FONT
      }
      catch (exception& err) {
        ERROR_MSG(_T("\nError while applying NLF font/RTL: %") NPRIs _T("\n"), CtoTStrParam(err.what()));
        return PS_ERROR;
      }

      cur_offset += 100;
    }
  }

  const int orig_uninstall_mode = uninstall_mode;

  set_uninstall_mode(0);
  if (GenerateLangTable(lt, num_lang_tables) != PS_OK)
    return PS_ERROR;

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  set_uninstall_mode(1);
  if (GenerateLangTable(lt, num_lang_tables) != PS_OK)
    return PS_ERROR;
#endif

  set_uninstall_mode(orig_uninstall_mode);

  SCRIPT_MSG(_T("Done!\n"));

  return PS_OK;
}

static void CreatePlatformStrfmt(const TCHAR *templ, TCHAR *out) {
  // NOTE: Only supports plain %s with no options
  for ( ;; out++ )
  {
    *out = *templ++;
    if (!*out) break;
    if (*out == L'%' && *templ == L's')
    {
      out++, templ++;
      size_t cch = my_strncpy(out, NPRIs, -1);
      out += --cch; // --cch because for loop does out++
    }
  }
}

void CEXEBuild::FillLanguageTable(LanguageTable *table) {
  for (int i = 0; i < NLF_STRINGS; i++) {
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (!NLFRefs[i].iUnRef && !NLFRefs[i].iRef)
      continue;
#else
    if (!NLFRefs[i].iRef)
      continue;
#endif

    else if (i == NLF_SPACE_REQ || i == NLF_SPACE_AVAIL)
    {
      if (no_space_texts)
      {
        continue;
      }
    }

    int sn, index;
    int pos = build_langstrings.get(NLFStrings[i].szLangStringName, &sn, &index);
    if (pos >= 0) {
      const TCHAR *str = table->lang_strings->get(sn);
      if (!str || !*str) {
        const TCHAR *us = UserInnerStrings.get(i);
        if (i == NLF_NAME_DA && (!us || !*us))
        {
          // if the user didn't set NLF_NAME_DA we set it to $(^Name)
          table->lang_strings->set(sn, _T("$(^Name)"));
        }
        if (us && *us) {
          table->lang_strings->set(sn, (TCHAR *) us);
        }
        else {
          const TCHAR *dstr = table->nlf.m_szStrings[i] ? table->nlf.m_szStrings[i] : NLFStrings[i].szDefault;
          if (!dstr)
            continue;
          if (i == NLF_BRANDING) {
            TCHAR temp[NSIS_MAX_STRLEN + sizeof(NSIS_VERSION)], temp2[COUNTOF(temp)];
            CreatePlatformStrfmt(dstr, temp2); // Change %s to %ls if required
            _stprintf(temp, temp2, NSIS_VERSION);
            table->lang_strings->set(sn, temp);
            continue;
          }
          else if (i == NLF_FONT)
          {
            TCHAR *font = *build_font ? build_font : table->nlf.m_szFont;
            if (font)
              table->lang_strings->set(sn, font);
            else
              table->lang_strings->set(sn, dstr);
            continue;
          }
          else if (i == NLF_FONTSIZE)
          {
            WORD font_size = *build_font ? (WORD) build_font_size : (WORD) table->nlf.m_iFontSize;
            if (font_size)
            {
              TCHAR temp[64];
              _stprintf(temp, _T("%d"), font_size);
              table->lang_strings->set(sn, temp);
            }
            else
              table->lang_strings->set(sn, dstr);
            continue;
          }
          table->lang_strings->set(sn, dstr);
        }
      }
    }
  }
}

void TrimTrailingNewlines(TCHAR *s) {
    size_t cch = _tcslen(s);
    while (s[cch-1] == _T('\n') || s[cch-1] == _T('\r'))
      s[cch-1] = 0, --cch;
}

static UINT GetNextNLFLine(NStreamLineReader&lr, TCHAR*Buf, UINT cchBuf) {
  for (;;) {
    UINT err = lr.ReadLine(Buf, cchBuf);
    if (NStream::OK != err) {
      if (lr.IsEOF()) err = NStream::OK;
      return err;
    }
    if (NStream::IsNewline(*Buf, false)) continue;
    if (_T('#') != *Buf && _T(';') != *Buf) return err;
  }
}
static inline bool GetNextNLFLine(NStreamLineReader&lr, TCHAR*Buf, UINT cchBuf, UINT&errlr) {
  errlr = GetNextNLFLine(lr, Buf, cchBuf);
  return NStream::OK == errlr;
}

// NSIS Language File parser
LanguageTable * CEXEBuild::LoadLangFile(TCHAR *filename) {
  NIStream strm;
  strm.StreamEncoding().SetCodepage(NStreamEncoding::ACP);
  if (!strm.OpenFileForReading(filename)) {
    ERROR_MSG(_T("Error: Can't open language file - \"%") NPRIs _T("\"!\n"),filename);
    return 0;
  }
  NStreamLineReader lr(strm);
  UINT errlr;

  // Check header
  TCHAR buf[NSIS_MAX_STRLEN];
  if (!GetNextNLFLine(lr, buf, NSIS_MAX_STRLEN, errlr)) {
l_readerr:
    ERROR_MSG(lr.GetErrorMessage(errlr).c_str());
    return 0;
  }
  if (_tcsncmp(buf, _T("NLF v"), 5)) {
    ERROR_MSG(_T("Error: Invalid language file.\n"));
    return 0;
  }
  int nlf_version = _ttoi(buf+5);
  if (nlf_version != NLF_VERSION) {
    if (nlf_version != 2 && nlf_version != 3 && nlf_version != 4 && nlf_version != 5) {
      ERROR_MSG(_T("Error: Language file version doesn't match NSIS version.\n"));
      return 0;
    }
  }

  // Get language ID
  if (!GetNextNLFLine(lr, buf, NSIS_MAX_STRLEN, errlr)) goto l_readerr;
  LANGID lang_id = _ttoi(buf);

  // Get appropriate table
  LanguageTable *table = GetLangTable(lang_id);
  if (!table) return 0;

  NLF *nlf = &table->nlf;
  if (nlf->m_bLoaded) {
    ERROR_MSG(_T("Error: can't load same language file twice.\n"));
    return 0;
  }

  // Generate language name
  TCHAR *p, *p2, *p3, t = 0;

  p = _tcsrchr(filename, _T('.'));
  if (p) {
    t = *p;
    *p = 0;
  }
  p2 = _tcsrchr(filename, _T('\\')), p3 = _tcsrchr(filename, _T('/'));
  if (p3 > p2) p2 = p3;
  if (p2) {
    p2++;
    nlf->m_szName = (TCHAR*)malloc((_tcslen(p2)+1)*sizeof(TCHAR));
    _tcscpy(nlf->m_szName, p2);
  }
  else {
    nlf->m_szName = (TCHAR*)malloc((_tcslen(filename)+1)*sizeof(TCHAR));
    _tcscpy(nlf->m_szName, filename);
  }
  if (p) *p = t;

  if (nlf_version != NLF_VERSION) {
    warning_fl(DW_NLF_OLDVERSION, _T("%") NPRIs _T(" language file version doesn't match. Using default English texts for missing strings."), nlf->m_szName);
  }

  // set ^Language
  nlf->m_szStrings[NLF_LANGUAGE] = _tcsdup(nlf->m_szName);

  size_t temp;

  // Get font
  if (!GetNextNLFLine(lr, buf, NSIS_MAX_STRLEN, errlr)) goto l_readerr;
  if (!nlf->m_szFont) {
    TrimTrailingNewlines(buf);
    if (buf[0] != _T('-') || buf[1] != 0) {
      nlf->m_szFont = (TCHAR*)malloc((_tcslen(buf)+1)*sizeof(TCHAR));
      _tcscpy(nlf->m_szFont, buf);
    }
  }

  if (!GetNextNLFLine(lr, buf, NSIS_MAX_STRLEN, errlr)) goto l_readerr;
  if (!nlf->m_iFontSize) {
    if (buf[0] != _T('-') || buf[1] != 0) {
      nlf->m_iFontSize = _ttoi(buf);
    }
  }

  // Get code page
  bool isnlfdataucp = false; // Unicode-only language?
  nlf->m_uCodePage = NSISRT_GetASCIICodepage();
  if (!GetNextNLFLine(lr, buf, NSIS_MAX_STRLEN, errlr)) goto l_readerr;
  TrimTrailingNewlines(buf);
  if (buf[0] != _T('-') || buf[1] != 0) {
    nlf->m_uCodePage = _ttoi(buf);
    isnlfdataucp = NStreamEncoding::IsUnicodeCodepage(nlf->m_uCodePage);
    if (isnlfdataucp && 1200 != nlf->m_uCodePage)
    {
      ERROR_MSG(_T("Error: Unicode-only language files must use codepage 1200!\n"));
      return 0;
    }
    if ((unsigned)nlf->m_uCodePage <= 1 && !lr.IsUnicode()) // Warn if someone uses a system specific codepage
    {
      warning_fl(DW_NLF_SYSCP, _T("%") NPRIs _T(" language file uses the system default codepage!"), nlf->m_szName);
    }
    if (CP_ACP != nlf->m_uCodePage && !isnlfdataucp && !IsValidCodePage(nlf->m_uCodePage))
    {
      warning_fl(DW_NLF_UNSUPPORTED_CP, _T("%") NPRIs _T(" language file uses a codepage (%d) that is not supported on this system, using ACP!"), nlf->m_szName, nlf->m_uCodePage);
      nlf->m_uCodePage = CP_ACP;
    }
  }

  // SVN is not a big fan of UTF16 so we should always use UTF8SIG
  if (isnlfdataucp && !lr.StreamEncoding().IsUTF8())
  {
    warning_fl(DW_NLF_NOT_PREFERRED_ENC, _T("%") NPRIs _T(" Unicode language file is not UTF8SIG."), nlf->m_szName);
  }

#ifdef _UNICODE
  if (!lr.IsUnicode())
  {
    if (nlf->m_szFont)
    {
      // Convert font name now that we know the codepage
      TCHAR* oldfont = nlf->m_szFont;
      nlf->m_szFont = _tcsdup(CtoTString2(TtoCString(oldfont), nlf->m_uCodePage)); // BUGBUG: Depends on lossless ACP -> TCHAR -> NLF CP -> TCHAR conversion!
      free(oldfont);
    }
    lr.StreamEncoding().SetCodepage(nlf->m_uCodePage); // Read the rest of this NLF file with the correct MBCS > TCHAR conversion
  }
#endif

  // Get RTL setting
  if (!GetNextNLFLine(lr, buf, NSIS_MAX_STRLEN, errlr)) goto l_readerr;
  nlf->m_szStrings[NLF_RTL] = (TCHAR*) malloc(2*sizeof(TCHAR));
  nlf->m_bRTL = false;
  if (buf[0] == _T('R') && buf[1] == _T('T') && buf[2] == _T('L') && (!buf[3] || buf[3] == _T('\r') || buf[3] == _T('\n'))) {
    nlf->m_bRTL = true;
  }
  _tcscpy(nlf->m_szStrings[NLF_RTL], nlf->m_bRTL ? _T("1") : _T("0"));

  // Read strings
  for (int i = 0; i < NLF_STRINGS_NO_SPECIAL; i++) {

    // skip virtual strings
    if (!NLFStrings[i].szDefault)
      continue;

    // Fill in for missing strings
    // 0 will mean default will be used from NLFStrings
    switch (i) {
      case NLF_BTN_LICENSE_AGREE:
      case NLF_BTN_LICENSE_DISAGREE:
        if (nlf_version >= 3) break;
      case NLF_LOG_INSTALL_PROCESS:
      case NLF_BYTE:
      case NLF_KILO:
      case NLF_MEGA:
      case NLF_GIGA:
      case NLF_REGISTERING:
      case NLF_UNREGISTERING:
        if (nlf_version >= 4) break;
      case NLF_FILE_ERROR_NOIGNORE:
        if (nlf_version >= 5) break;
      case NLF_USUBCAPTION_OPTIONS:
      case NLF_USUBCAPTION_DIR:
      case NLF_CLICK_NEXT:
      case NLF_CLICK_INSTALL:
      case NLF_CLICK_UNINSTALL:
      case NLF_LICENSE_TEXT:
      case NLF_LICENSE_TEXT_FSCB:
      case NLF_LICENSE_TEXT_FSRB:
      case NLF_ULICENSE_TEXT:
      case NLF_ULICENSE_TEXT_FSCB:
      case NLF_ULICENSE_TEXT_FSRB:
      case NLF_COMP_TEXT:
      case NLF_UCOMP_TEXT:
      case NLF_UCOMP_SUBTEXT1:
      case NLF_UCOMP_SUBTEXT1_NO_INST_TYPES:
      case NLF_UCOMP_SUBTEXT2:
      case NLF_DIR_TEXT:
      case NLF_DIR_BROWSETEXT:
      case NLF_UDIR_TEXT:
      case NLF_UDIR_SUBTEXT:
      case NLF_UDIR_BROWSETEXT:
      case NLF_UNINST_TEXT:
        if (nlf_version >= 6) break;
        nlf->m_szStrings[i] = 0;
        continue; // This applies to the for loop and not this switch!
    }

    errlr = GetNextNLFLine(lr, buf, NSIS_MAX_STRLEN);
    if (_tcslen(buf) == NSIS_MAX_STRLEN-1) {
      ERROR_MSG(_T("Error: String too long (string #%d - \"%") NPRIs _T("\")\n"), i, NLFStrings[i].szLangStringName);
      return 0;
    }
    if (NStream::OK != errlr) goto l_readerr;
    
    temp=_tcslen(buf);
    // Not using TrimTrailingNewlines because we need temp to be correct for the quote trimming
    while (buf[temp-1] == _T('\n') || buf[temp-1] == _T('\r')) {
      buf[--temp] = 0;
    }

    TCHAR *in = buf;

    // trim quotes
    if (buf[0] == _T('"') && buf[temp-1] == _T('"')) {
      in++;
      buf[--temp] = 0;
    }

    nlf->m_szStrings[i] = (TCHAR*)malloc((temp+1)*sizeof(TCHAR));
    TCHAR *out;
    for (out = nlf->m_szStrings[i]; *in; in++, out++) {
      if (*in == _T('\\')) {
        in++;
        switch (*in) {
          case _T('n'):
            *out = _T('\n');
            break;
          case _T('r'):
            *out = _T('\r');
            break;
          case _T('t'):
            *out = _T('\t');
            break;
          default:
            *out++ = _T('\\');
            *out = *in;
        }
      }
      else *out = *in;
    }
    *out = 0;
  }

  nlf->m_bLoaded = true;
  return table;
}

void CEXEBuild::DeleteLangTable(LanguageTable *table) {
  if (table->nlf.m_szName)
    free(table->nlf.m_szName);
  if (table->nlf.m_szFont)
    free(table->nlf.m_szFont);
  delete table->lang_strings;
  for (int i = 0; i < NLF_STRINGS; i++) {
    if (table->nlf.m_szStrings[i])
      free(table->nlf.m_szStrings[i]);
  }
}
