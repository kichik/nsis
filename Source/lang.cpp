// Lang.cpp by Amir Szekely 3rd August 2002

#include <Windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "build.h"

extern const char *NSIS_VERSION;

extern char *english_strings[] = {
  "Nullsoft Install System %s",
  "%s Setup",
  "%s Uninstall",
  ": License Agreement",
  ": Installation Options",
  ": Installation Directory",
  ": Installing Files",
  ": Completed",
  ": Confirmation",
  ": Uninstalling Files",
  ": Completed",
  "< Back",
  "Next >",
  "I Agree",
  "Install",
  "Uninstall",
  "Cancel",
  "Close",
  "Browse...",
  "Show details",
  "Name",
  "Completed",
  "Custom",
  "Select the type of install:",
  "Select components to install:",
  "Or, select the optional components you wish to install:",
  "Select the directory to install %s in:",
  "Space available: ",
  "Space required: ",
  "Uninstalling from:",
  "Error opening file for writing: \r\n\t\"$0\"\r\nHit abort to abort installation,\r\nretry to retry writing the file, or\r\nignore to skip this file",
  "Can't write: ",
  "Copy failed",
  "Copy to ",
  "Could not find symbol: ",
  "Could not load: ",
  "Create directory: ",
  "Create shortcut: ",
  "Created uninstaller: ",
  "Delete file:",
  "Delete on reboot: ",
  "Error creating shortcut: ",
  "Error creating: ",
  "Error decompressing data! Corrupted installer?",
  "Error registering DLL",
  "ExecShell: ",
  "Execute: ",
  "Extract: ",
  "Extract: error writing to file ",
  "Install corrupted: invalid opcode",
  "No OLE for: ",
  "Output directory: ",
  "Remove directory: ",
  "Rename on reboot: ",
  "Rename: ",
  "Skipped: ",
  "Copy Details To Clipboard"
};

StringTable* CEXEBuild::GetTable(LANGID &lang) {
  lang=lang?lang:last_used_lang;
  last_used_lang=lang;
  StringTable *table = 0;
  for (int i = 0; i < string_tables.size(); i++) {
    if (lang == string_tables[i]->lang_id) {
      table = string_tables[i];
      break;
    }
  }
  if (!table) {
    table = new StringTable;
    if (!table) {
      ERROR_MSG("Internal compiler error #12345: malloc(%d) failed\n",sizeof(StringTable));
      return 0;
    }
    memset(table, 0, sizeof(StringTable)-sizeof(GrowBuf)*2);
    table->lang_id = table->lang_id = lang;
    string_tables.push_back(table);
  }

  return table;
}

int CEXEBuild::SetString(char *string, int id, int process, LANGID lang/*=0*/) {
  StringTable *table = GetTable(lang);
  if (!table) return PS_ERROR;
  return SetString(string, id, process, table);
}

int CEXEBuild::SetString(char *string, int id, int process, StringTable *table) {
  int *str = 0;
  int *ustr = 0;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  #define HANDLE_STRING_C(id,strname) case id: str=&(table->strname); ustr=&(table->u##strname); break;
#else
  #define HANDLE_STRING_C(id,strname) HANDLE_STRING_I(id,strname)
#endif
  #define HANDLE_STRING_I(id,strname) case id: str=&(table->strname); break;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  #define HANDLE_STRING_U(id,strname) case id: ustr=&(table->strname); break;
#endif
  switch (id) {
    HANDLE_STRING_C(NLF_BRANDING, common.branding);
    HANDLE_STRING_C(NLF_BTN_CANCEL, common.cancelbutton);
    HANDLE_STRING_C(NLF_BTN_CLOSE, common.closebutton);
    HANDLE_STRING_C(NLF_BTN_DETAILS, common.showdetailsbutton);
    HANDLE_STRING_C(NLF_COMPLETED, common.completed);
#ifdef NSIS_SUPPORT_FILE
    HANDLE_STRING_C(NLF_FILE_ERROR, common.fileerrtext);
#endif

    HANDLE_STRING_I(NLF_CAPTION, common.caption);
    HANDLE_STRING_I(NLF_SUBCAPTION_LICENSE, common.subcaptions[0]);
    HANDLE_STRING_I(NLF_SUBCAPTION_OPTIONS, common.subcaptions[1]);
    HANDLE_STRING_I(NLF_SUBCAPTION_DIR, common.subcaptions[2]);
    HANDLE_STRING_I(NLF_SUBCAPTION_INSTFILES, common.subcaptions[3]);
    HANDLE_STRING_I(NLF_SUBCAPTION_COMPLETED, common.subcaptions[4]);
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
    HANDLE_STRING_I(NLF_BTN_NEXT, installer.nextbutton);
    HANDLE_STRING_I(NLF_BTN_BACK, installer.backbutton);
#ifdef NSIS_CONFIG_LICENSEPAGE
    HANDLE_STRING_I(NLF_BTN_LICENSE, installer.licensebutton);
#endif
    HANDLE_STRING_I(NLF_BTN_INSTALL, installer.installbutton);
    HANDLE_STRING_I(NLF_BTN_BROWSE, installer.browse);
#ifdef NSIS_CONFIG_COMPONENTPAGE
    HANDLE_STRING_I(NLF_COMP_SUBTEXT1, installer.componentsubtext[0]);
    HANDLE_STRING_I(NLF_COMP_SUBTEXT2, installer.componentsubtext[1]);
#endif
    HANDLE_STRING_I(NLF_COMP_CUSTOM, installer.custom);
    HANDLE_STRING_I(NLF_DIR_SUBTEXT, installer.dirsubtext);
    HANDLE_STRING_I(NLF_SPACE_AVAIL, installer.spaceavailable);
    HANDLE_STRING_I(NLF_SPACE_REQ, installer.spacerequired);
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    HANDLE_STRING_U(NLF_UCAPTION, ucommon.caption);
    HANDLE_STRING_U(NLF_USUBCAPTION_CONFIRM, ucommon.subcaptions[0]);
    HANDLE_STRING_U(NLF_USUBCAPTION_INSTFILES, ucommon.subcaptions[1]);
    HANDLE_STRING_U(NLF_USUBCAPTION_COMPLETED, ucommon.subcaptions[2]);
    HANDLE_STRING_U(NLF_BTN_UNINSTALL, uninstall.uninstbutton);
    HANDLE_STRING_U(NLF_UNINST_SUBTEXT, uninstall.uninstalltext2);
#endif

    HANDLE_STRING_C(LANG_NAME, common.name);

#ifdef NSIS_CONFIG_COMPONENTPAGE
    HANDLE_STRING_I(LANG_COMP_TEXT, installer.componenttext);
#endif

#ifdef NSIS_CONFIG_LICENSEPAGE
    HANDLE_STRING_I(LANG_LICENSE_TEXT, installer.licensetext);
    HANDLE_STRING_I(LANG_LICENSE_DATA, installer.licensedata);
#endif

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
    HANDLE_STRING_I(LANG_DIR_TEXT, installer.text);
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    HANDLE_STRING_U(LANG_UNINST_TEXT, uninstall.uninstalltext);
#endif

    default:
      ERROR_MSG("Error: string doesn't exist or is not changeable (%d)\n", id);
      return PS_ERROR;
  }

  if (str) *str = add_string_main(string,process);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (ustr) *ustr = add_string_uninst(string,process);
#endif

  return PS_OK;
}

int CEXEBuild::SetUserString(char *name, LANGID lang, char *string) {
  StringTable *table = GetTable(lang);
  if (!table) return PS_ERROR;

  GrowBuf *user_strings;
  StringList *user_strings_list;
  bool uninst;
  if (!(uninst = !strnicmp(name,"un.",3))) {
    user_strings=&table->user_strings;
    user_strings_list=&build_userlangstrings;
  }
  else {
    name += 3;
    user_strings=&table->user_ustrings;
    user_strings_list=&ubuild_userlangstrings;
  }

  int idx;
  if (user_strings_list->find(name, 0, &idx) < 0) {
    user_strings_list->add(name, 0);
    user_strings_list->find(name, 0, &idx);
  }

  user_strings->resize((idx+1)*sizeof(int));
  ((int*)user_strings->get())[idx] = uninst ? add_string_uninst(string,1) : add_string_main(string,1);

  for (int j = 0; j < string_tables.size(); j++) {
    if (j == idx) continue;
    if (uninst) {
      if (string_tables[j]->user_ustrings.getlen() < (idx+1)*sizeof(int)) {
        string_tables[j]->user_ustrings.resize((idx+1)*sizeof(int));
        ((int*)string_tables[j]->user_ustrings.get())[idx] = 0;
      }
    }
    else {
      if (string_tables[j]->user_strings.getlen() < (idx+1)*sizeof(int)) {
        string_tables[j]->user_strings.resize((idx+1)*sizeof(int));
        ((int*)string_tables[j]->user_strings.get())[idx] = 0;
      }
    }
  }

  return PS_OK;
}

bool CEXEBuild::_IsSet(int *str, WORD lang) {
  if (!str) return false;
  lang = lang?lang:build_nlfs.size()?build_nlfs[build_nlfs.size()-1]->GetLang():0;
  lang = lang?lang:string_tables.size()?string_tables[0]->lang_id:1033; // Default is English (1033)
  int i;
  for (i = 0; i < string_tables.size(); i++) {
    if (lang == string_tables[i]->lang_id) {
      break;
    }
  }
  if (i == string_tables.size()) return false;
  if (*(int*)(int(str)-int(string_tables[0])+int(string_tables[i]))) return true;
  return false;
}

int CEXEBuild::WriteStringTables() {
  int i;

  SCRIPT_MSG("Generating language tables... ");

  // If we have no tables (user didn't set any string and didn't load any NLF) create the default one
  LANGID lang = 1033;
  StringTable *table = GetTable(lang);
  if (!table) return PS_ERROR;

  // Fill tables with defaults (if needed) and with instruction strings
  int st_num = string_tables.size();
  for (i = 0; i < st_num; i++)
    FillDefaultsIfNeeded(string_tables[i]);

  // Add string tables into their datablock
  for (i = 0; i < st_num; i++) {
    build_langtables.add(&string_tables[i]->lang_id, sizeof(LANGID));
    build_langtables.add(&string_tables[i]->common, sizeof(common_strings));
    build_langtables.add(&string_tables[i]->installer, sizeof(installer_strings));
    if (build_userlangstrings.getnum())
      build_langtables.add(string_tables[i]->user_strings.get(), string_tables[i]->user_strings.getlen());
  }
  build_header.common.str_tables_num = st_num;
  build_header.common.language_table_size = sizeof(LANGID) + sizeof(common_strings) + sizeof(installer_strings) + (build_userlangstrings.getnum() * sizeof(int));

  for (i = 0; i < st_num; i++) {
    ubuild_langtables.add(&string_tables[i]->lang_id, sizeof(LANGID));
    ubuild_langtables.add(&string_tables[i]->ucommon, sizeof(common_strings));
    ubuild_langtables.add(&string_tables[i]->uninstall, sizeof(uninstall_strings));
    if (ubuild_userlangstrings.getnum())
      ubuild_langtables.add(string_tables[i]->user_ustrings.get(), string_tables[i]->user_ustrings.getlen());
  }
  build_uninst.common.str_tables_num = st_num;
  build_uninst.common.language_table_size = sizeof(LANGID) + sizeof(common_strings) + sizeof(uninstall_strings) + (ubuild_userlangstrings.getnum() * sizeof(int));

  SCRIPT_MSG("Done!\n");

  return PS_OK;
}

void CEXEBuild::FillDefaultsIfNeeded(StringTable *table, NLF *nlf/*=0*/) {
  if (!nlf) {
    for (int i = 0; i < build_nlfs.size(); i++) {
      if (build_nlfs[i]->GetLang() == table->lang_id) {
          nlf = build_nlfs[i];
          break;
      }
    }
  }

#define str(id) (nlf?nlf->GetString(id):english_strings[id])

#ifdef NSIS_CONFIG_COMPONENTPAGE
  // if component page, do component strings:
  if (table->installer.componenttext)
  {
    int x;
    int iscp=0;
    for (x = 1; x < build_header.num_sections&&!iscp; x ++)
    {
      char c=build_strlist.get()[((section*)build_sections.get())[x].name_ptr];
      if (c && c != '-') iscp++;
    }
    if (iscp)
    {
      if (!table->installer.custom) table->installer.custom=add_string_main(str(NLF_COMP_CUSTOM),0);
      if (!table->common.subcaptions[1])
        table->common.subcaptions[1]=add_string_main(str(NLF_SUBCAPTION_OPTIONS));

      if (!build_header.install_types_ptr[0])
      {
        if (!table->installer.componentsubtext[1])
          table->installer.componentsubtext[1]=add_string_main(str(NLF_COMP_SUBTEXT1_NO_INST_TYPES),0);
      }
      else
      {
        if (!table->installer.componentsubtext[0])
          table->installer.componentsubtext[0]=add_string_main(str(NLF_COMP_SUBTEXT1),0);
        if (build_header.no_custom_instmode_flag!=1 && !table->installer.componentsubtext[1])
          table->installer.componentsubtext[1]=add_string_main(str(NLF_COMP_SUBTEXT2),0);
      }
    }
    else table->installer.componenttext=0;
  }
#endif

  static bool nameWarned = false;
  if (!table->common.name)
  {
    if (!nameWarned) {
      warning("Name command not specified. Assuming default.");
      nameWarned = true;
    }
    table->common.name=add_string_main(str(NLF_DEF_NAME),0);
    table->ucommon.name=add_string_uninst(str(NLF_DEF_NAME),0);
  }
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT

#ifdef NSIS_CONFIG_LICENSEPAGE
  if (!table->installer.licensedata || !table->installer.licensetext)
  {
    table->installer.licensedata=0;
    table->installer.licensetext=0;
  }

  if (table->installer.licensedata)
  {
    if (!table->common.subcaptions[0])
      table->common.subcaptions[0]=add_string_main(str(NLF_SUBCAPTION_LICENSE));
    if (!table->installer.licensebutton)
      table->installer.licensebutton=add_string_main(str(NLF_BTN_LICENSE),0);
  }
#endif //NSIS_CONFIG_LICENSEPAGE

  if (table->installer.text)
  {
    if (!table->installer.dirsubtext)
    {
      char buf[2048];
      wsprintf(buf,str(NLF_DIR_SUBTEXT),build_strlist.get()+table->common.name);
      table->installer.dirsubtext=add_string_main(buf,0);
    }
    if (!table->common.subcaptions[2])
      table->common.subcaptions[2]=add_string_main(str(NLF_SUBCAPTION_DIR));
    if (!table->installer.browse) table->installer.browse=add_string_main(str(NLF_BTN_BROWSE),0);
    if (!table->installer.spaceavailable && !no_space_texts) table->installer.spaceavailable=add_string_main(str(NLF_SPACE_AVAIL),0);
  }

  if (table->installer.text
#ifdef NSIS_CONFIG_COMPONENTPAGE
    || table->installer.componenttext
#endif
    )
  {
    // Changed by Amir Szekely 22nd July 2002
    // Adds the ability to disable space texts
    if (!table->installer.spacerequired && !no_space_texts) table->installer.spacerequired=add_string_main(str(NLF_SPACE_REQ),0);
    if (!table->installer.nextbutton) table->installer.nextbutton=add_string_main(str(NLF_BTN_NEXT),0);
    if (!table->installer.installbutton) table->installer.installbutton=add_string_main(str(NLF_BTN_INSTALL),0);
  }

  if (!table->common.subcaptions[3])
    table->common.subcaptions[3]=add_string_main(str(NLF_SUBCAPTION_INSTFILES));
  if (!table->common.subcaptions[4])
    table->common.subcaptions[4]=add_string_main(str(NLF_USUBCAPTION_COMPLETED));

  if (!table->common.branding)
  {
    char buf[256];
    wsprintf(buf,str(NLF_BRANDING),NSIS_VERSION);
    table->common.branding=add_string_main(buf,0);
  }
  if (!table->installer.backbutton) table->installer.backbutton=add_string_main(str(NLF_BTN_BACK),0);
  if (!table->common.cancelbutton) table->common.cancelbutton=add_string_main(str(NLF_BTN_CANCEL),0);
  if (!table->common.showdetailsbutton) table->common.showdetailsbutton=add_string_main(str(NLF_BTN_DETAILS),0);

  if (!table->common.closebutton) table->common.closebutton=add_string_main(str(NLF_BTN_CLOSE),0);
  if (!table->common.completed) table->common.completed=add_string_main(str(NLF_COMPLETED),0);
#endif

#ifdef NSIS_SUPPORT_FILE
  if (m_inst_fileused && !table->common.fileerrtext)
  {
    table->common.fileerrtext=add_string_main(str(NLF_FILE_ERROR));
  }
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (ubuild_entries.getlen())
  {
    if (uninstaller_writes_used) {
      if (!table->uninstall.uninstalltext2)
        table->uninstall.uninstalltext2=add_string_uninst(str(NLF_UNINST_SUBTEXT),0);
      if (!table->ucommon.subcaptions[0])
        table->ucommon.subcaptions[0]=add_string_uninst(str(NLF_USUBCAPTION_CONFIRM));
      if (!table->ucommon.subcaptions[1])
        table->ucommon.subcaptions[1]=add_string_uninst(str(NLF_USUBCAPTION_INSTFILES));
      if (!table->ucommon.subcaptions[2])
        table->ucommon.subcaptions[2]=add_string_uninst(str(NLF_USUBCAPTION_COMPLETED));
      if (!table->ucommon.caption)
      {
        char buf[1024];
        wsprintf(buf,str(NLF_UCAPTION),ubuild_strlist.get()+table->ucommon.name);
        table->ucommon.caption=add_string_uninst(buf);
      }
      table->ucommon.branding=add_string_uninst(build_strlist.get() + table->common.branding,0);
      table->ucommon.cancelbutton=add_string_uninst(build_strlist.get() + table->common.cancelbutton,0);
      table->ucommon.showdetailsbutton=add_string_uninst(build_strlist.get() + table->common.showdetailsbutton,0);
      table->ucommon.closebutton=add_string_uninst(build_strlist.get() + table->common.closebutton,0);
      table->ucommon.completed=add_string_uninst(build_strlist.get() + table->common.completed,0);

      if (!table->uninstall.uninstbutton) table->uninstall.uninstbutton=add_string_uninst(str(NLF_BTN_UNINSTALL),0);
    }
  }

#ifdef NSIS_SUPPORT_FILE
  if (m_uninst_fileused && !table->ucommon.fileerrtext)
  {
    table->ucommon.fileerrtext=add_string_uninst(build_strlist.get() + table->common.fileerrtext);
  }
#endif

#endif

  if (!table->common.caption)
  {
    char buf[1024];
    wsprintf(buf,str(NLF_CAPTION),build_strlist.get()+table->common.name);
    table->common.caption=add_string_main(buf);
  }

#define SET_INSTRUCTION(id,s) table->common.s=add_string_main(str(id),0);table->ucommon.s=add_string_uninst(str(id),0)

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  #if defined(NSIS_SUPPORT_DELETE) || defined(NSIS_SUPPORT_RMDIR) || defined(NSIS_SUPPORT_FILE)
    SET_INSTRUCTION(NLF_CANT_WRITE, cant_write);
  #endif
  #ifdef NSIS_SUPPORT_RMDIR
    SET_INSTRUCTION(NLF_REMOVE_DIR, remove_dir);
  #endif
  #ifdef NSIS_SUPPORT_COPYFILES
    SET_INSTRUCTION(NLF_COPY_FAILED, copy_failed);
    SET_INSTRUCTION(NLF_COPY_TO, copy_to);
  #endif
  #ifdef NSIS_SUPPORT_ACTIVEXREG
    SET_INSTRUCTION(NLF_SYMBOL_NOT_FOUND, symbol_not_found);
    SET_INSTRUCTION(NLF_COULD_NOT_LOAD, could_not_load);
    SET_INSTRUCTION(NLF_NO_OLE, no_ole);
    // not used anywhere - SET_INSTRUCTION(NLF_ERR_REG_DLL, err_reg_dll);
  #endif
  #ifdef NSIS_SUPPORT_CREATESHORTCUT
    SET_INSTRUCTION(NLF_CREATE_SHORTCUT, create_shortcut);
    SET_INSTRUCTION(NLF_ERR_CREATING_SHORTCUT, err_creating_shortcut);
  #endif
  #ifdef NSIS_SUPPORT_DELETE
    SET_INSTRUCTION(NLF_DEL_FILE, del_file);
    #ifdef NSIS_SUPPORT_MOVEONREBOOT
      SET_INSTRUCTION(NLF_DEL_ON_REBOOT, del_on_reboot);
    #endif
  #endif
  #ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    SET_INSTRUCTION(NLF_CREATED_UNINST, created_uninst);
    SET_INSTRUCTION(NLF_ERR_CREATING, err_creating);
  #endif
  #ifdef NSIS_SUPPORT_SHELLEXECUTE
    SET_INSTRUCTION(NLF_EXEC_SHELL, exec_shell);
  #endif
  #ifdef NSIS_SUPPORT_EXECUTE
    SET_INSTRUCTION(NLF_EXEC, exec);
  #endif
  #ifdef NSIS_SUPPORT_MOVEONREBOOT
    SET_INSTRUCTION(NLF_RENAME_ON_REBOOT, rename_on_reboot);
  #endif
  #ifdef NSIS_SUPPORT_RENAME
    SET_INSTRUCTION(NLF_RENAME, rename);
  #endif

  #ifdef NSIS_SUPPORT_FILE
    SET_INSTRUCTION(NLF_EXTRACT, extract);
    SET_INSTRUCTION(NLF_SKIPPED, skipped);
  #endif
  SET_INSTRUCTION(NLF_OUTPUT_DIR, output_dir);
  SET_INSTRUCTION(NLF_CREATE_DIR, create_dir);
  SET_INSTRUCTION(NLF_COPY_DETAILS, copy_details);
#endif//NSIS_CONFIG_VISIBLE_SUPPORT

  #ifdef NSIS_SUPPORT_FILE
    SET_INSTRUCTION(NLF_ERR_WRITING, err_writing);
    SET_INSTRUCTION(NLF_ERR_DECOMPRESSING, err_decompressing);
  #endif
  SET_INSTRUCTION(NLF_INST_CORRUPTED, inst_corrupted);
}

bool CEXEBuild::_IsNotSet(int *str) {
  if (!str) return true;
  for (int i = 0; i < string_tables.size(); i++) {
    if (*(int*)(int(str)-int(string_tables[0])+int(string_tables[i]))) {
      return false;
    }
  }
  return true;
}

char SkipComments(FILE *f) {
  char c;
  while (c = fgetc(f)) {
    while (c == '\n' || c == '\r') {
      c = fgetc(f); // Skip empty lines
    }
    if (c == '#' || c == ';') {
      while (c = fgetc(f)) {
       if (c == '\n') break;
      }
    }
    else break;
  }
  return c;
}

// NSIS Language File parser
NLF::NLF(char *filename) {
  FILE *f = fopen(filename, "r");
  if (!f) throw runtime_error("Can't open language file!");

  char buf[1024];
  buf[0] = SkipComments(f);
  fgets(buf+1, 1024, f);

  // Check header
  if (strncmp(buf, "NLF v", 5)) throw runtime_error("Invalid language file!");
  int nlf_version = atoi(buf+5);
  if (atoi(buf+5) != NLF_VERSION) throw runtime_error("Language file version doesn't match NSIS version!");

  // Get language ID
  buf[0] = SkipComments(f);
  fgets(buf+1, 1024, f);
  m_wLangId = atoi(buf);

  // Read strings
  for (int i = 0; i < NLF_STRINGS; i++) {
    buf[0] = SkipComments(f);
    fgets(buf+1, NSIS_MAX_STRLEN, f);
    if (lstrlen(buf) == NSIS_MAX_STRLEN-1) {
      wsprintf(buf, "String too long (string #%d)!", i);
      throw runtime_error(buf);
    }
    while (buf[lstrlen(buf)-1] == '\n' || buf[lstrlen(buf)-1] == '\r') {
      buf[lstrlen(buf)-1] = 0;
    }
    m_szStrings[i] = new char[lstrlen(buf)+1];
    for (char *out = m_szStrings[i], *in = buf; *in; in++, out++) {
      if (*in == '\\') {
        in++;
        switch (*in) {
        	case 'n':
            *out = '\n';
            break;
          case 'r':
            *out = '\r';
            break;
          case 't':
            *out = '\t';
            break;
          default:
            *out++ = '\\';
            *out = *in;
        }
      }
      else *out = *in;
    }
    *out = 0;
  }
  fclose(f);
}

NLF::~NLF() {
  for (int i = 0; i < NLF_STRINGS; i++) {
    delete [] m_szStrings[i];
  }
}

WORD NLF::GetLang() {
  return m_wLangId;
}

char* NLF::GetString(int idx) {
  if (idx < 0 || idx >= NLF_STRINGS) return 0;
  return m_szStrings[idx];
}
