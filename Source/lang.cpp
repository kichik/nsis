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
  "Skipped: "
};

int CEXEBuild::SetString(char *string, int id, int process, WORD lang/*=0*/) {
  lang = lang?lang:build_nlfs.size()?build_nlfs[build_nlfs.size()-1]->GetLang():0;
  lang = lang?lang:string_tables.size()?string_tables[0]->lang_id:1033; // Default is English (1033)
  StringTable *table = 0;
  for (int i = 0; i < string_tables.size(); i++) {
    if (lang == string_tables[i]->lang_id) {
      table = string_tables[i];
      break;
    }
  }
  if (!table) {
    table = (StringTable*)malloc(sizeof(StringTable));
    if (!table) {
      ERROR_MSG("Internal compiler error #12345: malloc(%d) failed\n",sizeof(StringTable));
      return PS_ERROR;
    }
    memset(table, -1, sizeof(StringTable));
    table->lang_id = table->ucommon.lang_id = table->installer.lang_id = table->uninstall.lang_id = lang;
    string_tables.push_back(table);
  }

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
    HANDLE_STRING_C(NLF_FILE_ERROR, common.fileerrtext);

    HANDLE_STRING_I(NLF_CAPTION, common.caption);
    HANDLE_STRING_I(NLF_SUBCAPTION_LICENSE, common.subcaptions[0]);
    HANDLE_STRING_I(NLF_SUBCAPTION_OPTIONS, common.subcaptions[1]);
    HANDLE_STRING_I(NLF_SUBCAPTION_DIR, common.subcaptions[2]);
    HANDLE_STRING_I(NLF_SUBCAPTION_INSTFILES, common.subcaptions[3]);
    HANDLE_STRING_I(NLF_SUBCAPTION_COMPLETED, common.subcaptions[4]);
    HANDLE_STRING_I(NLF_BTN_NEXT, installer.nextbutton);
    HANDLE_STRING_I(NLF_BTN_BACK, installer.backbutton);
    HANDLE_STRING_I(NLF_BTN_LICENSE, installer.licensebutton);
    HANDLE_STRING_I(NLF_BTN_INSTALL, installer.installbutton);
    HANDLE_STRING_I(NLF_BTN_BROWSE, installer.browse);
    HANDLE_STRING_I(NLF_COMP_SUBTEXT1, installer.componentsubtext[0]);
    HANDLE_STRING_I(NLF_COMP_SUBTEXT2, installer.componentsubtext[1]);
    HANDLE_STRING_I(NLF_DIR_SUBTEXT, installer.dirsubtext);
    HANDLE_STRING_I(NLF_SPACE_AVAIL, installer.spaceavailable);
    HANDLE_STRING_I(NLF_SPACE_REQ, installer.spacerequired);

    HANDLE_STRING_U(NLF_UCAPTION, ucommon.caption);
    HANDLE_STRING_U(NLF_USUBCAPTION_CONFIRM, ucommon.subcaptions[0]);
    HANDLE_STRING_U(NLF_USUBCAPTION_INSTFILES, ucommon.subcaptions[1]);
    HANDLE_STRING_U(NLF_USUBCAPTION_COMPLETED, ucommon.subcaptions[2]);
    HANDLE_STRING_U(NLF_BTN_UNINSTALL, uninstall.uninstbutton);
    HANDLE_STRING_U(NLF_UNINST_SUBTEXT, uninstall.uninstalltext2);

    HANDLE_STRING_C(LANG_NAME, common.name);

    HANDLE_STRING_I(LANG_COMP_TEXT, installer.componenttext);
    HANDLE_STRING_I(LANG_LICENSE_TEXT, installer.licensetext);
    HANDLE_STRING_I(LANG_LICENSE_DATA, installer.licensedata);
    HANDLE_STRING_I(LANG_DIR_TEXT, installer.text);

    HANDLE_STRING_U(LANG_UNINST_TEXT, uninstall.uninstalltext);

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
  if (*(int*)(int(str)-int(string_tables[0])+int(string_tables[i]))>=0) return true;
  return false;
}

int CEXEBuild::WriteStringTables() {
  int i;

  // If we have no tables (user didn't set any string and didn't load any NLF) create the default one
  if (string_tables.size() == 0) {
    build_header.str_tables_num = 1;
    StringTable *table = (StringTable*)malloc(sizeof(StringTable));
    if (!table) {
      ERROR_MSG("Internal compiler error #12345: malloc(%d) failed\n",sizeof(StringTable));
      return PS_ERROR;
    }
    memset(table, -1, sizeof(StringTable));
    table->lang_id=table->ucommon.lang_id=table->installer.lang_id=table->uninstall.lang_id=1033; // English
    string_tables.push_back(table);
  }

  // Fill tables with defaults (if needed) and with instruction strings
  int st_num = string_tables.size();
  for (i = 0; i < st_num; i++)
    FillDefaultsIfNeeded(string_tables[i]);

  // Add string tables into datablock
  GrowBuf ist;
  for (i = 0; i < st_num; i++)
    ist.add(&string_tables[i]->installer, sizeof(installer_strings));
  build_header.str_tables_num = st_num;
  build_header.str_tables = add_data((char*)ist.get(), st_num*sizeof(installer_strings), &build_datablock);

  GrowBuf cst;
  for (i = 0; i < st_num; i++)
    cst.add(&string_tables[i]->common, sizeof(common_strings));
  build_header.common.str_tables = add_data((char*)cst.get(), st_num*sizeof(common_strings), &build_datablock);

  GrowBuf ust;
  for (i = 0; i < st_num; i++)
    ust.add(&string_tables[i]->uninstall, sizeof(uninstall_strings));
  build_uninst.str_tables_num = st_num;
  build_uninst.str_tables = add_data((char*)ust.get(), st_num*sizeof(uninstall_strings), &ubuild_datablock);

  GrowBuf ucst;
  for (i = 0; i < st_num; i++)
    ucst.add(&string_tables[i]->ucommon, sizeof(common_strings));
  build_uninst.common.str_tables = add_data((char*)ucst.get(), st_num*sizeof(common_strings), &ubuild_datablock);

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
  if (table->installer.componenttext>=0)
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
      if (table->installer.custom<0) table->installer.custom=add_string_main(str(NLF_COMP_CUSTOM),0);
      if (table->common.subcaptions[1]<0)
        table->common.subcaptions[1]=add_string_main(str(NLF_SUBCAPTION_OPTIONS));

      if (build_header.install_types_ptr[0] < 0)
      {
        if (table->installer.componentsubtext[1]<0)
          table->installer.componentsubtext[1]=add_string_main(str(NLF_COMP_SUBTEXT1_NO_INST_TYPES),0);
      }
      else
      {
        if (table->installer.componentsubtext[0]<0)
          table->installer.componentsubtext[0]=add_string_main(str(NLF_COMP_SUBTEXT1),0);
        if (build_header.no_custom_instmode_flag!=1 && table->installer.componentsubtext[1]<0)
          table->installer.componentsubtext[1]=add_string_main(str(NLF_COMP_SUBTEXT2),0);
      }
    }
    else table->installer.componenttext=-1;
  }
#endif
  
  static bool nameWarned = false;
  if (table->common.name < 0)
  {
    if (!nameWarned) {
      warning("Name command not specified. Assuming default.");
      nameWarned = true;
    }
    table->common.name=add_string_main(str(NLF_DEF_NAME),0);
    table->ucommon.name=add_string_uninst(str(NLF_DEF_NAME),0);
  }

#ifdef NSIS_CONFIG_LICENSEPAGE
  if (table->installer.licensedata<0 || table->installer.licensetext<0)
  {
    table->installer.licensedata=-1;
    table->installer.licensetext=-1;
  }

  if (table->installer.licensedata>=0)
  {
    if (table->common.subcaptions[0]<0)
      table->common.subcaptions[0]=add_string_main(str(NLF_SUBCAPTION_LICENSE));
    if (table->installer.licensebutton<0)
      table->installer.licensebutton=add_string_main(str(NLF_BTN_LICENSE),0);
  }
#endif //NSIS_CONFIG_LICENSEPAGE

  if (table->installer.text >= 0)
  {
    if (table->installer.dirsubtext<0)
    {
      char buf[2048];
      wsprintf(buf,str(NLF_DIR_SUBTEXT),build_strlist.get()+table->common.name);
      table->installer.dirsubtext=add_string_main(buf,0);
    }
    if (table->common.subcaptions[2]<0)
      table->common.subcaptions[2]=add_string_main(str(NLF_SUBCAPTION_DIR));
    if (table->installer.browse<0) table->installer.browse=add_string_main(str(NLF_BTN_BROWSE),0);
    if (table->installer.spaceavailable<0 && !no_space_texts) table->installer.spaceavailable=add_string_main(str(NLF_SPACE_AVAIL),0);
  }

  if (table->installer.text >= 0 
#ifdef NSIS_CONFIG_COMPONENTPAGE
    || table->installer.componenttext>=0
#endif
    )  
  {
    // Changed by Amir Szekely 22nd July 2002
    // Adds the ability to disable space texts
    if (table->installer.spacerequired<0 && !no_space_texts) table->installer.spacerequired=add_string_main(str(NLF_SPACE_REQ),0);
    if (table->installer.nextbutton<0) table->installer.nextbutton=add_string_main(str(NLF_BTN_NEXT),0);
    if (table->installer.installbutton<0) table->installer.installbutton=add_string_main(str(NLF_BTN_INSTALL),0);
  }

  if (table->common.subcaptions[3]<0)
    table->common.subcaptions[3]=add_string_main(str(NLF_SUBCAPTION_INSTFILES));
  if (table->common.subcaptions[4]<0)
    table->common.subcaptions[4]=add_string_main(str(NLF_USUBCAPTION_COMPLETED));

  if (table->common.branding<0)
  {
    char buf[256];
    wsprintf(buf,str(NLF_BRANDING),NSIS_VERSION);
    table->common.branding=add_string_main(buf,0);
  }
  if (table->installer.backbutton<0) table->installer.backbutton=add_string_main(str(NLF_BTN_BACK),0);
  if (table->common.cancelbutton<0) table->common.cancelbutton=add_string_main(str(NLF_BTN_CANCEL),0);
  if (table->common.showdetailsbutton<0) table->common.showdetailsbutton=add_string_main(str(NLF_BTN_DETAILS),0);

  if (table->common.closebutton<0) table->common.closebutton=add_string_main(str(NLF_BTN_CLOSE),0);
  if (table->common.completed<0) table->common.completed=add_string_main(str(NLF_COMPLETED),0);
#ifdef NSIS_SUPPORT_FILE
  if (m_inst_fileused && table->common.fileerrtext<0)
  {
    table->common.fileerrtext=add_string_main(str(NLF_FILE_ERROR));
  }
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (ubuild_entries.getlen())
  {
    if (uninstaller_writes_used) {
      if (table->uninstall.uninstalltext2<0)
        table->uninstall.uninstalltext2=add_string_uninst(str(NLF_UNINST_SUBTEXT),0);
      if (table->ucommon.subcaptions[0]<0)
        table->ucommon.subcaptions[0]=add_string_uninst(str(NLF_USUBCAPTION_CONFIRM));
      if (table->ucommon.subcaptions[1]<0)
        table->ucommon.subcaptions[1]=add_string_uninst(str(NLF_USUBCAPTION_INSTFILES));
      if (table->ucommon.subcaptions[2]<0)
        table->ucommon.subcaptions[2]=add_string_uninst(str(NLF_USUBCAPTION_COMPLETED));
      if (table->ucommon.caption < 0)
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

      if (table->uninstall.uninstbutton<0) table->uninstall.uninstbutton=add_string_uninst(str(NLF_BTN_UNINSTALL),0);
    }
  }

#ifdef NSIS_SUPPORT_FILE
  if (m_uninst_fileused && table->ucommon.fileerrtext<0)
  {
    table->ucommon.fileerrtext=add_string_uninst(build_strlist.get() + table->common.fileerrtext);
  }
#endif

#endif

  if (table->common.caption < 0)
  {
    char buf[1024];
    wsprintf(buf,str(NLF_CAPTION),build_strlist.get()+table->common.name);
    table->common.caption=add_string_main(buf);
  }

#define SET_INSTRUCTION(id,s) table->common.s=add_string_main(str(id),0);table->ucommon.s=add_string_uninst(str(id),0)
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
  SET_INSTRUCTION(NLF_ERR_WRITING, err_writing);
  SET_INSTRUCTION(NLF_ERR_DECOMPRESSING, err_decompressing);
  SET_INSTRUCTION(NLF_SKIPPED, skipped);
#endif
  SET_INSTRUCTION(NLF_INST_CORRUPTED, inst_corrupted);
  SET_INSTRUCTION(NLF_OUTPUT_DIR, output_dir);
  SET_INSTRUCTION(NLF_CREATE_DIR, create_dir);
}

bool CEXEBuild::_IsNotSet(int *str) {
  if (!str) return true;
  for (int i = 0; i < string_tables.size(); i++) {
    if (*(int*)(int(str)-int(string_tables[0])+int(string_tables[i])) >= 0) {
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