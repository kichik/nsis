#ifndef _UI_H_
#define _UI_H_

// Added by Amir Szekely 3rd August 2002
extern installer_strings *cur_install_strings_table;
extern common_strings *cur_common_strings_table;
extern uninstall_strings *cur_uninstall_strings_table;

int ui_doinstall(void);
void update_status_text_from_tab(int texttab, const char *text2);
void update_status_text(const char *text1, const char *text2);
extern int ui_st_updateflag;

extern char g_autoclose;
extern void *g_inst_combinedheader;
extern section *g_inst_section;
extern entry *g_inst_entry;

#define g_inst_header ((header *)g_inst_combinedheader)
#define g_inst_cmnheader ((common_header *)g_inst_combinedheader)

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
#define g_inst_uninstheader ((uninstall_header *)g_inst_combinedheader)
extern int g_is_uninstaller;
#endif

#endif//_UI_H_
