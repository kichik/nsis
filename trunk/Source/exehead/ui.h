#ifndef _UI_H_
#define _UI_H_

int ui_doinstall(void);
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
