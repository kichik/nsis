#ifndef _UI_H_
#define _UI_H_

// Added by Amir Szekely 3rd August 2002
extern char *language_tables;
extern int *cur_language_table;

int NSISCALL ui_doinstall(void);
void NSISCALL update_status_text_from_lang(int id, const char *text2);
void NSISCALL update_status_text(const char *text1, const char *text2);
extern int ui_st_updateflag;
extern int num_sections;

//extern int g_autoclose;
extern void *g_inst_combinedheader;
extern page *g_inst_page;
extern section *g_inst_section;
extern entry *g_inst_entry;

#define g_inst_header ((header *)g_inst_combinedheader)
#define g_inst_cmnheader ((common_header *)g_inst_combinedheader)

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
#define g_inst_uninstheader ((uninstall_header *)g_inst_combinedheader)
extern int g_is_uninstaller;
#endif

#ifdef NSIS_CONFIG_LOG
void NSISCALL build_g_logfile(void);
#endif

// sent to the last child window to tell it that the install thread is done
#define WM_NOTIFY_INSTPROC_DONE (WM_USER+0x4)

// sent to every child window to tell it it can start executing NSIS code
#define WM_NOTIFY_START (WM_USER+0x5)

// sent to the outer window to tell it to go to the next inner window
#define WM_NOTIFY_OUTER_NEXT (WM_USER+0x8)

// sent to every child window to tell it it is closing soon
#define WM_NOTIFY_INIGO_MONTOYA (WM_USER+0xb)

// update message used by DirProc and SelProc for space display
#define WM_IN_UPDATEMSG (WM_USER+0xf)

// the selected insttype has changed
#define WM_NOTIFY_INSTTYPE_CHANGE (WM_USER+0x32)

// custom pages should send this message to let NSIS know they're ready
#define WM_NOTIFY_CUSTOM_READY (WM_USER+0xd)

// simulates clicking on the tree
#define WM_TREEVIEW_KEYHACK (WM_USER+0x13)

// section text changed
#define WM_NOTIFY_SECTEXT WM_USER+0x17

// section flags changed
#define WM_NOTIFY_SECFLAGS WM_USER+0x18

// notifies a component selection change (.onMouseOverSection)
#define WM_NOTIFY_SELCHANGE WM_USER+0x19

#endif//_UI_H_
