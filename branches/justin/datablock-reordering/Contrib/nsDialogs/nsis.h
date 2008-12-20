#ifndef __NS_DIALOGS__NSIS_H__
#define __NS_DIALOGS__NSIS_H__

#include <windows.h>
#include "defs.h"

#define EXDLL_INIT()           {  \
        g_stringsize=string_size; \
        g_stacktop=stacktop;      \
        g_variables=variables; }

// For page showing plug-ins
#define WM_NOTIFY_OUTER_NEXT (WM_USER+0x8)
#define WM_NOTIFY_CUSTOM_READY (WM_USER+0xd)
#define NOTIFY_BYE_BYE 'x'

typedef struct _stack_t {
  struct _stack_t *next;
  char text[1]; // this should be the length of string_size
} stack_t;

extern int g_stringsize;
extern stack_t **g_stacktop;
extern char *g_variables;

int NSDFUNC myatoi(const char *s);
int NSDFUNC popstring(char *str, int size);
void NSDFUNC pushstring(const char *str);
int NSDFUNC popint();
void NSDFUNC pushint(int value);

typedef struct {
  int autoclose;
  int all_user_var;
  int exec_error;
  int abort;
  int exec_reboot;
  int reboot_called;
  int XXX_cur_insttype; // deprecated
  int XXX_insttype_changed; // deprecated
  int silent;
  int instdir_error;
  int rtl;
  int errlvl;
  int alter_reg_view;
} exec_flags_type;

typedef struct {
  exec_flags_type *exec_flags;
  int (__stdcall *ExecuteCodeSegment)(int, HWND);
  void (__stdcall *validate_filename)(char *);
} extra_parameters;


#endif//__NS_DIALOGS__NSIS_H__
