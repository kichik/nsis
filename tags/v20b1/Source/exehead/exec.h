#ifndef _EXEC_H_
#define _EXEC_H_

extern union flags {
  struct {
    int autoclose;
    int all_user_var;
    int exec_error;
#ifdef NSIS_SUPPORT_REBOOT
    int exec_reboot;
#endif
  };
  int flags[1];
} g_flags;

int NSISCALL ExecuteCodeSegment(int pos, HWND hwndProgress); // returns 0 on success

#endif//_EXEC_H_
