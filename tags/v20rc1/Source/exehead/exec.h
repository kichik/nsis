#ifndef _EXEC_H_
#define _EXEC_H_

extern union exec_flags g_exec_flags;

int NSISCALL ExecuteCodeSegment(int pos, HWND hwndProgress); // returns 0 on success

#endif//_EXEC_H_
