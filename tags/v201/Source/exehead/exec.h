#ifndef _EXEC_H_
#define _EXEC_H_

extern exec_flags g_exec_flags;

int NSISCALL ExecuteCodeSegment(int pos, HWND hwndProgress); // returns 0 on success
int NSISCALL ExecuteCallbackFunction(int num); // returns 0 on success

#endif//_EXEC_H_
