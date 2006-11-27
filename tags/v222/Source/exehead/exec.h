/*
 * exec.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2006 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef _EXEC_H_
#define _EXEC_H_

extern exec_flags g_exec_flags;

int NSISCALL ExecuteCodeSegment(int pos, HWND hwndProgress); // returns 0 on success
int NSISCALL ExecuteCallbackFunction(int num); // returns 0 on success

#endif//_EXEC_H_
