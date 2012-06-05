/*
 * Defs.h
 * 
 * This file is a part of LZMA compression module for NSIS.
 * 
 * Original LZMA SDK Copyright (C) 1999-2006 Igor Pavlov
 * Modifications Copyright (C) 2003-2006 Amir Szekely <kichik@netvision.net.il>
 * 
 * Licensed under the Common Public License version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef __COMMON_DEFS_H
#define __COMMON_DEFS_H

template <class T> inline T MyMin(T a, T b)
  {  return a < b ? a : b; }
template <class T> inline T MyMax(T a, T b)
  {  return a > b ? a : b; }

template <class T> inline int MyCompare(T a, T b)
  {  return a < b ? -1 : (a == b ? 0 : 1); }

inline int BoolToInt(bool value)
  { return (value ? 1: 0); }

inline bool IntToBool(int value)
  { return (value != 0); }

#endif
