/*
 * MyGuidDef.h
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

#ifndef GUID_DEFINED
#define GUID_DEFINED

#include "Types.h"

typedef struct {
  UInt32 Data1;
  UInt16 Data2;
  UInt16 Data3;
  unsigned char Data4[8];
} GUID;

#ifdef __cplusplus
#define REFGUID const GUID &
#else
#define REFGUID const GUID *
#endif

#define REFCLSID REFGUID
#define REFIID REFGUID

#ifdef __cplusplus
inline bool operator==(REFGUID g1, REFGUID g2)
{ 
  for (int i = 0; i < (int)sizeof(g1); i++)
    if (((unsigned char *)&g1)[i] != ((unsigned char *)&g2)[i])
      return false;
  return true;
}
inline bool operator!=(REFGUID g1, REFGUID g2) { return !(g1 == g2); }
#endif

#ifdef __cplusplus
  #define MY_EXTERN_C extern "C"
#else
  #define MY_EXTERN_C extern
#endif

#endif // GUID_DEFINED


#ifdef DEFINE_GUID
#undef DEFINE_GUID
#endif

#ifdef INITGUID
  #define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
    MY_EXTERN_C const GUID name = { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }
#else
  #define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
    MY_EXTERN_C const GUID name
#endif
