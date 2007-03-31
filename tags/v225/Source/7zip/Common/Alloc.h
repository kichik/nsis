/*
 * Alloc.h
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

#ifndef __COMMON_ALLOC_H
#define __COMMON_ALLOC_H

#include <stddef.h>

void *MyAlloc(size_t size) throw();
void MyFree(void *address) throw();

#ifdef _WIN32

bool SetLargePageSize();

void *MidAlloc(size_t size) throw();
void MidFree(void *address) throw();
void *BigAlloc(size_t size) throw();
void BigFree(void *address) throw();

#else

#define MidAlloc(size) MyAlloc(size)
#define MidFree(address) MyFree(address)
#define BigAlloc(size) MyAlloc(size)
#define BigFree(address) MyFree(address)

#endif

#endif
