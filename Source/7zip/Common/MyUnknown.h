/*
 * MyUnknown.h
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

#ifndef __MYUNKNOWN_H
#define __MYUNKNOWN_H

#ifdef _WIN32

#ifdef _WIN32_WCE
#if (_WIN32_WCE > 300)
#include <basetyps.h>
#else
#define MIDL_INTERFACE(x) struct 
#endif
#else
#include <basetyps.h>
#endif

#include <unknwn.h>

#else 
#include "MyWindows.h"
#endif
  
#endif
