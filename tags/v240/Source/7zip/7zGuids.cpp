/*
 * 7zGuids.cpp
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

// DLLExports.cpp

// #include "StdAfx.h"

#ifdef WIN32
#  include <objbase.h>
#  include <initguid.h>
#endif

#include "../Platform.h"

#define INITGUID
#include "7zip/ICoder.h"
#include "7zip/Compress/LZ/IMatchFinder.h"
