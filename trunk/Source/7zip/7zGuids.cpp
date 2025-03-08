/*
 * 7zGuids.cpp
 * 
 * This file is a part of LZMA compression module for NSIS.
 * 
 * Original LZMA SDK Copyright (C) 1999-2006 Igor Pavlov
 * Modifications Copyright (C) 2003-2025 Amir Szekely <kichik@netvision.net.il>
 * 
 * Licensed under the Common Public License version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Reviewed for Unicode support by Jim Park -- 08/24/2007
 */

// DLLExports.cpp

// #include "StdAfx.h"

#ifdef _WIN32
#  include <initguid.h>
#  include <objbase.h>
#endif

#ifndef INITGUID
#  define INITGUID
#endif

#include "../Platform.h"

#include "7zip/ICoder.h"
#include "7zip/Compress/LZ/IMatchFinder.h"
