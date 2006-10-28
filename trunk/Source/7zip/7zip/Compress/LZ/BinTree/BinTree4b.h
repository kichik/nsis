/*
 * BinTreeb.h
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

#ifndef __BINTREE4B__H
#define __BINTREE4B__H

#undef BT_CLSID
#define BT_CLSID CLSID_CMatchFinderBT4b

#undef BT_NAMESPACE
#define BT_NAMESPACE NBT4B

#define HASH_ARRAY_2
#define HASH_ARRAY_3
#define HASH_BIG

#include "BinTreeMF.h"
#include "BinTreeMFMain.h"

#undef HASH_ARRAY_2
#undef HASH_ARRAY_3
#undef HASH_BIG

#endif

