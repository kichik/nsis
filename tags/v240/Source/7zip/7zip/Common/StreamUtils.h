/*
 * StreamUtils.h
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

#ifndef __STREAMUTILS_H
#define __STREAMUTILS_H

#include "../IStream.h"

HRESULT ReadStream(ISequentialInStream *stream, void *data, UInt32 size, UInt32 *processedSize);
HRESULT WriteStream(ISequentialOutStream *stream, const void *data, UInt32 size, UInt32 *processedSize);

#endif
