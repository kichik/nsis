/*
 * icon.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef _ICON_H_
#define _ICON_H_

// reads icon file filename and places its icons in the resource wIconId using resource editor re
void replace_icon(CResourceEditor* re, WORD wIconId, const char* filename);

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon (inside filename) that should replace the installer icon data
unsigned char* generate_uninstall_icon_data(const char* filename, size_t &size);
// Fill the array of icons for uninstall with their offsets
int generate_unicons_offsets(unsigned char* exeHeader, size_t exeHeaderSize, unsigned char* uninstIconData);
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT

#endif//_ICON_H_
