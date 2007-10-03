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

#include "ResourceEditor.h"

#include <vector>

typedef struct
{
  WORD wReserved;
  WORD wIsIcon;
  WORD wCount;
} IconGroupHeader;

typedef struct
{
  BYTE bWidth;
  BYTE bHeight;
  BYTE bPaletteEntries;
  BYTE bReserved;
  WORD wPlanes;
  WORD wBitsPerPixel;
  DWORD dwRawSize;
} IconGroupEntry;

typedef struct
{
  IconGroupEntry header;
  DWORD dwImageOffset;
} FileIconGroupEntry;

typedef struct
{
  IconGroupEntry header;
  WORD wRsrcId;
} RsrcIconGroupEntry;

typedef struct
{
  unsigned index;
  IconGroupEntry meta;
  LPBYTE data;
} Icon;

typedef std::vector<Icon> IconGroup;

IconGroup load_icon_file(const char* filename);
void free_loaded_icon(IconGroup icon);

void set_icon(CResourceEditor* re, WORD wIconId, IconGroup icon1, IconGroup icon2);

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon (inside filename) that should replace the installer icon data
LPBYTE generate_uninstall_icon_data(IconGroup icon1, IconGroup icon2, size_t &size);
// Fill the array of icons for uninstall with their offsets
int generate_unicons_offsets(LPBYTE exeHeader, size_t exeHeaderSize, LPBYTE uninstIconData, WORD wIconId);
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT

#endif//_ICON_H_
