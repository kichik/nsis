/*
 * components.h
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

#ifndef ___COMPONENTS_H___
#define ___COMPONENTS_H___

void NSISCALL SectionFlagsChanged(unsigned int index);
#define RefreshSectionGroups() _RefreshSectionGroups(0, 0)
unsigned int NSISCALL _RefreshSectionGroups(unsigned int i, int not_first_call);
#ifdef NSIS_CONFIG_COMPONENTPAGE
void NSISCALL SetInstType(int inst_type);
unsigned int NSISCALL GetInstType(HTREEITEM *items);
#endif//NSIS_CONFIG_COMPONENTPAGE

#endif//!___COMPONENTS_H___
