/*
 * plugin.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2025 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef _PLUGIN_H_
#define _PLUGIN_H_

#include "../Platform.h"
#include "fileform.h"
#include "api.h"

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT

extern int NSISCALL RegisterPluginCallback(HMODULE pluginHandle, NSISPLUGINCALLBACK proc);

extern void NSISCALL Plugins_SendMsgToAllPlugins(int msg);
extern void NSISCALL Plugins_UnloadAll();
extern BOOL NSISCALL Plugins_CanUnload(HANDLE pluginHandle);

#endif /* #ifdef NSIS_CONFIG_PLUGIN_SUPPORT */

#endif /* _PLUGIN_H_ */
