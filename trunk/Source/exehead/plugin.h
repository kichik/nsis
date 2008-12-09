/*
 * plugin.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2008 Nullsoft and Contributors
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

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT

// Starting with NSIS 2.42, you can check the version of the plugin API in exec_flags->plugin_api_version
// The format is 0xXXXXYYYY where X is the major version and Y is the minor version (MAKELONG(y,x))
// When doing version checks, always remember to use >=, ex: if (pX->exec_flags->plugin_api_version >= NSISPIAPIVER_1_0) {}

#define NSISPIAPIVER_1_0 0x00010000
#define NSISPIAPIVER_CURR NSISPIAPIVER_1_0

// NSIS Plug-In Callback Messages
enum NSPIM 
{
	NSPIM_UNLOAD,    // This is the last message a plugin gets, do final cleanup
	NSPIM_GUIUNLOAD, // Called after .onGUIEnd
};

// Prototype for callbacks registered with extra_parameters->RegisterPluginCallback()
// Return NULL for unknown messages
// Should always be __cdecl for future expansion possibilities
typedef UINT_PTR (*NSISPLUGINCALLBACK)(NSPIM);

extern BOOL NSISCALL RegisterPluginCallback(HMODULE pluginHandle, NSISPLUGINCALLBACK proc);

extern void NSISCALL Plugins_SendMsgToAllPlugins(int msg);
extern void NSISCALL Plugins_UnloadAll();
extern BOOL NSISCALL Plugins_CanUnload(HANDLE pluginHandle);

#endif /* #ifdef NSIS_CONFIG_PLUGIN_SUPPORT */

#endif /* _PLUGIN_H_ */
