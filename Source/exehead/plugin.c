/*
 * plugin.c
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

#include "plugin.h"

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT

typedef struct _loaded_plugin
{
  struct _loaded_plugin* next;
  NSISPLUGINCALLBACK proc;
  HMODULE dll;
}
loaded_plugin;

static loaded_plugin* g_plugins = 0; // not thread safe!

void NSISCALL Plugins_SendMsgToAllPlugins(int msg)
{
  loaded_plugin* p;

  for (p = g_plugins; p; p = p->next)
  {
    p->proc(msg);
  }
}

void NSISCALL Plugins_UnloadAll() 
{
  loaded_plugin* p = g_plugins;

  Plugins_SendMsgToAllPlugins(NSPIM_UNLOAD);

  while (p)
  {
    loaded_plugin* oldp = p;
    p = oldp->next;
    FreeLibrary(oldp->dll);
    GlobalFree(oldp);
  }

  g_plugins = NULL;
}

BOOL NSISCALL Plugins_CanUnload(HANDLE pluginHandle)
{
  loaded_plugin* p;

  for (p = g_plugins; p; p = p->next)
  {
    if (p->dll == pluginHandle)
    {
      return FALSE;
    }
  }
  return TRUE;
}

int NSISCALL RegisterPluginCallback(HMODULE pluginHandle, NSISPLUGINCALLBACK proc)
{
  loaded_plugin* p;

  if (!Plugins_CanUnload(pluginHandle))
  {
    // already registered
    return 1;
  }
  
  p = (loaded_plugin*) GlobalAlloc(GPTR, sizeof(loaded_plugin));
  if (p)
  {
    p->proc   = proc;
    p->dll    = pluginHandle;
    p->next   = g_plugins;

    g_plugins = p;

    return 0;
  }

  return -1;
}

#endif /* #ifdef NSIS_CONFIG_PLUGIN_SUPPORT */
