// this file is for backward compatibility only
// use the new plugin.h and plugin.lib directly for new plug-ins

#ifndef _EXDLL_H_
#define _EXDLL_H_

#include <windows.h>
#include <nsis/pluginapi.h> // nsis plugin

// this may not work if /NODEFAULTLIB is used
#ifdef _MSC_VER
#  pragma comment(lib, "plugin.lib")
#endif

#endif//_EXDLL_H_
