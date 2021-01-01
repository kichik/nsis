/*
 * fileform.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Unicode support by Jim Park -- 08/22/2007
 */

#include "fileform.h"

#ifdef __GNUC__
// GCC warns about array bounds when accessing g_usrvarssection[2] because it is only [1] at compile time, 
// the other part of this hack is in util.c where g_usrvarsstart is initialized.
extern const NSIS_STRING*const g_usrvarsstart;
#define g_usrvars ( (NSIS_STRING*) (g_usrvarsstart) )
#else
extern NSIS_STRING g_usrvarssection[1];
#define g_usrvars g_usrvarssection
#endif

#define state_command_line        (((NSIS_STRING *) g_usrvars)[20])
#define state_install_directory   (((NSIS_STRING *) g_usrvars)[21])
#define state_output_directory    (((NSIS_STRING *) g_usrvars)[22])
#define state_exe_directory       (((NSIS_STRING *) g_usrvars)[23])
#define state_language            (((NSIS_STRING *) g_usrvars)[24])
#define state_temp_dir            (((NSIS_STRING *) g_usrvars)[25])
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
#  define state_plugins_dir       (((NSIS_STRING *) g_usrvars)[26])
#endif
#define state_exe_path            (((NSIS_STRING *) g_usrvars)[27])
#define state_exe_file            (((NSIS_STRING *) g_usrvars)[28])
#define state_click_next          (((NSIS_STRING *) g_usrvars)[30])

extern TCHAR g_caption[NSIS_MAX_STRLEN*2];
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
extern HWND g_hwnd;
extern HANDLE g_hInstance;
extern HWND insthwnd,insthwndbutton;
#else
#define g_hwnd 0
#define g_hInstance 0
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
