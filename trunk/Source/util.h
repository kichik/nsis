#ifndef _UTIL_H_
#define _UTIL_H_

#include "ResourceEditor.h"

// these are the standard pause-before-quit shit.
extern int g_dopause;
extern void dopause(void);

// Adds the bitmap in filename using resource editor re as id id.
// If width or height are specified it will also make sure the bitmap is in that size
int update_bitmap(CResourceEditor* re, WORD id, char* filename, int width=0, int height=0, int maxbpp=0);

// reads icon file filename and places its icons in the resource wIconId using resource editor re. Also updates icondata_size.
int replace_icon(CResourceEditor* re, WORD wIconId, char* filename);

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon (inside filename) that should replace the installer icon data
unsigned char* generate_uninstall_icon_data(char* filename);
// Fill the array of icons for uninstall with their offsets
int generate_unicons_offsets(unsigned char* exeHeader, unsigned char* uninstIconData);
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
// Returns dialog's raw data from a given loaded module
BYTE* get_dlg(HINSTANCE hUIFile, WORD dlgId, char* filename);
#endif

#endif //_UTIL_H_
