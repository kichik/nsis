/* 
  Copyright (c) 2002 Robert Rainwater
  Portions Copyright (c) 2002 Justin Frankel and Fritz Elfert

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

*/
#ifndef UTILS_H
#define UTILS_H

#define REGSEC HKEY_LOCAL_MACHINE // JF> modified this to HKLM so that 
                                  // nsis uninstaller would remove. this means
                                  // window placement is shared across users, but
                                  // bfd.
#define REGKEY "Software\\NSIS"
#define REGLOC "MakeNSISWPlacement"

extern const char NSISW_VERSION[];

// Methods
void SetTitle(HWND hwnd,char *substr);
void SetBranding(HWND hwnd);
void CopyToClipboard(HWND hwnd);
void ClearLog(HWND hwnd);
void LogMessage(HWND hwnd,const char *str);
void ErrorMessage(HWND hwnd,const char *str);
void DisableItems(HWND hwnd);
void EnableItems(HWND hwnd);
void RestoreWindowPos(HWND hwnd);
void SaveWindowPos(HWND hwnd);
void ResetObjects();

#endif