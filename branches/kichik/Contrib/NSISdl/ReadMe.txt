  NSIS-DL 1.1 - http downloading DLL for NSIS
  Copyright (C) 2001 Yaroslav Faybishenko & Justin Frankel

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




This dll can be used from NSIS to download files via http.

How to use (for another example, see waplugin.nsi in the nsis directory):

  Pass the url and filename on the stack
  Result is returned in $0
	"cancel" if cancelled
	"success" if success
        otherwise, an error string describing the error

Example:
  ; pack the dll in the install file
  File /oname=$TEMP\nsdtmp09.dll nsisdl.dll

  ; make the call to download
  Push "http://www.xcf.berkeley.edu/~yaroslav/photos/mike/mike1-full.jpg"
  Push "$INSTDIR\test.jpg"
  CallInstDLL $TEMP\nsdtmp09.dll download ; for a quiet install, use download_quiet

  ; delete DLL from temporary directory
  Delete $TEMP\nsdtmp09.dll

  ; check if download succeeded
  StrCmp $0 "success" successful
  StrCmp $0 "cancel" cancelled

  ; we failed
    DetailPrint "Download failed: $0"
    goto done

  cancelled:
    DetailPrint "Download cancelled"
    goto done
  successful:
    DetailPrint "Download successful"
    ExecShell $INSTDIR\test.jpg
    goto done
