  NSIS-DL 1.1 - http downloading DLL for NSIS
  Copyright (C) 2001-2002 Yaroslav Faybishenko & Justin Frankel

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

  You can also pass /TIMEOUT=### to set the timeout in milliseconds
  Result is pushed to the stack
	"cancel" if cancelled
	"success" if success
    otherwise, an error string describing the error

  NSISdl::download http://www.nullsoft.com/free/nsis/nsis198.exe poo.exe
or
  NSISdl::download /TIMEOUT=30000 http://www.nullsoft.com/free/nsis/nsis198.exe poo.exe


then, pop a value from the stack and for errors:

  Pop $0
  StrCmp $0 "success" yay
    Abort "Error downloading file
  yay:

To translate NSISdl add:
/TRANSLATE downloading connecting second minute hour plural progress remianing
to the call line.

Default values are:
  downloading - "Downloading %s"
  connecting - "Connecting ..."
  second - "second"
  minute - "minute"
  hour - "hour"
  plural - "s"
  progress - "%dkB (%d%%) of %dkB @ %d.%01dkB/s"
  remaining -  "(%d %s%s remaining)"

/TRANSLATE must come before /TIMEOUT.