; Copyright (C) 2001 Michael Bishop
;
;  This software is provided 'as-is', without any express or implied
;  warranty.  In no event will the authors be held liable for any damages
;  arising from the use of this software.
;
;  Permission is granted to anyone to use this software for any purpose,
;  including commercial applications, and to alter it and redistribute it
;  freely, subject to the following restrictions:
;
;  1. The origin of this software must not be misrepresented; you must not
;     claim that you wrote the original software. If you use this software
;     in a product, an acknowledgment in the product documentation would be
;     appreciated but is not required.
;  2. Altered source versions must be plainly marked as such, and must not be
;     misrepresented as being the original software.
;  3. This notice may not be removed or altered from any source distribution.
;

; Test installation script

; The name of the installer
Name "Test Install"

; The file to write
OutFile "test-setup.exe"
ShowInstDetails show

; The default installation directory
InstallDir $PROGRAMFILES\Test

DirText "Choose dir"
LicenseText "You are about to install test install and it owns, you will love it, we think."
LicenseData "..\..\license.txt"
ComponentText "Choose components"

; Do not automatically close the installation window
AutoCloseWindow false

; Define some installation templates
InstType "Typical"        ; 1

Section "Required Components"
ReadINIStr $R0 $7 "Field 1" State
DetailPrint "Install X=$R0"
ReadINIStr $R0 $7 "Field 2" State
DetailPrint "Install Y=$R0"
ReadINIStr $R0 $7 "Field 3" State
DetailPrint "Install Z=$R0"
ReadINIStr $R0 $7 "Field 4" State
DetailPrint "File=$R0"
ReadINIStr $R0 $7 "Field 5" State
DetailPrint "Dir=$R0"

SectionEnd

Section "more components"
Nop
SectionEnd


; $9 = counter
; $8 = DLL
; $7 = ini
Function .onInit
  StrCpy $9 0
  GetTempFileName $8
  GetTempFileName $7
  File /oname=$8 ..\..\Plugins\InstallOptions.dll
  File /oname=$7 "test.ini"
FunctionEnd

; cleanup on exit.
Function .onInstSuccess
Call Cleanup
FunctionEnd

Function .onInstFailed
Call Cleanup
FunctionEnd

Function .onUserAbort
Call Cleanup
FunctionEnd

Function Cleanup
  Delete $8
  Delete $7
FunctionEnd

Function .onNextPage
  StrCmp $9 1 good
    IntOp $9 $9 + 1
    Return
  good:
  Call RunConfigure
  Pop $0
  StrCmp $0 "cancel" "" nocancel
    Call Cleanup
    Quit
  nocancel:
  StrCmp $0 "back" "" noback
    Abort
  noback:
  IntOp $9 $9 + 1
FunctionEnd

Function .onPrevPage
  StrCmp $9 2 good
    IntOp $9 $9 - 1
    Return
  good:
  Call RunConfigure
  Pop $0
  StrCmp $0 "cancel" "" nocancel
    Call Cleanup
    Quit
  nocancel:
  StrCmp $0 "back" back
    Abort
  back:
  IntOp $9 $9 - 1
FunctionEnd

Function RunConfigure
  Push $7
  CallInstDLL $8 dialog
FunctionEnd