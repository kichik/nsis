; RegTool
; Written by Joost Verburg
;
; This tool is used by the Library.nsh macros  to register
; dynamic link libraries and type libraries after a reboot.

;--------------------------------

Name "RegTool"
OutFile "RegToolGenerator.exe"
SilentInstall silent
SilentUninstall silent

SetCompressor lzma

Icon "${NSISDIR}\Contrib\Graphics\Icons\classic-install.ico"
UninstallIcon "${NSISDIR}\Contrib\Graphics\Icons\classic-install.ico"

AllowRootDirInstall true

;--------------------------------

Var MODE
Var FILENAME
Var FOLDER

;--------------------------------

Section

  WriteUninstaller $EXEDIR\RegTool.bin

SectionEnd

Section uninstall

  StrCpy $0 -1

  loop:

    IntOp $0 $0 + 1

    EnumRegValue $FILENAME HKLM "Software\NSIS.Library.RegTool" $0
    StrCmp $FILENAME "" done

      ReadRegStr $MODE HKLM "Software\NSIS.Library.RegTool" $FILENAME

      StrCmp $MODE "DT" 0 +4

        Call un.RegDLL
        Call un.RegTLB
        Goto loop

      StrCmp $MODE "D" 0 +3

        Call un.RegDLL
        Goto loop

      StrCmp $MODE "T" 0 +3

        Call un.RegTLB
        Goto loop

      Goto loop

  done:

  DeleteRegKey HKLM "Software\NSIS.Library.RegTool"
  Delete $INSTDIR\NSIS.Library.RegTool.exe

SectionEnd

Function un.RegDLL

  Push $FILENAME
  Call un.GetParent
  Pop $FOLDER

  SetOutPath $FOLDER
  RegDLL $FILENAME

FunctionEnd

Function un.RegTLB

  TypeLib::Register $FILENAME

FunctionEnd

; GetParent
; input, top of stack  (e.g. C:\Program Files\Poop)
; output, top of stack (replaces, with e.g. C:\Program Files)
; modifies no other variables.
;
; Usage:
;   Push "C:\Program Files\Directory\Whatever"
;   Call GetParent
;   Pop $R0
;   ; at this point $R0 will equal "C:\Program Files\Directory"

Function un.GetParent

  Exch $R0
  Push $R1
  Push $R2
  Push $R3

  StrCpy $R1 0
  StrLen $R2 $R0

  loop:
    IntOp $R1 $R1 + 1
    IntCmp $R1 $R2 get 0 get
    StrCpy $R3 $R0 1 -$R1
    StrCmp $R3 "\" get
    Goto loop

  get:
    StrCpy $R0 $R0 -$R1

    Pop $R3
    Pop $R2
    Pop $R1
    Exch $R0
 
FunctionEnd
