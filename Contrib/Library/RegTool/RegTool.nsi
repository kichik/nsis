; RegTool
; Written by Joost Verburg
;
; This tool is used by the Library.nsh macros  to register
; dynamic link libraries and type libraries after a reboot.

;--------------------------------

Name "RegTool"
OutFile "RegTool.bin"
SilentInstall silent

SetCompressor lzma

;--------------------------------

Var COMMAND_LINE
Var MODE
Var FILENAME
Var FOLDER

;--------------------------------

Section

  Call GetParameters
  Pop $COMMAND_LINE
  
  StrCpy $MODE $COMMAND_LINE 1
  StrCpy $FILENAME $COMMAND_LINE "" 2
  
  ;DLL
  StrCmp $MODE "D" 0 no_dll

    Push $FILENAME
    Call GetParent
    Pop $FOLDER
    
    SetOutPath $FOLDER
    RegDLL $FILENAME

  no_dll:

  ;TLB
  StrCmp $MODE "T" 0 no_tlb

    TypeLib::Register $FILENAME

  no_tlb:
  
  System::Call 'kernel32::GetModuleFileNameA(i 0, t .R0, i 1024) i r1'
  Delete /REBOOTOK $R0
  

SectionEnd

;--------------------------------

; GetParameters
; input, none
; output, top of stack (replaces, with e.g. whatever)
; modifies no other variables.

Function GetParameters

  Push $R0
  Push $R1
  Push $R2
  Push $R3
  
  StrCpy $R2 1
  StrLen $R3 $CMDLINE
  
  ;Check for quote or space
  StrCpy $R0 $CMDLINE $R2
  StrCmp $R0 '"' 0 +3
    StrCpy $R1 '"'
    Goto loop
  StrCpy $R1 " "
  
  loop:
    IntOp $R2 $R2 + 1
    StrCpy $R0 $CMDLINE 1 $R2
    StrCmp $R0 $R1 get
    StrCmp $R2 $R3 get
    Goto loop
  
  get:
    IntOp $R2 $R2 + 1
    StrCpy $R0 $CMDLINE 1 $R2
    StrCmp $R0 " " get
    StrCpy $R0 $CMDLINE "" $R2
  
  Pop $R3
  Pop $R2
  Pop $R1
  Exch $R0

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

Function GetParent

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
