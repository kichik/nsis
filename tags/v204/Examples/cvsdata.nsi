;Install CVS Data for anonymous access to the NSIS CVS server
;Silent installer executed by NSIS Update

;Written by Joost Verburg

;--------------------------------
;Configuration

Name "CVS Data"
OutFile "..\Bin\InstallCVSData.exe"
SetCompressor lzma

SilentInstall silent

;--------------------------------
;Functions

Function .onInit

  StrCpy $INSTDIR "$EXEDIR\.."

  Call GetParameters
  Pop $0

  StrCmp $0 "nooverwrite" 0 +3
    IfFileExists "$INSTDIR\CVS\Root" 0 +2
      Abort

FunctionEnd

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

;--------------------------------
;Installer Section

Section

  SetOutPath $INSTDIR
  File /r ..\Root
  File /r ..\Repository
  File /r ..\Entries
  File /nonfatal /r ..\Entries.log

SectionEnd