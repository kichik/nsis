;Install CVS Data for anonymous access to the NSIS CVS server
;Silent installer executed by NSIS Update

;Written by Joost Verburg

;--------------------------------
;Configuration

Name "CVS Data"
OutFile "..\Bin\InstallCVSData.exe"
SetCompressor lzma

SilentInstall silent

Var NSISPATH
Var UNINSTALL
Var TEMP1

;--------------------------------
;Macro

!macro CVSDATA DIR

  SetOutPath "$NSISPATH\${DIR}\CVS"
  File "/oname=$NSISPATH\${DIR}\CVS\Entries" "..\${DIR}\CVS\Entries"
  ;CVS sometimes uses Entries.log files. Ignore warnings about not existing Entries.log files.
  File /nonfatal "/oname=$NSISPATH\${DIR}\CVS\Entries.log" "..\${DIR}\CVS\Entries.log"
  File "/oname=$NSISPATH\${DIR}\CVS\Repository" "..\${DIR}\CVS\Repository"
  File "/oname=$NSISPATH\${DIR}\CVS\Root" "..\${DIR}\CVS\Root"
    
!macroend

;--------------------------------
;Functions

Function .onInit

  StrCpy $NSISPATH "$EXEDIR\.."
  
  Call GetParameters
  Pop $TEMP1
  
  StrCmp $TEMP1 "nooverwrite" 0 +3
    IfFileExists "$NSISPATH\CVS\Root" 0 +2
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

  !insertmacro CVSDATA "."
  !insertmacro CVSDATA "Bin"
  !insertmacro CVSDATA "Contrib"
  !insertmacro CVSDATA "Contrib\AdvSplash"
  !insertmacro CVSDATA "Contrib\Banner"
  !insertmacro CVSDATA "Contrib\BgImage"
  !insertmacro CVSDATA "Contrib\Dialer"
  !insertmacro CVSDATA "Contrib\ExDLL"
  !insertmacro CVSDATA "Contrib\Graphics"
  !insertmacro CVSDATA "Contrib\Graphics\Checks"
  !insertmacro CVSDATA "Contrib\Graphics\Icons"
  !insertmacro CVSDATA "Contrib\Graphics\Header"
  !insertmacro CVSDATA "Contrib\Graphics\Wizard"
  !insertmacro CVSDATA "Contrib\InstallOptions"
  !insertmacro CVSDATA "Contrib\LangDLL"
  !insertmacro CVSDATA "Contrib\Language files"
  !insertmacro CVSDATA "Contrib\Makensisw"
  !insertmacro CVSDATA "Contrib\Math"
  !insertmacro CVSDATA "Contrib\Math\Source"
  !insertmacro CVSDATA "Contrib\Modern UI"
  !insertmacro CVSDATA "Contrib\Modern UI\images"
  !insertmacro CVSDATA "Contrib\Modern UI\Language files"
  !insertmacro CVSDATA "Contrib\nsExec"
  !insertmacro CVSDATA "Contrib\NSISdl"
  !insertmacro CVSDATA "Contrib\Splash"
  !insertmacro CVSDATA "Contrib\StartMenu"
  !insertmacro CVSDATA "Contrib\System"
  !insertmacro CVSDATA "Contrib\System\Source"
  !insertmacro CVSDATA "Contrib\UIs"
  !insertmacro CVSDATA "Contrib\UIs\UI Holder"
  !insertmacro CVSDATA "Contrib\UserInfo"
  !insertmacro CVSDATA "Contrib\VPatch"
  !insertmacro CVSDATA "Contrib\VPatch\Source"
  !insertmacro CVSDATA "Contrib\VPatch\Source\GenPat"
  !insertmacro CVSDATA "Contrib\VPatch\Source\GUI"
  !insertmacro CVSDATA "Contrib\VPatch\Source\Plugin"
  !insertmacro CVSDATA "Contrib\zip2exe"
  !insertmacro CVSDATA "Contrib\zip2exe\zlib"
  !insertmacro CVSDATA "Docs"
  !insertmacro CVSDATA "Examples"
  !insertmacro CVSDATA "Examples\Modern UI"
  !insertmacro CVSDATA "Include"
  !insertmacro CVSDATA "Menu"
  !insertmacro CVSDATA "Menu\images"
  !insertmacro CVSDATA "Plugins"
  !insertmacro CVSDATA "Source"
  !insertmacro CVSDATA "Source\bzip2"
  !insertmacro CVSDATA "Source\exehead"
  !insertmacro CVSDATA "Source\zlib"
  !insertmacro CVSDATA "Source\7zip"
  !insertmacro CVSDATA "Source\7zip\7zip"
  !insertmacro CVSDATA "Source\7zip\7zip\Common"
  !insertmacro CVSDATA "Source\7zip\7zip\Compress"
  !insertmacro CVSDATA "Source\7zip\7zip\Compress\LZ"
  !insertmacro CVSDATA "Source\7zip\7zip\Compress\LZ\BinTree"
  !insertmacro CVSDATA "Source\7zip\7zip\Compress\LZMA"
  !insertmacro CVSDATA "Source\7zip\7zip\Compress\RangeCoder"
  !insertmacro CVSDATA "Source\7zip\Common"

SectionEnd