;Install CVS Data for anonymous
;access to the NSIS CVS server

;Silent installer executed
;by NSIS Update

;Written by Joost Verburg

!echo "Compiling CVS Data Setup..."

!verbose 3

;--------------------------------
;Configuration

Name "CVS Data"
OutFile "..\Bin\InstallCVSData.exe"
SetCompressor bzip2

SilentInstall silent

!define NSISPATH $1

;--------------------------------
;Macro

!macro CVSDATA DIR

  SetOutPath "${NSISPATH}\${DIR}\CVS"
  File "/oname=${NSISPATH}\${DIR}\CVS\Entries" "..\${DIR}\CVS\Entries"
  ;CVS sometimes uses Entries.log files. Ignore warnings about not existing Entries.log files.
  File /nonfatal "/oname=${NSISPATH}\${DIR}\CVS\Entries.log" "..\${DIR}\CVS\Entries.log"
  File "/oname=${NSISPATH}\${DIR}\CVS\Repository" "..\${DIR}\CVS\Repository"
  File "/oname=${NSISPATH}\${DIR}\CVS\Root" "..\${DIR}\CVS\Root"
    
!macroend

;--------------------------------
;Functions

Function .onInit

  StrCpy ${NSISPATH} "$EXEDIR\.."
  
  IfFileExists "${NSISPATH}\CVS\Root" "" +3
    MessageBox MB_YESNO|MB_ICONEXCLAMATION \
    "Your NSIS folder already contains CVS data. Do you want to overwrite your current data?" IDYES +2
    Quit

FunctionEnd

;--------------------------------
;Installer Section

Section ""

!insertmacro CVSDATA "."
!insertmacro CVSDATA "Bin"
!insertmacro CVSDATA "Contrib"
!insertmacro CVSDATA "Contrib\AdvSplash"
!insertmacro CVSDATA "Contrib\Banner"
!insertmacro CVSDATA "Contrib\BgImage"
!insertmacro CVSDATA "Contrib\Dialer"
!insertmacro CVSDATA "Contrib\ExDLL"
!insertmacro CVSDATA "Contrib\Icons"
!insertmacro CVSDATA "Contrib\InstallOptions"
!insertmacro CVSDATA "Contrib\LangDLL"
!insertmacro CVSDATA "Contrib\Language files"
!insertmacro CVSDATA "Contrib\Makensisw"
!insertmacro CVSDATA "Contrib\Makensisw\jnetlib"
!insertmacro CVSDATA "Contrib\Modern UI"
!insertmacro CVSDATA "Contrib\Modern UI\Language files"
!insertmacro CVSDATA "Contrib\NSIS Update"
!insertmacro CVSDATA "Contrib\NSIS Update\Resources"
!insertmacro CVSDATA "Contrib\NSIS Update\Resources\BIN"
!insertmacro CVSDATA "Contrib\NSIS Update\Resources\GUI"
!insertmacro CVSDATA "Contrib\nsExec"
!insertmacro CVSDATA "Contrib\NSISdl"
!insertmacro CVSDATA "Contrib\Splash"
!insertmacro CVSDATA "Contrib\StartMenu"
!insertmacro CVSDATA "Contrib\System"
!insertmacro CVSDATA "Contrib\System\Source"
!insertmacro CVSDATA "Contrib\UIs"
!insertmacro CVSDATA "Contrib\UIs\UI Holder"
!insertmacro CVSDATA "Contrib\UserInfo"
!insertmacro CVSDATA "Contrib\zip2exe"
!insertmacro CVSDATA "Contrib\zip2exe\zlib"
!insertmacro CVSDATA "Docs"
!insertmacro CVSDATA "Docs\src"
!insertmacro CVSDATA "Docs\src\bin"
!insertmacro CVSDATA "Docs\src\bin\halibut"
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

SectionEnd

!verbose 4