;NSIS Modern User Interface version 1.6
;Install Options Example Script
;Written by Joost Verburg

!define MUI_PRODUCT "Test Software" ;Define your own software name here
!define MUI_VERSION "1.0" ;Define your own software version here

!include "${NSISDIR}\Contrib\Modern UI\System.nsh"

;--------------------------------
;Configuration
  
  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTSPAGE
  !define MUI_DIRECTORYPAGE
  !define MUI_ABORTWARNING
  
  !define MUI_UNINSTALLER
  
  !define MUI_CUSTOMPAGECOMMANDS
    
  !define TEMP1 $R0
  !define TEMP2 $R1
  
  ;Language
  !insertmacro MUI_LANGUAGE "English"

  ;General
  OutFile "InstallOptions.exe"
  
  ;Install Options pages
  
    ;Header
    LangString TEXT_IO_TITLE ${LANG_ENGLISH} "Install Options Page"
    LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Create your own dialog!"
  
    ;Window titles
    LangString TEXT_IO_PAGETITLE_A ${LANG_ENGLISH} ": Custom Page A"
    LangString TEXT_IO_PAGETITLE_B ${LANG_ENGLISH} ": Custom Page B"
    LangString TEXT_IO_PAGETITLE_C ${LANG_ENGLISH} ": Custom Page C"
  
  ;Page order 
  !insertmacro MUI_PAGECOMMAND_LICENSE
  Page custom SetCustomA "$(TEXT_IO_PAGETITLE_A)"
  Page custom SetCustomB "$(TEXT_IO_PAGETITLE_B)"
  !insertmacro MUI_PAGECOMMAND_COMPONENTS
  !insertmacro MUI_PAGECOMMAND_DIRECTORY
  Page custom SetCustomC "$(TEXT_IO_PAGETITLE_C)"
  !insertmacro MUI_PAGECOMMAND_INSTFILES
  
  ;License page
  LicenseData "${NSISDIR}\Contrib\Modern UI\License.txt"

  ;Component-selection page
    ;Descriptions
    LangString DESC_SecCopyUI ${LANG_ENGLISH} "Copy the modern.exe file to the application folder."

  ;Folder-selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
  
  ;Things that need to be extracted on startup (keep these lines before any File command!)
  ;Only useful for BZIP2 compression
  ;Use ReserveFile for your own Install Options ini files too!
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS
  ReserveFile "ioA.ini"
  ReserveFile "ioB.ini"
  ReserveFile "ioC.ini"

;--------------------------------
;Modern UI System

!insertmacro MUI_SYSTEM

;--------------------------------
;Installer Sections

Section "modern.exe" SecCopyUI

  ;Add your stuff here

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"
  
  ;Read a value from an Install Options INI File
  !insertmacro MUI_INSTALLOPTIONS_READ ${TEMP1} "ioC.ini" "Field 2" "State"
  StrCmp ${TEMP1} "1" "" +2
    ;Checked
    MessageBox MB_OK "A MessageBox..."
    
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

!insertmacro MUI_SECTIONS_FINISHHEADER ;Insert this macro after the sections

;--------------------------------
;Descriptions

!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyUI} $(DESC_SecCopyUI)
!insertmacro MUI_FUNCTIONS_DESCRIPTION_END

;--------------------------------
;Installer Functions

Function .onInit

  ;Extract Install Options INI Files
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioA.ini"
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioB.ini"
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioC.ini"
  
FunctionEnd

Function SetCustomA
  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioA.ini"
FunctionEnd

Function SetCustomB
  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioB.ini"
FunctionEnd

Function SetCustomC
  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioC.ini"

FunctionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;Add your stuff here

  Delete "$INSTDIR\modern.exe"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  !insertmacro MUI_UNFINISHHEADER

SectionEnd

;eof