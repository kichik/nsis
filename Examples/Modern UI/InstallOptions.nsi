;NSIS Modern User Interface version 1.63
;InstallOptions Example Script
;Written by Joost Verburg

!define MUI_PRODUCT "Test Software" ;Define your own software name here
!define MUI_VERSION "1.0" ;Define your own software version here

!include "MUI.nsh"

!define TEMP $R0

;--------------------------------
;Configuration

  ;General
  OutFile "InstallOptions.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
  
  ;Remember install folder
  InstallDirRegKey HKCU "Softare\${MUI_PRODUCT}" ""

;--------------------------------
;Modern UI Configuration

  !define MUI_CUSTOMPAGECOMMANDS

  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTSPAGE
  !define MUI_DIRECTORYPAGE
  
  !define MUI_ABORTWARNING
  
  !define MUI_UNINSTALLER
  !define MUI_UNCONFIRMPAGE

;--------------------------------
;Pages
  
  !insertmacro MUI_PAGECOMMAND_LICENSE
  Page custom SetCustomA "$(TEXT_IO_PAGETITLE_A)"
  Page custom SetCustomB "$(TEXT_IO_PAGETITLE_B)"
  !insertmacro MUI_PAGECOMMAND_COMPONENTS
  !insertmacro MUI_PAGECOMMAND_DIRECTORY
  Page custom SetCustomC "$(TEXT_IO_PAGETITLE_C)"
  !insertmacro MUI_PAGECOMMAND_INSTFILES

;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"
  
;--------------------------------
;Language Strings

  ;Description
  LangString DESC_SecCopyUI ${LANG_ENGLISH} "Copy the modern.exe file to the application folder."
  
  ;Header
  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions Page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Create your own dialog!"
  
  ;Window titles
  LangString TEXT_IO_PAGETITLE_A ${LANG_ENGLISH} ": Custom Page A"
  LangString TEXT_IO_PAGETITLE_B ${LANG_ENGLISH} ": Custom Page B"
  LangString TEXT_IO_PAGETITLE_C ${LANG_ENGLISH} ": Custom Page C"

;--------------------------------
;Data
  
  LicenseData "${NSISDIR}\Contrib\Modern UI\License.txt"

;--------------------------------
;Reserve Files
  
  ;Things that need to be extracted on first (keep these lines before any File command!)
  ;Only useful for BZIP2 compression
  
  ReserveFile "ioA.ini"
  ReserveFile "ioB.ini"
  ReserveFile "ioC.ini"
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

;--------------------------------
;Installer Sections

Section "modern.exe" SecCopyUI

  ;Add your stuff here

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"
  
  ;Store install folder
  WriteRegStr HKCU "Softare\${MUI_PRODUCT}" "" $INSTDIR
  
  ;Read a value from an InstallOptions INI File
  !insertmacro MUI_INSTALLOPTIONS_READ ${TEMP} "ioC.ini" "Field 2" "State"
  StrCmp ${TEMP} "1" "" +2
    ;Checked
    MessageBox MB_OK "A MessageBox..."
    
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

;Display the Finish header
;Insert this macro after the sections if you are not using a finish page
!insertmacro MUI_SECTIONS_FINISHHEADER

;--------------------------------
;Descriptions

!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyUI} $(DESC_SecCopyUI)
!insertmacro MUI_FUNCTIONS_DESCRIPTION_END

;--------------------------------
;Installer Functions

Function .onInit

  ;Extract InstallOptions INI Files
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

  DeleteRegKey /ifempty HKCU "Software\${MUI_PRODUCT}"

  !insertmacro MUI_UNFINISHHEADER

SectionEnd