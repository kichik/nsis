;NSIS Modern UI version 1.3
;Basic Example Script
;Written by Joost Verburg

!define NAME "Test Software" ;Define your own software name here
!define VERSION "1.0" ;Define your own software version here

!verbose 3
  !include "${NSISDIR}\Contrib\Modern UI\System.nsh"
!verbose 4

;--------------------------------
;Configuration

  !insertmacro MUI_BASICFUNCTIONS_INIT

  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTPAGE
  !define MUI_DIRSELECTPAGE
  !define MUI_ABORTWARNING
  !define MUI_UNINSTALLER

  ;Language
    ;English
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\English.nsh"

  ;General
  Name "${NAME} ${VERSION}"
  OutFile "Basic.exe"

  ;User interface - icons, ui file, check bitmap, progress bar etc.
  !insertmacro MUI_INTERFACE "modern.exe" "modern-install.ico" "modern-uninstall.ico" "modern.bmp" "smooth" "Tahoma" "$9" ;$9 is the variable used to store the current page, do not use this var!

  ;License dialog
  LicenseData "${NSISDIR}\Contrib\Modern UI\License.txt"

  ;Descriptions
  LangString DESC_SecCopyUI ${LANG_ENGLISH} "Copy the modern.exe file to the application folder."

  ;Folder-select dialog
  InstallDir "$PROGRAMFILES\${NAME}"

;--------------------------------
;Installer Sections

Section "modern.exe" SecCopyUI

  ;ADD YOUR OWN STUFF HERE!

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

Section ""

  ;Invisible section to display the Finish header
  !insertmacro MUI_FINISHHEADER

SectionEnd

;--------------------------------
;Installer Functions

!insertmacro MUI_BASICFUNCTIONS

!insertmacro MUI_FUNCTION_DESCRIPTION_START
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyUI} $(DESC_SecCopyUI)
!insertmacro MUI_FUNCTION_DESCRIPTION_END
 
!insertmacro MUI_FUNCTION_ABORTWARNING

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN STUFF HERE!

  Delete "$INSTDIR\modern.exe"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  ;Display the Finish header
  !insertmacro MUI_UNFINISHHEADER

SectionEnd

;--------------------------------
;Uninstaller Functions

!insertmacro MUI_UNBASICFUNCTIONS