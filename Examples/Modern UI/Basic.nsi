;NSIS Modern User Interface version 1.4
;Basic Macro System Example Script
;Written by Joost Verburg

!define NAME "Test Software" ;Define your own software name here
!define VERSION "1.0" ;Define your own software version here

!include "${NSISDIR}\Contrib\Modern UI\System.nsh"

;--------------------------------
;Configuration

  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTSPAGE
  !define MUI_DIRECTORYPAGE
  !define MUI_ABORTWARNING
  !define MUI_UNINSTALLER

  ;Language
    ;English
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\English.nsh"

  ;General
  OutFile "Basic.exe"
  Name "${NAME} ${VERSION}"

  ;License page
  LicenseData "${NSISDIR}\Contrib\Modern UI\License.txt"

  ;Descriptions
  LangString DESC_SecCopyUI ${LANG_ENGLISH} "Copy the modern.exe file to the application folder."

  ;Folder-selection page
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
;Modern UI System

!insertmacro MUI_SYSTEM

!insertmacro MUI_FUNCTIONS_DESCRIPTION_START
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyUI} $(DESC_SecCopyUI)
!insertmacro MUI_FUNCTIONS_DESCRIPTION_END
 

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