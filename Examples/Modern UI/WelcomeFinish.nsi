;NSIS Modern User Interface version 1.64
;Welcome/Finish Page Example Script
;Written by Joost Verburg

!define MUI_PRODUCT "Modern UI Test" ;Define your own software name here
!define MUI_VERSION "1.0" ;Define your own software version here

!include "MUI.nsh"
  
;--------------------------------
;Configuration

  ;General
  OutFile "WelcomeFinish.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
  
  ;Remember install folder
  InstallDirRegKey HKCU "Software\${MUI_PRODUCT}" ""

;--------------------------------
;Modern UI Configuration

  !define MUI_WELCOMEPAGE
  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTSPAGE
  !define MUI_DIRECTORYPAGE
  !define MUI_FINISHPAGE
    !define MUI_FINISHPAGE_RUN "$INSTDIR\modern.exe"
  
  !define MUI_ABORTWARNING
  
  !define MUI_UNINSTALLER
  !define MUI_UNCONFIRMPAGE
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"
  
;--------------------------------
;Language Strings

  ;Description
  LangString DESC_SecCopyUI ${LANG_ENGLISH} "Copy the modern.exe file to the application folder."

;--------------------------------
;Data
  
  LicenseData "${NSISDIR}\Contrib\Modern UI\License.txt"
  
;--------------------------------
;Reserve Files

  ;Things that need to be extracted on first (keep these lines before any File command!)
  ;Only useful for BZIP2 compression
  !insertmacro MUI_RESERVEFILE_WELCOMEFINISHPAGE

;--------------------------------
;Installer Sections

Section "modern.exe" SecCopyUI

  ;Add your stuff here

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"
  
  ;Store install folder
  WriteRegStr HKCU "Software\${MUI_PRODUCT}" "" $INSTDIR
    
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

;--------------------------------
;Descriptions

!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyUI} $(DESC_SecCopyUI)
!insertmacro MUI_FUNCTIONS_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;Add your stuff here

  Delete "$INSTDIR\modern.exe"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  DeleteRegKey /ifempty HKCU "Software\${MUI_PRODUCT}"

SectionEnd