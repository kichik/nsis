;NSIS Modern User Interface version 1.65
;Basic Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

!include "MUI.nsh"

;--------------------------------
;Product Info

!define MUI_PRODUCT "Modern UI Test"
!define MUI_VERSION "1.65"

;--------------------------------
;Configuration

  ;General
  OutFile "Basic.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
  
  ;Get install folder from registry if available
  InstallDirRegKey HKCU "Software\${MUI_PRODUCT}" ""

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Modern UI Configuration

  !define MUI_ABORTWARNING
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Data
  
  LicenseData "${NSISDIR}\Contrib\Modern UI\License.txt"

;--------------------------------
;Installer Sections

Section "Dummy Test File" SecCopyUI

  ;ADD YOUR OWN STUFF HERE!

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"
  
  ;Store install folder
  WriteRegStr HKCU "Software\${MUI_PRODUCT}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

;--------------------------------
;Descriptions

LangString DESC_SecCopyUI ${LANG_ENGLISH} "Copy modern.exe to the application folder."

!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyUI} $(DESC_SecCopyUI)
!insertmacro MUI_FUNCTIONS_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN STUFF HERE!

  Delete "$INSTDIR\modern.exe"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  DeleteRegKey /ifempty HKCU "Software\${MUI_PRODUCT}"

SectionEnd