;NSIS Modern User Interface version 1.65
;User variables Example Script
;Written by Ramon

;--------------------------------
;Include Modern UI

!include "MUI.nsh"

;--------------------------------
;Product Info

!define MUI_PRODUCT "User Variables"
!define MUI_VERSION "1.65"

;--------------------------------
;Configuration

  ;General
  OutFile "VersionInfo.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"

  ;Get install folder from registry if available
  InstallDirRegKey HKCU "Software\${MUI_PRODUCT}" ""
  
  ShowInstDetails nevershow

;--------------------------------
;Version Information
  VISetVersionLanguage 2057 1200 ; English UK
  VIAddTranslation 2057 1200     ; English UK
  VIProductVersion "1.2.3.4"
  VIAddVersionKey "ProductName" "NSIS"
  VIAddVersionKey "Comments" "visit us at nsis.sourceforge.net"
  VIAddVersionKey "CompanyName" "NSIS Team"
  VIAddVersionKey "LegalTrademarks" "Nullsoft Installer System"
  VIAddVersionKey "LegalCopyright" "© Nullsoft Installer System"
  VIAddVersionKey "FileDescription" "NSIS Self-extracting Setup"
  VIAddVersionKey "FileVersion" "1.2.3"

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Dummy Section" SecCopyUI

     /* Copy your files here */

SectionEnd

