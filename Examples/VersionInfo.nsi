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
  VIProductVersion "1.2.3.4"
  VIProductName "NSIS"
  VIComments "visit us at nsis.sourceforge.net"
  VICompanyName "NSIS Team"
  VILegalTrademarks "NullSoft Installer System"
  VILegalCopyrights "NullSoft Installer System"
  VIDescription "NSIS Self-extracting Setup"

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

