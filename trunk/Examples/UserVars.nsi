; VersionInfo.nsi
;
; This script shows you how to declare and user VARIABLES.

;--------------------------------
!include "MUI.nsh"

!define MUI_PRODUCT "User Variables"
!define MUI_VERSION "1.0"

;--------------------------------
;Configuration

  ;General
  OutFile "UserVars.exe"
  ShowInstDetails nevershow
  InstallDir "$PROGRAMFILES\Test UserVars"

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
; Declaration of User Variables with command DIM, allowed charaters for variables names : [a-z][A-Z][0-9] and '_'

  Var "un.Info" ; this one can only be used in uninstaller methods
  Var "Name"
  Var "Serial"

;--------------------------------
;Installer Sections

Section "Dummy Section" SecCopyUI

     StrCpy $0 "Admin"
     StrCpy "$Name" $0
     StrCpy "$Serial" "12345"
     MessageBox MB_OK "User Name: $Name:  $\n$\nSerial Number: $Serial"

     CreateDirectory $INSTDIR
     WriteUninstaller "$INSTDIR\Uninst.exe"
SectionEnd


Function un.OnUninstSuccess
     HideWindow
     MessageBox MB_OK "$un.Info"
FunctionEnd

Section "Uninstall"

     StrCpy $un.Info "${MUI_PRODUCT} ${MUI_VERSION} uninstalled successfully."
     Delete "$INSTDIR\Uninst.exe"
     RmDir $INSTDIR

SectionEnd
