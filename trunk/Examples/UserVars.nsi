; UserVars.nsi
;
; This script shows you how to declare and user variables.

;--------------------------------

  Name "User Variables Text"
  OutFile "UserVars.exe"
  
  InstallDir "$PROGRAMFILES\User Variables Test"
  DirText "Choose a folder to install in which to install the test:" " "

;--------------------------------
; Declaration of user variables (Var command), allowed charaters for variables names : [a-z][A-Z][0-9] and '_'

  Var "Name"
  Var "Serial"
  
  Var "un.Info" ; this one can only be used in the uninstaller

;--------------------------------
; Installer

Section "Dummy Section" SecDummy

     StrCpy $0 "Admin"
     StrCpy "$Name" $0
     StrCpy "$Serial" "12345"
     MessageBox MB_OK "User Name: $Name:  $\n$\nSerial Number: $Serial"

     CreateDirectory $INSTDIR
     WriteUninstaller "$INSTDIR\Uninst.exe"
     
SectionEnd

;--------------------------------
; Uninstaller

Section "Uninstall"

     StrCpy $un.Info "User variables test uninstalled successfully."
     Delete "$INSTDIR\Uninst.exe"
     RmDir $INSTDIR

SectionEnd

Function un.OnUninstSuccess

     HideWindow
     MessageBox MB_OK "$un.Info"
     
FunctionEnd