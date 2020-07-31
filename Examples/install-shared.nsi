/*

This example script installs a simple application for all users on a machine.

All-users installers should only write to HKLM, $ProgramFiles, $CommonFiles and the 
"All context" versions of $LocalAppData, $Templates, $SMPrograms etc.

It should not write to HKCU nor any folders in the users profile!
If the application requires writable template data in $AppData it 
must copy the required files from a shared location the 
first time a user launches the application.

*/

!define NAME "All-users example"
!define REGPATH_UNINSTSUBKEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"
Name "${NAME}"
OutFile "Install ${NAME}.exe"
Unicode True
RequestExecutionLevel Admin ; Request admin rights on WinVista+ (when UAC is turned on)
InstallDir "$ProgramFiles\$(^Name)"
InstallDirRegKey HKLM "${REGPATH_UNINSTSUBKEY}" "UninstallString"

!include LogicLib.nsh
!include Integration.nsh

Page Directory
Page InstFiles

Uninstpage UninstConfirm
Uninstpage InstFiles

!macro EnsureAdminRights
  UserInfo::GetAccountType
  Pop $0
  ${If} $0 != "admin" ; Require admin rights on WinNT4+
    MessageBox MB_IconStop "Administrator rights required!"
    SetErrorLevel 740 ; ERROR_ELEVATION_REQUIRED
    Quit
  ${EndIf}
!macroend

Function .onInit
  SetShellVarContext All
  !insertmacro EnsureAdminRights
FunctionEnd

Function un.onInit
  SetShellVarContext All
  !insertmacro EnsureAdminRights
FunctionEnd


Section "Program files (Required)"
  SectionIn Ro

  SetOutPath $InstDir
  WriteUninstaller "$InstDir\Uninst.exe"
  WriteRegStr HKLM "${REGPATH_UNINSTSUBKEY}" "DisplayName" "${NAME}"
  WriteRegStr HKLM "${REGPATH_UNINSTSUBKEY}" "DisplayIcon" "$InstDir\MyApp.exe,0"
  WriteRegStr HKLM "${REGPATH_UNINSTSUBKEY}" "UninstallString" '"$InstDir\Uninst.exe"'
  WriteRegDWORD HKLM "${REGPATH_UNINSTSUBKEY}" "NoModify" 1
  WriteRegDWORD HKLM "${REGPATH_UNINSTSUBKEY}" "NoRepair" 1

  File "/oname=$InstDir\MyApp.exe" "${NSISDIR}\Bin\MakeLangId.exe" ; Pretend that we have a real application to install
SectionEnd

Section "Start Menu shortcut"
  CreateShortcut /NoWorkingDir "$SMPrograms\${NAME}.lnk" "$InstDir\MyApp.exe"
SectionEnd


Section -Uninstall
  ${UnpinShortcut} "$SMPrograms\${NAME}.lnk"
  Delete "$SMPrograms\${NAME}.lnk"

  Delete "$InstDir\MyApp.exe"
  Delete "$InstDir\Uninst.exe"
  RMDir "$InstDir"
  DeleteRegKey HKLM "${REGPATH_UNINSTSUBKEY}"
SectionEnd
