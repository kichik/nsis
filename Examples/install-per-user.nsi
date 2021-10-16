/*

This example script installs a simple application for a single user.

If multiple users on the same machine run this installer, each user
will end up with a separate install that is not affected by
update/removal operations performed by other users.

Per-user installers should only write to HKCU and 
folders inside the users profile.

*/

!define NAME "Per-User example"
!define REGPATH_UNINSTSUBKEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"
Name "${NAME}"
OutFile "Install ${NAME}.exe"
Unicode True
RequestExecutionLevel User ; We don't need UAC elevation
InstallDir "" ; Don't set a default $InstDir so we can detect /D= and InstallDirRegKey
InstallDirRegKey HKCU "${REGPATH_UNINSTSUBKEY}" "UninstallString"

!include LogicLib.nsh
!include WinCore.nsh


Page Directory
Page InstFiles

Uninstpage UninstConfirm
Uninstpage InstFiles


Function .onInit
  SetShellVarContext Current

  ${If} $InstDir == "" ; No /D= nor InstallDirRegKey?
    GetKnownFolderPath $InstDir ${FOLDERID_UserProgramFiles} ; This folder only exists on Win7+
    StrCmp $InstDir "" 0 +2 
    StrCpy $InstDir "$LocalAppData\Programs" ; Fallback directory

    StrCpy $InstDir "$InstDir\$(^Name)"
  ${EndIf}
FunctionEnd


Section "Program files (Required)"
  SectionIn Ro

  SetOutPath $InstDir
  WriteUninstaller "$InstDir\Uninst.exe"
  WriteRegStr HKCU "${REGPATH_UNINSTSUBKEY}" "DisplayName" "${NAME}"
  WriteRegStr HKCU "${REGPATH_UNINSTSUBKEY}" "UninstallString" '"$InstDir\Uninst.exe"'
  WriteRegDWORD HKCU "${REGPATH_UNINSTSUBKEY}" "NoModify" 1
  WriteRegDWORD HKCU "${REGPATH_UNINSTSUBKEY}" "NoRepair" 1

  File "/oname=$InstDir\MyApp.exe" "${NSISDIR}\Bin\MakeLangId.exe" ; Pretend that we have a real application to install

  ;WriteRegStr HKCU "Software\Classes\.myfileext" "myfiletype"
  ;WriteRegStr HKCU "Software\Classes\myfiletype\shell\myapp\command" "" '"$InstDir\MyApp.exe" "%1"'
SectionEnd

Section "Start Menu shortcut"
  CreateShortcut /NoWorkingDir "$SMPrograms\${NAME}.lnk" "$InstDir\MyApp.exe"
SectionEnd


Section -Uninstall
  Delete "$InstDir\MyApp.exe"
  Delete "$InstDir\Uninst.exe"
  RMDir "$InstDir"
  DeleteRegKey HKCU "${REGPATH_UNINSTSUBKEY}"
  ;DeleteRegKey HKCU "Software\Classes\myfiletype\shell\myapp"
  ;DeleteRegKey /IfEmpty HKCU "Software\Classes\myfiletype\shell"
  ;DeleteRegKey /IfEmpty HKCU "Software\Classes\myfiletype"

  Delete "$SMPrograms\${NAME}.lnk"
SectionEnd
