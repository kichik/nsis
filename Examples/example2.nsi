; example2.nsi
;
; This script is based on example1.nsi, but adds uninstall support
; and (optionally) start menu shortcuts.
;
; It will install notepad.exe into a directory that the user selects,
;

; The name of the installer
Name "Example2"

; The file to write
OutFile "example2.exe"

; The default installation directory
InstallDir $PROGRAMFILES\Example2
; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM SOFTWARE\NSIS_Example2 "Install_Dir"

; The text to prompt the user to enter a directory
ComponentText "This will install the less simple example2 on your computer. Select which optional things you want installed."
; The text to prompt the user to enter a directory
DirText "Choose a directory to install in to:"

; The stuff to install
Section "Example2 (required)"
  SectionIn RO
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  ; Put file there
  File "..\makensisw.exe"
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\NSIS_Example2 "Install_Dir" "$INSTDIR"
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Example2" "DisplayName" "NSIS Example2 (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Example2" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteUninstaller "uninstall.exe"
SectionEnd

; optional section
Section "Start Menu Shortcuts"
  CreateDirectory "$SMPROGRAMS\Example2"
  CreateShortCut "$SMPROGRAMS\Example2\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\Example2\Example2 (notepad).lnk" "$WINDIR\notepad.exe" "" "$INSTDIR\makensisw.exe" 0
SectionEnd

; uninstall stuff

UninstallText "This will uninstall example2. Hit next to continue."

; special uninstall section.
Section "Uninstall"
  ; remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Example2"
  DeleteRegKey HKLM SOFTWARE\NSIS_Example2
  ; remove files
  Delete $INSTDIR\makensisw.exe
  ; MUST REMOVE UNINSTALLER, too
  Delete $INSTDIR\uninstall.exe
  ; remove shortcuts, if any.
  Delete "$SMPROGRAMS\Example2\*.*"
  ; remove directories used.
  RMDir "$SMPROGRAMS\Example2"
  RMDir "$INSTDIR"
SectionEnd

; eof
