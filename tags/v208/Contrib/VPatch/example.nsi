;VPatch example
;Written by Joost Verburg

;--------------------------------

; The name of the installer
Name "VPatch Test"

; The file to write
OutFile "vpatchtest.exe"

; The default installation directory
InstallDir "$PROGRAMFILES\VPatch Test"

; The text to prompt the user to enter a directory
DirText "Choose a folder in which to install the VPatch Test!"

; Show details
ShowInstDetails show

;--------------------------------

Section ""

  ; Set output path to the installation directory
  SetOutPath $INSTDIR
  
  ; Extract the old file
  File oldfile.txt
 
  ; Extract the patch to the plug-ins folder (temporary)
  InitPluginsDir
  File /oname=$PLUGINSDIR\patch.pat patch.pat
  
  ; Update the old file to the new file using the patch
  DetailPrint "Updating oldfile.txt using patch..."
  vpatch::vpatchfile "$PLUGINSDIR\patch.pat" "$INSTDIR\oldfile.txt" "$INSTDIR\newfile.txt"
  
  ; Show result
  Pop $R0
  DetailPrint "Result: $R0"
  
SectionEnd