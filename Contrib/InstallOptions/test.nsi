;Install Options Test Script
;Written by Joost Verburg

!define TEMP1 $R0

;The name of the installer
Name "Install Options Test"

; The file to write
OutFile "Test.exe"

; The default installation directory
InstallDir "$PROGRAMFILES\IOTest"

DirText "Choose a directory"
LicenseText "A license"
LicenseData "..\..\License.txt"
ComponentText "Choose components"

Page license
Page custom SetCustom ;Custom page. Install Options gets called in SetCustom,
Page components
Page directory
Page instfiles

Section "Components"
  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.nsi" "Field 1" "State"
  MessageBox MB_OK "Install X=$R0"
  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.nsi" "Field 2" "State"
  MessageBox MB_OK "Install Y=$R0"
  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.nsi" "Field 3" "State"
  MessageBox MB_OK "Install Z=$R0"
  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.nsi" "Field 4" "State"
  MessageBox MB_OK "File=$R0"
  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.nsi" "Field 5" "State"
  MessageBox MB_OK "Dir=$R0"
SectionEnd

Function .onInit
  ;Extract Install Options files
  Call Initialize_____Plugins
  SetDetailsPrint both
  
  File /oname=$PLUGINSDIR\test.nsi "test.nsi"
FunctionEnd

Function SetCustom

 Push ${TEMP1}

  InstallOptions::dialog "$PLUGINSDIR\test.nsi"
  Pop ${TEMP1}

  StrCmp ${TEMP1} "cancel" "" +3
    Pop ${TEMP1}
    Quit

  StrCmp ${TEMP1} "back" "" +3
    Pop ${TEMP1}
    Abort
    
 Pop ${TEMP1}
  
FunctionEnd