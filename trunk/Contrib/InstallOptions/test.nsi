;InstallOptions Test Script
;Written by Joost Verburg
;--------------------------

!define TEMP1 $R0 ;Temp variable

;The name of the installer
Name "InstallOptions Test"

;The file to write
OutFile "Test.exe"

;The default installation directory
InstallDir "$PROGRAMFILES\IOTest"

;Things that need to be extracted on startup (keep these lines before any File command!)
;Only useful for BZIP2 compression
;Use ReserveFile for your own InstallOptions INI files too!

ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
ReserveFile "test.ini"

;Texts on the dialogs
DirText "Choose a directory"
LicenseText "A license"
LicenseData "..\..\License.txt"
ComponentText "Choose components"

;Order of pages
Page license
Page custom SetCustom ": Testing InstallOptions" ;Custom page. InstallOptions gets called in SetCustom.
Page components
Page directory
Page instfiles

Section "Components"

  ;Get Install Options dialog user input

  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.ini" "Field 1" "State"
  MessageBox MB_OK "Install X=${TEMP1}"
  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.ini" "Field 2" "State"
  MessageBox MB_OK "Install Y=${TEMP1}"
  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.ini" "Field 3" "State"
  MessageBox MB_OK "Install Z=${TEMP1}"
  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.ini" "Field 4" "State"
  MessageBox MB_OK "File=${TEMP1}"
  ReadINIStr ${TEMP1} "$PLUGINSDIR\test.ini" "Field 5" "State"
  MessageBox MB_OK "Dir=${TEMP1}"
  
SectionEnd

Function .onInit

  ;Extract InstallOptions files
  ;$PLUGINSDIR will automatically be removed when the installer closes
  
  InitPluginsDir
  File /oname=$PLUGINSDIR\test.ini "test.ini"
  
FunctionEnd

Function SetCustom

  ;Display the InstallOptions dialog
  
  Push ${TEMP1}

    InstallOptions::dialog "$PLUGINSDIR\test.ini"
    Pop ${TEMP1}
  
  Pop ${TEMP1}

FunctionEnd