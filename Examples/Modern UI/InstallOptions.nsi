;NSIS Modern User Interface version 1.4
;Install Options Example Script
;Written by Joost Verburg

!define NAME "Test Software" ;Define your own software name here
!define VERSION "1.0" ;Define your own software version here

!include "${NSISDIR}\Contrib\Modern UI\System.nsh"

;--------------------------------
;Configuration
  
  !define MUI_INSTALLOPTIONS
  
  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTSPAGE
  !define MUI_DIRECTORYPAGE
  !define MUI_ABORTWARNING
  !define MUI_UNINSTALLER
  
  !define TEMP1 $R0

  ;Language
    ;English
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\English.nsh"

  ;General
  OutFile "InstallOptions.exe"
  Name "${NAME} ${VERSION}"
  
  ;Page order
  !define MUI_CUSTOMPAGECOMMANDS ;Use customized pages
  
  !insertmacro MUI_PAGECOMMAND_LICENSE
  Page custom SetCustomA
  Page custom SetCustomB
  !insertmacro MUI_PAGECOMMAND_COMPONENTS
  !insertmacro MUI_PAGECOMMAND_DIRECTORY
  Page custom SetCustomC
  !insertmacro MUI_PAGECOMMAND_INSTFILES
  
  ;License page
  LicenseData "${NSISDIR}\Contrib\Modern UI\License.txt"

  ;Component-selection page
    ;Descriptions
    LangString DESC_SecCopyUI ${LANG_ENGLISH} "Copy the modern.exe file to the application folder."
    LangString DESC_SecCreateUninst ${LANG_ENGLISH} "Create a uninstaller which can automatically delete ${NAME}."

  ;Folder-selection page
  InstallDir "$PROGRAMFILES\${NAME}"
  
  ;Install Options pages
  LangString TEXT_IO_TITLE ${LANG_ENGLISH} "Install Options Page"
  LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Create your own dialog!"
  
  ;Uninstaller
  !define MUI_UNCUSTOMPAGECOMMANDS
  !insertmacro MUI_UNPAGECOMMAND_CONFIRM
  !insertmacro MUI_UNPAGECOMMAND_INSTFILES
  
  ;Things that need to be extracted on startup (keep these lines before any File command!)
  ;Use ReserveFile for your own Install Options ini files too!
  ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
  ReserveFile "ioA.ini"
  ReserveFile "ioB.ini"
  ReserveFile "ioC.ini"

;--------------------------------
;Installer Sections

Function .onInit
  ;Init InstallOptions
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioA.ini"
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioB.ini"
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioC.ini"
  ;Titles for Install Options dialogs
  !insertmacro MUI_INSTALLOPTIONS_WRITE "ioA.ini" "Settings" "Title" "$(MUI_TEXT_SETUPCAPTION): Custom page A"
  !insertmacro MUI_INSTALLOPTIONS_WRITE "ioB.ini" "Settings" "Title" "$(MUI_TEXT_SETUPCAPTION): Custom page B"
  !insertmacro MUI_INSTALLOPTIONS_WRITE "ioC.ini" "Settings" "Title" "$(MUI_TEXT_SETUPCAPTION): Custom page C"
  !ifdef MUI_ABORTWARNING
    ;Abort warnings for Install Options dialogs
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioA.ini" "Settings" "CancelConfirm" "$(MUI_TEXT_ABORTWARNING)"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioA.ini" "Settings" "CancelConfirmCaption" "$(MUI_TEXT_SETUPCAPTION)"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioB.ini" "Settings" "CancelConfirm" "$(MUI_TEXT_ABORTWARNING)"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioB.ini" "Settings" "CancelConfirmCaption" "$(MUI_TEXT_SETUPCAPTION)"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioC.ini" "Settings" "CancelConfirm" "$(MUI_TEXT_ABORTWARNING)"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioC.ini" "Settings" "CancelConfirmCaption" "$(MUI_TEXT_SETUPCAPTION)"
  !endif
FunctionEnd

Section "modern.exe" SecCopyUI

  ;Add your stuff here

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"
  
  ;Read a value from an Install Options INI File
  !insertmacro MUI_INSTALLOPTIONS_READ ${TEMP1} "ioC.ini" "Field 2" "State"
  StrCmp ${TEMP1} "1" "" +2
    ;Checked
    MessageBox MB_OK "A MessageBox..."
    
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

Section ""

  ;Invisible section to display the Finish header
  !insertmacro MUI_FINISHHEADER

SectionEnd

;--------------------------------
;Installer Functions

Function SetCustomA
  !insertmacro MUI_HEADER_TEXT $(TEXT_IO_TITLE) $(TEXT_IO_SUBTITLE)
  !insertmacro MUI_INSTALLOPTIONS_SHOW "ioA.ini"
FunctionEnd

Function SetCustomB
  !insertmacro MUI_HEADER_TEXT $(TEXT_IO_TITLE) $(TEXT_IO_SUBTITLE)
  !insertmacro MUI_INSTALLOPTIONS_SHOW "ioB.ini"
FunctionEnd

Function SetCustomC
  !insertmacro MUI_HEADER_TEXT $(TEXT_IO_TITLE) $(TEXT_IO_SUBTITLE)
  !insertmacro MUI_INSTALLOPTIONS_SHOW "ioC.ini"
FunctionEnd

;--------------------------------
;Modern UI System

!insertmacro MUI_SYSTEM

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;Add your stuff here

  Delete "$INSTDIR\modern.exe"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  !insertmacro MUI_UNFINISHHEADER

SectionEnd

;eof