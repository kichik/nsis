;NSIS Modern User Interface
;InstallOptions Example Script
;Written by Joost Verburg

;---------------------
;Header files

  !include "MUI.nsh"
  !include "InstallOptions.nsh"

;--------------------------------
;General

  ;Name and file
  Name "Modern UI Test"
  OutFile "InstallOptions.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\Modern UI Test"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "Software\Modern UI Test" ""

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE "${NSISDIR}\Docs\Modern UI\License.txt"
  Page custom CustomPageA
  !insertmacro MUI_PAGE_COMPONENTS
  Page custom CustomPageB
  !insertmacro MUI_PAGE_DIRECTORY
  Page custom CustomPageC
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Reserve Files
  
  ;If you are using solid compression, files that are required before
  ;the actual installation should be stored first in the data block,
  ;because this will make your installer start faster.
  
  ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"

  ReserveFile "ioA.ini"
  ReserveFile "ioB.ini"
  ReserveFile "ioC.ini"

;--------------------------------
;Variables

  Var IniValue

;--------------------------------
;Installer Sections

Section "Dummy Section" SecDummy

  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  
  ;Store installation folder
  WriteRegStr HKCU "Software\Modern UI Test" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
  ;Read a value from an InstallOptions INI file
  !insertmacro INSTALLOPTIONS_READ $IniValue "ioC.ini" "Field 2" "State"
  
  ;Display a messagebox if check box was checked
  StrCmp $IniValue "1" "" +2
    MessageBox MB_OK "You checked the check box, here is the MessageBox..."

SectionEnd

;--------------------------------
;Installer Functions

Function .onInit

  ;Extract InstallOptions INI files
  !insertmacro INSTALLOPTIONS_EXTRACT "ioA.ini"
  !insertmacro INSTALLOPTIONS_EXTRACT "ioB.ini"
  !insertmacro INSTALLOPTIONS_EXTRACT "ioC.ini"
  
FunctionEnd

LangString TEXT_IO_TITLE ${LANG_ENGLISH} "InstallOptions page"
LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "This is a page created using the InstallOptions plug-in."

Function CustomPageA

  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
  !insertmacro INSTALLOPTIONS_DISPLAY "ioA.ini"

FunctionEnd

Function CustomPageB

  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
  !insertmacro INSTALLOPTIONS_DISPLAY "ioB.ini"

FunctionEnd

Function CustomPageC

  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
  !insertmacro INSTALLOPTIONS_DISPLAY "ioC.ini"

FunctionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecDummy ${LANG_ENGLISH} "A test section."

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecDummy} $(DESC_SecDummy)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...

  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  DeleteRegKey /ifempty HKCU "Software\Modern UI Test"

SectionEnd