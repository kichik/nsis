;NSIS Modern User Interface version 1.65
;Start Menu Folder Selection Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;Product Info

  !define MUI_PRODUCT "Modern UI Test"
  !define MUI_VERSION "1.65"

;--------------------------------
;Configuration

  ;General
  OutFile "StartMenu.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
  
  ;Get install folder from registry if available
  InstallDirRegKey HKCU "Software\${MUI_PRODUCT}" ""

;--------------------------------
;Variables

  Var TEMP

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_STARTMENU
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Modern UI Configuration

  !define MUI_ABORTWARNING
  
  ;Remember the Start Menu Folder
  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU" 
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\${MUI_PRODUCT}" 
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Data
  
  LicenseData "${NSISDIR}\Contrib\Modern UI\License.txt"

;--------------------------------
;Reserve Files
  
  ;Things that need to be extracted on first (keep these lines before any File command!)
  ;Only for BZIP2 compression
  !insertmacro MUI_RESERVEFILE_STARTMENU

;--------------------------------
;Installer Sections

Section "Dummy Section" SecDummy

  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN STUFF HERE!
  
  ;Store install folder
  WriteRegStr HKCU "Software\${MUI_PRODUCT}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
  !insertmacro MUI_STARTMENU_WRITE_BEGIN
    
    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$MUI_STARTMENU_FOLDER"
    CreateShortCut "$SMPROGRAMS\$MUI_STARTMENU_FOLDER\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
  
  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

;--------------------------------
;Descriptions

  LangString DESC_SecDummy ${LANG_ENGLISH} "A test section."

  !insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecDummy} $(DESC_SecDummy)
  !insertmacro MUI_FUNCTIONS_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN STUFF HERE!

  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"
  
  !insertmacro MUI_STARTMENU_GETFOLDER $TEMP
    
  Delete "$SMPROGRAMS\$TEMP\Uninstall.lnk"
  
  ;Delete empty start menu parent diretories
  StrCpy $TEMP "$SMPROGRAMS\$TEMP"
 
  startMenuDeleteLoop:
    RMDir ${TEMP}
    GetFullPathName $TEMP "$TEMP\.."
    
    IfErrors startMenuDeleteLoopDone
  
    StrCmp ${TEMP} $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop
  startMenuDeleteLoopDone:

  DeleteRegKey /ifempty HKCU "Software\${MUI_PRODUCT}"

SectionEnd