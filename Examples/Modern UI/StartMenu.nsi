;NSIS Modern User Interface version 1.66
;Start Menu Folder Selection Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;Product Info

  !define MUI_PRODUCT "Modern UI Test"
  !define MUI_VERSION "1.66"

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

  Var MUI_TEMP

;--------------------------------
;Modern UI Configuration

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE "${NSISDIR}\Contrib\Modern UI\License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  
  ;Start Menu Folder Page Configuration
  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU" 
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\${MUI_PRODUCT}" 
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"
  
  !insertmacro MUI_PAGE_STARTMENU
  
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

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
    CreateDirectory "$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}"
    CreateShortCut "$SMPROGRAMS\${MUI_STARTMENUPAGE_VARIABLE}\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
  
  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

;--------------------------------
;Descriptions

  LangString DESC_SecDummy ${LANG_ENGLISH} "A test section."

  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecDummy} $(DESC_SecDummy)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN STUFF HERE!

  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"
  
  !insertmacro MUI_STARTMENU_GETFOLDER $MUI_TEMP
    
  Delete "$SMPROGRAMS\$MUI_TEMP\Uninstall.lnk"
  
  ;Delete empty start menu parent diretories
  StrCpy $MUI_TEMP "$SMPROGRAMS\$MUI_TEMP"
 
  startMenuDeleteLoop:
    RMDir $MUI_TEMP
    GetFullPathName $MUI_TEMP "$MUI_TEMP\.."
    
    IfErrors startMenuDeleteLoopDone
  
    StrCmp $MUI_TEMP $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop
  startMenuDeleteLoopDone:

  DeleteRegKey /ifempty HKCU "Software\${MUI_PRODUCT}"

SectionEnd
