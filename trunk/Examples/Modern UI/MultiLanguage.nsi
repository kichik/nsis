;NSIS Modern User Interface version 1.65
;Multilingual Example Script
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
  OutFile "MultiLanguage.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
  
  ;Get install folder from registry if available
  InstallDirRegKey HKCU "Software\${MUI_PRODUCT}" ""
  
;--------------------------------
;Pages

  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Modern UI Configuration

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKCU" 
  !define MUI_LANGDLL_REGISTRY_KEY "Software\${MUI_PRODUCT}" 
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"

  !define MUI_ABORTWARNING

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"
  !insertmacro MUI_LANGUAGE "French"
  !insertmacro MUI_LANGUAGE "German"
  !insertmacro MUI_LANGUAGE "Spanish"
  !insertmacro MUI_LANGUAGE "SimpChinese"
  !insertmacro MUI_LANGUAGE "TradChinese"    
  !insertmacro MUI_LANGUAGE "Japanese"
  !insertmacro MUI_LANGUAGE "Korean"
  !insertmacro MUI_LANGUAGE "Italian"
  !insertmacro MUI_LANGUAGE "Dutch"
  !insertmacro MUI_LANGUAGE "Danish"
  !insertmacro MUI_LANGUAGE "Swedish"
  !insertmacro MUI_LANGUAGE "Greek"
  !insertmacro MUI_LANGUAGE "Russian"
  !insertmacro MUI_LANGUAGE "Portuguese"
  !insertmacro MUI_LANGUAGE "PortugueseBR"
  !insertmacro MUI_LANGUAGE "Polish"
  !insertmacro MUI_LANGUAGE "Ukrainian"
  !insertmacro MUI_LANGUAGE "Czech"
  !insertmacro MUI_LANGUAGE "Slovak"
  !insertmacro MUI_LANGUAGE "Croatian"
  !insertmacro MUI_LANGUAGE "Bulgarian"
  !insertmacro MUI_LANGUAGE "Hungarian"
  !insertmacro MUI_LANGUAGE "Thai"
  !insertmacro MUI_LANGUAGE "Romanian"
  !insertmacro MUI_LANGUAGE "Macedonian"
  !insertmacro MUI_LANGUAGE "Turkish"
  !insertmacro MUI_LANGUAGE "Lithuanian"
  !insertmacro MUI_LANGUAGE "Catalan"

;--------------------------------
;Reserve Files
  
  ;Things that need to be extracted on first (keep these lines before any File command!)
  ;Only for BZIP2 compression
  !insertmacro MUI_RESERVEFILE_LANGDLL

;--------------------------------
;Installer Sections

Section "Dummy Test File" SecCopyUI

  ;ADD YOUR OWN STUFF HERE!

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"
  
  ;Store install folder
  WriteRegStr HKCU "Software\${MUI_PRODUCT}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

;--------------------------------
;Installer Functions

Function .onInit

  !insertmacro MUI_LANGDLL_DISPLAY

FunctionEnd

;--------------------------------
;Descriptions

LangString DESC_SecCopyUI ${LANG_ENGLISH} "modern.exe: English description"
LangString DESC_SecCopyUI ${LANG_FRENCH} "modern.exe: French description"
LangString DESC_SecCopyUI ${LANG_GERMAN} "modern.exe: German description"
LangString DESC_SecCopyUI ${LANG_SPANISH} "modern.exe: Spanish description"
LangString DESC_SecCopyUI ${LANG_SIMPCHINESE} "modern.exe: Simplified Chinese description"
LangString DESC_SecCopyUI ${LANG_TRADCHINESE} "modern.exe: Traditional Chinese description"
LangString DESC_SecCopyUI ${LANG_JAPANESE} "modern.exe: Japanese description"
LangString DESC_SecCopyUI ${LANG_KOREAN} "modern.exe: Korean description"
LangString DESC_SecCopyUI ${LANG_ITALIAN} "modern.exe: Italian description"
LangString DESC_SecCopyUI ${LANG_DUTCH} "modern.exe: Dutch description"
LangString DESC_SecCopyUI ${LANG_DANISH} "modern.exe: Danish description"
LangString DESC_SecCopyUI ${LANG_SWEDISH} "modern.exe: Swedish description"
LangString DESC_SecCopyUI ${LANG_GREEK} "modern.exe: Greek description"
LangString DESC_SecCopyUI ${LANG_RUSSIAN} "modern.exe: Russian description"
LangString DESC_SecCopyUI ${LANG_PORTUGUESE} "modern.exe: Portuguese description"
LangString DESC_SecCopyUI ${LANG_PORTUGUESEBR} "modern.exe: Portuguese (Brasil) description"
LangString DESC_SecCopyUI ${LANG_POLISH} "modern.exe: Polish description"
LangString DESC_SecCopyUI ${LANG_UKRAINIAN} "modern.exe: Ukrainian description"
LangString DESC_SecCopyUI ${LANG_CZECH} "modern.exe: Czechian description"
LangString DESC_SecCopyUI ${LANG_SLOVAK} "modern.exe: Slovakian description"
LangString DESC_SecCopyUI ${LANG_CROATIAN} "modern.exe: Slovakian description"
LangString DESC_SecCopyUI ${LANG_BULGARIAN} "modern.exe: Bulgarian description"
LangString DESC_SecCopyUI ${LANG_HUNGARIAN} "modern.exe: Hungarian description"
LangString DESC_SecCopyUI ${LANG_THAI} "modern.exe: Thai description"
LangString DESC_SecCopyUI ${LANG_ROMANIAN} "modern.exe: Romanian description"
LangString DESC_SecCopyUI ${LANG_MACEDONIAN} "modern.exe: Macedonian description"
LangString DESC_SecCopyUI ${LANG_TURKISH} "modern.exe: Turkish description"
LangString DESC_SecCopyUI ${LANG_LITHUANIAN} "modern.exe: Lithuanian description"
LangString DESC_SecCopyUI ${LANG_CATALAN} "modern.exe: Catalan description"

!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyUI} $(DESC_SecCopyUI)
!insertmacro MUI_FUNCTIONS_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN STUFF HERE!

  Delete "$INSTDIR\modern.exe"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  DeleteRegKey /ifempty HKCU "Software\${MUI_PRODUCT}"

SectionEnd

;--------------------------------
;Uninstaller Functions

Function un.onInit

  !insertmacro MUI_UNGETLANGUAGE
  
FunctionEnd