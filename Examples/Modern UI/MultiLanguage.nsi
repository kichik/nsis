;NSIS Modern User Interface version 1.66
;Multilingual Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;Configuration

  ;General
  Name "Modern UI Test 1.66"
  OutFile "MultiLanguage.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\Modern UI Test"
  
  ;Get install folder from registry if available
  InstallDirRegKey HKCU "Software\Modern UI Test" ""

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKCU" 
  !define MUI_LANGDLL_REGISTRY_KEY "Software\Modern UI Test" 
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

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
  !insertmacro MUI_LANGUAGE "Estonian"
  !insertmacro MUI_LANGUAGE "Turkish"
  !insertmacro MUI_LANGUAGE "Lithuanian"
  !insertmacro MUI_LANGUAGE "Catalan"
  !insertmacro MUI_LANGUAGE "Serbian"

;--------------------------------
;Reserve Files
  
  ;These files should be inserted before other files in the data block
  ;Keep these lines before any File command
  ;Only for BZIP2 (solid) compression
  !insertmacro MUI_RESERVEFILE_LANGDLL

;--------------------------------
;Installer Sections

Section "Dummy Section" SecDummy

  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN STUFF HERE!
  
  ;Store install folder
  WriteRegStr HKCU "Software\Modern UI Test" "" $INSTDIR
  
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

  LangString DESC_SecDummy ${LANG_ENGLISH} "Test section: English description"
  LangString DESC_SecDummy ${LANG_FRENCH} "Test section: French description"
  LangString DESC_SecDummy ${LANG_GERMAN} "Test section: German description"
  LangString DESC_SecDummy ${LANG_SPANISH} "Test section: Spanish description"
  LangString DESC_SecDummy ${LANG_SIMPCHINESE} "Test section: Simplified Chinese description"
  LangString DESC_SecDummy ${LANG_TRADCHINESE} "Test section: Traditional Chinese description"
  LangString DESC_SecDummy ${LANG_JAPANESE} "Test section: Japanese description"
  LangString DESC_SecDummy ${LANG_KOREAN} "Test section: Korean description"
  LangString DESC_SecDummy ${LANG_ITALIAN} "Test section: Italian description"
  LangString DESC_SecDummy ${LANG_DUTCH} "Test section: Dutch description"
  LangString DESC_SecDummy ${LANG_DANISH} "Test section: Danish description"
  LangString DESC_SecDummy ${LANG_SWEDISH} "Test section: Swedish description"
  LangString DESC_SecDummy ${LANG_GREEK} "Test section: Greek description"
  LangString DESC_SecDummy ${LANG_RUSSIAN} "Test section: Russian description"
  LangString DESC_SecDummy ${LANG_PORTUGUESE} "Test section: Portuguese description"
  LangString DESC_SecDummy ${LANG_PORTUGUESEBR} "Test section: Portuguese (Brasil) description"
  LangString DESC_SecDummy ${LANG_POLISH} "Test section: Polish description"
  LangString DESC_SecDummy ${LANG_UKRAINIAN} "Test section: Ukrainian description"
  LangString DESC_SecDummy ${LANG_CZECH} "Test section: Czechian description"
  LangString DESC_SecDummy ${LANG_SLOVAK} "Test section: Slovakian description"
  LangString DESC_SecDummy ${LANG_CROATIAN} "Test section: Slovakian description"
  LangString DESC_SecDummy ${LANG_BULGARIAN} "Test section: Bulgarian description"
  LangString DESC_SecDummy ${LANG_HUNGARIAN} "Test section: Hungarian description"
  LangString DESC_SecDummy ${LANG_THAI} "Test section: Thai description"
  LangString DESC_SecDummy ${LANG_ROMANIAN} "Test section: Romanian description"
  LangString DESC_SecDummy ${LANG_MACEDONIAN} "Test section: Macedonian description"
  LangString DESC_SecDummy ${LANG_ESTONIAN} "Test section: Estonian description"
  LangString DESC_SecDummy ${LANG_TURKISH} "Test section: Turkish description"
  LangString DESC_SecDummy ${LANG_LITHUANIAN} "Test section: Lithuanian description"
  LangString DESC_SecDummy ${LANG_CATALAN} "Test section: Catalan description"
  LangString DESC_SecDummy ${LANG_SERBIAN} "Test section: Serbian description"

  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecDummy} $(DESC_SecDummy)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN STUFF HERE!

  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  DeleteRegKey /ifempty HKCU "Software\Modern UI Test"

SectionEnd

;--------------------------------
;Uninstaller Functions

Function un.onInit

  !insertmacro MUI_UNGETLANGUAGE
  
FunctionEnd