;NSIS Modern User Interface version 1.5
;MultiLanguage Example Script
;Written by Joost Verburg

!define MUI_PRODUCT "Test Software" ;Define your own software name here
!define MUI_VERSION "1.0" ;Define your own software version here

!include "${NSISDIR}\Contrib\Modern UI\System.nsh"

;--------------------------------
;Configuration

  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTSPAGE
  !define MUI_DIRECTORYPAGE
  !define MUI_ABORTWARNING
  !define MUI_UNINSTALLER

  ;Languages
  !insertmacro MUI_LANGUAGE "English"
  !insertmacro MUI_LANGUAGE "French"
  !insertmacro MUI_LANGUAGE "German"
  !insertmacro MUI_LANGUAGE "Spanish"
  !insertmacro MUI_LANGUAGE "SimpChinese"
  !insertmacro MUI_LANGUAGE "TradChinese"    
  !insertmacro MUI_LANGUAGE "Japanese"    
  !insertmacro MUI_LANGUAGE "Italian"
  !insertmacro MUI_LANGUAGE "Dutch"
  !insertmacro MUI_LANGUAGE "Polish"
  !insertmacro MUI_LANGUAGE "Greek"
  !insertmacro MUI_LANGUAGE "Russian"
  !insertmacro MUI_LANGUAGE "PortugueseBR"
  !insertmacro MUI_LANGUAGE "Ukrainian"
  
  OutFile "MultiLanguage.exe"

  ;License page
  LicenseData /LANG=${LANG_ENGLISH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_FRENCH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_GERMAN} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_SPANISH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_SIMPCHINESE} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_TRADCHINESE} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_JAPANESE} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_ITALIAN} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_DUTCH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_POLISH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_GREEK} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_RUSSIAN} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_PORTUGUESEBR} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_UKRAINIAN} "${NSISDIR}\Contrib\Modern UI\License.txt"

  ;Component-selection page
    ;Titles
    LangString TITLE_SecCopyUI ${LANG_ENGLISH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_FRENCH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_GERMAN} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_SPANISH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_SIMPCHINESE} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_TRADCHINESE} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_JAPANESE} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_ITALIAN} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_DUTCH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_POLISH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_GREEK} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_RUSSIAN} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_PORTUGUESEBR} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_UKRAINIAN} "modern.exe"
    
    ;Descriptions
    LangString DESC_SecCopyUI ${LANG_ENGLISH} "modern.exe: English description"
    LangString DESC_SecCopyUI ${LANG_FRENCH} "modern.exe: French description"
    LangString DESC_SecCopyUI ${LANG_GERMAN} "modern.exe: German description"
    LangString DESC_SecCopyUI ${LANG_SPANISH} "modern.exe: Spanish description"
    LangString DESC_SecCopyUI ${LANG_SIMPCHINESE} "modern.exe: Simplified Chinese description"
    LangString DESC_SecCopyUI ${LANG_TRADCHINESE} "modern.exe: Traditional Chinese description"
    LangString DESC_SecCopyUI ${LANG_JAPANESE} "modern.exe: Japanese description"
    LangString DESC_SecCopyUI ${LANG_ITALIAN} "modern.exe: Italian description"
    LangString DESC_SecCopyUI ${LANG_DUTCH} "modern.exe: Dutch description"
    LangString DESC_SecCopyUI ${LANG_POLISH} "modern.exe: Polish description"
    LangString DESC_SecCopyUI ${LANG_GREEK} "modern.exe: Greek description"
    LangString DESC_SecCopyUI ${LANG_RUSSIAN} "modern.exe: Greek description"
    LangString DESC_SecCopyUI ${LANG_PORTUGUESEBR} "modern.exe: Portuguese (Brasil) description"
    LangString DESC_SecCopyUI ${LANG_UKRAINIAN} "modern.exe: Ukrainian description"
    
  ;Folder-selection page
  InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
  
;--------------------------------
;Modern UI System

!insertmacro MUI_SYSTEM
  
;--------------------------------
;Installer Sections

Section $(TITLE_SecCopyUI) SecCopyUI

  ;ADD YOUR OWN STUFF HERE!

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"
  
  ;Write language to the registry (for the uninstaller)
  WriteRegStr HKCU "Software\${MUI_PRODUCT}" "Installer Language" $LANGUAGE
  
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
SectionEnd
  
!insertmacro MUI_SECTIONS_FINISHHEADER ;Insert this macro after the sections

;--------------------------------
;Installer Functions

Function .onInit

  ;Language selection

  Push Tahoma
  Push 8

  Push ${LANG_ENGLISH}
  Push "${MUI_ENGLISH_LANGNAME}"
  Push ${LANG_FRENCH}
  Push "${MUI_FRENCH_LANGNAME}"
  Push ${LANG_GERMAN}
  Push "${MUI_GERMAN_LANGNAME}"
  Push ${LANG_SPANISH}
  Push "${MUI_SPANISH_LANGNAME}"
  Push ${LANG_TRADCHINESE}
  Push "${MUI_TRADCHINESE_LANGNAME}"
  Push ${LANG_SIMPCHINESE}
  Push "${MUI_SIMPCHINESE_LANGNAME}"
  Push ${LANG_JAPANESE}
  Push "${MUI_JAPANESE_LANGNAME}"
  Push ${LANG_ITALIAN}
  Push "${MUI_ITALIAN_LANGNAME}"
  Push ${LANG_DUTCH}
  Push "${MUI_DUTCH_LANGNAME}"
  Push ${LANG_POLISH}
  Push "${MUI_POLISH_LANGNAME}"
  Push ${LANG_GREEK}
  Push "${MUI_GREEK_LANGNAME}"
  Push ${LANG_RUSSIAN}
  Push "${MUI_RUSSIAN_LANGNAME}"
  Push ${LANG_PORTUGUESEBR}
  Push "${MUI_PORTUGUESEBR_LANGNAME}"
  Push ${LANG_UKRAINIAN}
  Push "${MUI_UKRAINIAN_LANGNAME}"
  
  Push 14F ;14 = number of languages, F = change font

  LangDLL::LangDialog "Installer Language" "Please select a language."

  Pop $LANGUAGE
  StrCmp $LANGUAGE "cancel" 0 +2
    Abort

FunctionEnd

;--------------------------------
;Descriptions

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
  
  DeleteRegValue HKCU "Software\${MUI_PRODUCT}" "Installer Language"

  ;Display the Finish header
  !insertmacro MUI_UNFINISHHEADER

SectionEnd

;--------------------------------
;Uninstaller Functions

Function un.onInit
  ReadRegStr $LANGUAGE HKCU "Software\${MUI_PRODUCT}" "Installer Language"
FunctionEnd