;NSIS Modern User Interface version 1.61
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
  !define MUI_UNCONFIRMPAGE

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
  !insertmacro MUI_LANGUAGE "Czech"
  !insertmacro MUI_LANGUAGE "Bulgarian"
  
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
  LicenseData /LANG=${LANG_CZECH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_BULGARIAN} "${NSISDIR}\Contrib\Modern UI\License.txt"

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
    LangString TITLE_SecCopyUI ${LANG_CZECH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_BULGARIAN} "modern.exe"
    
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
    LangString DESC_SecCopyUI ${LANG_CZECH} "modern.exe: Czechian description"
    LangString DESC_SecCopyUI ${LANG_BULGARIAN} "modern.exe: Bulgarian description"
    
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
  
;Display the Finish header
;Insert this macro after the sections if you are not using a finish page
!insertmacro MUI_SECTIONS_FINISHHEADER

;--------------------------------
;Installer Functions

Function .onInit

  ;Language selection

  ;Font
  Push Tahoma
  Push 8

  ;Languages
  !insertmacro MUI_LANGDLL_PUSH "English"
  !insertmacro MUI_LANGDLL_PUSH "French"
  !insertmacro MUI_LANGDLL_PUSH "German"
  !insertmacro MUI_LANGDLL_PUSH "Spanish"
  !insertmacro MUI_LANGDLL_PUSH "SimpChinese"
  !insertmacro MUI_LANGDLL_PUSH "TradChinese"    
  !insertmacro MUI_LANGDLL_PUSH "Japanese"    
  !insertmacro MUI_LANGDLL_PUSH "Italian"
  !insertmacro MUI_LANGDLL_PUSH "Dutch"
  !insertmacro MUI_LANGDLL_PUSH "Polish"
  !insertmacro MUI_LANGDLL_PUSH "Greek"
  !insertmacro MUI_LANGDLL_PUSH "Russian"
  !insertmacro MUI_LANGDLL_PUSH "PortugueseBR"
  !insertmacro MUI_LANGDLL_PUSH "Ukrainian"
  !insertmacro MUI_LANGDLL_PUSH "Czech"
  !insertmacro MUI_LANGDLL_PUSH "Bulgarian"
  
  Push 16F ;16 = number of languages, F = change font

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

  ;Get language from registry
  ReadRegStr $LANGUAGE HKCU "Software\${MUI_PRODUCT}" "Installer Language"
  
FunctionEnd