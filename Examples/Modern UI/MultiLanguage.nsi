;NSIS Modern UI version 1.3
;MultiLanguage & LangDLL Example Script
;Written by Joost Verburg

!define NAME "Test Software" ;Define your own software name here
!define VERSION "1.0" ;Define your own software version here

!verbose 3
  !include "${NSISDIR}\Contrib\Modern UI\System.nsh"
!verbose 4

;--------------------------------
;Configuration

  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTPAGE
  !define MUI_DIRSELECTPAGE
  !define MUI_ABORTWARNING
  !define MUI_UNINSTALLER

  ;Languages
    ;English
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\English.nsh"
       
    ;French
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\French.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\French.nsh"
    
    ;German
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\German.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\German.nsh"
    
    ;Dutch
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\Dutch.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\Dutch.nsh"
    
    ;Polish
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\Polish.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\Polish.nsh"
    
    ;Greek
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\Greek.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\Greek.nsh"
    
    ;Simplified Chinese
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\SimpChinese.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\SimpChinese.nsh"

    ;Traditional Chinese
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\TradChinese.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\TradChinese.nsh"    

  ;General
  Name /LANG=${LANG_ENGLISH} "${NAME} ${VERSION}"
  Name /LANG=${LANG_FRENCH} "${NAME} ${VERSION}"
  Name /LANG=${LANG_GERMAN} "${NAME} ${VERSION}"
  Name /LANG=${LANG_DUTCH} "${NAME} ${VERSION}"
  Name /LANG=${LANG_POLISH} "${NAME} ${VERSION}"
  Name /LANG=${LANG_GREEK} "${NAME} ${VERSION}"
  Name /LANG=${LANG_SIMPCHINESE} "${NAME} ${VERSION}"
  Name /LANG=${LANG_TRADCHINESE} "${NAME} ${VERSION}"
  OutFile "MultiLanguage.exe"

  ;User interface - icons, ui file, check bitmap, progress bar etc.
  !insertmacro MUI_INTERFACE "modern.exe" "modern-install.ico" "modern-uninstall.ico" "modern.bmp" "smooth" "Tahoma" "$9" ;$9 is the variable used to store the current page, do not use this var!

  ;License dialog
  LicenseData /LANG=${LANG_ENGLISH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_FRENCH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_GERMAN} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_DUTCH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_POLISH} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_GREEK} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_SIMPCHINESE} "${NSISDIR}\Contrib\Modern UI\License.txt"
  LicenseData /LANG=${LANG_TRADCHINESE} "${NSISDIR}\Contrib\Modern UI\License.txt"

  ;Component-select dialog
    ;Titles
    LangString TITLE_SecCopyUI ${LANG_ENGLISH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_FRENCH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_GERMAN} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_DUTCH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_POLISH} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_GREEK} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_SIMPCHINESE} "modern.exe"
    LangString TITLE_SecCopyUI ${LANG_TRADCHINESE} "modern.exe"
    LangString TITLE_SecCreateUninst ${LANG_ENGLISH} "Uninstaller (English)"
    LangString TITLE_SecCreateUninst ${LANG_FRENCH} "Uninstaller (French)"
    LangString TITLE_SecCreateUninst ${LANG_GERMAN} "Uninstaller (German)"
    LangString TITLE_SecCreateUninst ${LANG_DUTCH} "Uninstaller (Dutch)"
    LangString TITLE_SecCreateUninst ${LANG_POLISH} "Uninstaller (Polish)"
    LangString TITLE_SecCreateUninst ${LANG_GREEK} "Uninstaller (Greek)"
    LangString TITLE_SecCreateUninst ${LANG_SIMPCHINESE} "Uninstaller (Simp Chinese)"
    LangString TITLE_SecCreateUninst ${LANG_TRADCHINESE} "Uninstaller (Trad Chinese)"
    
    ;Descriptions
    LangString DESC_SecCopyUI ${LANG_ENGLISH} "modern.exe: English description"
    LangString DESC_SecCopyUI ${LANG_FRENCH} "modern.exe: French description"
    LangString DESC_SecCopyUI ${LANG_GERMAN} "modern.exe: German description"
    LangString DESC_SecCopyUI ${LANG_DUTCH} "modern.exe: Dutch description"
    LangString DESC_SecCopyUI ${LANG_POLISH} "modern.exe: Polish description"
    LangString DESC_SecCopyUI ${LANG_GREEK} "modern.exe: Greek description"
    LangString DESC_SecCopyUI ${LANG_SIMPCHINESE} "modern.exe: Simplified Chinese description"
    LangString DESC_SecCopyUI ${LANG_TRADCHINESE} "modern.exe: Traditional Chinese description"
    LangString DESC_SecCreateUninst ${LANG_ENGLISH} "Uninstaller: English description"
    LangString DESC_SecCreateUninst ${LANG_FRENCH} "Uninstaller: French description"
    LangString DESC_SecCreateUninst ${LANG_GERMAN} "Uninstaller: German description"
    LangString DESC_SecCreateUninst ${LANG_DUTCH} "Uninstaller: Dutch description"
    LangString DESC_SecCreateUninst ${LANG_POLISH} "Uninstaller: Polish description"
    LangString DESC_SecCreateUninst ${LANG_GREEK} "Uninstaller: Greek description"
    LangString DESC_SecCreateUninst ${LANG_SIMPCHINESE} "Uninstaller: Simplified Chinese description"
    LangString DESC_SecCreateUninst ${LANG_TRADCHINESE} "Uninstaller: Traditional Chinese description"

  ;Folder-select dialog
  InstallDir "$PROGRAMFILES\${NAME}"
  
;--------------------------------
;Installer Sections

Section $(TITLE_SecCopyUI) SecCopyUI

  ;Add your stuff here

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"

SectionEnd

Section $(TITLE_SecCreateUninst) SecCreateUninst

  ;Add your stuff here

  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

Section ""

  ;Invisible section to display the Finish header & write the language to the registry
  
  WriteRegStr HKCU "Software\${NAME}" "Installer Language" $LANGUAGE
  
  !insertmacro MUI_FINISHHEADER SetPage

SectionEnd

;--------------------------------
;Installer Functions

Function .onInit

  Push Tahoma
  Push 8

  Push ${LANG_ENGLISH}
  Push "${MUI_ENGLISH_LANGNAME}"
  Push ${LANG_FRENCH}
  Push "${MUI_FRENCH_LANGNAME}"
  Push ${LANG_GERMAN}
  Push "${MUI_GERMAN_LANGNAME}"
  Push ${LANG_DUTCH}
  Push "${MUI_DUTCH_LANGNAME}"
  Push ${LANG_POLISH}
  Push "${MUI_POLISH_LANGNAME}"
  Push ${LANG_GREEK}
  Push "${MUI_GREEK_LANGNAME}"
  Push ${LANG_TRADCHINESE}
  Push "${MUI_TRADCHINESE_LANGNAME}"
  Push ${LANG_SIMPCHINESE}
  Push "${MUI_SIMPCHINESE_LANGNAME}"
  
  Push 8F ; 8 is the number of languages, F = change font

  LangDLL::LangDialog "Installer Language" "Please select a language."

  Pop $LANGUAGE
  StrCmp $LANGUAGE "cancel" 0 +2
    Abort

FunctionEnd

Function .onInitDialog

    !insertmacro MUI_INNERDIALOG_INIT

    !insertmacro MUI_INNERDIALOG_START 1
      !insertmacro MUI_INNERDIALOG_TEXT 1040 $(MUI_INNERTEXT_LICENSE)
    !insertmacro MUI_INNERDIALOG_STOP 1

    !insertmacro MUI_INNERDIALOG_START 2
      !insertmacro MUI_INNERDIALOG_TEXT 1042 $(MUI_INNERTEXT_DESCRIPTION_TITLE)
      !insertmacro MUI_INNERDIALOG_TEXT 1043 $(MUI_INNERTEXT_DESCRIPTION_INFO)
    !insertmacro MUI_INNERDIALOG_STOP 2

    !insertmacro MUI_INNERDIALOG_START 3
      !insertmacro MUI_INNERDIALOG_TEXT 1041 $(MUI_INNERTEXT_DESTINATIONFOLDER)
    !insertmacro MUI_INNERDIALOG_STOP 3

  !insertmacro MUI_INNERDIALOG_END
  
FunctionEnd

Function .onNextPage

  !insertmacro MUI_NEXTPAGE SetPage
  
FunctionEnd

Function .onPrevPage

  !insertmacro MUI_PREVPAGE SetPage
  
FunctionEnd

Function SetPage

  !insertmacro MUI_PAGE_INIT

    !insertmacro MUI_PAGE_START 1
       !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 1

    !insertmacro MUI_PAGE_START 2
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_COMPONENTS_TITLE) $(MUI_TEXT_COMPONENTS_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 2

    !insertmacro MUI_PAGE_START 3
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_DIRSELECT_TITLE) $(MUI_TEXT_DIRSELECT_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 3

    !insertmacro MUI_PAGE_START 4
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_INSTALLING_TITLE) $(MUI_TEXT_INSTALLING_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 4

    !insertmacro MUI_PAGE_START 5
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_FINISHED_TITLE) $(MUI_TEXT_FINISHED_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 5

 !insertmacro MUI_PAGE_END

FunctionEnd

Function .onMouseOverSection

  !insertmacro MUI_DESCRIPTION_INIT

    !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyUI} $(DESC_SecCopyUI)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecCreateUninst} $(DESC_SecCreateUninst)

 !insertmacro MUI_DESCRIPTION_END

FunctionEnd

Function .onUserAbort

  !insertmacro MUI_ABORTWARNING

FunctionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;Add your stuff here

  Delete "$INSTDIR\modern.exe"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"
  
  DeleteRegValue HKCU "Software\${NAME}" "Installer Language"

  !insertmacro MUI_FINISHHEADER un.SetPage

SectionEnd

;--------------------------------
;Uninstaller Functions

Function un.onInit

  ReadRegStr $LANGUAGE HKCU "Software\${NAME}" "Installer Language"
  
FunctionEnd

Function un.onNextPage

  !insertmacro MUI_NEXTPAGE un.SetPage
  
FunctionEnd

Function un.SetPage
  
  !insertmacro MUI_PAGE_INIT
    
    !insertmacro MUI_PAGE_START 1
      !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_INTRO_TITLE) $(MUI_UNTEXT_INTRO_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 1

    !insertmacro MUI_PAGE_START 2
      !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_UNINSTALLING_TITLE) $(MUI_UNTEXT_UNINSTALLING_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 2

    !insertmacro MUI_PAGE_START 3
      !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_FINISHED_TITLE) $(MUI_UNTEXT_FINISHED_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 3

  !insertmacro MUI_PAGE_END

FunctionEnd

;eof