;NSIS Modern User Interface version 1.3
;Advanced Macro System & Install Options Example Script
;Written by Joost Verburg

!define NAME "Test Software" ;Define your own software name here
!define VERSION "1.0" ;Define your own software version here

!include "${NSISDIR}\Contrib\Modern UI\System.nsh"

;--------------------------------
;Configuration
  
  !define MUI_INSTALLOPTIONS
  
  !define MUI_LICENSEPAGE
  !define MUI_COMPONENTPAGE
  !define MUI_DIRSELECTPAGE
  !define MUI_INSTALLBUTTONTEXT_NEXT
  !define MUI_ABORTWARNING
  !define MUI_UNINSTALLER
  
  !define MUI_SETPAGE_FUNCTIONNAME "SetPage"
  !define MUI_UNSETPAGE_FUNCTIONNAME "un.SetPage"
  
  !define TEMP1 $R0

  ;Language
    ;English
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\Language files\English.nsh"

  ;General
  Name "${NAME} ${VERSION}"
  OutFile "InstallOptions.exe"

  !insertmacro MUI_INTERFACE
  !insertmacro MUI_INSTALLOPTIONS "$7" "$8" ;Variables for the Install Options system. Do not use them in .onNext/PrevPage and SetPage

  ;License page
  LicenseData "${NSISDIR}\Contrib\Modern UI\License.txt"

  ;Component-selection page
    ;Descriptions
    LangString DESC_SecCopyUI ${LANG_ENGLISH} "Copy the modern.exe file to the application folder."
    LangString DESC_SecCreateUninst ${LANG_ENGLISH} "Create a uninstaller which can automatically delete ${NAME}."

  ;Folder-selection page
  InstallDir "$PROGRAMFILES\${NAME}"
  
  ;Install Options pages
  LangString MUI_TEXT_IO_TITLE ${LANG_ENGLISH} "Install Options Page"
  LangString MUI_TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Create your own dialog!"
  
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

SectionEnd

Section "Create uninstaller" SecCreateUninst

  ;Add your stuff here

  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

Section ""

  ;Invisible section to display the Finish header
  !insertmacro MUI_FINISHHEADER

SectionEnd

;--------------------------------
;Installer Functions

Function .onInitDialog

    !insertmacro MUI_INNERDIALOG_INIT

    !insertmacro MUI_INNERDIALOG_START 1
      !insertmacro MUI_INNERDIALOG_TEXT 1040 $(MUI_INNERTEXT_LICENSE)
    !insertmacro MUI_INNERDIALOG_STOP 1

    !insertmacro MUI_INNERDIALOG_START 4
      !insertmacro MUI_INNERDIALOG_TEXT 1042 $(MUI_INNERTEXT_DESCRIPTION_TITLE)
      !insertmacro MUI_INNERDIALOG_TEXT 1043 $(MUI_INNERTEXT_DESCRIPTION_INFO)
    !insertmacro MUI_INNERDIALOG_STOP 4

    !insertmacro MUI_INNERDIALOG_START 5
      !insertmacro MUI_INNERDIALOG_TEXT 1041 $(MUI_INNERTEXT_DESTINATIONFOLDER)
    !insertmacro MUI_INNERDIALOG_STOP 5

  !insertmacro MUI_INNERDIALOG_END
  
FunctionEnd

Function .onNextPage

  !insertmacro MUI_INSTALLOPTIONS_NEXTPAGE
  !insertmacro MUI_NEXTPAGE
  
FunctionEnd

Function .onPrevPage

  !insertmacro MUI_INSTALLOPTIONS_PREVPAGE
  !insertmacro MUI_PREVPAGE
  
FunctionEnd

Function SetPage

  !insertmacro MUI_PAGE_INIT

    !insertmacro MUI_PAGE_START 1
       !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 1

    !insertmacro MUI_PAGE_START 2
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_IO_TITLE) $(MUI_TEXT_IO_SUBTITLE)
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioA.ini" "Settings" "Title" "${NAME} ${VERSION} Setup: Install Options A"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioA.ini" "Settings" "CancelConfirm" "Are you sure you want to quit ${NAME} Setup?"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioA.ini" "Settings" "CancelConfirmCaption" "${NAME} ${VERSION} Setup"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioA.ini" "Settings" "CancelConfirmFlags" "MB_ICONEXCLAMATION"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioA.ini" "Settings" "BackButtonText" $(MUI_BUTTONTEXT_BACK)
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioA.ini" "Settings" "NextButtonText" $(MUI_BUTTONTEXT_NEXT)
      !insertmacro MUI_INSTALLOPTIONS_SHOW 2 "ioA.ini" "" "IO" ;Next page is an IO page
    !insertmacro MUI_PAGE_STOP 2
    
    !insertmacro MUI_PAGE_START 3
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_IO_TITLE) $(MUI_TEXT_IO_SUBTITLE)
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioB.ini" "Settings" "Title" "${NAME} ${VERSION} Setup: Install Options B"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioB.ini" "Settings" "CancelConfirm" "Are you sure you want to quit ${NAME} Setup?"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioB.ini" "Settings" "CancelConfirmCaption" "${NAME} ${VERSION} Setup"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioB.ini" "Settings" "CancelConfirmFlags" "MB_ICONEXCLAMATION"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioB.ini" "Settings" "BackButtonText" $(MUI_BUTTONTEXT_BACK)
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioB.ini" "Settings" "NextButtonText" $(MUI_BUTTONTEXT_NEXT)
      !insertmacro MUI_INSTALLOPTIONS_SHOW 3 "ioB.ini" "IO" "" ;Previous page is an IO page
    !insertmacro MUI_PAGE_STOP 3

    !insertmacro MUI_PAGE_START 4
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_COMPONENTS_TITLE) $(MUI_TEXT_COMPONENTS_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 4

    !insertmacro MUI_PAGE_START 5
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_DIRSELECT_TITLE) $(MUI_TEXT_DIRSELECT_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 5

    !insertmacro MUI_PAGE_START 6
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_IO_TITLE) $(MUI_TEXT_IO_SUBTITLE)
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioC.ini" "Settings" "Title" "${NAME} ${VERSION} Setup: Install Options C"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioC.ini" "Settings" "CancelConfirm" "Are you sure you want to quit ${NAME} Setup?"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioC.ini" "Settings" "CancelConfirmCaption" "${NAME} ${VERSION} Setup"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioC.ini" "Settings" "CancelConfirmFlags" "MB_ICONEXCLAMATION"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioC.ini" "Settings" "BackButtonText" $(MUI_BUTTONTEXT_BACK)
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioC.ini" "Settings" "NextButtonText" $(MUI_BUTTONTEXT_INSTALL)
      !insertmacro MUI_INSTALLOPTIONS_SHOW 6 "ioC.ini" "" "" ;Next/previous pages are no IO pages
    !insertmacro MUI_PAGE_STOP 6

    !insertmacro MUI_PAGE_START 7
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_INSTALLING_TITLE) $(MUI_TEXT_INSTALLING_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 7

    !insertmacro MUI_PAGE_START 8
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_FINISHED_TITLE) $(MUI_TEXT_FINISHED_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 8

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

  !insertmacro MUI_UNFINISHHEADER

SectionEnd

;--------------------------------
;Uninstaller Functions

Function un.onNextPage

  !insertmacro MUI_INSTALLOPTIONS_NEXTPAGE
  !insertmacro MUI_UNNEXTPAGE
  
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