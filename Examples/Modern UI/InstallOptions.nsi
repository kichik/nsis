;NSIS Modern UI version 1.3
;Install Options Example Script
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
  !define MUI_INSTALLBUTTONTEXT_NEXT
  !define MUI_UNINSTALLER

  ;Language
    ;English
    LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
    !include "${NSISDIR}\Contrib\Modern UI\English.nsh"

  ;General
  Name "${NAME} ${VERSION}"
  OutFile "InstallOptions.exe"

  ;User interface - icons, ui file, check bitmap, progress bar etc.
  !insertmacro MUI_INTERFACE "modern.exe" "adni18-installer-C-no48xp.ico" "adni18-uninstall-C-no48xp.ico" "modern.bmp" "smooth" "$9" ;$9 is the variable used to store the current page, do not use this var!
  !insertmacro MUI_INSTALLOPTIONS "$7" "$8" ;Variables for the Install Options system. Do not use them in .onNext/PrevPage and SetPage

  ;License dialog
  LicenseData "License.txt"

  ;Component-select dialog
    ;Descriptions
    LangString DESC_SecCopyUI ${LANG_ENGLISH} "Copy the modern.exe file to the application folder."
    LangString DESC_SecCreateUninst ${LANG_ENGLISH} "Create a uninstaller which can automatically delete ${NAME}."

  ;Folder-select dialog
  InstallDir "$PROGRAMFILES\${NAME}"
  
  ;Install Options dialogs
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

SectionEnd

Section "Create uninstaller" SecCreateUninst

  ;Add your stuff here

  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

Section ""

  ;Invisible section to display the Finish header
  !insertmacro MUI_FINISHHEADER SetPage

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
  !insertmacro MUI_NEXTPAGE SetPage
FunctionEnd

Function .onPrevPage
  !insertmacro MUI_INSTALLOPTIONS_PREVPAGE
  !insertmacro MUI_PREVPAGE SetPage
FunctionEnd

Function SetPage

  !insertmacro MUI_PAGE_INIT

    !insertmacro MUI_PAGE_START 1
       !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 1

    !insertmacro MUI_PAGE_START 2
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_IO_TITLE) $(MUI_TEXT_IO_SUBTITLE)
      WriteIniStr "$PLUGINSDIR\ioA.ini" "Settings" "Title" "${NAME} ${VERSION} Setup: Install Options A"
      WriteIniStr "$PLUGINSDIR\ioA.ini" "Settings" "CancelConfirm" "Are you sure you want to quit ${NAME} Setup?"
      WriteIniStr "$PLUGINSDIR\ioA.ini" "Settings" "CancelConfirmCaption" "${NAME} ${VERSION} Setup"
      WriteIniStr "$PLUGINSDIR\ioA.ini" "Settings" "CancelConfirmFlags" "MB_ICONEXCLAMATION"
      WriteIniStr "$PLUGINSDIR\ioA.ini" "Settings" "BackButtonText" $(MUI_BUTTONTEXT_BACK)
      WriteIniStr "$PLUGINSDIR\ioA.ini" "Settings" "NextButtonText" $(MUI_BUTTONTEXT_NEXT)
      !insertmacro MUI_INSTALLOPTIONS_SHOW 2 "ioA.ini" "" "IO" ;Next page is an IO page
    !insertmacro MUI_PAGE_STOP 2
    
    !insertmacro MUI_PAGE_START 3
      !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_IO_TITLE) $(MUI_TEXT_IO_SUBTITLE)
      WriteIniStr "$PLUGINSDIR\ioB.ini" "Settings" "Title" "${NAME} ${VERSION} Setup: Install Options B"
      WriteIniStr "$PLUGINSDIR\ioB.ini" "Settings" "CancelConfirm" "Are you sure you want to quit ${NAME} Setup?"
      WriteIniStr "$PLUGINSDIR\ioB.ini" "Settings" "CancelConfirmCaption" "${NAME} ${VERSION} Setup"
      WriteIniStr "$PLUGINSDIR\ioB.ini" "Settings" "CancelConfirmFlags" "MB_ICONEXCLAMATION"
      WriteIniStr "$PLUGINSDIR\ioB.ini" "Settings" "BackButtonText" $(MUI_BUTTONTEXT_BACK)
      WriteIniStr "$PLUGINSDIR\ioB.ini" "Settings" "NextButtonText" $(MUI_BUTTONTEXT_NEXT)
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
      WriteIniStr "$PLUGINSDIR\ioC.ini" "Settings" "Title" "${NAME} ${VERSION} Setup: Install Options C"
      WriteIniStr "$PLUGINSDIR\ioC.ini" "Settings" "CancelConfirm" "Are you sure you want to quit ${NAME} Setup?"
      WriteIniStr "$PLUGINSDIR\ioC.ini" "Settings" "CancelConfirmCaption" "${NAME} ${VERSION} Setup"
      WriteIniStr "$PLUGINSDIR\ioC.ini" "Settings" "CancelConfirmFlags" "MB_ICONEXCLAMATION"
      WriteIniStr "$PLUGINSDIR\ioC.ini" "Settings" "BackButtonText" $(MUI_BUTTONTEXT_BACK)
      WriteIniStr "$PLUGINSDIR\ioC.ini" "Settings" "NextButtonText" $(MUI_BUTTONTEXT_INSTALL)
      !insertmacro MUI_INSTALLOPTIONS_SHOW 6 "ioC.ini" "" "" ;Next/previous pages are NO IO pages
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

  !insertmacro MUI_FINISHHEADER un.SetPage

SectionEnd

;--------------------------------
;Uninstaller Functions

Function un.onNextPage
  !insertmacro MUI_INSTALLOPTIONS_NEXTPAGE
  !insertmacro MUI_NEXTPAGE un.onNextPage
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