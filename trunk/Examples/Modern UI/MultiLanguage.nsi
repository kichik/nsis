;NSIS Modern Style UI version 1.20b
;Multilanguage & LangDLL Example Script
;Written by Joost Verburg

!define NAME "Test Software" ;Define your own software name here
!define VERSION "1.0" ;Define your own software version here

!verbose 3
  !include "ModernUI.nsh"
!verbose 4

;--------------------------------
;Configuration

  ;Language Files
  LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
  LoadLanguageFile "${NSISDIR}\Contrib\Language files\Dutch.nlf"

  ;General
  Name /LANG=${LANG_ENGLISH} "${NAME} ${VERSION}"
  Name /LANG=${LANG_DUTCH} "${NAME} ${VERSION}"
  OutFile "Multilanguage.exe"
  SetOverwrite on

  ;User interface
  !insertmacro MUI_INTERFACE "modern.exe" "adni18-installer-C-no48xp.ico" "adni18-uninstall-C-no48xp.ico" "modern.bmp" "smooth" "$9" ;$9 is the variable used to store the current page, do not use this var!

  ;License dialog
  LicenseText /LANG=${LANG_ENGLISH} "Press Page Down to see the rest of the agreement."
  LicenseText /LANG=${LANG_DUTCH} "Druk op Page Down om de rest van de overeenkomt te zien."
  LicenseData /LANG=${LANG_ENGLISH} "License.txt"
  LicenseData /LANG=${LANG_DUTCH} "License.txt"

  ;Component-select dialog
  ComponentText /LANG=${LANG_ENGLISH} "Check the components you want to install and uncheck the components you don't want to install. Click Next to continue."
  ComponentText /LANG=${LANG_DUTCH} "Selecteer de onderdelen die u wilt installer en deselecteer de onderdelen die u niet wilt installeren. Klik Volgende om verder te gaan."

  ;Folder-select dialog
  InstallDir "$PROGRAMFILES\${NAME}"
  DirText /LANG=${LANG_ENGLISH} "Setup will install ${NAME} in the following folder.$\r$\n$\r$\nTo install in this folder, click Install. To install in a different folder, click Browse and select another folder." " "
  DirText /LANG=${LANG_DUTCH} "Setup zal ${NAME} in de volgende map installeren.$\r$\n$\r$\nOm in een deze map te intalleren, klik Installeer. Om in een andere map te installeren, klik Bladeren en selecteerd een andere map." " "

  ;Uninstaller
  UninstallText /LANG=${LANG_ENGLISH} "This will uninstall ${NAME} from your system."
  UninstallText /LANG=${LANG_DUTCH} "Dit programma zal ${NAME} verwijderen van uw systeem."

  ;Things that need to be extracted on startup (keep these lines before any File command!)
  ReserveFile "${NSISDIR}\Plugins\LangDLL.dll"

;--------------------------------
;Installer Sections

Section "Modern.exe" SecCopyUI

  ;Add your stuff here

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"

SectionEnd

Section "Create uninstaller" SecCreateUninst

  ;Write the language to the registry (for the uninstaller)
  WriteRegStr HKCU "Software\${NAME}" "Installer Language" "$LANGUAGE"

  ;Add your stuff here

  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

Section ""

  ;Invisible section to display the Finish header
  !insertmacro MUI_FINISHHEADER SetPage

SectionEnd

;--------------------------------
;Installer Functions

Function .onInit

  LangDLL::LangDialog "Installer Language" "Please select a language." "2F" "English" "${LANG_ENGLISH}" "Nederlands" "${LANG_DUTCH}" "8" "Tahoma" ;2 is the number of lanugages, F means change font

  Pop $LANGUAGE
  StrCmp $LANGUAGE "cancel" 0 +2
    Abort

  StrCmp $LANGUAGE ${LANG_DUTCH} "" +2
    SectionSetText ${SecCreateUninst} "Deïnstallatie programma"

FunctionEnd

Function .onInitDialog

  !insertmacro MUI_INNERDIALOG_INIT

    !insertmacro MUI_INNERDIALOG_START 1
      !insertmacro MUI_INNERDIALOG_TEXT ${LANG_ENGLISH} 1040 "If you accept all the terms of the agreement, choose I Agree to continue. If you choose Cancel, Setup will close. You must accept the agreement to install ${NAME}."
      !insertmacro MUI_INNERDIALOG_TEXT ${LANG_DUTCH} 1040 "Als u de overeenkomt accepteert, kies Akkoord om verder te gaan. Als u Annuleren kiest zal Setup sluiten. U moet met de overeenkomst acceptren om ${NAME} te installeren."
    !insertmacro MUI_INNERDIALOG_STOP 1

    !insertmacro MUI_INNERDIALOG_START 2
       !insertmacro MUI_INNERDIALOG_TEXT ${LANG_ENGLISH} 1042 "Description"
       !insertmacro MUI_INNERDIALOG_TEXT ${LANG_ENGLISH} ${LANG_DUTCH} "Hover your mouse over a component to see it's description."
       !insertmacro MUI_INNERDIALOG_TEXT ${LANG_DUTCH} 1042 "Beschrijving"
       !insertmacro MUI_INNERDIALOG_TEXT ${LANG_DUTCH} ${LANG_DUTCH} "Beweeg uw muis over een onderdeel om een beschrijving te zien."
    !insertmacro MUI_INNERDIALOG_STOP 2

    !insertmacro MUI_INNERDIALOG_START 3
       !insertmacro MUI_INNERDIALOG_TEXT ${LANG_ENGLISH} 1041 "Destination Folder"
       !insertmacro MUI_INNERDIALOG_TEXT ${LANG_ENGLISH} 1041 "Installatie Map"
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
      !insertmacro MUI_HEADER_TEXT ${LANG_ENGLISH} "License Agreement" "Please review the license terms before installing ${NAME}."
      !insertmacro MUI_HEADER_TEXT ${LANG_DUTCH} "Licentie Overeenkomst" "Lees de licentie overeenkomst voordat u ${NAME} installeerd."
    !insertmacro MUI_PAGE_STOP 1

    !insertmacro MUI_PAGE_START 2
       !insertmacro MUI_HEADER_TEXT ${LANG_ENGLISH} "Choose Components" "Choose the components you want to install."
       !insertmacro MUI_HEADER_TEXT ${LANG_DUTCH} "Kies Onderdelen" "Kies de onderdelen die u wilt installeren."
    !insertmacro MUI_PAGE_STOP 2

    !insertmacro MUI_PAGE_START 3
       !insertmacro MUI_HEADER_TEXT ${LANG_ENGLISH} "Choose Install Location" "Choose the folder in which to install ${NAME}."
       !insertmacro MUI_HEADER_TEXT ${LANG_DUTCH} "Kies Installatie Locatie" "Kies de map waarin u ${NAME} in wilt installeren."
    !insertmacro MUI_PAGE_STOP 3

    !insertmacro MUI_PAGE_START 4
      !insertmacro MUI_HEADER_TEXT ${LANG_ENGLISH} "Installing" "Please wait while ${NAME} is being installed."
      !insertmacro MUI_HEADER_TEXT ${LANG_DUTCH} "Bezig met installeren" "Een ogenblik geduld terwijl ${NAME} wordt geinstalleerd."
    !insertmacro MUI_PAGE_STOP 4

    !insertmacro MUI_PAGE_START 5
      !insertmacro MUI_HEADER_TEXT ${LANG_ENGLISH} "Finished" "Setup was completed successfully."
      !insertmacro MUI_HEADER_TEXT ${LANG_DUTCH} "Gereed" "De installatie is succesvol verlopen."
    !insertmacro MUI_PAGE_STOP 5

  !insertmacro MUI_PAGE_END

FunctionEnd

Function .onMouseOverSection

  !insertmacro MUI_DESCRIPTION_INIT

    !insertmacro MUI_DESCRIPTION_TEXT ${LANG_ENGLISH} ${SecCopyUI} "Copy the modern.exe file to the application folder."
    !insertmacro MUI_DESCRIPTION_TEXT ${LANG_ENGLISH} ${SecCreateUninst} "Create a uninstaller which can automatically delete ${NAME}."

    !insertmacro MUI_DESCRIPTION_TEXT ${LANG_DUTCH} ${SecCopyUI} "Kopieër modern.exe naar de programma map."
    !insertmacro MUI_DESCRIPTION_TEXT ${LANG_DUTCH} ${SecCreateUninst} "Maak een deïnstallatie programma dat ${NAME} automatisch kan verwijderen."

  !insertmacro MUI_DESCRIPTION_END

FunctionEnd

Function .onUserAbort

  !insertmacro MUI_ABORTWARNING ${LANG_ENGLISH} "Are you sure you want to quit ${NAME} Setup?"
  !insertmacro MUI_ABORTWARNING ${LANG_DUTCH} "Weet u zeker dat u ${NAME} Setup wilt afsluiten?"
  !insertmacro MUI_ABORTWARNING_END

FunctionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;Add your stuff here

  Delete "$INSTDIR\modern.exe"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  ;Security - do not delete anything if ${NAME} is empty
  StrCmp "${NAME}" "" +2
    DeleteRegKey HKCU "Software\${NAME}"

  !insertmacro MUI_FINISHHEADER un.SetPage

SectionEnd

;--------------------------------
;Uninstaller Functions

Function un.onInit

  Push ${MUI_TEMP1}

    ;Get the language from the registry (save by uninstaller)
    ReadRegStr ${MUI_TEMP1} HKCU "Software\${NAME}" "Installer Language"
    StrCmp ${MUI_TEMP1} "" +2
      StrCpy $LANGUAGE ${MUI_TEMP1}

  Pop ${MUI_TEMP1}

FunctionEnd

Function un.onNextPage

  !insertmacro MUI_NEXTPAGE un.SetPage

FunctionEnd

Function un.SetPage

 !insertmacro MUI_PAGE_INIT

   !insertmacro MUI_PAGE_START 1
     !insertmacro MUI_HEADER_TEXT ${LANG_ENGLISH} "Uninstall ${NAME}" "Remove ${NAME} from your system."
     !insertmacro MUI_HEADER_TEXT ${LANG_DUTCH} "Deïnstalleer ${NAME}" "Verwijder ${NAME} van uw system."
   !insertmacro MUI_PAGE_STOP 1

   !insertmacro MUI_PAGE_START 2
     !insertmacro MUI_HEADER_TEXT ${LANG_ENGLISH} "Uninstalling" "Please wait while ${NAME} is being uninstalled."
     !insertmacro MUI_HEADER_TEXT ${LANG_DUTCH} "Bezig met deïnstalleren" "Een ogenblik gedult terwijl ${NAME} van uw system wordt verwijderd."
   !insertmacro MUI_PAGE_STOP 2

    !insertmacro MUI_PAGE_START 3
      !insertmacro MUI_HEADER_TEXT ${LANG_ENGLISH} "Finished" "${NAME} has been removed from your system."
      !insertmacro MUI_HEADER_TEXT ${LANG_DUTCH} "Gereed" "${NAME} is verwijderd van uw systeem."
    !insertmacro MUI_PAGE_STOP 3

  !insertmacro MUI_PAGE_END

FunctionEnd

;eof
