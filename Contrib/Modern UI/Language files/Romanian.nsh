;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Romanian (1048)
;Translated by Cristian Pirvu (pcristip@yahoo.com) - Small changes by Sorin Sbarnea - INTERSOL SRL (sorin@intersol.ro)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ROMANIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Romana" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Alegeti Inainte pentru a continua."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Alegeti Instalare pentru a incepe instalarea."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Bine ati venit la instalarea produsului ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "${MUI_PRODUCT} incepe instalarea.\r\n\r\nE recomandat sa inchideti toate aplicatiile inainte. Aceasta va permite programului sa modifice anumite fisiere de sistem fara repornirea calculatorului.\r\n\r\n"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licenta de utilizare"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Cititi cu atentie termenii licentei inaintea instalarii ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Apasati Page Down pentru a vedea restul licentei."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Daca acceptati termenii licentei, alegeti De Acord pentru a continua. Trebuie sa acceptati licenta ca sa instalati ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Daca acceptati termenii acordului, bifati caseta de mai jos. Pentru a instala ${MUI_PRODUCT} trebuie sa accesptati termenii din acordul de licenta."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Daca acceptati termenii acordului, alegeti prima optiune de mai jos. Pentru a instala ${MUI_PRODUCT} trebuie sa accesati acordul."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Alegeti componente"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Alegeti componentele produsului ${MUI_PRODUCT} pe care vreti sa le instalati."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descriere"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Miscati mausul deasupra unei componente pentru a vedea descrierea."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Alegeti locatia instalarii"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Alegeti directorul de instalat al produsului ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "In process de instalare"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Asteaptati, ${MUI_PRODUCT} se instaleaza."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Instalare completa"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Instalarea s-a terminat cu succes."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Instalare oprita"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Instalarea nu s-a terminat cu succes."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Terminare"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Terminarea instalarii pentru ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} a fost instalat.\r\n\r\nApasati Termina pentru a incheia instalarea."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Trebuie sa restartati calculatorul pentru a termina instalarea. Vreti sa restartati acum?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Restarteaza acum"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Vreau sa restartez ulterior"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Executa ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Arata fisierul citeste.ma"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Alegeti directorul din Start Menu"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Alegeti un director din Start Menu pentru shortcut-urile aplicatiei."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Selectati directorul din Start Menu in care sa creez shortcut-urile programului. Puteti de asemenea sa creati un director nou daca tastati un nume nou."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nu crea shortcut-uri"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Sunteti sigur(a) ca vreti sa opriti instalarea pentru ${MUI_PRODUCT} ?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Apasati butonul Dezinstalare pentru a porni dezinstalarea."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Dezinstaleaza ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Sterge ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Alegeti componentele"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Selectati componentele ${MUI_PRODUCT} pe care vreti sa le dezinstalati."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "In proces de dezinstalare"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Asteaptati pana ${MUI_PRODUCT} este dezinstalat."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Dezinstalarea e completa"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Dezinstalarea s-a terminat cu succes."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Dezinstalarea anulata"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Dezinstalarea nu a fost terminata cu succes"

!insertmacro MUI_LANGUAGEFILE_END