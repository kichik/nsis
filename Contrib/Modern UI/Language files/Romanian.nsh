;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Romanian (1048)
;Translated by Cristian Pirvu (pcristip@yahoo.com) - Small changes by Sorin Sbarnea - INTERSOL SRL (sorin@intersol.ro)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ROMANIAN"

  !define MUI_LANGNAME "Romana" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Bine ati venit la instalarea produsului $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "$(^Name) incepe instalarea.\r\n\r\nE recomandat sa inchideti toate aplicatiile inainte. Aceasta va permite programului sa modifice anumite fisiere de sistem fara repornirea calculatorului.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Licenta de utilizare"
  !define MUI_TEXT_LICENSE_SUBTITLE "Cititi cu atentie termenii licentei inaintea instalarii $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Apasati Page Down pentru a vedea restul licentei."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Daca acceptati termenii licentei, alegeti De Acord pentru a continua. Trebuie sa acceptati licenta ca sa instalati $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Daca acceptati termenii acordului, bifati caseta de mai jos. Pentru a instala $(^Name) trebuie sa accesptati termenii din acordul de licenta."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Daca acceptati termenii acordului, alegeti prima optiune de mai jos. Pentru a instala $(^Name) trebuie sa accesati acordul."

  !define MUI_TEXT_COMPONENTS_TITLE "Alegeti componente"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Alegeti componentele produsului $(^Name) pe care vreti sa le instalati."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descriere"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Miscati mausul deasupra unei componente pentru a vedea descrierea."

  !define MUI_TEXT_DIRECTORY_TITLE "Alegeti locatia instalarii"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Alegeti directorul de instalat al produsului $(^Name)."

  !define MUI_TEXT_INSTALLING_TITLE "In process de instalare"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Asteaptati, $(^Name) se instaleaza."

  !define MUI_TEXT_FINISH_TITLE "Instalare completa"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalarea s-a terminat cu succes."

  !define MUI_TEXT_ABORT_TITLE "Instalare oprita"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalarea nu s-a terminat cu succes."

  !define MUI_BUTTONTEXT_FINISH "&Terminare"
  !define MUI_TEXT_FINISH_INFO_TITLE "Terminarea instalarii pentru $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) a fost instalat.\r\n\r\nApasati Termina pentru a incheia instalarea."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Trebuie sa restartati calculatorul pentru a termina instalarea. Vreti sa restartati acum?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Restarteaza acum"
  !define MUI_TEXT_FINISH_REBOOTLATER "Vreau sa restartez ulterior"
  !define MUI_TEXT_FINISH_RUN "Executa $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Arata fisierul citeste.ma"

  !define MUI_TEXT_STARTMENU_TITLE "Alegeti directorul din Start Menu"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Alegeti un director din Start Menu pentru shortcut-urile aplicatiei."
  !define MUI_INNERTEXT_STARTMENU_TOP "Selectati directorul din Start Menu in care sa creez shortcut-urile programului. Puteti de asemenea sa creati un director nou daca tastati un nume nou."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nu crea shortcut-uri"

  !define MUI_TEXT_ABORTWARNING "Sunteti sigur(a) ca vreti sa opriti instalarea pentru $(^Name) ?"


  !define MUI_UNTEXT_CONFIRM_TITLE "Dezinstaleaza $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Sterge $(^Name)."

  !define MUI_UNTEXT_COMPONENTS_TITLE "Alegeti componentele"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Selectati componentele $(^Name) pe care vreti sa le dezinstalati."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "In proces de dezinstalare"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Asteaptati pana $(^Name) este dezinstalat."

  !define MUI_UNTEXT_FINISH_TITLE "Dezinstalarea e completa"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Dezinstalarea s-a terminat cu succes."

  !define MUI_UNTEXT_ABORT_TITLE "Dezinstalarea anulata"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Dezinstalarea nu a fost terminata cu succes"

!insertmacro MUI_LANGUAGEFILE_END