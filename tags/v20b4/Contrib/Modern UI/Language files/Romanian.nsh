;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.67

;Language: Romanian (1048)
;Translated by Cristian Pirvu (pcristip@yahoo.com)
;Updates by Sorin Sbarnea - INTERSOL SRL (sbarneasorin@intersol.ro) - ROBO Design (www.robodesign.ro)
;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ROMANIAN"
 ;Use only ASCII characters (if this is not possible, use the English name)
  !define MUI_LANGNAME "Romana"

  !define MUI_TEXT_WELCOME_INFO_TITLE "Bine ati venit la instalarea produsului $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "$(^Name) incepe instalarea.\r\n\r\nE recomandat sa inchideti toate aplicatiile inainte. Aceasta va permite programului sa modifice anumite fisiere de sistem fara repornirea calculatorului.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Licenta de utilizare"
  !define MUI_TEXT_LICENSE_SUBTITLE "Cititi cu atentie termenii licentei inaintea instalarii $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Apasati Page Down pentru a vedea restul licentei."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Daca acceptati termenii licentei, alegeti De Acord pentru a continua. Trebuie sa acceptati licenta pentru a instala $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Daca acceptati termenii acordului, bifati caseta de mai jos. Pentru a instala $(^Name) trebuie sa acceptati termenii din acordul de licenta."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Daca acceptati termenii acordului, alegeti prima optiune de mai jos. Pentru a instala $(^Name) trebuie sa accesati acordul."

  !define MUI_TEXT_COMPONENTS_TITLE "Alegeti componente"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Alegeti componentele produsului $(^Name) pe care doriti sa le instalati."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descriere"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Mutati mouse-ul deasupra unei componente pentru a vedea descrierea."

  !define MUI_TEXT_DIRECTORY_TITLE "Alegeti locatia instalarii"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Alegeti directorul de instalare al produsului $(^Name)."

  !define MUI_TEXT_INSTALLING_TITLE "In proces de instalare"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Va rugam sa asteptati, $(^Name) se instaleaza."

  !define MUI_TEXT_FINISH_TITLE "Instalare completa"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalarea s-a terminat cu succes."

  !define MUI_TEXT_ABORT_TITLE "Instalare oprita"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalarea nu s-a terminat cu succes."

  !define MUI_BUTTONTEXT_FINISH "&Terminare"
  !define MUI_TEXT_FINISH_INFO_TITLE "Terminarea instalarii pentru $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) a fost instalat.\r\n\r\nApasati Termina pentru a incheia instalarea."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Trebuie sa reporniti calculatorul pentru a termina instalarea. Doriti sa-l reporniti acum?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reporneste acum"
  !define MUI_TEXT_FINISH_REBOOTLATER "Repornesc eu mai tarziu"
  !define MUI_TEXT_FINISH_RUN "Executa $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Arata fisierul citeste.ma (readme.txt)"

  !define MUI_TEXT_STARTMENU_TITLE "Alegeti directorul din Start Menu"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Alegeti un director din Start Menu pentru comenzi rapide la aplicatii."
  !define MUI_INNERTEXT_STARTMENU_TOP "Selectati directorul din Start Menu in care sa creez comenzi rapide programului. Puteti de asemenea sa creati un director nou daca scrieti un nume nou."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nu doresc comenzi rapide"

  !define MUI_TEXT_ABORTWARNING "Sunteti sigur(a) ca doriti sa opriti instalarea pentru $(^Name) ?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Bine a-ti venit in rutina de uninstall -\r\n\Asistent pentru $(^Name)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Acest Asistent o sa va insoteasca prin rutina de uninstall a programului $(^Name).\r\n\r\nVa rugam opriti programul $(^Name), inainte de a continua rutina de uninstall.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Dezinstaleaza $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Elimina $(^Name)."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licenta de utilizare"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Va rugam sa revedeti termenii din licenta inainte de dezinstalarea $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Daca acceptati termenii licentei, apasati De acord. Trebuie sa acceptati termenii pentru a dezinstala $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Daca acceptati termenii acordului, bifati caseta de mai jos. Pentru a dezinstala $(^Name) trebuie sa acceptati termenii din acordul de licenta."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Daca acceptati termenii acordului, alegeti prima optiune de mai jos. Pentru a instala $(^Name) trebuie sa accesati acordul. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Alegeti componentele"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Alegeti caracteristicile $(^Name) pentru dezinstalare."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Alegeti locatiza pentru dezinstalare"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Alegeti dosarul din care sa dezinstalati $(^Name)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "In proces de dezinstalare"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Va rugam sa asteptati, $(^Name) se dezinstaleaza."
    
  !define MUI_UNTEXT_FINISH_TITLE "Dezinstalare finalizata"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Dezinstalarea a fost finalizata cu succes."
  
  !define MUI_UNTEXT_ABORT_TITLE "Dezinstalare oprita"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Dezinstalarea a fost oprita."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Rutina uninstall pentru $(^Name) va fi incheiata"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name) a fost indepartat din computerul dumneavoastra.\r\n\r\nClick pe Inchide, pentru a termina Asistentul de uninstall."

  !define MUI_UNTEXT_ABORTWARNING "Sinte-ti sigur, ca doriti ca rutina de uninstall a programului $(^Name) sa fie intrerupta?"
  
!insertmacro MUI_LANGUAGEFILE_END