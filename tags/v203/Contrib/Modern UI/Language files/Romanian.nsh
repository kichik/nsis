;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Romanian (1048)
;Translated by Cristian Pirvu (pcristip@yahoo.com)
;Updates by Sorin Sbarnea - INTERSOL SRL (sbarneasorin@intersol.ro) - ROBO Design (www.robodesign.ro)
;New revision by George Radu (georadu@hotmail.com) http://mediatae.3x.ro

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Romanian"

  !define MUI_LANGNAME "Romana" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Bine ati venit la instalarea produsului $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Aceasta aplicatie va instala produsului $(^NameDA).\r\n\r\nEste recomandat sa inchideti toate aplicatiile inainte de inceperea procesului de instalare. Aceasta va poate asigura un proces de instalare fara erori sau situatii neprevazute.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Contract de licenta"
  !define MUI_TEXT_LICENSE_SUBTITLE "Cititi cu atentie termenii contractului de licenta inainte de a instala $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Apasati Page Down pentru a vizualiza restul contractului de licenta."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Daca acceptati termenii contractului de licenta, apasati De Acord. Pentru a instala $(^NameDA) trebuie sa acceptati termenii din contractul de licenta."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Daca acceptati termenii contractului de licenta, bifati caseta de mai jos. Pentru a instala $(^NameDA) trebuie sa acceptati termenii din contractul de licenta. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Daca acceptati termenii contractului de licenta, selectati prima optiune de mai jos. Pentru a instala $(^NameDA) trebuie sa acceptati termenii din contractul de licenta. $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Selectare componente"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Selectati componentele produsului $(^NameDA) pe care doriti sa le instalati."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descriere"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Asezati mausul deasupra fiecarei componente pentru a vizualiza descrierea acesteia."

  !define MUI_TEXT_DIRECTORY_TITLE "Selectare director destinatie"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Selectati directorul in care doriti sa instalati $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "In curs de instalare"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Va rugam sa asteptati, $(^NameDA) se instaleaza."

  !define MUI_TEXT_FINISH_TITLE "Instalare terminata"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalarea s-a terminat cu succes."

  !define MUI_TEXT_ABORT_TITLE "Instalare revocata"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalarea a fost revocata de utilizator."

  !define MUI_BUTTONTEXT_FINISH "&Terminare"
  !define MUI_TEXT_FINISH_INFO_TITLE "Terminare instalare $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) a fost instalat.\r\n\r\nApasati Terminare pentru a incheia instalarea."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Trebuie sa reporniti computerul pentru a termina instalarea. Doriti sa-l reporniti acum?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reporneste acum"
  !define MUI_TEXT_FINISH_REBOOTLATER "Repornesc eu mai tarziu"
  !define MUI_TEXT_FINISH_RUN "Executare $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Afisare fisierul cu informatii."

  !define MUI_TEXT_STARTMENU_TITLE "Selectare grup Meniul Start"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Selectati un grup in Meniul Start pentru a crea comenzi rapide produsului."
  !define MUI_INNERTEXT_STARTMENU_TOP "Selectati grupul din Meniul Start in care vor fi create comenzi rapide produsului. Puteti de asemenea sa creati un grup nou."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nu doresc comenzi rapide"

  !define MUI_TEXT_ABORTWARNING "Sunteti sigur(a) ca doriti sa revocati instalarea produsului $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Bine ati venit la eliminarea produsului $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Aceasta aplicatie va elimina produsului $(^NameDA).\r\n\r\nEste recomandat sa inchideti toate aplicatiile inainte de inceperea procesului de eliminare. Aceasta va poate asigura un proces de eliminare fara erori sau situatii neprevazute.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Eliminare $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Eliminare $(^NameDA) din computerul dumneavoastra."

  !define MUI_UNTEXT_LICENSE_TITLE "Contract de licenta"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Cititi cu atentie termenii contractului de licenta inainte de a elimina $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Daca acceptati termenii contractului de licenta, apasati De Acord. Pentru a elimina $(^NameDA) trebuie sa acceptati termenii din contractul de licenta."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Daca acceptati termenii contractului de licenta, bifati caseta de mai jos. Pentru a elimina $(^NameDA) trebuie sa acceptati termenii din contractul de licenta. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Daca acceptati termenii contractului de licenta, selectati prima optiune de mai jos. Pentru a elimina $(^NameDA) trebuie sa acceptati termenii din contractul de licenta. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Selectare componente"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Selectati componentele produsului $(^NameDA) pe care doriti sa le eliminati."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Selectare director de eliminat"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Selectati directorul din care doriti sa eliminati $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "In curs de eliminare"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Va rugam sa asteptati, $(^NameDA) se elimina."

  !define MUI_UNTEXT_FINISH_TITLE "Eliminare terminata"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Eliminarea s-a terminat cu succes."

  !define MUI_UNTEXT_ABORT_TITLE "Eliminare revocata"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Eliminarea a fost revocata de utilizator."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Terminare eliminare $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) a fost eliminat.\r\n\r\nApasati Terminare pentru a incheia eliminarea."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Trebuie sa reporniti computerul pentru a termina eliminarea. Doriti sa-l reporniti acum??"

  !define MUI_UNTEXT_ABORTWARNING "Sunteti sigur(a) ca doriti sa revocati eliminarea produsului $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END