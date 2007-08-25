;Language: Romanian (1048)
;Translated by Cristian Pirvu (pcristip@yahoo.com)
;Updates by Sorin Sbarnea - INTERSOL SRL (sbarneasorin@intersol.ro) - ROBO Design (www.robodesign.ro)
;New revision by George Radu (georadu@hotmail.com) http://mediatae.3x.ro

!insertmacro LANGFILE "Romanian" "Romana"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Bine ati venit la instalarea produsului $(^NameDA)"
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TEXT "Aceasta aplicatie va instala produsului $(^NameDA).$\r$\n$\r$\nEste recomandat sa inchideti toate aplicatiile inainte de inceperea procesului de instalare. Aceasta va poate asigura un proces de instalare fara erori sau situatii neprevazute.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TITLE "Bine ati venit la eliminarea produsului $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TEXT "Aceasta aplicatie va elimina produsului $(^NameDA).$\r$\n$\r$\nEste recomandat sa inchideti toate aplicatiile inainte de inceperea procesului de eliminare. Aceasta va poate asigura un proces de eliminare fara erori sau situatii neprevazute.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_WELCOMEPAGE | MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Terminare"
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Contract de licenta"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Cititi cu atentie termenii contractului de licenta inainte de a instala $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Daca acceptati termenii contractului de licenta, apasati De Acord. Pentru a instala $(^NameDA) trebuie sa acceptati termenii din contractul de licenta."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Daca acceptati termenii contractului de licenta, bifati caseta de mai jos. Pentru a instala $(^NameDA) trebuie sa acceptati termenii din contractul de licenta. $_CLICK"
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Daca acceptati termenii contractului de licenta, selectati prima optiune de mai jos. Pentru a instala $(^NameDA) trebuie sa acceptati termenii din contractul de licenta. $_CLICK"
!endif

!ifdef MUI_UNLICENSEPAGE
  ${LangFileString} MUI_UNTEXT_LICENSE_TITLE "Contract de licenta"
  ${LangFileString} MUI_UNTEXT_LICENSE_SUBTITLE "Cititi cu atentie termenii contractului de licenta inainte de a elimina $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM "Daca acceptati termenii contractului de licenta, apasati De Acord. Pentru a elimina $(^NameDA) trebuie sa acceptati termenii din contractul de licenta."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Daca acceptati termenii contractului de licenta, bifati caseta de mai jos. Pentru a elimina $(^NameDA) trebuie sa acceptati termenii din contractul de licenta. $_CLICK"
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Daca acceptati termenii contractului de licenta, selectati prima optiune de mai jos. Pentru a elimina $(^NameDA) trebuie sa acceptati termenii din contractul de licenta. $_CLICK"
!endif

!ifdef MUI_LICENSEPAGE | MUI_UNLICENSEPAGE
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Apasati Page Down pentru a vizualiza restul contractului de licenta."
!endif

!ifdef MUI_COMPONENTSPAGE
  ${LangFileString} MUI_TEXT_COMPONENTS_TITLE "Selectare componente"
  ${LangFileString} MUI_TEXT_COMPONENTS_SUBTITLE "Selectati componentele produsului $(^NameDA) pe care doriti sa le instalati."
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descriere"
!endif

!ifdef MUI_UNCOMPONENETSPAGE
  ${LangFileString} MUI_UNTEXT_COMPONENTS_TITLE "Selectare componente"
  ${LangFileString} MUI_UNTEXT_COMPONENTS_SUBTITLE "Selectati componentele produsului $(^NameDA) pe care doriti sa le eliminati."
!endif

!ifdef MUI_COMPONENTSPAGE | MUI_UNCOMPONENTSPAGE
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Asezati mausul deasupra fiecarei componente pentru a vizualiza descrierea acesteia."
  !else
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Asezati mausul deasupra fiecarei componente pentru a vizualiza descrierea acesteia."
  !endif
!endif

!ifdef MUI_DIRECTORYPAGE
  ${LangFileString} MUI_TEXT_DIRECTORY_TITLE "Selectare director destinatie"
  ${LangFileString} MUI_TEXT_DIRECTORY_SUBTITLE "Selectati directorul in care doriti sa instalati $(^NameDA)."
!endif

!ifdef MUI_UNDIRECTORYSPAGE
  ${LangFileString} MUI_UNTEXT_DIRECTORY_TITLE "Selectare director de eliminat"
  ${LangFileString} MUI_UNTEXT_DIRECTORY_SUBTITLE "Selectati directorul din care doriti sa eliminati $(^NameDA)."
!endif

!ifdef MUI_INSTFILESPAGE
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "In curs de instalare"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Va rugam sa asteptati, $(^NameDA) se instaleaza."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Instalare terminata"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "Instalarea s-a terminat cu succes."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Instalare revocata"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "Instalarea a fost revocata de utilizator."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "In curs de eliminare"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Va rugam sa asteptati, $(^NameDA) se elimina."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Eliminare terminata"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "Eliminarea s-a terminat cu succes."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Eliminare revocata"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "Eliminarea a fost revocata de utilizator."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Terminare instalare $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) a fost instalat.$\r$\n$\r$\nApasati Terminare pentru a incheia instalarea."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Trebuie sa reporniti computerul pentru a termina instalarea. Doriti sa-l reporniti acum?"
!endif

!ifdef MUI_UNFINISHPAGE
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TITLE "Terminare eliminare $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) a fost eliminat.$\r$\n$\r$\nApasati Terminare pentru a incheia eliminarea."
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_REBOOT "Trebuie sa reporniti computerul pentru a termina eliminarea. Doriti sa-l reporniti acum??"
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Reporneste acum"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Repornesc eu mai tarziu"
  ${LangFileString} MUI_TEXT_FINISH_RUN "Executare $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "Afisare fisierul cu informatii."
!endif

!ifdef MUI_STARTMENUPAGE
  ${LangFileString} MUI_TEXT_STARTMENU_TITLE "Selectare grup Meniul Start"
  ${LangFileString} MUI_TEXT_STARTMENU_SUBTITLE "Selectati un grup in Meniul Start pentru a crea comenzi rapide produsului."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_TOP "Selectati grupul din Meniul Start in care vor fi create comenzi rapide produsului. Puteti de asemenea sa creati un grup nou."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_CHECKBOX "Nu doresc comenzi rapide"
!endif

!ifdef MUI_UNCONFIRMPAGE
  ${LangFileString} MUI_UNTEXT_CONFIRM_TITLE "Eliminare $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_CONFIRM_SUBTITLE "Eliminare $(^NameDA) din computerul dumneavoastra."
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "Sunteti sigur(a) ca doriti sa revocati instalarea produsului $(^Name)?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Sunteti sigur(a) ca doriti sa revocati eliminarea produsului $(^Name)?"
!endif
