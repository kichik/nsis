;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.67

;Language: Serbian (2074)
;Translation by Vladan Obradovic

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SERBIAN"

  !define MUI_LANGNAME "Serbian" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Dobro došli u instalaciju programa $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Program æe instalirati $(^Name)-u na Vaš raèunar.\r\n\r\nPreporuèuje se da se zatvore sve aplikacije pre odpoèinjanja Instalacije. Time æe se omoguæiti Instalaciji da ažurira potrebne sistemska datoteke bez potrebe za ponovnim pokretanjem vašeg raèunara.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Licenca"
  !define MUI_TEXT_LICENSE_SUBTITLE "Proèitajte licencu pre instalacije $(^Name)-e."
  !define MUI_INNERTEXT_LICENSE_TOP "Pritisnite Page Down da vidite ostatak licence."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ukoliko prihvatate uslove licence, odaberite Prihvatam za nastavak. Licenca se mora prihvatiti ukolko želite da instalirate $(^Name)-u."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ako prihvatate uslove licence, kliknite na check box ispod. Licenca se mora prihvatiti ukolko želite da instalirate $(^Name)-u.  $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ako prihvatate uslove licence, odaberite prvu opciju ispod. Licenca se mora prihvatiti ukolko želite da instalirate $(^Name)-u. $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Odaberite komponente"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Odaberite koje komponente $(^Name)-e želite da instalirate."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pozicionirajte se mišem iznad komponente da vidite njen opis."

  !define MUI_TEXT_DIRECTORY_TITLE "Odaberite instalacioni direktorijum"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Odaberite direktorijum u koji želite da instalirate $(^Name)-u."

  !define MUI_TEXT_INSTALLING_TITLE "Instalacija"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Molim Vas prièekajte instalacija $(^Name)-e je u toku."

  !define MUI_TEXT_FINISH_TITLE "Instalacija završena"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalacija je uspešno završena."

  !define MUI_TEXT_ABORT_TITLE "Instalacija prekinuta"
  !define MUI_TEXT_ABORT_SUBTITLE "Setup nije uspešno završen."

  !define MUI_BUTTONTEXT_FINISH "&Završi"
  !define MUI_TEXT_FINISH_INFO_TITLE "Završavanje $(^Name) Instalacije"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) je instalirana na Vaš raèunar.\r\n\r\nKliknite Završi da zatvorite instalacioni program."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Vaš raèunar je potrebno restartovati da bi instalacija $(^Name)-e bila kompletirana. Želite li da raèunar restartujete odmah?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Restartuj odmah"
  !define MUI_TEXT_FINISH_REBOOTLATER "Želim ga ruèno restartovati kasnije"
  !define MUI_TEXT_FINISH_RUN "Pokreni $(^Name)-u"
  !define MUI_TEXT_FINISH_SHOWREADME "Pokaži Readme"

  !define MUI_TEXT_STARTMENU_TITLE "Odaberite Direktorijum u Start Meniju"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Odaberite direktorijum u Start Meniju za programske shortcutove."
  !define MUI_INNERTEXT_STARTMENU_TOP "Odaberite direktorijum u Start Meniju u kom želite napraviti programske shortcutove. Takoðe, možete uneti ime da bi se naparavio novi direktorijum."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nemoj praviti shortcut-ove"

  !define MUI_TEXT_ABORTWARNING "Da li ste sigurni da želite prekinuti $(^Name) Instalaciju?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Dobrodošli u postupak deinstalacije programa $(^Name)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ovaj èarobnjak æe Vas provesti kroz postupak deinstalacije programa $(^Name).\r\n\r\nPre samog poèetka, molim Vas zatvorite program $(^Name) ukoliko je otvoren.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Deinstalacija programa $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Program $(^Name) æe biti deinstaliran s Vašeg raèunara."

  !define MUI_UNTEXT_LICENSE_TITLE "Licenca"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Proèitajte licencu pre deinstalacije programa $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ukoliko prihvatate uslove licence, odaberite 'Prihvatam' za nastavak. Morate prihvatiti licencu za deinstalaciju programa $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ukoliko prihvatate uslove iz licence, obeležite kvadratiæ ispod. Morate prihvatiti licencu za deinstalaciju programa $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ukoliko prihvatate uslove iz licence, odaberite prvu opciju ispod. Morate prihvatiti licencu za deinstalaciju programa $(^Name). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Izbor komponenti"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Odaberite koje komponente programa $(^Name) želite deinstalirati."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Odaberite direktorijum koji se deinstalira"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Odaberite programsku grupu iz koje želite deinstalirati program $(^Name)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Deinstalacija"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prièekajte završetak deinstalacije programa $(^Name)."

  !define MUI_UNTEXT_FINISH_TITLE "Završeno"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Deinstalacija je uspešno završena."
  
  !define MUI_UNTEXT_ABORT_TITLE "Deinstalacija prekinuta"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Deinstalacija nije uspešno završena."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Kraj deinstalacije programa $(^Name)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Program $(^Name) je deinstaliran s Vašeg raèunara.\r\n\r\nOdaberite 'Kraj' za zatvaranje ove deinstalacije."

  !define MUI_UNTEXT_ABORTWARNING "Jeste li sigurni da želite prekinuti deinstalaciju programa $(^Name)?"  

!insertmacro MUI_LANGUAGEFILE_END