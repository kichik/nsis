;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Serbian (2074)
;Translation by Vladan Obradovic

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SERBIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Serbian" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Dobro došli u instalaciju programa $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Program æe instalirati $(^Name)-u na Vaš raèunar.\r\n\r\nPreporuèuje se da se zatvore sve aplikacije pre odpoèinjanja Instalacije. Time æe se omoguæiti Instalaciji da ažurira potrebne sistemska datoteke bez potrebe za ponovnim pokretanjem vašeg raèunara.\r\n\r\n$_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licenca"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Proèitajte licencu pre instalacije $(^Name)-e."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Pritisnite Page Down da vidite ostatak licence."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ukoliko prihvatate uslove licence, odaberite Prihvatam za nastavak. Licenca se mora prihvatiti ukolko želite da instalirate $(^Name)-u."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ako prihvatate uslove licence, kliknite na check box ispod. Licenca se mora prihvatiti ukolko želite da instalirate $(^Name)-u.  $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ako prihvatate uslove licence, odaberite prvu opciju ispod. Licenca se mora prihvatiti ukolko želite da instalirate $(^Name)-u. $_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Odaberite komponente"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Odaberite koje komponente $(^Name)-e želite da instalirate."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pozicionirajte se mišem iznad komponente da vidite njen opis."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Odaberite instalacioni direktorijum"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Odaberite direktorijum u koji želite da instalirate $(^Name)-u."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Instalacija"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Molim Vas prièekajte instalacija $(^Name)-e je u toku."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Instalacija završena"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Instalacija je uspešno završena."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Instalacija prekinuta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Setup nije uspešno završen."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Završi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Završavanje $(^Name) Instalacije"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "$(^Name) je instalirana na Vaš raèunar.\r\n\r\nKliknite Završi da zatvorite instalacioni program."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Vaš raèunar je potrebno restartovati da bi instalacija $(^Name)-e bila kompletirana. Želite li da raèunar restartujete odmah?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Restartuj odmah"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Želim ga ruèno restartovati kasnije"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Pokreni $(^Name)-u"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Pokaži Readme"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Odaberite Direktorijum u Start Meniju"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Odaberite direktorijum u Start Meniju za programske shortcutove."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Odaberite direktorijum u Start Meniju u kom želite napraviti programske shortcutove. Takoðe, možete uneti ime da bi se naparavio novi direktorijum."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nemoj praviti shortcut-ove"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Da li ste sigurni da želite prekinuti $(^Name) Instalaciju?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "Uklanjanje programa $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "Program $(^Name) æe biti uklonjen s ovog raèunara."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_TITLE "Licenca"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_SUBTITLE "Proèitajte licencu pre deinstalacije programa $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM "Ukoliko prihvatate uslove licence, odaberite 'Prihvatam' za nastavak. Morate prihvatiti licencu za deinstalaciju programa $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ukoliko prihvatate uslove iz licence, obeležite kvadratiæ ispod. Morate prihvatiti licencu za deinstalaciju programa $(^Name). $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ukoliko prihvatate uslove iz licence, odaberite prvu opciju ispod. Morate prihvatiti licencu za deinstalaciju programa $(^Name). $_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Izbor komponenti"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Odaberite koje komponente programa $(^Name) želite deinstalirati."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_TITLE "Odaberite direktorijum koji se deinstalira"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_SUBTITLE "Odaberite programsku grupu iz koje želite deinstalirati program $(^Name)."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Deinstalacija"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prièekajte završetak deinstalacije programa $(^Name)."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Završeno"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Deinstalacija je uspešno završena."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Deinstalacija prekinuta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Deinstalacija nije uspešno završena."

!insertmacro MUI_LANGUAGEFILE_END