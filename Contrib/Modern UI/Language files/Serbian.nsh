;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Serbian (2074)
;Translation by Vladan "vladano@EUnet.yu" Obradovic

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SERBIAN"

  !define MUI_LANGNAME "Serbian" ;Name of the language in the language itself

  !define MUI_TEXT_WELCOME_INFO_TITLE "Dobro došao u $(^Name) Instalaciju"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ovo æe instalirati $(^Name) na tvoj raèunar.\r\n\r\nPreporuèujem da zatvoriš sve ostale aplikacije pre zapoèinjanja Instalacije. To ce dozvoliti Instalaciji da nadogradi neke sistemska datoteke bez potrebe za ponovnim pokretanjem vašeg raèunara.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Licenca"
  !define MUI_TEXT_LICENSE_SUBTITLE "Proèitaj licencu pre instalacije $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Pritisni Page Down da vidiš ostatak licence."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ako prihvataš uslove licence, odaberi Prihvatam za nastavak. Moraš prihvatiti licencu da instaliraš $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ako prihvataš uslove licence, klikni na kvadratiæ ispod. Moraš prihvatiti licencu da instaliraš $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ako prihvataš uslove licence, odaberi prvu opciju ispod. Moraš prihvatiti licencu da instaliraš $(^Name). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Odaberi komponente"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Odaberi koje komponente $(^Name) želiš instalirati."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Proði mišem iznad komponente da vidiš njen opis."

  !define MUI_TEXT_DIRECTORY_TITLE "Odaberi instalacioni direktorijum"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Odaberi direktorijum u koji želiš instalirati $(^Name)."

  !define MUI_TEXT_INSTALLING_TITLE "Instalacija"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Prièekaj dok se instalira $(^Name)."

  !define MUI_TEXT_FINISH_TITLE "Instalacija završena"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalacija je uspešno završena."

  !define MUI_TEXT_ABORT_TITLE "Instalacija prekinuta"
  !define MUI_TEXT_ABORT_SUBTITLE "Setup nije uspešno završen."

  !define MUI_BUTTONTEXT_FINISH "&Završi"
  !define MUI_TEXT_FINISH_INFO_TITLE "Završavam $(^Name) Instalaciju"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) je instaliran na tvoj raèunar.\r\n\r\nKlikni Završi da zatvoriš ovaj prozor."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Tvoj raèunar je potrebno resetovati da završi instalaciju $(^Name). Želiš li da ga resetuješ sada?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Resetuj sada"
  !define MUI_TEXT_FINISH_REBOOTLATER "Želim ga ruèno resetovati kasnije"
  !define MUI_TEXT_FINISH_RUN "Pokreni $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Pokaži Readme"

  !define MUI_TEXT_STARTMENU_TITLE "Odaberi Direktorijum u Start Meniju"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Odaberi direktorijum u Start Meniju za programske shortcutove."
  !define MUI_INNERTEXT_STARTMENU_TOP "Odaberi direktorijum u Start Meniju u kojem želiš napraviti programske shortcutove. Možeš i uneti ime pa napraviti novi direktorijum."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nemoj napraviti shortcute"

  !define MUI_TEXT_ABORTWARNING "Jesi li siguran da želiš izaæi iz $(^Name) Instalacije?"


  !define MUI_UNTEXT_CONFIRM_TITLE "Deinstaliraj $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE  "Obriši $(^Name) sa tvog raèunara."

  !define MUI_UNTEXT_LICENSE_TITLE "Licenca"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Please review the license terms before uninstalling $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "If you accept the terms of the agreement, click I Agree to continue. You must accept the agreement to uninstall $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "If you accept the terms of the agreement, click the check box below. You must accept the agreement to uninstall $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "If you accept the terms of the agreement, select the first option below. You must accept the agreement to uninstall $(^Name). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Choose Components"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Choose which features of $(^Name) you want to uninstall."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Choose Uninstall Location"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Choose the folder from which to uninstall $(^Name)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Deinstaliraj"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prièekaj dok se deinstalira $(^Name)."

  !define MUI_UNTEXT_FINISH_TITLE "Završeno"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Deinstalacija je uspešno završena."
  
  !define MUI_UNTEXT_ABORT_TITLE "Deinstalacija prekinuta"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Deinstalacija nije uspešno završena."

!insertmacro MUI_LANGUAGEFILE_END