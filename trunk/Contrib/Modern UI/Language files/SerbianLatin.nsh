;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Serbian Latin (2074)
;Translation by Srðan Obuæina <obucina@srpskijezik.edu.yu>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SerbianLatin"

  !define MUI_LANGNAME "Serbian Latin" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Dobrodošli u vodiè za instalaciju programa $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Biæete voðeni kroz proces instalacije programa $(^NameDA).\r\n\r\nPreporuèljivo je da iskljuèite sve druge programe pre poèetka instalacije. Ovo može omoguæiti ažuriranje sistemskih fajlova bez potrebe za restartovanjem raèunara.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Dogovor o pravu korišæenja"
  !define MUI_TEXT_LICENSE_SUBTITLE "Pažljivo proèitajte dogovor o pravu korišæenja pre instalacije programa $(^NameDA)-e."
  !define MUI_INNERTEXT_LICENSE_TOP "Pritisnite Page Down da bi videli ostatak dogovora."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ako prihvatate sve uslove dogovora, pritisnite dugme 'Prihvatam' za nastavak. Morate prihvatiti dogovor da bi instalirali program $(^NameDA)-u."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ako prihvatate sve uslove dogovora, obeležite kvadratiæ ispod. Morate prihvatiti dogovor da bi instalirali program $(^NameDA)-u. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ako prihvatate sve uslove dogovora, izaberite prvu opciju ispod. Morate prihvatiti dogovor da bi instalirali program $(^NameDA)-u. $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Izbor komponenti za instalaciju"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Izaberite komponente za instalaciju. Instaliraju se samo oznaèene komponente."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Preðite kursorom miša preko imena komponente da biste videli njen opis."

  !define MUI_TEXT_DIRECTORY_TITLE "Izbor foldera za instalaciju"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Izaberite folder u koji æete instalirati program $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Instalacija"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Saèekajte dok se program $(^NameDA) instalira."

  !define MUI_TEXT_FINISH_TITLE "Završena instalacija"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalacija je uspešno završena."

  !define MUI_TEXT_ABORT_TITLE "Prekinuta instalacija"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalacija je prekinuta i nije uspešno završena."

  !define MUI_BUTTONTEXT_FINISH "Kraj"
  !define MUI_TEXT_FINISH_INFO_TITLE "Završena instalacija programa $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Program $(^NameDA) je instaliran na raèunar.\r\n\r\nPritisnite dugme 'Kraj' za zatvaranje ovog prozora."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Raèunar mora biti restartovan da bi se proces instalacije programa $(^NameDA) uspešno završio. Želite li odmah da restartujete raèunar?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Restartuj odmah"
  !define MUI_TEXT_FINISH_REBOOTLATER "Bez restartovanja"
  !define MUI_TEXT_FINISH_RUN "Pokreni program $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Prikaži ProèitajMe fajl"

  !define MUI_TEXT_STARTMENU_TITLE "Izbor foldera u Start meniju"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Izaberite folder u Start meniju u kome æe se kreirati preèice."
  !define MUI_INNERTEXT_STARTMENU_TOP "Izaberite folder u Start meniju u kome želite da budu kreirane preèice programa. Možete upisati i ime za kreiranje novog foldera."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Bez kreiranja preèica"

  !define MUI_TEXT_ABORTWARNING "Sigurno želite da prekinete instalaciju programa $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Dobrodošli u deinstalaciju programa $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Biæete voðeni kroz proces deinstalacije programa $(^NameDA).\r\n\r\nPre poèetka deinstalacije, uverite se da je program $(^NameDA) iskljuèen. $_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Deinstalacija programa $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Deinstalacija programa $(^NameDA) sa raèunara."

  !define MUI_UNTEXT_LICENSE_TITLE "Dogovor o pravu korišæenja"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Pažljivo proèitajte dogovor o pravu korišæenja pre deinstalacije programa $(^NameDA)-e."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ako prihvatate sve uslove dogovora, pritisnite dugme 'Prihvatam' za nastavak. Morate prihvatiti dogovor da bi deinstalirali program $(^NameDA)-u."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ako prihvatate sve uslove dogovora, obeležite kvadratiæ ispod. Morate prihvatiti dogovor da bi deinstalirali program $(^NameDA)-u. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ako prihvatate sve uslove dogovora, izaberite prvu opciju ispod. Morate prihvatiti dogovor da bi deinstalirali program $(^NameDA)-u. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Izbor komponenti za deinstalaciju"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Izaberite komponente za deinstalaciju. Deinstaliraju se samo oznaèene komponente."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Izbor foldera za deinstalaciju"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Izaberite folder iz koga æete deinstalirati program $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Deinstalacija"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Saèekajte dok se program $(^NameDA) deinstalira."

  !define MUI_UNTEXT_FINISH_TITLE "Završena deinstalacija"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Deinstalacija je uspešno završena."
  
  !define MUI_UNTEXT_ABORT_TITLE "Prekinuta deinstalacija"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Deinstalacija je prekinuta i nije uspešno završena."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Završena deinstalacija programa $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Program $(^NameDA) je deinstaliran sa raèunara.\r\n\r\nPritisnite dugme 'Kraj' za zatvaranje ovog prozora."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Raèunar mora biti restartovan da bi završili deinstalaciju programa $(^NameDA). Želite li odmah da restartujete raèunar?"

  !define MUI_UNTEXT_ABORTWARNING "Sigurno želite da prekinete deinstalaciju programa $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END
