;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Serbian Latin (2074)
;By Vladan Obradovic and Srðan Obuæina <obucina@srpskijezik.edu.yu>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SERBIANLATIN"

  !define MUI_LANGNAME "Serbian Latin" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Dobrodošli u instalacioni proces programa $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Biæete voðeni kroz proces instaliranja programa $(^NameDA).\r\n\r\nPreporuèlivo je da iskljuèite sve druge programe pre poèetka instaliranja. Ovo može omoguæiti ažuriranje sistemskih fajlova bez potrebe za restartovanjem raèunara.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Dogovor o pravu korišæenja"
  !define MUI_TEXT_LICENSE_SUBTITLE "Pažljivo proèitajte dogovor o pravu korišæenja pre instaliranja programa $(^NameDA)-e."
  !define MUI_INNERTEXT_LICENSE_TOP "Pritisnite Page Down da bi videli ostatak dogovora."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ako prihvatate sve uslove dogovora, pritisnite dugme 'Prihvatam' za nastavak. Morate prihvatiti dogovor da bi instalirali program $(^NameDA)-u."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ako prihvatate sve uslove dogovora, obeležite kvadratiæ ispod. Morate prihvatiti dogovor da bi instalirali program $(^NameDA)-u. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ako prihvatate sve uslove dogovora, izaberite prvu opciju ispod. Morate prihvatiti dogovor da bi instalirali program $(^NameDA)-u. $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Izbor komponenti za instaliranje"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Izaberite komponente za instaliranje. Instaliraju se samo oznaèene komponente."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Preðite kursorom miša preko imena komponente da biste videli njen opis."

  !define MUI_TEXT_DIRECTORY_TITLE "Izbor direktorijuma za instaliranje"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Izaberite direktorijum u koji æete instalirati program $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Instaliranje"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Saèekajte dok se program $(^NameDA) instalira."

  !define MUI_TEXT_FINISH_TITLE "Završeno instaliranje"
  !define MUI_TEXT_FINISH_SUBTITLE "Instaliranje je uspešno završeno."

  !define MUI_TEXT_ABORT_TITLE "Prekinuto instaliranje"
  !define MUI_TEXT_ABORT_SUBTITLE "Instaliranje je prekinuto i nije uspešno završeno."

  !define MUI_BUTTONTEXT_FINISH "Kraj"
  !define MUI_TEXT_FINISH_INFO_TITLE "Završeno instaliranje programa $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Program $(^NameDA) je instaliran na raèunar.\r\n\r\nPritisnite dugme 'Kraj' za zatvaranje ovog prozora."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Raèunar mora biti restartovan da bi se proces instaliranja programa $(^NameDA) uspešno završio. Želite li odmah da restartujete raèunar?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Restartuj odmah"
  !define MUI_TEXT_FINISH_REBOOTLATER "Bez restartovanja"
  !define MUI_TEXT_FINISH_RUN "Pokretanje programa $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Prikaži ProèitajMe fajl"

  !define MUI_TEXT_STARTMENU_TITLE "Izbor direktorijuma u Start meniju"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Izaberite direktorijum u Start meniju u kome æe se kreirati preèice."
  !define MUI_INNERTEXT_STARTMENU_TOP "Izaberite direktorijum u Start meniju u kome želite da budu kreirane preèice programa. Takoðe možete upisati i ime za kreiranje novog direktorijuma."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Bez kreiranja preèica"

  !define MUI_TEXT_ABORTWARNING "Sigurno želite da prekinete instaliranje programa $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Dobrodošli u deinstaliranje programa $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Biæete voðeni kroz proces deinstaliranja programa $(^NameDA).\r\n\r\nPre poèetka deinstaliranja, uverite se da je program $(^NameDA) iskljuèen. $_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Deinstaliranje programa $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Deinstaliranje programa $(^NameDA) sa raèunara."

  !define MUI_UNTEXT_LICENSE_TITLE "Dogovor o pravu korišæenja"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Pažlivo proèitajte dogovor o pravu korišæenja pre deinstaliranja programa $(^NameDA)-e."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ako prihvatate sve uslove dogovora, pritisnite dugme 'Prihvatam' za nastavak. Morate prihvatiti dogovor da bi deinstalirali program $(^NameDA)-u."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ako prihvatate sve uslove dogovora, obeležite kvadratiæ ispod. Morate prihvatiti dogovor da bi deinstalirali program $(^NameDA)-u. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ako prihvatate sve uslove dogovora, izaberite prvu opciju ispod. Morate prihvatiti dogovor da bi deinstalirali program $(^NameDA)-u. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Izbor komponenti za deinstaliranje"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Izaberite komponente za deinstaliranje. Deinstaliraju se samo oznaèene komponente."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Izbor direktorijuma za deinstaliranje"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Izaberite direktorijum iz koga æete deinstalirati program $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Deinstaliranje"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Saèekajte dok se program $(^NameDA) deinstalira."

  !define MUI_UNTEXT_FINISH_TITLE "Završeno deinstaliranje"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Deinstaliranje je uspešno završena."
  
  !define MUI_UNTEXT_ABORT_TITLE "Prekinuto deinstaliranje"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Deinstaliranje je prekinuto i nije uspešno završeno."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Završeno deinstaliranje programa $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Program $(^NameDA) je deinstaliran sa raèunara.\r\n\r\nPritisnite dugme 'Kraj' za zatvaranje ovog prozora."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Raèunar mora biti restartovan da bi završili deinstaliranje programa $(^NameDA). Želite li odmah da restartujete raèunar?"

  !define MUI_UNTEXT_ABORTWARNING "Sigurno želite da prekinete deinstaliranje programa $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END