;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Croatian (1050)
;By Igor Ostriz

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "CROATIAN"

  !define MUI_LANGNAME "Hrvatski" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Dobrodošli u instalaciju programa $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Instalacija programa $(^Name) na Vaše raèunalo sastoji se od nekoliko jednostavnih koraka kroz koje æe Vas provesti ovaj èarobnjak.\r\n\r\nPreporuèamo zatvaranje svih ostalih aplikacija prije samog poèetka instalacije. To æe omoguæiti nadogradnju nekih sistemskih datoteka bez potrebe za ponovnim pokretanjem Vašeg raèunala. U svakom trenutku instalaciju možete prekinuti pritiskom na 'Odustani'.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Licenèni ugovor"
  !define MUI_TEXT_LICENSE_SUBTITLE "Molim proèitajte licencu prije instalacije programa $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "'Page Down' za ostatak licence."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ukoliko prihvaæate uvjete licence, odaberite 'Prihvaæam' za nastavak. Morate prihvatiti licencu za instalaciju programa $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ukoliko prihvaæate uvjete licence, oznaèite ispod kvadratiæ. Morate prihvatiti licencu za instalaciju programa $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ukoliko prihvaæate uvjete licence, odaberite ispod prvu opciju. Morate prihvatiti licencu za instalaciju programa $(^Name). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Izbor komponenti"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Odaberite komponente programa $(^Name) koje želite instalirati."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Prijeðite pokazivaèem iznad komponente za njezin opis."

  !define MUI_TEXT_DIRECTORY_TITLE "Odaberite odredište za instalaciju"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Odaberite mapu u koju želite instalirati program $(^Name)."

  !define MUI_TEXT_INSTALLING_TITLE "Instaliranje"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Molim prièekajte na završetak instalacije programa $(^Name)."

  !define MUI_TEXT_FINISH_TITLE "Kraj instalacije"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalacija je u potpunosti završila uspješno."

  !define MUI_TEXT_ABORT_TITLE "Instalacija je prekinuta"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalacija nije završila uspješno."

  !define MUI_BUTTONTEXT_FINISH "&Kraj"
  !define MUI_TEXT_FINISH_INFO_TITLE "Dovršenje instalacije programa $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Program $(^Name) je instaliran na Vaše raèunalo.\r\n\r\nOdaberite 'Kraj' za završetak."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Vaše raèunalo treba ponovno pokrenuti za završetak instalacije programa $(^Name). Želite li to uèiniti sada?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Ponovno pokreni raèunalo sada"
  !define MUI_TEXT_FINISH_REBOOTLATER "Ponovno æu pokrenuti raèunalo kasnije"
  !define MUI_TEXT_FINISH_RUN "&Pokreni program $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Prikaži &Readme"

  !define MUI_TEXT_STARTMENU_TITLE "Izbor mape u Start meniju"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Odaberite ime za programsku mapu unutar Start menija."
  !define MUI_INNERTEXT_STARTMENU_TOP "Program æe pripadati odabranoj programskoj mapi u Start meniju. Možete odrediti novo ime za mapu ili odabrati veæ postojeæu."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nemoj napraviti preèace"

  !define MUI_TEXT_ABORTWARNING "Jeste li sigurni da želite prekinuti instalaciju programa $(^Name)?"


  !define MUI_UNTEXT_CONFIRM_TITLE "Uklanjanje programa $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Program $(^Name) æe biti uklonjen s ovog raèunala."

  !define MUI_UNTEXT_LICENSE_TITLE "Licenèni ugovor"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Molim proèitajte licencu prije uklanjanja programa $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ukoliko prihvaæate uvjete licence, odaberite 'Prihvaæam' za nastavak. Morate prihvatiti licencu za uklanjanje programa $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ukoliko prihvaæate uvjete licence, oznaèite kvadratiæ ispod. Morate prihvatiti licencu za uklanjanje programa $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ukoliko prihvaæate uvjete licence, odaberite prvu opciju ispod. Morate prihvatiti licencu za uklanjanje programa $(^Name). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Izbor komponenti"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Odaberite koje komponente programa $(^Name) želite ukloniti."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Odaberite polazište za uklanjanje"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Odaberite mapu iz koje želite ukloniti program $(^Name)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Uklanjanje"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Molim prièekajte na završetak uklanjanja programa $(^Name)."

  !define MUI_UNTEXT_FINISH_TITLE "Završeno"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Uklanjanje je u potpunosti završilo uspješno."
  
  !define MUI_UNTEXT_ABORT_TITLE "Uklanjanje je prekinuto"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Uklanjanje nije završilo uspješno."

!insertmacro MUI_LANGUAGEFILE_END