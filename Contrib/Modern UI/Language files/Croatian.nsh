;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Croatian (1050)
;By Igor Ostriz

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "CROATIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Hrvatski" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Dobrodošli u instalaciju programa $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Ovaj èarbnjak æe Vas provesti kroz instalaciju programa $(^Name) na Vaše raèunalo.\r\n\r\nPreporuèamo da zatvorite sve ostale aplikacije prije poèetka instalacije. To æe omoguæiti nadogradnju nekih sistemskih datoteka bez potrebe za ponovnim pokretanjem Vašeg raèunala.\r\n\r\n$_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licenèni ugovor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Molim proèitajte licencu prije instalacije programa $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "'Page Down' za ostatak licence."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ukoliko prihvaæate uvjete licence, odaberite 'Prihvaæam' za nastavak. Morate prihvatiti licencu za instalaciju programa $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ukoliko prihvaæate uvjete licence, oznaèite ispod kvadratiæ. Morate prihvatiti licencu za instalaciju programa $(^Name). $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ukoliko prihvaæate uvjete licence, odaberite ispod prvu opciju. Morate prihvatiti licencu za instalaciju programa $(^Name). $_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Izbor komponenti"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Odaberite komponente programa $(^Name) koje želite instalirati."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Prijeðite pokazivaèem iznad komponente za njezin opis."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Odaberite odredište za instalaciju"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Odaberite mapu u koji želite instalirati program $(^Name)."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Instaliranje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Molim prièekajte na završetak instalacije programa $(^Name)."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Kraj instalacije"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Instalacija je u potpunosti završila uspješno."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Instalacija je prekinuta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Instalacija nije završila uspješno."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Kraj"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Dovršenje instalacije programa $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "Program $(^Name) je instaliran na Vaše raèunalo.\r\n\r\nOdaberite 'Kraj' za završetak."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Vaše raèunalo treba ponovno pokrenuti za završetak instalacije programa $(^Name). Želite li to uèiniti sada?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Ponovno pokreni raèunalo sada"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Ponovno æu pokrenuti raèunalo kasnije"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "&Pokreni program $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Prikaži &Readme"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Odaberite mapu u Start meniju"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Odaberite mapu u Start meniju za programske preèace."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Odaberite mapu u Start Meniju u kojem želite napraviti programske preèace. Možete i upisati ime za stvaranje nove mape."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nemoj napraviti preèace"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Jeste li sigurni da želite prekinuti instalaciju programa $(^Name)?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "Uklanjanje programa $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "Program $(^Name) æe biti uklonjen s ovog raèunala."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_TITLE "Licenèni ugovor"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_SUBTITLE "Molim proèitajte licencu prije uklanjanja programa $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM "Ukoliko prihvaæate uvjete licence, odaberite 'Prihvaæam' za nastavak. Morate prihvatiti licencu za uklanjanje programa $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ukoliko prihvaæate uvjete licence, oznaèite kvadratiæ ispod. Morate prihvatiti licencu za uklanjanje programa $(^Name). $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ukoliko prihvaæate uvjete licence, odaberite prvu opciju ispod. Morate prihvatiti licencu za uklanjanje programa $(^Name). $_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Izbor komponenti"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Odaberite koje komponente programa $(^Name) želite ukloniti."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_TITLE "Odaberite polazište za uklanjanje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_DIRECTORY_SUBTITLE "Odaberite mapu iz koje želite ukloniti program $(^Name)."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Uklanjanje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Molim prièekajte na završetak uklanjanja programa $(^Name)."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Završeno"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Uklanjanje je u potpunosti završilo uspješno."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Uklanjanje je prekinuto"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Uklanjanje nije završilo uspješno."

!insertmacro MUI_LANGUAGEFILE_END