;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Croatian (1050)
;By Igor Ostriz

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "CROATIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Hrvatski" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Odaberite 'Dalje' za nastavak."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Odaberite 'Instaliraj' za poèetak instalacije."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Dobrodošli u instalaciju programa ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Ovaj èarbnjak æe Vas provesti kroz instalaciju programa ${MUI_PRODUCT} na Vaše raèunalo.\r\n\r\nPreporuèamo da zatvorite sve ostale aplikacije prije poèetka instalacije. To æe omoguæiti nadogradnju nekih sistemskih datoteka bez potrebe za ponovnim pokretanjem Vašeg raèunala.\r\n\r\n"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licenèni ugovor"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Molim proèitajte licencu prije instalacije programa ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "'Page Down' za ostatak licence."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Ukoliko prihvaæate uvjete licence, odaberite 'Prihvaæam' za nastavak. Morate prihvatiti licencu za instalaciju programa ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ukoliko prihvaæate uvjete licence, oznaèite kvadratiæ ispod. Morate prihvatiti licencu za instalaciju programa ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ukoliko prihvaæate uvjete licence, odaberite prvu opciju ispod. Morate prihvatiti licencu za instalaciju programa ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Izbor komponenti"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Odaberite komponente programa ${MUI_PRODUCT} koje želite instalirati."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Oznaèite komponente koje želite instalirati, uklonite oznaku s neželjenih komponenti."  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Prijeðite pokazivaèem iznad komponente za njezin opis."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Odaberite odredšte za instalaciju"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Odaberite mapu u koji želite instalirati program ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Program ${MUI_PRODUCT} æe biti instaliran u navedenu mapu.$\r$\n$\r$\nUkoliko želite instalirati u neku drugu mapu, odaberite 'Pregledaj...' i zatim željenu mapu."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Mapa za instalaciju"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Instaliranje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Molim prièekajte na završetak instalacije programa ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Kraj instalacije"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Instalacija je u potpunosti završila uspješno."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Instalacija je prekinuta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Instalacija nije završila uspješno."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Kraj"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Dovršenje instalacije programa ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "Program ${MUI_PRODUCT} je instaliran na Vaše raèunalo.\r\n\r\nOdaberite 'Kraj' da završetak."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Vaše raèunalo treba ponovno pokrenuti za završetak instalacije programa ${MUI_PRODUCT}. Želite li to uèiniti sada?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Ponovno pokreni raèunalo sada"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Ponovno æu pokrenuti raèunalo kasnije"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Pokreni program ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Prikaži Readme"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Odaberite mapu u Start meniju"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Odaberite mapu u Start meniju za programske preèace."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Odaberite mapu u Start Meniju u kojem želite napraviti programske preèace. Možete i sami upisati ime za stvaranje nove mape."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nemoj napraviti preèace"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Jeste li sigurni da želite prekinuti instalaciju programa ${MUI_PRODUCT}?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Odaberite 'Ukloni' za poèetak uklanjanja."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Ukloni program ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Ukloni program ${MUI_PRODUCT} sa ovog raèunala."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Ova aplikacija æe ukloniti program ${MUI_PRODUCT} s Vašeg raèunala."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Uklanjanje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Molim prièekajte na završetak uklanjanja programa ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Završeno"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Uklanjanje je u potpunosti završilo uspješno."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Uklanjanje je prekinuto"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Uklanjanje nije završilo uspješno."

!insertmacro MUI_LANGUAGEFILE_END