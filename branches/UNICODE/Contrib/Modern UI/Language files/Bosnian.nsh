;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Bosnian (5146)
;By Salih Èavkiæ, cavkic@skynet.be

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Bosnian"

  !define MUI_LANGNAME "Bosanski" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Dobrodošli u program za instalaciju $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ovaj program æe instalirati $(^NameDA) na Vaš sistem. \r\n\r\nPreporuèujemo da neizostavno zatvorite sve druge otvorene programe prije nego što definitivno zapoènete sa instaliranjem. To æe omoguæiti bolju nadogradnju odreðenih sistemskih datoteka bez potrebe da Vaš raèunar ponovo startujete. Instaliranje programa možete prekinuti pritiskom na dugme 'Odustani'.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Licencni ugovor"
  !define MUI_TEXT_LICENSE_SUBTITLE "Molim proèitajte licencni ugovor $(^NameDA) prije instalacije programa."
  !define MUI_INNERTEXT_LICENSE_TOP "Pritisnite 'Page Down' na tastaturi za ostatak licence."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ako prihvatate uslove licence, odaberite 'Prihvatam' za nastavak. Morate prihvatiti licencu za instalaciju programa $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ako prihvatate uslove licence, oznaèite donji kvadratiæ. Morate prihvatiti licencu za instalaciju programa $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ako prihvatate uslove licence, odaberite prvu donju opciju. Morate prihvatiti licencu za instalaciju programa $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Izbor komponenti za instalaciju"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Izaberite komponente programa $(^NameDA) koje želite instalirati."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Postavite kursor od miša iznad komponente da biste vidjeli njezin opis."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Selektirajte jednu komponentu da vidite njezin opis."
  !endif

  !define MUI_TEXT_DIRECTORY_TITLE "Odaberite odredište za instalaciju"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Odaberite mapu u koju želite instalirati program $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Instaliranje"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Molim prièekajte na završetak instalacije programa $(^NameDA)."

  !define MUI_TEXT_FINISH_TITLE "Kraj instalacije"
  !define MUI_TEXT_FINISH_SUBTITLE "Instalacija je u potpunosti uspješno završila."

  !define MUI_TEXT_ABORT_TITLE "Instalacija je prekinuta"
  !define MUI_TEXT_ABORT_SUBTITLE "Instalacija nije završila uspješno."

  !define MUI_BUTTONTEXT_FINISH "&Kraj"
  !define MUI_TEXT_FINISH_INFO_TITLE "Dovršavanje instalacije programa $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Program $(^NameDA) je instaliran na Vaše raèunar.\r\n\r\nPritisnite dugme 'Kraj' za završetak."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Raèunar treba ponovno startovati za dovršavanje instalacije programa $(^NameDA). Želite li to uèiniti sada?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Startuj raèunar odmah sad"
  !define MUI_TEXT_FINISH_REBOOTLATER "Ponovno æu pokrenuti raèunar kasnije"
  !define MUI_TEXT_FINISH_RUN "&Pokreni program $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Prikaži datoteku &Readme"

  !define MUI_TEXT_STARTMENU_TITLE "Izbor mape u Start meniju"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Izaberite ime za programsku mapu unutar Start menija."
  !define MUI_INNERTEXT_STARTMENU_TOP "Izaberite jednu mapu u Start meniju u kojoj želite da se kreiraju preèice programa. Možete takoðer unijeti ime za novu mapu ili selektirati veæ postojeæu."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nemojte praviti preèice"

  !define MUI_TEXT_ABORTWARNING "Jeste li sigurni da želite prekinuti instalaciju programa $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Dobrodošli u postupak uklanjanja programa $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ovaj æe Vas vodiè provesti kroz postupak uklanjanja programa $(^NameDA).\r\n\r\nPrije samog poèetka, molim zatvorite program $(^NameDA) ukoliko je sluèajno otvoren.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Uklanjanje programa $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Program $(^NameDA) æe biti uklonjen sa Vašeg raèunara."

  !define MUI_UNTEXT_LICENSE_TITLE "Licencni ugovor o pravu korištenja"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Molim proèitajte licencu prije uklanjanja programa $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ako prihvatate uslove licence, odaberite 'Prihvatam' za nastavak. Morate prihvatiti licencu za uklanjanje programa $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ako prihvatate uslove licence, oznaèite donji kvadratiæ. Morate prihvatiti licencu za uklanjanje programa $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ako prihvatate uslove licence, odaberite prvu donju opciju. Morate prihvatiti licencu za uklanjanje programa $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Izbor komponenti za uklanjanje"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Izaberite komponente programa $(^NameDA) koje želite ukloniti."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Izaberite polazište za uklanjanje"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Selektirajte mapu iz koje želite ukloniti program $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Uklanjanje"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Molim Vas prièekajte da vodiè završi uklanjanje $(^NameDA) programa."

  !define MUI_UNTEXT_FINISH_TITLE "Uklanjanje je završeno"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Uklanjanje je u potpunosti završilo uspješno."
  
  !define MUI_UNTEXT_ABORT_TITLE "Uklanjanje je prekinuto"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Uklanjanje nije završilo uspješno."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Završetak uklanjanja programa $(^NameDA) sa Vašeg sistema."
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Program $(^NameDA) je uklonjen sa Vašeg raèunara.\r\n\r\nPritisnite dugme 'Kraj' za zatvaranje ovog prozora."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Vaš raèunar trebate ponovno startovati da dovršite uklanjanje programa $(^NameDA). Želite li da odmah sad ponovo startujete raèunar?"

  !define MUI_UNTEXT_ABORTWARNING "Jeste li sigurni da želite prekinuti uklanjanje $(^Name) programa?"  

!insertmacro MUI_LANGUAGEFILE_END

