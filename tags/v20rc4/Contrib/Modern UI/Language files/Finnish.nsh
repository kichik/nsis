;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Finnish (1035)
;By Eclipser (Jonne Lehtinen) <Eclipser at pilvikaupunki dot com>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "FINNISH"

  !define MUI_LANGNAME "Suomi" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Tervetuloa ohjelman $(^NameDA) asennukseen"
  !define MUI_TEXT_WELCOME_INFO_TEXT "T‰m‰ avustaja ohjaa sinut ohjelman $(^NameDA) asennuksen l‰pi.\r\n\r\nOn suositeltavaa sulkea kaikki muut ohjelmat ennen asennuksen aloittamista, jotta asennus voisi p‰ivitt‰‰ tiettyj‰ j‰rjestelm‰tiedostoja k‰ynnist‰m‰tt‰ konetta uudelleen.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Lisenssisopimus"
  !define MUI_TEXT_LICENSE_SUBTITLE "Lue lisenssiehdot tarkasti ennen ohjelman $(^NameDA) asentamista."
  !define MUI_INNERTEXT_LICENSE_TOP "Paina Page Down n‰hd‰ksesi loput sopimuksesta."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Jos hyv‰ksyt ehdot, valitse Hyv‰ksyn jatkaaksesi. Sinun pit‰‰ hyv‰ksy‰ ehdot asentaaksesi ohjelman $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jos hyv‰ksyt ehdot, laita rasti alla olevaan ruutuun. Sinun pit‰‰ hyv‰ksy‰ ehdot asentaaksesi ohjelman $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jos hyv‰ksyt ehdot, valitse ensimm‰inen vaihtoehto alapuolelta. Sinun pit‰‰ hyv‰ksy‰ ehdot asentaaksesi ohjelman $(^NameDA). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Valitse komponentit"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Valitse toiminnot, jotka haluat asentaa ohjelmaan $(^NameDA)."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Selitys"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Siirr‰ hiiri komponentin nimen p‰‰lle saadaksesi sen selityksen."

  !define MUI_TEXT_DIRECTORY_TITLE "Valitse asennuskohde"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Valitse hakemisto, johon haluat asentaa ohjelman $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Asennetaan"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Odota... $(^NameDA) asennetaan..."

  !define MUI_TEXT_FINISH_TITLE "Asennus valmis"
  !define MUI_TEXT_FINISH_SUBTITLE "Asennus valmistui onnistuneesti."

  !define MUI_TEXT_ABORT_TITLE "Asennus keskeytettiin"
  !define MUI_TEXT_ABORT_SUBTITLE "Asennus ei onnistunut."

  !define MUI_BUTTONTEXT_FINISH "&Valmis"
  !define MUI_TEXT_FINISH_INFO_TITLE "Viimeistell‰‰n ohjelman $(^NameDA) asennusta"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) on asennettu koneellesi.\r\n\r\nValitse Valmis sulkeaksesi avustajan."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Tietokoneesi pit‰‰ k‰ynnist‰‰ uudelleen jotta ohjelman $(^NameDA) asennus saataisiin valmiiksi. Haluatko k‰ynnist‰‰ koneen uudelleen nyt?"
  !define MUI_TEXT_FINISH_REBOOTNOW "K‰ynnist‰ uudelleen nyt"
  !define MUI_TEXT_FINISH_REBOOTLATER "K‰ynnist‰n koneen myˆhemmin uudelleen"
  !define MUI_TEXT_FINISH_RUN "K‰ynnist‰ $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "N‰yt‰ LueMinut"

  !define MUI_TEXT_STARTMENU_TITLE "Valitse K‰ynnist‰-valikon hakemisto"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Valitse K‰ynnist‰-valikon hakemisto ohjelman pikakuvakkeille."
  !define MUI_INNERTEXT_STARTMENU_TOP "Valitse K‰ynnist‰-valikon hakemisto, johon haluaisit luoda ohjelman pikakuvakkeet. Voit myˆs kirjoittaa uuden nimen."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "ƒl‰ luo pikakuvakkeita"

  !define MUI_TEXT_ABORTWARNING "Haluatko varmasti lopettaa $(^Name) Asennuksen?"


  !define MUI_UNTEXT_CONFIRM_TITLE "Poista $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Poista $(^NameDA) tietokoneestasi."

  !define MUI_UNTEXT_COMPONENTS_TITLE "Valitse komponentit"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Valitse $(^NameDA) toiminnot, jotka haluat poistaa."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Poistetaan"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Odota... Ohjelmaa $(^NameDA) poistetaan."

  !define MUI_UNTEXT_FINISH_TITLE "Poisto valmis"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Ohjelma poistettiin onnistuneesti."

  !define MUI_UNTEXT_ABORT_TITLE "Poisto lopetettu"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Ohjelmaa poisto ep‰onnistuneesti."

!insertmacro MUI_LANGUAGEFILE_END
