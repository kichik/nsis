;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Finnish (1035)
;By Eclipser (Jonne Lehtinen) <Eclipser at pilvikaupunki dot com>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "FINNISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Suomi" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Tervetuloa ohjelman ${MUI_PRODUCT} asennukseen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "T‰m‰ avustaja ohjaa sinut ohjelman ${MUI_PRODUCT} asennuksen l‰pi.\r\n\r\nOn suositeltavaa sulkea kaikki muut ohjelmat ennen asennuksen aloittamista, jotta asennus voisi p‰ivitt‰‰ tiettyj‰ j‰rjestelm‰tiedostoja k‰ynnist‰m‰tt‰ konetta uudelleen.\r\n\r\n"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Lisenssisopimus"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Lue lisenssiehdot tarkasti ennen ohjelman ${MUI_PRODUCT} asentamista."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Paina Page Down n‰hd‰ksesi loput sopimuksesta."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Jos hyv‰ksyt ehdot, valitse Hyv‰ksyn jatkaaksesi. Sinun pit‰‰ hyv‰ksy‰ ehdot asentaaksesi ohjelman ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jos hyv‰ksyt ehdot, laita rasti alla olevaan ruutuun. Sinun pit‰‰ hyv‰ksy‰ ehdot asentaaksesi ohjelman ${MUI_PRODUCT}. $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jos hyv‰ksyt ehdot, valitse ensimm‰inen vaihtoehto alapuolelta. Sinun pit‰‰ hyv‰ksy‰ ehdot asentaaksesi ohjelman ${MUI_PRODUCT}. $_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Valitse komponentit"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Valitse toiminnot, jotka haluat asentaa ohjelmaan ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Selitys"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Siirr‰ hiiri komponentin nimen p‰‰lle saadaksesi sen selityksen."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Valitse asennuskohde"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Valitse hakemisto, johon haluat asentaa ohjelman ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Asennetaan"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Odota... ${MUI_PRODUCT} asennetaan..."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Asennus valmis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Asennus valmistui onnistuneesti."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Asennus keskeytettiin"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Asennus ei onnistunut."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Valmis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Viimeistell‰‰n ohjelman ${MUI_PRODUCT} asennusta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} on asennettu koneellesi.\r\n\r\nValitse Valmis sulkeaksesi avustajan."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Tietokoneesi pit‰‰ k‰ynnist‰‰ uudelleen jotta ohjelman ${MUI_PRODUCT} asennus saataisiin valmiiksi. Haluatko k‰ynnist‰‰ koneen uudelleen nyt?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "K‰ynnist‰ uudelleen nyt"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "K‰ynnist‰n koneen myˆhemmin uudelleen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "K‰ynnist‰ ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "N‰yt‰ LueMinut"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Valitse K‰ynnist‰-valikon hakemisto"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Valitse K‰ynnist‰-valikon hakemisto ohjelman pikakuvakkeille."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Valitse K‰ynnist‰-valikon hakemisto, johon haluaisit luoda ohjelman pikakuvakkeet. Voit myˆs kirjoittaa uuden nimen."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "ƒl‰ luo pikakuvakkeita"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Haluatko varmasti lopettaa ${MUI_PRODUCT} Asennuksen?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Valitse Poista poistaaksesi ohjelman."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "Poista ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "Poista ${MUI_PRODUCT} tietokoneestasi."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Valitse komponentit"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Valitse ${MUI_PRODUCT} toiminnot, jotka haluat poistaa."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Poistetaan"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Odota... Ohjelmaa ${MUI_PRODUCT} poistetaan."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Poisto valmis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Ohjelma poistettiin onnistuneesti."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Poisto lopetettu"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Ohjelmaa poisto ep‰onnistuneesti."

!insertmacro MUI_LANGUAGEFILE_END
