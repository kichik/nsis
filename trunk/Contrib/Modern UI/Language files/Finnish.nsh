;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.64

;Language: Finnish (1035)
;By AKX (Aarni Koskela) <webmaster@mailsvr.zzn.com>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "FINNISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Suomi" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Klikkaa Seuraava jatkaaksesi."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Klikkaa Asenna aloittaaksesi asennuksen."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Tervetuloa ohjelman ${MUI_PRODUCT} Asennukseen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "T‰m‰ avustaja ohjaa sinut ohjelman ${MUI_PRODUCT} asennuksen l‰pi.\r\n\r\nOn suositeltavaa sulkea kaikki muut ohjelmat ennen Asennuksen k‰ynnist‰mist‰, sill‰ silloin Asennus voi p‰ivitt‰‰ tiettyj‰ j‰rjestelm‰tiedostoja k‰ynnist‰m‰tt‰ konetta uudelleen.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Lisenssisopimus"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Ole hyv‰ ja lue n‰m‰ lisenssiehdot ennen jatkamista."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Paina Page Down n‰hd‰ksesi loput tekstist‰."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Jos hyv‰ksyt kaikki ehdot, valitse Hyv‰ksyn jatkaaksesi. Sinun pit‰‰ hyv‰ksy‰ ehdot asentaaksesi ohjelman ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Valitse komponentit"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Valitse mit‰ ${MUI_PRODUCT}:n toimintoja haluat asentaa."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Rasti komponentit, jotka haluat asentaa ja poista valinnat komponenteista, joita et halua asennettavan."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Selitys"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Siirr‰ hiiri komponentin nimen p‰‰lle saadaksesi siit‰ tarkemman selityksen."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Valitse asennuskansio"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Valitse kansio, johon haluat asentaa ohjelman ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Asennus asentaa ohjelman ${MUI_PRODUCT} t‰h‰n kansioon.$\r$\n$\r$\nMik‰li haluat asentaa toiseen kansioon, valitse Selaa ja etsi kansio."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Kansio"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Asennetaan"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Odota... ${MUI_PRODUCT} asennetaan..."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Asennus valmis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Asennus onnistui."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Asennus lopetettu" 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Asennus ei onnistunut." 

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Valmis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Viimeistell‰‰n ohjelman ${MUI_PRODUCT} asennus"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} on asennettu onnistuneesti.\r\n\r\nKlikkaa Valmis sulkeaksesi avustajan."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Tietokone pit‰‰ k‰ynnist‰‰ uudelleen jotta ohjelman ${MUI_PRODUCT} asennus saataisiin valmiiksi. Haluatko k‰ynnist‰‰ koneen uudelleen nyt?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "K‰ynnist‰ uudelleen nyt"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "K‰ynnist‰n koneen myˆhemmin uudelleen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Aja ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "N‰yt‰ Readme"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Valitse K‰ynnist‰-valikon kansio"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Valitse K‰ynnist‰-valikon kansio ohjelman kuvakkeille."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Valitse kansio, johon haluat ohjelman kuvakkeet asennettavan. Voit myˆs kirjoittaa uuden nimen."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "ƒl‰ luo kuvakkeita"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Haluatko varmasti lopettaa ${MUI_PRODUCT} Asennuksen?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Klikkaa Poista poistaaksesi ohjelman."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Poista ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Poista ${MUI_PRODUCT} tietokoneestasi."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "T‰m‰ avustaja poistaa ohjelman ${MUI_PRODUCT} tietokoneestasi."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Poistetaan"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Ole hyv‰ ja odota, ohjelmaa ${MUI_PRODUCT} poistetaan."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Poisto valmis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Ohjelma on poistettu onnistuneesti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Poisto lopetettu" 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Ohjelmaa ei poistettu onnistuneesti." 

!insertmacro MUI_LANGUAGEFILE_END