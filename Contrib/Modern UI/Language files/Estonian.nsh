;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Estonian (1061)
;By izzo (izzo@hot.ee)

;--------------------------------

  !insertmacro MUI_LANGUAGEFILE_BEGIN "ESTONIAN"

  !define MUI_LANGNAME "Eesti keel" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Tere tulemast $(^NameDA) paigaldamisele!"
  !define MUI_TEXT_WELCOME_INFO_TEXT "See abiline aitab paigaldada programmi: $(^NameDA).\r\n\r\nEnne paigaldamise alustamist on soovitatav kõik teised programmid sulgeda, see võimaldab teatud süsteemifaile uuendada ilma arvutit taaskäivitamata.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Litsentsileping"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Enne $(^NameDA) paigaldamist vaata palun litsentsileping üle."
  !define MUI_INNERTEXT_LICENSE_TOP "Vajuta Page Down, et näha ülejäänud teksti."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Kui sa oled lepingu tingimustega nõus, vali jätkamiseks 'Nõustun'. $(^NameDA) paigaldamiseks pead sa tingimustega nõustuma."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Kui nõustute lepingu tingimustega, valige allolev märkeruut. $(^NameDA) paigaldamiseks peate lepinguga nõustuma. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Kui nõustute lepingu tingimustega, märkige alpool esimene valik. $(^NameDA) paigaldamiseks peate lepinguga nõustuma. $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Vali komponendid"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Vali millised $(^NameDA) osad sa soovid paigaldada."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Kirjeldus"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Nihuta hiir komponendile, et näha selle kirjeldust."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Vali asukoht"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Vali kaust kuhu paigaldada $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Paigaldan..."
  !define MUI_TEXT_INSTALLING_SUBTITLE "Palun oota kuni $(^NameDA) on paigaldatud."
  
  !define MUI_TEXT_FINISH_TITLE "Programm paigaldatud"
  !define MUI_TEXT_FINISH_SUBTITLE "Paigaldus edukalt sooritatud."

  !define MUI_TEXT_ABORT_TITLE "Paigaldus katkestatud"
  !define MUI_TEXT_ABORT_SUBTITLE "Paigaldamine ebaõnnestus."

  !define MUI_BUTTONTEXT_FINISH "&Lõpeta"
  !define MUI_TEXT_FINISH_INFO_TITLE "$(^NameDA) paigalduse lõpule viimine"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) on sinu arvutisse paigaldatud.\r\n\r\nAbilise sulgemiseks vajuta Lõpeta."
  !define MUI_TEXT_FINISH_INFO_REBOOT "$(^NameDA) paigaldamise lõpetamiseks tuleb arvuti taaskäivitada. Kas tahad arvuti kohe taaskäivitada ?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Taaskäivita kohe"
  !define MUI_TEXT_FINISH_REBOOTLATER "Taaskäivitan hiljem käsitsi"
  !define MUI_TEXT_FINISH_RUN "Käivita $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Kuva 'Loe mind'"
  
  !define MUI_TEXT_STARTMENU_TITLE "Vali Start-menüü kaust"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Vali Start-menüü kaust, kust teha otsetee programmi juurde."
  !define MUI_INNERTEXT_STARTMENU_TOP "Vali Start-menüü kaust, kuhu sulle meeldiks paigutada programmi otseteed. Võid ka sisestada nime, et luua uus kaust."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ära loo otseteid"
  
  !define MUI_TEXT_ABORTWARNING "Oled sa kindel et soovid $(^Name) paigaldamise katkestada?"  
  
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Eemalda $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Eemalda $(^NameDA) oma arvutist."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Eemaldan..."
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Palun oota kuni $(^NameDA) eemaldatakse."
    
  !define MUI_UNTEXT_FINISH_TITLE "Eemaldamine lõpetatud"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Eemaldamine edukalt lõpule viidud."
  
  !define MUI_UNTEXT_ABORT_TITLE "Eemaldamine katkestatud"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Eemaldamine ebaõnestus."

!insertmacro MUI_LANGUAGEFILE_END