;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Estonian (1061)
;By izzo (izzo@hot.ee)

;--------------------------------

  !insertmacro MUI_LANGUAGEFILE_BEGIN "ESTONIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Eesti keel" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Jätkamiseks vajuta 'Edasi'."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Programmi paigaldamiseks vajuta nuppu 'Paigalda'."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Tere tulemast ${MUI_PRODUCT} paigaldamisele!"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "See abiline aitab paigaldada programmi: ${MUI_PRODUCT}.\r\n\r\nEnne paigaldamise alustamist on soovitatav kõik teised programmid sulgeda, see võimaldab teatud süsteemifaile uuendada ilma arvutit taaskäivitamata.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Litsentsileping"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Enne ${MUI_PRODUCT} paigaldamist vaata palun litsentsileping üle."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Vajuta Page Down, et näha ülejäänud teksti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Kui sa oled lepingu tingimustega nõus, vali jätkamiseks 'Nõustun'. ${MUI_PRODUCT} paigaldamiseks pead sa tingimustega nõustuma."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Kui nõustute lepingu tingimustega, valige allolev märkeruut. ${MUI_PRODUCT} paigaldamiseks peate lepinguga nõustuma."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Kui nõustute lepingu tingimustega, märkige alpool esimene valik. ${MUI_PRODUCT} paigaldamiseks peate lepinguga nõustuma."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Vali komponendid"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Vali millised ${MUI_PRODUCT} osad sa soovid paigaldada."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Märgista komponendid mida soovid paigaldada."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Kirjeldus"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Nihuta hiir komponendile, et näha selle kirjeldust."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Vali asukoht"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Vali kaust kuhu paigaldada ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT} paigaldatakse järgnevasse kausta.$\r$\n$\r$\nEt mujale paigaldada vajuta 'Sirvi' ja vali teine kaust."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Sihtkaust"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Paigaldan..."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Palun oota kuni ${MUI_PRODUCT} on paigaldatud."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Programm paigaldatud"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Paigaldus edukalt sooritatud."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Paigaldus katkestatud"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Paigaldamine ebaõnnestus."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Lõpeta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "${MUI_PRODUCT} paigalduse lõpule viimine"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} on sinu arvutisse paigaldatud.\r\n\r\nAbilise sulgemiseks vajuta Lõpeta."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "${MUI_PRODUCT} paigaldamise lõpetamiseks tuleb arvuti taaskäivitada. Kas tahad arvuti kohe taaskäivitada ?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Taaskäivita kohe"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Taaskäivitan hiljem käsitsi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Käivita ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Kuva 'Loe mind'"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Vali Start-menüü kaust"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Vali Start-menüü kaust, kust teha otsetee programmi juurde."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Vali Start-menüü kaust, kuhu sulle meeldiks paigutada programmi otseteed. Võid ka sisestada nime, et luua uus kaust."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Ära loo otseteid"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Oled sa kindel et soovid ${MUI_PRODUCT} paigaldamise katkestada?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Eemaldamise alustamiseks vajuta 'Eemalda'."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Eemalda ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Eemalda ${MUI_PRODUCT} oma arvutist."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "See abiline eemaldab ${MUI_PRODUCT} sinu arvutist."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Eemaldan..."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Palun oota kuni ${MUI_PRODUCT} eemaldatakse."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Eemaldamine lõpetatud"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Eemaldamine edukalt lõpule viidud."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Eemaldamine katkestatud"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Eemaldamine ebaõnestus."

!insertmacro MUI_LANGUAGEFILE_END