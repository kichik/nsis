;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.64

;Language: Lithuanian (1063)
;By Andrius Norkaitis (NorCis)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "LITHUANIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Lietuviu" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Paspauskite Toliau, jei norite testi idiegima."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Paspauskite Idiegti, jei norite pradeti."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Sveiki atvyke i ${MUI_PRODUCT} idiegimo programa."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Ši programa jums pades lengvai idiegti ${MUI_PRODUCT}.\r\n\r\nRekomenduojama išjungti visas programas, prieš pradedant idiegima. Tai leis atnaujinti sistemos bylas neperkraunat kompiuterio.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Naudojimo sutartis"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Prašome perskaityti sutarti prieš idiegdami ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Paspauskite Page Down ir perskaitykite visa sutarti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Jei jus sutinkate su nurodytomis salygomis, spauskite Sutinku. Jus privalote sutikti, jei norite idiegti ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "s, padekite varnele tam skirtame laukelyje. Jus privalote sutikti, jei norite idiegti ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jei jus sutinkate su nurodytomis salygomis, pasirinkite pirma pasirinkima esancia žemiau. YJus privalote sutikti, jei norite idiegti ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Pasirinkite"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Pasirinkite kokias ${MUI_PRODUCT} galimybes jus norite idiegti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Pažymekite komponentus, kuriuos norite idiegti ir nuimkite varnele, kuriu jus nenorite."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Paaiškinimas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Užveskite peles kursoriu ant komponento ir pamatysite jo aprašyma."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Pasirinkite idiegimo vieta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Pasirinkite kataloga i kuri idiegsite ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT} bus idiegtas žemiau nurodytame kataloge.$\r$\n$\r$\nNoredami idiegti kitur, paspauskite Pasirinkti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Idiegimo vieta"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Diegiama"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Prašome palaukti, kol ${MUI_PRODUCT} bus idiegtas."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Idiegimas baigtas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Idiegimas baigtas sekmingai."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Idiegimas nutrauktas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Idiegimas nebuvo baigtas sekmingai."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Baigti"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Baigiu ${MUI_PRODUCT} idiegimo procesa"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} buvo idiegtas i jusu kompiuteri.\r\n\r\nPaspauskite Baigti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Jusu kompiuteris turi buti perkrautas, kad butu baigtas ${MUI_PRODUCT} idiegimas. Ar jus norite perkrauti dabar?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Perkrauti dabar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Aš noriu perkrauti pats veliau"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "&Ijungti ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "&Parodyti dokumentacija"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Pasirinkite Start Menu kataloga"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Pasirinkite Start Menu kataloga, kuriame bus sukurtos programos nuorodos."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Pasirinkite Start Menu kataloga, kuriame bus sukurtos programos nuorodos. Jus taip pat galite sukurti nauja kataloga."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nekurti nuorodu"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Ar jus tikrai norite išjungti ${MUI_PRODUCT} idiegimo programa?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Paspauskite Panaikinti, jei norite pradeti."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Panaikinti ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Ištrinti ${MUI_PRODUCT} iš jusu kompiuterio."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Ši programa ištrins ${MUI_PRODUCT} iš jusu kompiuterio."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Panaikinama"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prašome palaukti, kol ${MUI_PRODUCT} bus panaikintas."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Programos ištrynimas baigtas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Ištrynimas baigtas sekmingai."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Ištrynimas nutrauktas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Ištrynimas nebuvo baigtas sekmingai."
  
!insertmacro MUI_LANGUAGEFILE_END