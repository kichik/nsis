;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Lithuanian (1063)
;By Andrius Norkaitis (NorCis) updated to 1.66 by Vytautas Krivickas (Vytautas)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "LITHUANIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Lietuviu" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Sveiki atvyke i $(^Name) idiegimo programa."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Ši programa jums pades lengvai idiegti $(^Name).\r\n\r\nRekomenduojama išjungti visas programas, prieš pradedant idiegima. Tai leis atnaujinti sistemos bylas neperkraunat kompiuterio.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Naudojimo sutartis"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Prašome perskaityti sutarti prieš idiegdami $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Paspauskite Page Down ir perskaitykite visa sutarti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Jei jus sutinkate su nurodytomis salygomis, spauskite Sutinku. Jus privalote sutikti, jei norite idiegti $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "s, padekite varnele tam skirtame laukelyje. Jus privalote sutikti, jei norite idiegti $(^Name). "
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jei jus sutinkate su nurodytomis salygomis, pasirinkite pirma pasirinkima esancia žemiau. Jus privalote sutikti, jei norite idiegti $(^Name). "
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Pasirinkite"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Pasirinkite kokias $(^Name) galimybes jus norite idiegti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Paaiškinimas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Užveskite peles kursoriu ant komponento ir pamatysite jo aprašyma."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Pasirinkite idiegimo vieta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Pasirinkite kataloga i kuri idiegsite $(^Name)."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Diegiama"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Prašome palaukti, kol $(^Name) bus idiegtas."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Idiegimas baigtas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Idiegimas baigtas sekmingai."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Idiegimas nutrauktas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Idiegimas nebuvo baigtas sekmingai."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Baigti"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Baigiu $(^Name) idiegimo procesa"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "$(^Name) buvo idiegtas i jusu kompiuteri.\r\n\r\nPaspauskite Baigti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Jusu kompiuteris turi buti perkrautas, kad butu baigtas $(^Name) idiegimas. Ar jus norite perkrauti dabar?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Perkrauti dabar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Aš noriu perkrauti pats veliau"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "&Ijungti $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "&Parodyti dokumentacija"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Pasirinkite Start Menu kataloga"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Pasirinkite Start Menu kataloga, kuriame bus sukurtos programos nuorodos."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Pasirinkite Start Menu kataloga, kuriame bus sukurtos programos nuorodos. Jus taip pat galite sukurti nauja kataloga."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nekurti nuorodu"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Ar jus tikrai norite išjungti $(^Name) idiegimo programa?"
  
  
;  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Paspauskite Panaikinti, jei norite pradeti."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "Panaikinti $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "Ištrinti $(^Name) iš jusu kompiuterio."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_TITLE "Naudojimo sutartis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_LICENSE_SUBTITLE "Prašome perskaityti sutarti prieš $(^Name) ištrinima."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM "Jei jus sutinkate su nurodytomis salygomis, spauskite Sutinku. Jus privalote sutikti, jei norite ištrinti $(^Name)."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "s, padekite varnele tam skirtame laukelyje. Jus privalote sutikti, jei norite ištrinti $(^Name). "
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jei jus sutinkate su nurodytomis salygomis, pasirinkite pirma pasirinkima esancia žemiau. Jus privalote sutikti, jei norite ištrinti $(^Name)."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Panaikinama"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prašome palaukti, kol $(^Name) bus panaikintas."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Pasirinkite"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Pasirinkite kokias $(^Name) galimybes jus norite ištrinti."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Pasirinkite ištrinimo vieta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Pasirinkite kataloga iš kurio ištrinsite $(^Name)."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Programos ištrynimas baigtas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Ištrynimas baigtas sekmingai."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Ištrynimas nutrauktas"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Ištrynimas nebuvo baigtas sekmingai."
  
!insertmacro MUI_LANGUAGEFILE_END