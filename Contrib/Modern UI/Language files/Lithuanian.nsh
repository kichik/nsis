;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Lithuanian (1063)
;By Andrius Norkaitis (NorCis) updated to 1.66 by Vytautas Krivickas (Vytautas)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "LITHUANIAN"

  !define MUI_LANGNAME "Lietuviu" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Sveiki atvyke i $(^Name) idiegimo programa."
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ši programa jums pades lengvai idiegti $(^Name).\r\n\r\nRekomenduojama išjungti visas programas, prieš pradedant idiegima. Tai leis atnaujinti sistemos bylas neperkraunat kompiuterio.\r\n\r\n"
  
  !define MUI_TEXT_LICENSE_TITLE "Naudojimo sutartis"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Prašome perskaityti sutarti prieš idiegdami $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Paspauskite Page Down ir perskaitykite visa sutarti."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Jei jus sutinkate su nurodytomis salygomis, spauskite Sutinku. Jus privalote sutikti, jei norite idiegti $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jei jus sutinkate su nurodytomis salygomis, padekite varnele tam skirtame laukelyje. Jus privalote sutikti, jei norite idiegti $(^Name). "
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jei jus sutinkate su nurodytomis salygomis, pasirinkite pirma pasirinkima esancia žemiau. Jus privalote sutikti, jei norite idiegti $(^Name). "
  
  !define MUI_TEXT_COMPONENTS_TITLE "Pasirinkite"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Pasirinkite kokias $(^Name) galimybes jus norite idiegti."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Paaiškinimas"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Užveskite peles kursoriu ant komponento ir pamatysite jo aprašyma."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Pasirinkite idiegimo vieta"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Pasirinkite kataloga i kuri idiegsite $(^Name)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Diegiama"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Prašome palaukti, kol $(^Name) bus idiegtas."
  
  !define MUI_TEXT_FINISH_TITLE "Idiegimas baigtas"
  !define MUI_TEXT_FINISH_SUBTITLE "Idiegimas baigtas sekmingai."
  
  !define MUI_TEXT_ABORT_TITLE "Idiegimas nutrauktas"
  !define MUI_TEXT_ABORT_SUBTITLE "Idiegimas nebuvo baigtas sekmingai."
  
  !define MUI_BUTTONTEXT_FINISH "&Baigti"
  !define MUI_TEXT_FINISH_INFO_TITLE "Baigiu $(^Name) idiegimo procesa"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) buvo idiegtas i jusu kompiuteri.\r\n\r\nPaspauskite Baigti."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Jusu kompiuteris turi buti perkrautas, kad butu baigtas $(^Name) idiegimas. Ar jus norite perkrauti dabar?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Perkrauti dabar"
  !define MUI_TEXT_FINISH_REBOOTLATER "Aš noriu perkrauti pats veliau"
  !define MUI_TEXT_FINISH_RUN "&Ijungti $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Parodyti dokumentacija"
  
  !define MUI_TEXT_STARTMENU_TITLE "Pasirinkite Start Menu kataloga"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Pasirinkite Start Menu kataloga, kuriame bus sukurtos programos nuorodos."
  !define MUI_INNERTEXT_STARTMENU_TOP "Pasirinkite Start Menu kataloga, kuriame bus sukurtos programos nuorodos. Jus taip pat galite sukurti nauja kataloga."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nekurti nuorodu"
  
  !define MUI_TEXT_ABORTWARNING "Ar jus tikrai norite išjungti $(^Name) idiegimo programa?"
  
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Panaikinti $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Ištrinti $(^Name) iš jusu kompiuterio."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Naudojimo sutartis"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Prašome perskaityti sutarti prieš $(^Name) ištrinima."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Jei jus sutinkate su nurodytomis salygomis, spauskite Sutinku. Jus privalote sutikti, jei norite ištrinti $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "s, padekite varnele tam skirtame laukelyje. Jus privalote sutikti, jei norite ištrinti $(^Name). "
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jei jus sutinkate su nurodytomis salygomis, pasirinkite pirma pasirinkima esancia žemiau. Jus privalote sutikti, jei norite ištrinti $(^Name)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Panaikinama"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prašome palaukti, kol $(^Name) bus panaikintas."

  !define MUI_TEXT_COMPONENTS_TITLE "Pasirinkite"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Pasirinkite kokias $(^Name) galimybes jus norite ištrinti."

  !define MUI_TEXT_DIRECTORY_TITLE "Pasirinkite ištrinimo vieta"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Pasirinkite kataloga iš kurio ištrinsite $(^Name)."
    
  !define MUI_UNTEXT_FINISH_TITLE "Programos ištrynimas baigtas"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Ištrynimas baigtas sekmingai."
  
  !define MUI_UNTEXT_ABORT_TITLE "Ištrynimas nutrauktas"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Ištrynimas nebuvo baigtas sekmingai."
  
!insertmacro MUI_LANGUAGEFILE_END