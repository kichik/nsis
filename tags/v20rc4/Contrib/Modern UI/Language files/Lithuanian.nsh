;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Lithuanian (1063)
;By Vytautas Krivickas (Vytautas). Updated by Danielius Scepanskis (Daan daniel@takas.lt) 2004.01.09

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "LITHUANIAN"

  !define MUI_LANGNAME "Lietuviu" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Sveiki atvykæ á $(^NameDA) ádiegimo programà."
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ði programa jums padës lengvai ádiegti $(^NameDA).\r\n\r\nRekomenduojama iðjungti visas programas, prieð pradedant ádiegimà. Tai leis atnaujinti sistemos failus neperkraunat kompiuterio.\r\n\r\n"
  
  !define MUI_TEXT_LICENSE_TITLE "Naudojimo sutartis"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Praðome perskaityti sutartá prieð ádiegdami $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Paspauskite Page Down ir perskaitykite visà sutartá."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Jei jûs sutinkate su nurodytomis sàlygomis, spauskite Sutinku. Jûs privalote sutikti, jei norite ádiegti $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Jei jûs sutinkate su nurodytomis sàlygomis, padëkite varnelæ tam skirtame laukelyje. Jûs privalote sutikti, jei norite ádiegti $(^NameDA). "
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jei jûs sutinkate su nurodytomis sàlygomis, pasirinkite pirmà pasirinkimà esantá þemiau. Jûs privalote sutikti, jei norite ádiegti $(^NameDA). "
  
  !define MUI_TEXT_COMPONENTS_TITLE "Pasirinkite"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Pasirinkite kokias $(^NameDA) galimybes jûs norite ádiegti."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Paaiðkinimas"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Uþveskite pelës þymeklá ant komponento ir pamatysite jo apraðymà."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Pasirinkite ádiegimo vietà"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Pasirinkite katalogà á kûri ádiegsite $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Diegiama"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Praðome palaukti, kol $(^NameDA) bus ádiegtas."
  
  !define MUI_TEXT_FINISH_TITLE "Ádiegimas baigtas"
  !define MUI_TEXT_FINISH_SUBTITLE "Ádiegimas baigtas sekmingai."
  
  !define MUI_TEXT_ABORT_TITLE "Ádiegimas nutrauktas"
  !define MUI_TEXT_ABORT_SUBTITLE "Ádiegimas nebuvo baigtas sekmingai."
  
  !define MUI_BUTTONTEXT_FINISH "&Baigti"
  !define MUI_TEXT_FINISH_INFO_TITLE "Baigiu $(^NameDA) ádiegimo procesà"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) buvo ádiegtas á jûsø kompiuterá.\r\n\r\nPaspauskite Baigti."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Jûsø kompiuteris turi bûti perkrautas, kad bûtø baigtas $(^NameDA) ádiegimas. Ar jûs norite perkrauti dabar?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Perkrauti dabar"
  !define MUI_TEXT_FINISH_REBOOTLATER "Að noriu perkrauti veliau pats"
  !define MUI_TEXT_FINISH_RUN "&Leisti $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Parodyti dokumentacijà"
  
  !define MUI_TEXT_STARTMENU_TITLE "Pasirinkite Start Menu katalogà"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Pasirinkite Start Menu katalogà, kuriame bus sukurtos programos nuorodos."
  !define MUI_INNERTEXT_STARTMENU_TOP "Pasirinkite Start Menu katalogà, kuriame bus sukurtos programos nuorodos. Jûs taip pat galite sukurti naujà katalogà."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nekurti nuorodø"
  
  !define MUI_TEXT_ABORTWARNING "Ar jûs tikrai norite iðjungti $(^Name) ádiegimo programà?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Sveiki atvykæ á $(^NameDA) paðalinimo programà."
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ði programa jums padës lengvai iðtrinti $(^NameDA).\r\n\r\nPrieð pradedant pasitikrinkite kad $(^NameDA) yra iðjungta.\r\n\r\n"

  !define MUI_UNTEXT_CONFIRM_TITLE "Panaikinti $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Iðtrinti $(^NameDA) ið jûsø kompiuterio."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Naudojimo sutartis"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Praðome perskaityti sutartá prieð $(^NameDA) paðalinimà."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Jei jûs sutinkate su nurodytomis sàlygomis, spauskite Sutinku. Jûs privalote sutikti, jei norite iðtrinti $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "s, padëkite varnelæ tam skirtame laukelyje. Jûs privalote sutikti, jei norite iðtrinti $(^NameDA). "
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Jei jûs sutinkate su nurodytomis sàlygomis, pasirinkite pirmà pasirinkimà esantá þemiau. Jûs privalote sutikti, jei norite iðtrinti $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Ðalinama"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Praðome palaukti, kol $(^NameDA) bus paðalinta."

  !define MUI_UNTEXT_COMPONENTS_TITLE "Pasirinkite"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Pasirinkite kokias $(^NameDA) galimybes jûs norite paðalinti."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Pasirinkite iðtrinimo vietà"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Pasirinkite katalogà ið kurio iðtrinsite $(^NameDA)."
    
  !define MUI_UNTEXT_FINISH_TITLE "Programos paðalinimas baigtas"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Iðtrynimas baigtas sekmingai."
  
  !define MUI_UNTEXT_ABORT_TITLE "Iðtrynimas nutrauktas"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Iðtrynimas nebuvo baigtas sekmingai."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Baigiu $(^NameDA) paðalinimo programà."
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) buvo iðtrinta ið jûsø kompiuterio.\r\n\r\nPaspauskite Baigti."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Jûsø kompiuteris turi bûti perkrautas, kad bûtø baigtas $(^NameDA) paðalinimas. Ar jûs norite perkrauti dabar?"
  
  !define MUI_UNTEXT_ABORTWARNING "Ar jûs tikrai norite iðjungti $(^Name) paðalinimo programà?"
  
!insertmacro MUI_LANGUAGEFILE_END