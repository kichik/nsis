;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Slovak (1051)
;By Kypec (peter.dzugas@mahe.sk)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SLOVAK"

  !define MUI_LANGNAME "Slovensky" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Vitajte v sprievodcovi inštalácie $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Tento sprievodca Vás prevedie inštaláciou $(^Name).\r\n\r\nPred zaèiatkom inštalácie je odporúèané zavrie všetky ostatné aplikácie. Tımto umoníte aktualizovanie prípadnıch systémovıch súborov bez potreby reštartova Váš poèítaè.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licenèná zmluva"
  !define MUI_TEXT_LICENSE_SUBTITLE "Pred inštaláciou $(^Name) si prosím preštudujte licenèné podmienky."
  !define MUI_INNERTEXT_LICENSE_TOP "Stlaèením klávesy Page Down posuniete text licenènej zmluvy."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ak súhlasíte s podmienkami zmluvy, zvo¾te Súhlasím pre pokraèovanie. Je nutné súhlasi s licenènou zmluvou, ak chcete pokraèova v inštalácii $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ak súhlasíte s podmienkami zmluvy, zaškrtnite nišie uvedené políèko. Je nutné súhlasi s licenènou zmluvou, ak chcete pokraèova v inštalácii $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ak súhlasíte s podmienkami zmluvy, oznaète prvú z nišie uvedenıch moností. Je nutné súhlasi s licenènou zmluvou, ak chcete pokraèova v inštalácii $(^Name)."
  
  !define MUI_TEXT_COMPONENTS_TITLE "Vo¾ba komponentov"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Zvo¾te komponenty $(^Name), ktoré chcete nainštalova."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Popis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pri pohybe myšou nad komponentom sa tu zobrazí jeho popis."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Vo¾ba umiestnenia inštalácie"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Zvo¾te adresár, do ktorého chcete nainštalova $(^Name)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Inštalácia"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Èakajte prosím kım prebieha inštalácia $(^Name)."
  
  !define MUI_TEXT_FINISH_TITLE "Dokonèenie inštalácie"
  !define MUI_TEXT_FINISH_SUBTITLE "Inštalácia bola úspešne dokonèená."

  !define MUI_TEXT_ABORT_TITLE "Inštalácia bola prerušená"
  !define MUI_TEXT_ABORT_SUBTITLE "Inštalácia nebola úspešne dokonèená."

  !define MUI_BUTTONTEXT_FINISH "&Dokonèi"
  !define MUI_TEXT_FINISH_INFO_TITLE "Dokonèenie sprievodcu inštalácie $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) bol nainštalovanı na Váš poèítaè.\r\nKliknite na Dokonèi pre uzavretie tohto sprievodcu."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Pre úplné dokonèenie inštalácie $(^Name) je nutné vykona reštart Vášho poèítaèa. Chcete reštartova ihneï?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reštartova ihneï"
  !define MUI_TEXT_FINISH_REBOOTLATER "Chcem reštartova ruène neskôr"
  !define MUI_TEXT_FINISH_RUN "&Spusti $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Uká Èítaj-ma"
  
  !define MUI_TEXT_STARTMENU_TITLE "Vo¾ba umiestnenia v ponuke Štart"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Zvo¾te poloku v ponuke Štart pre umiestnenie zástupcov $(^Name)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Vyberte poloku v ponuke Štart, v ktorej chcete vytvori zástupcov programu. Takisto môte napísa názov pre vytvorenie novej poloky."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nevytvára zástupcov"
  
  !define MUI_TEXT_ABORTWARNING "Naozaj chcete ukonèi inštaláciu $(^Name)?"  
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Vitajte v sprievodcovi odinštalácie $(^Name)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Tento sprievodca Vás prevedie odinštaláciou $(^Name).\r\n\r\nPred zaèiatkom odinštalácie sa uistite, e $(^Name) nie je práve teraz spustenı.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Odinštalovanie $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Odstránenie $(^Name) z Vášho poèítaèa."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licenèná zmluva"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Pred odinštalovaním programu $(^Name) si prosím preštudujte licenèné podmienky."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ak súhlasíte s podmienkami zmluvy, zvo¾te Súhlasím pre pokraèovanie. Je nutné súhlasi s licenènou zmluvou, ak chcete pokraèova v odinštalovaní $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ak súhlasíte s podmienkami zmluvy, zaškrtnite nišie uvedené políèko. Je nutné súhlasi s licenènou zmluvou, ak chcete pokraèova v odinštalovaní $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ak súhlasíte s podmienkami zmluvy, oznaète prvú z nišie uvedenıch moností. Je nutné súhlasi s licenènou zmluvou, ak chcete pokraèova v odinštalovaní $(^Name). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Vo¾ba komponentov"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Zvo¾te komponenty $(^Name), ktoré chcete odinštalova."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Vo¾ba umiestnenia odinštalácie"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Zvo¾te adresár, z ktorého chcete odinštalova $(^Name)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Odinštalovanie"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Èakajte prosím kım prebieha odinštalovanie $(^Name)."
    
  !define MUI_UNTEXT_FINISH_TITLE "Dokonèenie odinštalácie"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Odinštalovanie bolo úspešne dokonèené."
  
  !define MUI_UNTEXT_ABORT_TITLE "Odinštalácia bola prerušená"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Odinštalovanie nebolo úspešne dokonèené."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Dokonèenie sprievodcu odinštalácie $(^Name)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name) bol odinštalovanı z Vášho poèítaèa.\r\n\r\nKliknite na Dokonèi pre uzavretie tohto sprievodcu."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Pre úplné dokonèenie odinštalácie $(^Name) je nutné vykona reštart Vášho poèítaèa. Chcete reštartova ihneï?"
  
  !define MUI_UNTEXT_ABORTWARNING "Naozaj chcete ukonèi odinštaláciu $(^Name)?"  
  
!insertmacro MUI_LANGUAGEFILE_END