;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Slovak (1051)
;Translated by: Kypec (peter.dzugas@mahe.sk), edited by: Marián Hikaník (podnety@mojepreklady.net)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Slovak"

  !define MUI_LANGNAME "Slovensky" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Vitajte v sprievodcovi inštaláciou programu $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Tento sprievodca Vás prevedie inštaláciou $(^NameDA).\r\n\r\nPred zaèiatkom inštalácie sa odporúèa ukonèi všetky ostatné programy. Tım umoníte aktualizovanie systémovıch súborov bez potreby reštartovania Vášho poèítaèa.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licenèná zmluva"
  !define MUI_TEXT_LICENSE_SUBTITLE "Pred inštaláciou $(^NameDA) si prosím preštudujte licenèné podmienky."
  !define MUI_INNERTEXT_LICENSE_TOP "Stlaèením klávesy Page Down posuniete text licenènej zmluvy."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ak súhlasíte s podmienkami zmluvy, kliknite na tlaèidlo Súhlasím a môete pokraèova v inštalácií. Ak chcete v inštalácií pokraèova, musíte odsúhlasi podmienky licenènej zmluvy $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ak súhlasíte s podmienkami zmluvy, zaškrtnite nišie uvedené políèko. Ak chcete v inštalácií pokraèova, musíte odsúhlasi podmienky licenènej zmluvy $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ak súhlasíte s podmienkami zmluvy, oznaète prvú z nišie uvedenıch moností. Ak chcete v inštalácií pokraèova, musíte odsúhlasi podmienky licenènej zmluvy $(^NameDA)."
  
  !define MUI_TEXT_COMPONENTS_TITLE "Vo¾ba súèastí programu"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Zvo¾te si tie súèasti programu $(^NameDA), ktoré chcete nainštalova."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Popis"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Pri prejdení kurzorom myši nad názvom súèasti sa zobrazí jej popis."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Oznaète nejakú súèas, ak chcete zobrazi jej podrobnejší popis."
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Vo¾ba umiestnenia programu"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Vyberte si prieèinok, do ktorého chcete nainštalova program $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Inštalácia"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Poèkajte prosím, kım prebehne inštalácia programu $(^NameDA)."
  
  !define MUI_TEXT_FINISH_TITLE "Ukonèenie inštalácie"
  !define MUI_TEXT_FINISH_SUBTITLE "Inštalácia bola dokonèená úspešne."

  !define MUI_TEXT_ABORT_TITLE "Prerušenie inštalácie"
  !define MUI_TEXT_ABORT_SUBTITLE "Inštaláciu sa nepodarilo dokonèi."

  !define MUI_BUTTONTEXT_FINISH "&Dokonèi"
  !define MUI_TEXT_FINISH_INFO_TITLE "Dokonèenie inštalácie programu $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Program $(^NameDA) bol nainštalovanı do Vášho poèítaèa.\r\nKliknite na tlaèidlo Dokonèi a tento sprievodca sa ukonèí."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Pre úplné dokonèenie inštalácie programu $(^NameDA) je potrebné reštartova Váš poèítaè. Chcete ho reštartova ihneï?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reštartova teraz"
  !define MUI_TEXT_FINISH_REBOOTLATER "Reštartova neskôr (manuálne)"
  !define MUI_TEXT_FINISH_RUN "&Spusti program $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Zobrazi súbor s informáciami"
  
  !define MUI_TEXT_STARTMENU_TITLE "Vo¾ba umiestnenia v ponuke Štart"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Vyberte si zloku v ponuke Štart, kam sa umiestnia odkazy na program $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Vyberte si zloku v ponuke Štart, v ktorej chcete vytvori odkazy na program. Takisto môte napísa názov pre vytvorenie novej zloky."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Nevytvára odkazy"
  
  !define MUI_TEXT_ABORTWARNING "Naozaj chcete ukonèi inštaláciu programu $(^Name)?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Vitajte v sprievodcovi odinštalovaním programu $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Tento sprievodca Vás prevedie procesom odinštalovania programu $(^NameDA).\r\n\r\nPred spustením procesu odinštalovania sa uistite, e program $(^NameDA) nie je práve aktívny.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Odinštalovanie programu $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Odstránenie programu $(^NameDA) z Vášho poèítaèa."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licenèná zmluva"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Pred odinštalovaním programu $(^NameDA) si prosím preèítajte licenèné podmienky."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ak súhlasíte s podmienkami zmluvy, zvo¾te Súhlasím. Licenènú zmluvu musíte odsúhlasi, ak chcete v odinštalovávaní programu $(^NameDA) pokraèova."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ak súhlasíte s podmienkami zmluvy, zaškrtnite nišie uvedené políèko. Licenènú zmluvu musíte odsúhlasi, ak chcete pokraèova v odinštalovávaní programu $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ak súhlasíte s podmienkami licenènej zmluvy, oznaète prvú z nišie uvedenıch moností. Licenènú zmluvu musíte odsúhlasi, ak chcete pokraèova v odinštalovávaní programu $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Vo¾ba súèastí"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Zvo¾te súèasti programu $(^NameDA), ktoré chcete odinštalova."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Umiestenie programu pre odinštalovanie"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Vyberte si prieèinok, z ktorého chcete odinštalova program $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Odinštalovanie"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Èakajte prosím, kım prebehne odinštalovanie programu $(^NameDA)."
    
  !define MUI_UNTEXT_FINISH_TITLE "Ukonèenie odinštalovania"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Odinštalovanie bolo úspešne dokonèené."
  
  !define MUI_UNTEXT_ABORT_TITLE "Prerušenie odinštalovania"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Odinštalovanie sa neukonèilo úspešne."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Dokonèenie sprievodcu odinštalovaním"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Program $(^NameDA) bol odinštalovanı z Vášho poèítaèa.\r\n\r\nKliknite na tlaèidlo Dokonèi a tento sprievodca sa ukonèí."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Pre úplné dokonèenie odinštalovania programu $(^NameDA) je nutné reštartova Váš poèítaè. Chcete ho reštartova ihneï?"
  
  !define MUI_UNTEXT_ABORTWARNING "Naozaj chcete ukonèi proces odinštalovania programu $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END