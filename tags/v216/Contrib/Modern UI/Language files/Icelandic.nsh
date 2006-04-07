;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Icelandic (1039)
;By Gretar Orri Kristinsson

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Icelandic"

  !define MUI_LANGNAME "Icelandic" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Velkominn til $(^NameDA) uppsetningarhjálparinnar"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Þessi hjálp mun leiða þig í gegnum uppsetninguna á $(^NameDA).\r\n\r\nRáðlagt er að loka öllum öðrum forritum áður en uppsetning hefst. Þetta mun gera uppfærslu ýmissa stýriskráa mögulega án þess að þurfa að endurræsa tölvuna.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Leyfissamningur"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Vinsamlegast skoðaðu vandlega leyfissamninginn áður en uppsetning á $(^NameDA) hefst."
  !define MUI_INNERTEXT_LICENSE_TOP "Smelltu á 'Síða Upp' takkann á lyklaborðinu til að sjá afganginn af samningnum."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Ef þú samþykkir skilmála samningsins, smelltu á 'Ég samþykki' til að halda áfram. Þú verður að samþykkja samninginn til þess að setja upp $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ef þú samþykkir skilmála samningsins, hakaðu þá í kassann hér að neðan. Þú verður að samþykkja samninginn til þess að setja upp $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ef þú samþykkir skilmála samningsins, veldu þá fyrsta valmöguleikann hér að neðan. Þú verður að samþykkja samninginn til þess að setja upp $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Velja hluti"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Velja hvað af eiginleikum $(^NameDA) þú vilt setja upp."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Lýsing"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Settu bendilinn yfir hlut til þess að sjá lýsingu þess."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Veldu hlut til þess að sjá lýsingu þess."
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Veldu uppsetningarskáarsafn"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Veldu það skráarsafn sem þú vilt setja $(^NameDA) upp í."
  
  !define MUI_TEXT_INSTALLING_TITLE "Set upp"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Vinsamlegast dokaðu við meðan $(^NameDA) er sett upp."
  
  !define MUI_TEXT_FINISH_TITLE "Uppsetningu lokið"
  !define MUI_TEXT_FINISH_SUBTITLE "Uppsetning tókst fullkomlega."
  
  !define MUI_TEXT_ABORT_TITLE "Hætt við uppsetningu"
  !define MUI_TEXT_ABORT_SUBTITLE "Uppsetningu lauk ekki fullkomlega."
  
  !define MUI_BUTTONTEXT_FINISH "&Ljúka"
  !define MUI_TEXT_FINISH_INFO_TITLE "Ljúka $(^NameDA) uppsetningarhjálpinni"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) er nú upp sett á tölvunni þinni.\r\n\r\nSmelltu á 'Ljúka' til að loka þessari hjálp."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Endurræsa verður tölvuna til að ljúka alveg við uppsetningunni á $(^NameDA). Viltu endurræsa núna?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Endurræsa núna"
  !define MUI_TEXT_FINISH_REBOOTLATER "Ég vil endurræsa seinna"
  !define MUI_TEXT_FINISH_RUN "&Keyra $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Skoða LestuMig"
  
  !define MUI_TEXT_STARTMENU_TITLE "Velja Startvalmyndarskráarsafn"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Velja Startvalmyndarskráarsafn fyrir $(^NameDA) flýtileiðirnar."
  !define MUI_INNERTEXT_STARTMENU_TOP "Veldu það Startvalmyndarskráarsafn sem þú vilt setja flýtileiðirnar fyrir forritið í. Þú getur einnig búið til nýtt skráarsafn með því að skrifa inn nýtt nafn."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ekki búa til flýtileiðir"
  
  !define MUI_TEXT_ABORTWARNING "Ertu viss um að þú viljir slökkva á $(^Name) uppsetningunni?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Velkominn til $(^NameDA) fjarlægingarhjálparinnar"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Þessi hjálp mun leiða þig í gegnum fjarlæginguna á $(^NameDA).\r\n\r\nÁður en fjarlæging hefst skal ganga úr skugga um að $(^NameDA) sé ekki opið.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Fjarlægja $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Fjarlægja $(^NameDA) úr tölvunni."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Leyfissamningur"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Vinsamlegast skoðaðu vandlega leyfissamninginn áður en fjarlæging á $(^NameDA) hefst."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Ef þú samþykkir skilmála samningsins, smelltu á 'Ég samþykki' til að halda áfram. Þú verður að samþykkja samninginn til þess að fjarlægja $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Ef þú samþykkir skilmála samningsins, hakaðu þá í kassann hér að neðan. Þú verður að samþykkja samninginn til þess að fjarlægja $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Ef þú samþykkir skilmála samningsins, veldu þá fyrsta valmöguleikann hér að neðan. Þú verður að samþykkja samninginn til þess að fjarlægja $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Velja hluti"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Velja hvað af eiginleikum $(^NameDA) þú vilt fjarlægja."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Veldu fjarlægingarskáarsafn"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Veldu það skráarsafn sem þú vilt fjarlægja $(^NameDA) úr."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Fjarlægi"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vinsamlegast dokaðu við meðan $(^NameDA) er fjarlægt."
    
  !define MUI_UNTEXT_FINISH_TITLE "Fjarlægingu lokið"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Fjarlæging tókst fullkomlega."
  
  !define MUI_UNTEXT_ABORT_TITLE "Hætt við fjarlægingu"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Fjarlægingu lauk ekki fullkomlega."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Ljúka $(^NameDA) fjarlægingarhjálpinni"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) hefur nú verið fjarlægt úr tölvunni.\r\n\r\nSmelltu á 'Ljúka' til að loka þessari hjálp."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Endurræsa verður tölvuna til að ljúka alveg við fjarlæginguna á $(^NameDA). Viltu endurræsa núna?"
  
  !define MUI_UNTEXT_ABORTWARNING "Ertu viss um að þú viljir slökkva á $(^Name) fjarlægingunni?"
  
!insertmacro MUI_LANGUAGEFILE_END