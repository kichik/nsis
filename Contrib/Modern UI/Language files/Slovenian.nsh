;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Slovenian (1060)
;By Janez Dolinar, edited by Martin Srebotnjak - Lugos.si

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Slovenian"

  !define MUI_LANGNAME "Slovenski jezik" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Dobrodošli v èarovniku namestitve $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ta èarovnik vam bo pomagal pri namestitvi programa $(^NameDA).\r\n\r\nPriporoèamo, da pred namestitvijo zaprete vsa ostala okna in programe. S tem omogoèite nemoteno namestitev programa in njegovih delov brez ponovnega zagona raèunalnika.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licenèna pogodba"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Prosimo, da si ogledate pogoje licenène pogodbe pred namestitvijo $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Za preostali del pogodbe pritisnite tipko 'Page Down'."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Èe se strinjate s pogoji, pritisnite Se strinjam. Da bi lahko namestili $(^NameDA), se morate s pogodbo strinjati."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Èe se strinjate z licenènimi pogoji pogodbe, spodaj obkljukajte ustrezno okence. Za namestitev $(^NameDA) se morate strinjati s pogoji pogodbe. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Èe se strinjate z licenènimi pogoji pogodbe, spodaj izberite prvo možnost. Za namestitev $(^NameDA) se morate strinjati s pogoji pogodbe. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Izberite bloke"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Izberite, katere bloke izdelka $(^NameDA) želite namestiti."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Povlecite miško nad blok, da vidite njegov opis."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Izberite blok za prikaz njegovega opisa."
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Izberite pot namestive"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Izberite mapo, v katero želite namestiti $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Namešèanje poteka"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Prosimo, poèakajte, $(^NameDA) se namešèa."
  
  !define MUI_TEXT_FINISH_TITLE "Dokonèana namestitev"
  !define MUI_TEXT_FINISH_SUBTITLE "Namestitev se je uspešno konèala."
  
  !define MUI_TEXT_ABORT_TITLE "Prekinjena namestitev"
  !define MUI_TEXT_ABORT_SUBTITLE "Namestitev ni bila uspešno zakljuèena."
  
  !define MUI_BUTTONTEXT_FINISH "Do&konèaj"
  !define MUI_TEXT_FINISH_INFO_TITLE "Zakljuèevanje namestitve $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Paket $(^NameDA) je bil namešèen na vaš raèunalnik.\r\n\r\nPritisnite Dokonèaj za zaprtje èarovnika."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Za dokonèanje namestitve $(^NameDA) morate ponovno zagnati raèunalnik. Želite zdaj ponovno zagnati raèunalnik?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Ponovni zagon"
  !define MUI_TEXT_FINISH_REBOOTLATER "Raèunalnik želim ponovno zagnati kasneje"
  !define MUI_TEXT_FINISH_RUN "&Zaženi $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Prikaži BeriMe"
  
  !define MUI_TEXT_STARTMENU_TITLE "Izberite mapo menija Start"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Izberite mapo menija Start za bližnjice do $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Izberite mapo menija Start, kjer želite ustvariti bližnjico do programa. Èe vpišete novo ime, boste ustvarili istoimensko mapo."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ne ustvari bližnjic"
  
  !define MUI_TEXT_ABORTWARNING "Ste preprièani, da želite prekiniti namestitev $(^Name)?"  
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Dobrodošli v èarovniku za odstranitev $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ta èarovnik vas bo vodil skozi odstranitev $(^NameDA).\r\n\r\nPreden priènete z odstranitvijo, se preprièajte, da aplikacija $(^NameDA) ni zagnana.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Odstranitev $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Odstrani $(^NameDA) iz vašega raèunalnika."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licenèna pogodba"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Prosimo, da pred odstranitvijo $(^NameDA) pregledate pogoje licenène pogodbe."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Èe se strinjate s pogoji licenène pogodbe, kliknite na Se strinjam. Za odstranitev $(^NameDA) se morate strinjati s pogoji."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Èe se strinjate s pogoji licenène pogodbe, kliknite na okence spodaj. Za odstranitev $(^NameDA) se morate strinjati s pogoji. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Èe se strinjate s pogoji licenène pogodbe, spodaj izberite prvo podano možnost. Za odstranitev $(^NameDA) se morate strinjati s pogoji. $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Izberite bloke"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Izberite bloke $(^NameDA), ki jih želite odstraniti."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Izberite mapo"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Izberite mapo, iz katere želite odstraniti $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Odstranjevanje poteka"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prosimo, poèakajte, dokler se paket $(^NameDA) odstranjuje."
    
  !define MUI_UNTEXT_FINISH_TITLE "Odstranitev konèana"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Odstranitev je uspešno konèana."
  
  !define MUI_UNTEXT_ABORT_TITLE "Odstranitev prekinjena"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Odstranitev ni bila konèana uspešno."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Èarovnik za odstranitev $(^NameDA) se zakljuèuje"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Aplikacija $(^NameDA) je odstranjena iz vašega raèunalnika.\r\n\r\nKliknite Dokonèaj, da konèate delo s èarovnikom."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Da bi se $(^NameDA) namestitev dokonèala, morate ponovno zagnati raèunalnik. Želite zdaj ponovno zagnati raèunalnik?"
  
  !define MUI_UNTEXT_ABORTWARNING "Ste preprièani, da želite zapustiti odstranitev $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END