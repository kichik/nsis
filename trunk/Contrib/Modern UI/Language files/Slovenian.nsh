;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Slovenian (1060)
;By Janez Dolinar

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SLOVENIAN"

  !define MUI_LANGNAME "Slovenscina" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Dobrodošli v $(^NameDA) èarovniku"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Ta èarovnik vam bo pomagal pri namestitvi $(^NameDA).\r\n\r\nPriporoèamo vam, da zaprete vsa ostala okna in programe pred namestitvijo. To bo omogoèilo nemoteno namestitev programa in njegovih delov brez ponovnega zagona raèunalnika.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licenèna pogodba"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Prosimo, preglejte pogoje pogodbe pred namestitvijo $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Prisnite tipko 'Page Down', za preostali del pogodbe."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Èe se strinjate s pogoji, pritisnite Se strinjam. S pogodbo se morate strinjati, da bi lahko namestili $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Èe se strinjate z pogoji licenènimi pogoji pogodbe, spodaj obkljukajte primerno okence. Za namestitev $(^NameDA) se morate strinjati s pogoji pogodbe. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Èe se strinjate z pogoji licenènimi pogoji pogodbe, spodaj izberite prvo možnost. Za namestitev $(^NameDA) se morate strinjati s pogoji pogodbe. $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Izberite bloke"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Izberite si, katere bloke izdelka $(^NameDA) želite namestiti."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Povlecite miško nad blok, da vidite njegov opis."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Izberite si pot namestive"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Izberite si mapo, v katero boste namestili $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Namešèanje poteka"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Prosimo poèakajte, $(^NameDA) se namešèa."
  
  !define MUI_TEXT_FINISH_TITLE "Namestitev dokonèana"
  !define MUI_TEXT_FINISH_SUBTITLE "Namestitev je bila konèana uspešno."
  
  !define MUI_TEXT_ABORT_TITLE "Namestitev je bila prekinjena"
  !define MUI_TEXT_ABORT_SUBTITLE "Namestitev ni bila konèana uspešno."
  
  !define MUI_BUTTONTEXT_FINISH "&Konèaj"
  !define MUI_TEXT_FINISH_INFO_TITLE "Zakljuèujem namestitev $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Paket $(^NameDA) je bil namešèen na vaš raèunalnik..\r\n\r\nPritisnite na Konèaj za zakljuèitev programa."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Vaš raèunalnik mora biti ponovno zagnan, da bi se $(^NameDA) namestitev lahko dokonèala. Želite raèunalnik ponovno zagnati sedaj?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Ponovni zagon"
  !define MUI_TEXT_FINISH_REBOOTLATER "Raèunalnik bom ponovno zagnal kasneje"
  !define MUI_TEXT_FINISH_RUN "Zaženi $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Prikaži informacije"
  
  !define MUI_TEXT_STARTMENU_TITLE "Izberite mapo Start menija"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Izberite mapo Start menija, kjer bodo bližnjice do programa."
  !define MUI_INNERTEXT_STARTMENU_TOP "Izberite mapo Start menija, kjer bi želeli ustvariti bližnjico do programa. Èe vpišete poljubno ime, se bo ustvarila mapa s tem imenom."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ne naredi bližnjice"
  
  !define MUI_TEXT_ABORTWARNING "Ste preprièani, da želite prekiniti namestitev $(^Name)?"  
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Dobrodošli v èarovniku za odstranitev $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Ta èarovnik vas bo vodil skozi odstranitev $(^NameDA).\r\n\r\nPreden priènete z odstranitvijo, se preprièajte, da $(^NameDA) ni zagnan.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Odstrani $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Odstrani $(^NameDA) iz vašega raèunalnika."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Licenèna pogodba"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Prosimo vas, da pregledate pogoje licenène pogodbe pred odstranitvijo $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Èe se strinjate z pogoji licenène pogodbe, kliknite na Se strinjam. Za odstranitev $(^NameDA) se morate strinjati s pogoji."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Èe se strinjate z pogoji licenène pogodbe, kliknite na okence spodaj. Za odstranitev $(^NameDA) se morate strinjati s pogoji. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Èe se strinjate z pogoji licenène pogodbe, izberite spodaj prvo podano možnost. Za odstranitev $(^NameDA) se morate strinjati s pogoji. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Izberite bloke"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Izberite si bloke $(^NameDA), ki jih želite odstraniti."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Izberite mapo"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Izberite mapo, iz katere želite odstraniti $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Odstranjevanje poteka"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prosimo poèakajte, dokler se paket $(^NameDA) odstranjuje."
    
  !define MUI_UNTEXT_FINISH_TITLE "Uninstallation Complete"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Uninstall was completed successfully."

  !define MUI_UNTEXT_ABORT_TITLE "Odstranitev je bila prekinjena"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Odstranitev ni bila konèana uspešno."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Èarovnik za odstranitev $(^NameDA) se zakljuèuje"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) je bil odstranjen iz vašega raèunalnika.\r\n\r\nKliknite na Dokonèaj, da konèate z èarovnikom."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Vaš raèunalnik se mora ponovno zagnati, da bi se lahko $(^NameDA) namestitev dokonèala. Želite sedaj ponovno zagnati vaš raèunalnik?"  
  
  !define MUI_UNTEXT_ABORTWARNING "Ste preprièani, da želite zapustiti odstranitev $(^Name)?"  

!insertmacro MUI_LANGUAGEFILE_END