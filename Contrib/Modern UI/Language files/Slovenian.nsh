;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Slovenian (0424)
;By Janez Dolinar

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SLOVENIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Slovenšèina" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Pritisnite Naprej za nadaljevanje."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Pritisnite Zaèni za zaèetek namestitve."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Dobrodošli v ${MUI_PRODUCT} èarovniku"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Ta program bo na vaš raèunalnik namestil ${MUI_PRODUCT}.\r\n\r\nPriporoèamo vam, da zaprete vsa ostala okna in programe pred namestitvijo. To bo omogoèalo nemoteno namestitev programa in njegovih delov brez ponovnega zagona raèunalnika.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licenèna pogodba"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Prosimo, preglejte pogoje pogodbe pred namestitvijo ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Prisnite tipko 'Page Down', za preostali del pogodbe."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Èe se strinjate s pogoji, pritisnite Se strinjam. S pogodbo se morate strinjati, da bi lahko namestili ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Èe se strinjate z pogoji licenènimi pogoji pogodbe, spodaj obkljukajte primerno okence. Za namestitev ${MUI_PRODUCT} se morate strinjati s pogoji pogodbe."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Èe se strinjate z pogoji licenènimi pogoji pogodbe, spodaj izberite prvo možnost. Za namestitev ${MUI_PRODUCT} se morate strinjati s pogoji pogodbe."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Izberite bloke"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Izberite si, katere bloke izdelka ${MUI_PRODUCT} želite namestiti."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Obkljukajte bloke, ki jih želite in odkljukajte tiste, ki jih ne želite."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Opis"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Povlecite miško nad blok, da vidite njegov opis."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Izberite si pot namestive"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Izberite si mapo, v katero boste namestili ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Program bo namestil ${MUI_PRODUCT} v sledeèo mapo.$\r$\n$\r$\nDa bi mapo spremenili, pritisnite na tipko Browse (Razišèi) in izberite drugo mapo."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Ciljna mapa"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Namešèanje poteka"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Prosimo poèakajte, ${MUI_PRODUCT} se namešèa."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Namestitev dokonèana"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Namestitev je bila konèana uspešno."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Namestitev je bila prekinjena"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Namestitev ni bila konèana uspešno."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Konèaj"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Zakljuèujem namestitev ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "Paket ${MUI_PRODUCT} je bil namešèen na vaš raèunalnik..\r\n\r\nPritisnite na Konèaj za zakljuèitev programa."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Vaš raèunalnik mora biti ponovno zagnan, da bi se ${MUI_PRODUCT} namestitev lahko dokonèala. Želite raèunalnik ponovno zagnati sedaj?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Ponovni zagon"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Raèunalnik bom ponovno zagnal kasneje"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Zaženi ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Prikaži informacije"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Izberite mapo Start menija"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Izberite mapo Start menija, kjer bodo bližnjice do programa."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Izberite mapo Start menija, kjer bi želeli ustvariti bližnjico do programa. Èe vpišete poljubno ime, se bo ustvarila mapa s tem imenom."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Ne naredi bližnjice"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Ste preprièani, da želite prekiniti namestitev ${MUI_PRODUCT}?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Pritisnite Odstrani za zaèetek odstranitve programa."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Odstrani ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Odstrani ${MUI_PRODUCT} iz vašega raèunalnika."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Ta program bo odstranil paket ${MUI_PRODUCT} iz vašega raèunalnika."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Odstranjevanje poteka"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prosimo poèakajte, dokler se paket ${MUI_PRODUCT} odstranjuje."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Uninstallation Complete"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Uninstall was completed successfully."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Odstranitev je bila prekinjena"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Odstranitev ni bila konèana uspešno."

!insertmacro MUI_LANGUAGEFILE_END
