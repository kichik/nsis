;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Norwegian (2068)
;By Jonas Lindsrøm (jonasc_88@hotmail.com) Reviewed and fixed by Jan Ivar Beddari, d0der at online.no

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Norwegian"

  !define MUI_LANGNAME "Norwegian" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Velkommen til veiviseren for installasjon av $(^NameDA) "
  !define MUI_TEXT_WELCOME_INFO_TEXT "Denne veiviseren vil lede deg gjennom installasjonen av $(^NameDA).\r\n\r\nDet anbefales at du avslutter alle andre programmer før du fortsetter. Dette vil la installasjonsprogrammet forandre på systemfiler uten at du må starte datamaskinen på nytt.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Lisensavtale"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Vennligst les gjennom lisensavtalen før du starter installasjonen av $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Trykk Page Down knappen for å se resten av lisensavtalen."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Hvis du godtar lisensavtalen trykk Godta for å fortsette. Du må godta lisensavtalen for å installere $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Hvis du godtar lisensavtalen, kryss av på merket under. Du må godta lisensavtalen for å installere $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Hvis du godtar lisensavtalen, velg det første alternativet ovenfor. Du må godta lisensavtalen for å installere $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Velg komponenter"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Velg hvilke deler av $(^NameDA) du ønsker å installere."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrivelse"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Beveg musen over komponentene for å se beskrivelsen."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Velg installasjonsmappe"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Velg hvilken mappe du vil installere $(^NameDA) i."
  
  !define MUI_TEXT_INSTALLING_TITLE "Installasjonen pågår"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Vennligst vent mens $(^NameDA) blir installert."
  
  !define MUI_TEXT_FINISH_TITLE "Installasjonen er ferdig"
  !define MUI_TEXT_FINISH_SUBTITLE "Installasjonen ble fullført uten feil."
  
  !define MUI_TEXT_ABORT_TITLE "Installasjonen er avbrutt"
  !define MUI_TEXT_ABORT_SUBTITLE "Installasjonen ble ikke fullført riktig."
  
  !define MUI_BUTTONTEXT_FINISH "&Ferdig"
  !define MUI_TEXT_FINISH_INFO_TITLE "Avslutter $(^NameDA) installasjonsveiviser"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) er klart til bruk på din datamskin.\r\n\r\nTrykk Ferdig for å avslutte installasjonsprogrammet."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Du må starte datamaskinen på nytt for å fullføre installasjonen av $(^NameDA). Vil du starte datamaskinen på nytt nå?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Ja. Start datamaskinen på nytt nå"
  !define MUI_TEXT_FINISH_REBOOTLATER "Nei. Jeg vil starte datamaskinen på nytt senere"
  !define MUI_TEXT_FINISH_RUN "&Kjør $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Vis Readme filen"
  
  !define MUI_TEXT_STARTMENU_TITLE "Velg plassering på startmenyen"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Velg hvilken mappe snarveiene til $(^NameDA) skal ligge i."
  !define MUI_INNERTEXT_STARTMENU_TOP "Velg mappe for snarveiene til programmet. Du kan også skrive inn et nytt navn for å lage en ny mappe."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ikke lag snarveier"
  
  !define MUI_TEXT_ABORTWARNING "Er du sikker på at du vil avslutte installasjonen av $(^Name)?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Velkommen til veiviseren for avinstallasjon av $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Denne veiviseren vil lede deg gjennom avinstallasjonen av $(^NameDA).\r\n\r\nFør du fortsetter må du forsikre deg om at $(^NameDA) ikke kjører.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Avinstaller $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Fjern $(^NameDA) fra din datamaskin."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Lisensavtale"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Vennligst les gjennom lisensavtalen før du avinstallerer $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Hvis du godtar lisensavtalen trykk Godta for å fortsette.  Du må godta lisensavtalen for å avintallere $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Hvis du godtar lisensavtalen, kryss av på merket under. Du må godta lisensavtalen for å avinstallere $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Hvis du godtar lisensavtalen, velg det første alternativet ovenfor. Du må godta lisensavtalen for å avinstallere $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Velg komponenter"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Velg hvilke deler av $(^NameDA) du ønsker å avinstallere."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Velg mappe for avinstallasjon"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Velg mappen du vil avinstallere $(^NameDA) fra."
    
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Avinstallasjon pågår"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vennligst vent mens $(^NameDA) blir avinstallert."

  !define MUI_UNTEXT_FINISH_TITLE "Avinstallasjon ferdig"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Avinstallasjonen ble utført uten feil."
  
  !define MUI_UNTEXT_ABORT_TITLE "Avinstallasjon avbrutt"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Avinstallasjonen ble ikke utført riktig."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Fullfører avinstallasjonen av $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) har blitt avinstallert fra din datamaskin.\r\n\r\nTrykk på ferdig for å avslutte denne veiviseren."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Datamaskinen må starte på nytt for å fullføre avinstallasjonen av $(^NameDA). Vil du starte datamaskinen på nytt nå?"  

  !define MUI_UNTEXT_ABORTWARNING "Er du sikker på at du vil avbryte avinstallasjonen av $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END