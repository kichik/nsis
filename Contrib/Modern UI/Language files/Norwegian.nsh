;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Norwegian (2068)
;By Jonas Lindsrøm (jonasc_88@hotmail.com)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "NORWEGIAN"

  !define MUI_LANGNAME "Norwegian" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Velkommen til $(^NameDA) installasjons veiviser"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Denne veiviseren vil lede deg gjennom installasjonen av $(^NameDA).\r\n\r\nIt Det anbefales at du slår av alle andre programmer når du kjører denne installasjonsveiviseren. Dette vil la installasjons programmet  forandre på visse systemfiler uren at du må starte maskinen om på nytt.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Lisens Avtale"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Vennligst les gjennom lisens avtalen før du starter installasjonen $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Trykk Page Down knappen for å se resten av lisens avtalen."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Hvis du godtar lisensavtalen trykk Godta for å fortsette. Du må godta lisensavtalen for å intallere $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Hvis du godtar lisensavtalen, kryss av på merket under. Du må godta lisensavtalen for å intallere $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Hvis du godtar lisensavtalen, velg det første alternativet ovenfor. Du må godta lisensavtalen for å intallere $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Velg komponenter"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Velg vilke deler av $(^NameDA) du ønker å installere."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrivelse"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Beveg musen over komponentene for å se beskrivelsen."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Velg installasjons bane"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Velg hvilken mappe du vil installere $(^NameDA) i."
  
  !define MUI_TEXT_INSTALLING_TITLE "Installerer"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Vennligst vent mens $(^NameDA) installerer."
  
  !define MUI_TEXT_FINISH_TITLE "Installasjonen er ferdig"
  !define MUI_TEXT_FINISH_SUBTITLE "Installasjonen ble fullført uten feil."
  
  !define MUI_TEXT_ABORT_TITLE "Installasjonen er avbrutt"
  !define MUI_TEXT_ABORT_SUBTITLE "Installasjonen ble ikke fullført riktig."
  
  !define MUI_BUTTONTEXT_FINISH "&Ferdig"
  !define MUI_TEXT_FINISH_INFO_TITLE "Avslutter $(^NameDA) istallasjonsveiviser"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) er blitt installert på din datamskin.\r\n\r\nTrykk Lukk for å avslutte installasjons programmet."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Du må starte datamaskinen på nytt for at $(^NameDA) skal bli intallert riktig. Vil du starte datamaskinen på nytt?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Starte datamskinen på nytt nå"
  !define MUI_TEXT_FINISH_REBOOTLATER "Jeg vil starte datamaskinen på nytt senere"
  !define MUI_TEXT_FINISH_RUN "&Kjør $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Vis Readme filen"
  
  !define MUI_TEXT_STARTMENU_TITLE "Velg plassering i start menyen"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Velg mappe du vil at snarveiene til $(^NameDA) skal ligge."
  !define MUI_INNERTEXT_STARTMENU_TOP "Velg mappe du vil lage snarveiene til programmet. Du kan også skrive inn et annet navn for å lag  en mappe."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ikke lag snarveier"
  
  !define MUI_TEXT_ABORTWARNING "Er du sikker på at du vil avslutte installasjonen av $(^Name)?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Velkommen til avisntallerinsveiveser for $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Denne veiviseren vil lede deg gjennom avistallasjonen av $(^NameDA).\r\n\r\nFør du starter avinstallasjonen må du forsikre deg om at ikke $(^NameDA) kjører.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Avistaller $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Fjern $(^NameDA) fra din datamaskin."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Lisens Avtale"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Vennligst les gjennom Lisens Avtalen før du avintallerer $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Hvis du godtar lisensavtalen trykk Godta for å fortsette.  Du må godta lisensavtalen for å avintallere $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Hvis du godtar lisensavtalen, kryss av på merket under. Du må godta lisensavtalen for å avinstallere $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Hvis du godtar lisensavtalen, velg det første alternativet ovenfor. Du må godta lisensavtalen for å avistallere $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Velg komponenter"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "velg vilke deler av $(^NameDA) du ønker å avinstallere."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Velg avinstallasjons bane"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Velg mappe du vil avistallere $(^NameDA) fra."
    
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Avistallerer"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vennligst vent mens $(^NameDA) blir avistallert."

  !define MUI_UNTEXT_FINISH_TITLE "Avistallasjon ferdig"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Avistallasjoen ble utført uten feil."
  
  !define MUI_UNTEXT_ABORT_TITLE "Avistallasjon avbrutt"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Avistallajonen ble ikke utført riktig."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Fullfører avistallasjonen av $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) har blitt avistallert fra din datamaskin.\r\n\r\nTrykk på ferdig for å avslutte denne veiviseren."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Datamaskinen må Starte på nytt for å fullføre installasjonen av $(^NameDA). Vil du starte datamaskinen på nytt nå?"  

  !define MUI_UNTEXT_ABORTWARNING "Er du sikker på at du vil avbryte avistalleringen av $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END