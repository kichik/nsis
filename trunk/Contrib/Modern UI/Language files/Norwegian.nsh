;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Norwegian (2068)
;By Jonas Lindsrøm (jonasc_88@hotmail.com)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "NORWEGIAN"

  !define MUI_LANGNAME "Norwegian" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Velkommen til $(^Name) installasjons veiviser"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Denne veiviseren vil lede deg gjennom installasjonen av $(^Name).\r\n\r\nIt Det anbefales at du slår av alle andre programmer når du kjører denne installasjonsveiviseren. Dette vil la installasjons programmet  forandre på visse systemfiler uren at du må starte maskinen om på nytt.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Lisens Avtale"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Vennligst les gjennom lisens avtalen før du starter installasjonen $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Trykk Page Down knappen for å se resten av lisens avtalen."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Hvis du godtar lisensavtalen trykk Godta for å fortsette. Du må godta lisensavtalen for å intallere $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Hvis du godtar lisensavtalen, kryss av på merket under. Du må godta lisensavtalen for å intallere $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Hvis du godtar lisensavtalen, velg det første alternativet ovenfor. Du må godta lisensavtalen for å intallere $(^Name). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Velg komponenter"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "velg vilke deler av $(^Name) du ønker å installere."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrivelse"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "hold musen over komponentene for å se beskrivelsen."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Velg installasjons bane"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Velg hvilken mappe du vil installere $(^Name) i."
  
  !define MUI_TEXT_INSTALLING_TITLE "Installerer"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Vennligst vent mens $(^Name) installerer."
  
  !define MUI_TEXT_FINISH_TITLE "Installasjonen er ferdig"
  !define MUI_TEXT_FINISH_SUBTITLE "Installasjonen ble fullført uten feil."
  
  !define MUI_TEXT_ABORT_TITLE "Installasjonen er avbrutt"
  !define MUI_TEXT_ABORT_SUBTITLE "Installasjonen ble ikke fullført riktig."
  
  !define MUI_BUTTONTEXT_FINISH "&Ferdig"
  !define MUI_TEXT_FINISH_INFO_TITLE "Avslutter $(^Name) istallasjonsveiviser"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) er blitt installert på din datamskin.\r\n\r\nTrykk Lukk for å avslutte installasjons programmet."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Din datamskin må omstartes for at $(^Name) skal bli intallert riktig. Vil du starte om datamaskinen nå?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Start om nå"
  !define MUI_TEXT_FINISH_REBOOTLATER "Jeg vil starte om senere"
  !define MUI_TEXT_FINISH_RUN "&Kjør $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Vis Readme filen"
  
  !define MUI_TEXT_STARTMENU_TITLE "Velg plassering i start menyen"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Velg mappe du vil at snarveiene til $(^Name) skal ligge."
  !define MUI_INNERTEXT_STARTMENU_TOP "Velg mappe du vil lage snarveiene til programmet. Du kan også skrive inn et annet navn for å lag  en mappe."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ikke lag snarveier"
  
  !define MUI_TEXT_ABORTWARNING "Er du sikker på at du vil avslutte installasjonen av $(^Name)?"  
  
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Avinstaller $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Fjern $(^Name) fra din datamaskin."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Lisens Avtale"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Vennligst les gjennom Lisens Avtalen før du avintallerer $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Hvis du godtar lisensavtalen trykk Godta for å fortsette.  Du må godta lisensavtalen for å avintallere $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Hvis du godtar lisensavtalen, kryss av på merket under. Du må godta lisensavtalen for å avinstallere $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Hvis du godtar lisensavtalen, velg det første alternativet ovenfor. Du må godta lisensavtalen for å avistallere $(^Name). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Velg komponenter"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "velg vilke deler av $(^Name) du ønker å avinstallere."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Velg avinstallasjons bane"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Velg mappe du vil avistallere $(^Name) fra."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Avistallerer"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vennligst vent mens $(^Name) blir avistallert."
    
  !define MUI_UNTEXT_FINISH_TITLE "Avistallasjon ferdig"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Avistallasjoen ble utført uten feil."
  
  !define MUI_UNTEXT_ABORT_TITLE "Avistallasjon avbrutt"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Avistallajonen ble ikke utført riktig."
  
!insertmacro MUI_LANGUAGEFILE_END