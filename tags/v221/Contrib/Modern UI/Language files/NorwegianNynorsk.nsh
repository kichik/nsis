;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Norwegian nynorsk (2068)
;By Vebjoern Sture and Håvard Mork (www.firefox.no)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "NorwegianNynorsk"

  !define MUI_LANGNAME "Norwegian nynorsk" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Velkommen til $(^NameDA) innstallasjonsvegvisar"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Denne vegvisaren vil leie deg gjennom installeringa av $(^NameDA).\n\nDet er tilrådd at du avsluttar alle andre program før du held fram. Dette vil la installeringsprogrammet oppdatera systemfiler utan at du må starta datamaskinen på nytt.\n\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Lisensavtale"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Les gjennom lisensavtalen før du startar installeringa av $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Trykk Page Down-knappen for å sjå resten av lisensavtala."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Trykk på «Godta» dersom du godtar betingelsane i avtala. Du må godta avtala for å installere $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Trykk på avkryssingsboksen nedanfor nedanfor dersom du godtar betingelsane i avtala. Du må godta avtala for å installere $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Vel det første alternativet nedanfor dersom du godtek vilkåra i avtala. Du må godta avtala for å installera $(^NameDA). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Vel komponentar"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Vel kva delar av $(^NameDA) du ynskjer å installera."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskriving"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Beveg musa over komponentene for å sjå beskrivinga."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Vel eit komponent for å se beskrivinga."
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Vel installasjonsmappe"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Vel kva mappe du vil installera $(^NameDA) i."
  
  !define MUI_TEXT_INSTALLING_TITLE "Installerer"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Vent mens $(^NameDA) blir installert."
  
  !define MUI_TEXT_FINISH_TITLE "Installeringa er fullført"
  !define MUI_TEXT_FINISH_SUBTITLE "Installeringa vart fullført."
  
  !define MUI_TEXT_ABORT_TITLE "Installeringa vart avbroten"
  !define MUI_TEXT_ABORT_SUBTITLE "Installeringa vart ikkje fullført."
  
  !define MUI_BUTTONTEXT_FINISH "&Fullfør"
  !define MUI_TEXT_FINISH_INFO_TITLE "Installering fullført"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) er installert og klar til bruk.\n\nTrykk på «Fullfør» for å avslutte installeringa."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Du må starta datamaskinen på nytt for å fullføra installeringa av $(^NameDA). Vil du starta på nytt no?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Start på nytt no"
  !define MUI_TEXT_FINISH_REBOOTLATER "Eg vil starta på nytt seinare"
  !define MUI_TEXT_FINISH_RUN "&Køyr $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Syn lesmeg"
  
  !define MUI_TEXT_STARTMENU_TITLE "Vel mappe på startmenyen"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Vel kva mappe snarvegane til $(^NameDA) skal liggja i."
  !define MUI_INNERTEXT_STARTMENU_TOP "Vel mappa du vil oppretta snarvegane til programmet i. Du kan òg skriva inn eit nytt namn for å laga ei ny mappe."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ikkje opprett snarvegar"
  
  !define MUI_TEXT_ABORTWARNING "Er du viss på at du vil avslutta installeringa av $(^Name)?"
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Velkommen til avinstallering av $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Denne vegvisaren vil leie deg gjennom avinstalleringen av $(^NameDA).\n\nFør du fortsetter må du forsikre deg om at $(^NameDA) ikkje er opent.\n\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Avinstaller $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Slett $(^NameDA) frå datamaskinen."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Lisensavtale"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Les gjennom lisensavtalen før du startar avinstalleringa av $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Trykk på «Godta» dersom du godtar betingelsane i avtala. Du må godta avtala for å avinstallera $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Trykk på avkryssingsboksen nedanfor nedanfor dersom du godtar betingelsane i avtala. Du må godta avtala for å avinstallera $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Vel det første alternativet nedanfor dersom du godtar betingelsane i avtala. Du må godta avtala for å avinstallera $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Vel funksjonar"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Vel kva for funksjonar du vil avinstallera i $(^NameDA)."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Vel avinstalleringplassering"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Vel mappa du vil avinstallere $(^NameDA) frå."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Avinstallerer"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vent medan $(^NameDA) vert avinstallert."
    
  !define MUI_UNTEXT_FINISH_TITLE "Avinstallering ferdig"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Avinstallering ble utført uten feil."
  
  !define MUI_UNTEXT_ABORT_TITLE "Avinstallering broten"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Avinstallering ble ikkje utført riktig."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Fullfører avinstalleringa av $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) er no avinstallert frå datamaskina di.\n\nTrykk på «Fullfør» for å avslutta denne vegvisaren."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Datamaskinen må starta på nytt for å fullføra avinstalleringa av $(^NameDA). Vil du starta datamaskina på nytt no?"
  
  !define MUI_UNTEXT_ABORTWARNING "Er du viss på at du vil avbryta avinstalleringa av $(^Name)?"
  
!insertmacro MUI_LANGUAGEFILE_END