;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Danish (1030)
;By Claus Futtrup

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Danish"

  !define MUI_LANGNAME "Danish" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Velkommen til setup-guiden for $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Denne guide vil installere $(^NameDA) på din computer.\r\n\r\nDet anbefales at du lukker alle kørende programmer inden start af setup-guiden. Dette vil tillade guiden at opdatere de nødvendige systemfiler uden at skulle genstarte computeren.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licens Aftale"
  !define MUI_TEXT_LICENSE_SUBTITLE "Læs venligst licens aftalen før du installerer $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Tryk Page Down for at se resten af aftalen."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Hvis du accepterer alle vilkårene i aftalen, tryk 'Jeg accepterer' for at forsætte. Du skal acceptere vilkårene for at installere $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Hvis du accepterer alle vilkårene i aftalen, afmærk check-boxen nedenfor. Du skal acceptere vilkårene for at installere $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Hvis du accepterer alle vilkårene i aftalen, vælg den første option nedenfor. Du skal acceptere vilkårene for at installere $(^NameDA). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Vælg komponenter"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Vælg hvilke features af $(^NameDA) du vil installere."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrivelse"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Placer musemarkøren over en komponent for at se beskrivelsen af komponenten."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Vælg installationsmappe"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Vælg hvilken mappe du vil installere $(^NameDA) i."
  
  !define MUI_TEXT_INSTALLING_TITLE "Installerer"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Vent venligst mens $(^NameDA) bliver installeret."
  
  !define MUI_TEXT_FINISH_TITLE "Installation gennemført"
  !define MUI_TEXT_FINISH_SUBTITLE "Setup-guiden blev gennemført med succes."

  !define MUI_TEXT_ABORT_TITLE "Installation afbrudt"
  !define MUI_TEXT_ABORT_SUBTITLE "Setup-guiden blev ikke gennemført."

  !define MUI_BUTTONTEXT_FINISH "&Afslut"
  !define MUI_TEXT_FINISH_INFO_TITLE "Afslutter $(^NameDA) setup-guiden"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) er blevet installeret på din computer.\r\n\r\nTryk 'Afslut' for at lukke setup-guiden."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Dit system skal genstartes før installeringen af $(^NameDA) er afsluttet. Vil du genstarte nu?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Genstart nu"
  !define MUI_TEXT_FINISH_REBOOTLATER "Jeg genstarter selv på et andet tidspunkt"
  !define MUI_TEXT_FINISH_RUN "&Kør $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Vis Readme"
  
  !define MUI_TEXT_STARTMENU_TITLE "Vælg Start Menu mappe"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Vælg en Start Menu mappe til programmets genveje."
  !define MUI_INNERTEXT_STARTMENU_TOP "Vælg Start Menu mappe hvor du vil lave programmets genveje. Du kan også skrive et navn for at oprette en ny mappe."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Opret ikke genveje"
  
  !define MUI_TEXT_ABORTWARNING "Er du sikker på at du vil afslutte $(^Name) installationen?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Velkommen til $(^NameDA) afinstallations-guiden"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Denne afinstallations-guide vil hjælpe dig gennem afinstallationen af $(^NameDA).\r\n\r\nFør start af afinstallationen, vær sikker på at $(^NameDA) ikke kører.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Afinstaller $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Slet $(^NameDA) fra dit system."

  !define MUI_UNTEXT_LICENSE_TITLE "Licens aftale"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Læs venligst licens vilkårene før afinstalleringen af $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Hvis du accepterer vilkårene for aftalen, tryk 'Jeg accepterer' for at fortsætte. Du skal acceptere aftalen for at afinstallere $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Hvis du accepterer vilkårene for aftalen, tryk check-boxen nedenfor. Du skal acceptere aftalen for at afinstallere $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Hvis du accepterer vilkårene for aftalen, vælge den første option nedenfor. Du skal acceptere aftalen for at afinstallere $(^NameDA). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Vælg komponenter"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Vælg hvilke features af $(^NameDA) du vil afinstallere."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Vælg afinstallationsmappe"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Vælg den mappe hvorfra du vil afinstallere $(^NameDA)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Afinstallerer"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vent venligst mens $(^NameDA) bliver afinstalleret."
    
  !define MUI_UNTEXT_FINISH_TITLE "Afinstallationen er færdig"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Afinstalleringen blev afsluttet med succes."

  !define MUI_UNTEXT_ABORT_TITLE "Afinstallationen er blevet afbrudt"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Afinstallationen blev ikke genmmenført."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Afslutter $(^NameDA) afinstallations-guiden"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) er blevet afinstalleret fra din computer.\r\n\r\nTryk 'Afslut' for at lukke denne guide."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Din computer skal genstartes for at gennemføre afinstallationen af $(^NameDA). Vil du genstarte nu?"
  
  !define MUI_UNTEXT_ABORTWARNING "Er du sikker på at du vil afbryde $(^Name) afinstallationen?"

!insertmacro MUI_LANGUAGEFILE_END