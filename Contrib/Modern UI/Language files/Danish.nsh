;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.63

;Language: Danish (1030)
;By Casper Bergenstoff

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Danish"

  !define MUI_LANGNAME "Dansk" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Velkommen til $(^NameDA) setup guide"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Dette vil installere $(^NameDA) på din computer.\r\n\r\ndet foretrækkes at du lukker alle kørende programmer inden start af guiden . Dette vil tillade guiden at opdatere bestemte systemfiler uden genstart af dit system.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licens Aftale"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Læs venligst licens reglerne før du installerer $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Tryk Page Down for at se resten af reglerne."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "hvis du accepterer alle reglerne, klik Jeg accepterer for at komme videre. Du skal acceptere reglerne for at komme videre $(^NameDA)."
  
  !define MUI_TEXT_COMPONENTS_TITLE "Vælg komponenter"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Vælg hvilke features af $(^NameDA) du vil installere."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrivelse"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Flyt musemarkøren over et komponent for at se beskrivelsen på dette."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Vælg installationsmappe"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Vælg hvilken mappe du vil installere $(^NameDA) i."
  
  !define MUI_TEXT_INSTALLING_TITLE "Installerer"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Vent venligst mens $(^NameDA) bliver installeret."
  
  !define MUI_BUTTONTEXT_FINISH "&Færdig"
  !define MUI_TEXT_FINISH_TITLE "Installation gennemført"
  !define MUI_TEXT_FINISH_SUBTITLE "Setup-guiden blev afsluttet med success."
  !define MUI_TEXT_FINISH_INFO_TITLE "Færdiggør $(^NameDA) installationsguiden"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) er blevet installeret på din computer.\r\n\r\nKlik færdig for at lukke denne guide."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Dit system skal genstartes før installeringen af $(^NameDA) er færdig. Vil du genstarte nu?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Genstart"
  !define MUI_TEXT_FINISH_REBOOTLATER "Jeg genstarter selv på andet tidspunkt"
  !define MUI_TEXT_FINISH_RUN "Kør $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "Vis læsmig"
  
  !define MUI_TEXT_STARTMENU_TITLE "Vælg Start Menu mappe"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Vælg en Start Menu mappe til programmet's genveje."
  !define MUI_INNERTEXT_STARTMENU_TOP "Vælg Start Menu mappe hvor du vil lave programmets genveje. Du kan også skrive et navn til en ny mappe."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Lav ikke genveje"
  
  !define MUI_TEXT_ABORTWARNING "Er du sikker på at du vil lukke $(^Name) Setup-guide?"  
  
  
  !define MUI_UNTEXT_CONFIRM_TITLE "afinstaller $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Fjern $(^NameDA) fra dit system."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "afinstallerer"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vent venligst imens $(^NameDA) bliver fjernet fra dit system."
    
  !define MUI_UNTEXT_FINISH_TITLE "Færdig"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Afinstalleringen blev afsluttet med success."
  !define MUI_UNTEXT_CONTINUE_UNINSTALL "Klik 'Afinstaller' for at starte afinstallationen."

!insertmacro MUI_LANGUAGEFILE_END