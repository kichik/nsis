;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.63

;Language: Danish (1030)
;By Casper Bergenstoff

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Danish"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Dansk" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Tryk næste for at fortsætte."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Klik installer for at installere."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Velkommen til ${MUI_PRODUCT} setup guide"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Dette vil installere ${MUI_PRODUCT} på din computer.\r\n\r\ndet foretrækkes at du lukker alle kørende programmer inden start af guiden . Dette vil tillade guiden at opdatere bestemte systemfiler uden genstart af dit system.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licens Aftale"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Læs venligst licens reglerne før du installerer ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Tryk Page Down for at se resten af reglerne."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "hvis du accepterer alle reglerne, klik Jeg accepterer for at komme videre. Du skal acceptere reglerne for at komme videre ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Vælg komponenter"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Vælg hvilke features af ${MUI_PRODUCT} du vil installere."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Sæt hak i de komponenter du vil installere og fjern hakket i dem du ikke vil installere."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrivelse"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Flyt musemarkøren over et komponent for at se beskrivelsen på dette."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Vælg installerings placering"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Vælg hvilken mappe du vil installere ${MUI_PRODUCT} i."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Setup-guiden vil installere ${MUI_PRODUCT} i følgende mappe.$\r$\n$\r$\nÆndr til anden placering , klik gennemse og vælg en anden mappe."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Placerings mappe"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Installerer"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Vent venligst mens ${MUI_PRODUCT} bliver installeret."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Installation gennemført"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Setup-guiden blev afsluttet med success."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Færdig"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Færdiggør ${MUI_PRODUCT} installationsguiden"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} er blevet installeret på din computer.\r\n\r\nKlik færdig for at lukke denne guide."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Dit system skal genstartes før installeringen af ${MUI_PRODUCT} er færdig. Vil du genstarte nu?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Genstart"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Jeg genstarter selv på andet tidspunkt"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Kør ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Vis læsmig"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Vælg Start Menu mappe"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Vælg en Start Menu mappe til programmet's genveje."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Vælg Start Menu mappe hvor du vil lave programmets genveje. Du kan også skrive et navn til en ny mappe."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Lav ikke genveje"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Er du sikker på at du vil lukke ${MUI_PRODUCT} Setup-guide?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "afinstaller ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Fjern ${MUI_PRODUCT} fra dit system."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Dette vil afinstallere ${MUI_PRODUCT} fra dit system."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "afinstallerer"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vent venligst imens ${MUI_PRODUCT} bliver fjernet fra dit system."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Færdig"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Afinstalleringen blev afsluttet med success."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Klik 'Afinstaller' for at starte afinstallationen."

!insertmacro MUI_LANGUAGEFILE_END