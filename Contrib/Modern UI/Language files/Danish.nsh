;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.6

;Language: English (1030)
;By Christopher www.terrorscript.dynu.com &Musicmp3box@hotmail.com

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "dansk"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "dansk" ;navnet af selve sproget i sig selv

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "tryk på Næste for at forsætte"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "tryk på installer for at starte installationen"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "velkommen til ${MUI_PRODUCT} installations guide"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "dette vil installer ${MUI_PRODUCT} på din computer.\r\n\r\nIt det anbefales at du lukker alle andre programmer før du starter installationen. dette vil sørge for at installationen til at opdater visse filer uden at genstarte din pc.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licens aftale"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "vær venlig at kigge på lisens aftalen før de fortsætter  ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "tryk Page Down for at se resten af lisens aftalen."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "hvis du accepterer alt under aftalen, vælg jeg accepter. du skal acceptere lisens aftalen for at installer ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "vælg programmer"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "vælg hvad du vil installer af ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "vælg de kompunenter du vil installer og fjern de kompunenter du ikke vil installer."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "beskrivelse"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "kør musen over et kompunent for at se dens beskrivelse."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "vælg hvor du vil have det installeret"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "vælg hvor du vil have det installeret ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "installationen vil installer ${MUI_PRODUCT} i følgende mappe.$\r$\n$\r$\n for at installer det i en anden mappe, tryk på gennemse og vælg en anden mappe."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Destinations mappen"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "installer nu"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "vent venlist mens ${MUI_PRODUCT} bliver installeret."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "installationen fuldført"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "installationen blev fuldført."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_WINDOWTITLE ": alslut"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&færdig"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "fuldf?rer ${MUI_PRODUCT} installations guide"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} er blevet installeret på din pc.\r\ntryk alslut for at lukke guiden."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "din pc skal genstartes for at fuldføre installationen af ${MUI_PRODUCT}. vil du genstarte nu?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "genstart nu"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "nej jeg vil genstarte senere"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "kør ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "vis readme"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE ": Start Menu mappe"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "vælg Start Menu mappe"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "vælg en start menu mappe til programmet genveje."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "vælg en start menu mappe til programmet genveje. du kan også skrive et navn for at lave en ny pappe."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "lav ikke genveje"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "er du sikker på at du vil afstulle ${MUI_PRODUCT} installationen?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "afinstaller ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "fjerne ${MUI_PRODUCT} fra din pc."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "dette vil fjerne ${MUI_PRODUCT} fra dit pc."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "afinstallerer"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "vent venlist mens ${MUI_PRODUCT} bliver fjernet."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "færdigt"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "fjernelsen var fuldført."
  
!insertmacro MUI_LANGUAGEFILE_END