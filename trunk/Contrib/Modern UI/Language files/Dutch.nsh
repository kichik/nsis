;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.4

;Language: Dutch (1043)
;By Joost Verburg

;--------------------------------
!verbose 3

!ifndef MUI_DUTCH_USED

!define MUI_DUTCH_USED

  LoadLanguageFile "${NSISDIR}\Contrib\Language files\Dutch.nlf"

  !define MUI_DUTCH_LANGNAME "Nederlands" ;Name of the language in the language itself (English, Deutsch, Français etc.)

  ;INSTALLER
  Name /LANG=${LANG_DUTCH} "${MUI_NAME}"
  
  !ifdef MUI_LICENSEPAGE
    LicenseText /LANG=${LANG_DUTCH} "Druk op Page Down om de rest van de overeenkomt te zien."
    LangString MUI_TEXT_LICENSE_TITLE ${LANG_DUTCH} "Licentie Overeenkomst"
    LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_DUTCH} "Lees de licentie overeenkomst voordat u ${MUI_PRODUCT} installeerd."
    LangString MUI_INNERTEXT_LICENSE ${LANG_DUTCH} "Als u de overeenkomt accepteert, kies Akkoord om verder te gaan. U moet met de overeenkomst acceptren om ${MUI_PRODUCT} te installeren."
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_DUTCH} "Selecteer de onderdelen die u wilt installleren en deselecteer de onderdelen die u niet wilt installeren. Klik Volgende om verder te gaan."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_DUTCH} "Kies Onderdelen"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_DUTCH} "Kies de onderdelen die u wilt installeren."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_DUTCH} "Beschrijving"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_DUTCH} "Beweeg uw muis over een onderdeel om een beschrijving te zien."
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_DUTCH} "Setup zal ${MUI_PRODUCT} in de volgende map installeren.$\r$\n$\r$\nOm in een deze map te intalleren, klik Installeren. Om in een andere map te installeren, klik Bladeren en selecteer een andere map."
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_DUTCH} "Kies Installatie Locatie"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_DUTCH} "Kies de map waarin u ${MUI_PRODUCT} in wilt installeren."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_DUTCH} "Installatie Map"
  !endif
 
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_DUTCH} "Bezig met installeren"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_DUTCH} "Een ogenblik geduld terwijl ${MUI_PRODUCT} wordt geinstalleerd."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_DUTCH} "Gereed"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_DUTCH} "De installatie is succesvol verlopen."
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_DUTCH} "Weet u zeker dat u ${MUI_PRODUCT} Setup wilt afsluiten?"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_DUTCH} "${MUI_PRODUCT} ${MUI_VERSION} Setup"
  !endif


  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_DUTCH} "Dit programma zal ${MUI_PRODUCT} verwijderen van uw systeem."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_DUTCH} "Deïnstalleer ${MUI_PRODUCT}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_DUTCH} "Verwijder ${MUI_PRODUCT} van uw system."
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_DUTCH} "Bezig met deïnstalleren"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_DUTCH} "Een ogenblik gedult terwijl ${MUI_PRODUCT} van uw system wordt verwijderd."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_DUTCH} "Gereed"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_DUTCH} "${MUI_PRODUCT} is verwijderd van uw systeem."
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_DUTCH} "${MUI_PRODUCT} ${MUI_VERSION} Setup"
  !endif
    
!endif

!verbose 4