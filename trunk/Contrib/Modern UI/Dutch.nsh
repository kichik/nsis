;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: Dutch (1043)
;By Joost Verburg

;--------------------------------

!ifndef MUI_DUTCH_USED

!define MUI_DUTCH_USED

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_DUTCH} "Druk op Page Down om de rest van de overeenkomt te zien."
  !endif
  
  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_DUTCH} "Selecteer de onderdelen die u wilt installleren en deselecteer de onderdelen die u niet wilt installeren. Klik Volgende om verder te gaan." " "
  !endif
  
  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_DUTCH} "Setup zal ${NAME} in de volgende map installeren.$\r$\n$\r$\nOm in een deze map te intalleren, klik Installeren. Om in een andere map te installeren, klik Bladeren en selecteer een andere map." " "
  !endif
  
  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_DUTCH} "Volgende >"
  !endif
  
  LangString MUI_TEXT_LICENSE_TITLE ${LANG_DUTCH} "Licentie Overeenkomst"
  LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_DUTCH} "Lees de licentie overeenkomst voordat u ${NAME} installeerd."
  LangString MUI_INNERTEXT_LICENSE ${LANG_DUTCH} "Als u de overeenkomt accepteert, kies Akkoord om verder te gaan. Als u Annuleren kiest zal Setup sluiten. U moet met de overeenkomst acceptren om ${NAME} te installeren."
  
  LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_DUTCH} "Kies Onderdelen"
  LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_DUTCH} "Kies de onderdelen die u wilt installeren."
  LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_DUTCH} "Beschrijving"
  LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_DUTCH} "Beweeg uw muis over een onderdeel om een beschrijving te zien."
  
  LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_DUTCH} "Kies Installatie Locatie"
  LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_DUTCH} "Kies de map waarin u ${NAME} in wilt installeren."
  LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_DUTCH} "Installatie Map"
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_DUTCH} "Bezig met installeren"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_DUTCH} "Een ogenblik geduld terwijl ${NAME} wordt geinstalleerd."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_DUTCH} "Gereed"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_DUTCH} "De installatie is succesvol verlopen."
  
  LangString MUI_MSGTEXT_ABORTWARNING ${LANG_DUTCH} "Weet u zeker dat u ${NAME} Setup wilt afsluiten?"
  
  LangString MUI_BUTTONTEXT_BACK ${LANG_DUTCH} "< Vorige"
  LangString MUI_BUTTONTEXT_NEXT ${LANG_DUTCH} "Volgende >"
  LangString MUI_BUTTONTEXT_CANCEL ${LANG_DUTCH} "Annuleren"
  LangString MUI_BUTTONTEXT_INSTALL ${LANG_DUTCH} "Installeren"


  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_DUTCH} "Dit programma zal ${NAME} verwijderen van uw systeem."
  !endif
  
  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    UnInstallButtonText /LANG=${LANG_DUTCH} "Volgende >"
  !endif
  
  LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_DUTCH} "Deïnstalleer ${NAME}"
  LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_DUTCH} "Verwijder ${NAME} van uw system."
  
  LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_DUTCH} "Bezig met deïnstalleren"
  LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_DUTCH} "Een ogenblik gedult terwijl ${NAME} van uw system wordt verwijderd."
  
  LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_DUTCH} "Gereed"
  LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_DUTCH} "${NAME} is verwijderd van uw systeem."
  
  LangString un.MUI_BUTTONTEXT_BACK ${LANG_DUTCH} "< Vorige"
  LangString un.MUI_BUTTONTEXT_NEXT ${LANG_DUTCH} "Volgende >"
  LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_DUTCH} "Annuleren"
  LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_DUTCH} "Verwijderen"
    
!endif