;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: Dutch (1043)
;By Joost Verburg

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "DUTCH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Nederlands" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licentie Overeenkomst"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Lees de licentie overeenkomst voordat u ${MUI_PRODUCT} installeerd."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Druk op Page Down om de rest van de overeenkomt te zien."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Als u de overeenkomt accepteert, kies Akkoord om verder te gaan. U moet met de overeenkomst acceptren om ${MUI_PRODUCT} te installeren."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Kies Onderdelen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Kies de onderdelen die u wilt installeren."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS "Selecteer de onderdelen die u wilt installleren en deselecteer de onderdelen die u niet wilt installeren. Klik Volgende om verder te gaan."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beschrijving"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Beweeg uw muis over een onderdeel om een beschrijving te zien."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Kies Installatie Locatie"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Kies de map waarin u ${MUI_PRODUCT} in wilt installeren."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Setup zal ${MUI_PRODUCT} in de volgende map installeren.$\r$\n$\r$\nOm in een deze map te intalleren, klik Installeren. Om in een andere map te installeren, klik Bladeren en selecteer een andere map."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Installatie Map"
 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Bezig met installeren"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Een ogenblik geduld terwijl ${MUI_PRODUCT} wordt geinstalleerd."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_TITLE "Gereed"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_SUBTITLE "De installatie is succesvol verlopen."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Weet u zeker dat u ${MUI_PRODUCT} Setup wilt afsluiten?"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WINDOWTITLE "${MUI_NAME} Installatie"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Deïnstalleer ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Verwijder ${MUI_PRODUCT} van uw system."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Dit programma zal ${MUI_PRODUCT} verwijderen van uw systeem."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Bezig met deïnstalleren"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Een ogenblik gedult terwijl ${MUI_PRODUCT} van uw system wordt verwijderd."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Gereed"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "${MUI_PRODUCT} is verwijderd van uw systeem."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_WINDOWTITLE "${MUI_NAME} Deïnstallatie"
   
!insertmacro MUI_LANGUAGEFILE_END