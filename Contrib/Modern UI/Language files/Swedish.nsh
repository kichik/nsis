;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Swedish (1053)
;By Magnus Bonnevier (magnus.bonnevier@telia.com), updated by Rickard Angbratt (r.angbratt@home.se)
;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SWEDISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Svenska" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Klicka Nästa för att fortsätta."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Klicka Installera för att starta installationen."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Välkommen till installationsguiden för ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Denna guide tar dig igenom installationen av ${MUI_PRODUCT}.\r\n\r\nDet är rekomenderat att du avslutar alla program innan du fortsätter installationen. Detta tillåter att installationen kan uppdatera vissa systemfiler utan att starta om din dator.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licensavtal"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Var vänlig läs igenom licensvilkoren innan du installerar ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Tryck Page Down för att se resten av licensavtalet."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Om du accepterar vilkoren i avtalet, klicka Jag Godkänner för att fortsätta. Du måste acceptera avtalet för att installera ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Om du accepterar vilkoren i avtalet, klicka i checkrutan nedan. Du måste acceptera avtalet för att installera ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Om du accepterar vilkoren i avtalet, välj det första alternativet nedan. Du måste acceptera avtalet för att installera ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Välj komponenter"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Välj vilka alternativ av ${MUI_PRODUCT} som du vill installera."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Checka dom alternativ du vill installera och checka av dom du inte vill installera."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrivning"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Håll muspekaren över ett alternativ för att se dess beskrivning."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Välj installationsväg"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Välj katalog att installera ${MUI_PRODUCT} i."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Guiden kommer att installera ${MUI_PRODUCT} i följande katalog.$\r$\n$\r$\nFör att installera i en annan katalog, klicka på Bläddra och välj en annan katalog."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Destinationskatalog"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Installerar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Var vänlig vänta medan ${MUI_PRODUCT} installeras."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Installationen är klar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Guiden avslutades korrekt."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Installationen avbröts"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Guiden geonomfördes inte korrekt."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Avsluta"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Avslutar installationsguiden för ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} har installerats på din dator.\r\n\r\nKlicka på Avsluta för att avsluta guiden."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Din dator måste startas om för att fullborda installationen av ${MUI_PRODUCT}. Vill du starta om nu?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Starta om nu"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Jag vill starta om själv senare"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "&Kör ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "&Visa Readme-filen"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Välj Startmenykatalog"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Välj en Startmenykatalog för programmets genvägar."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Välj startmenykatalog i vilken du vill skapa programmets genvägar. Du kan ange ett eget namn för att skapa en ny katalog."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Skapa ej genvägar"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Är du säker på att du vill avbryta installationen av ${MUI_PRODUCT}?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Klicka på Avinstallera för att starta avinstallationen."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Avinstallera ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Ta bort ${MUI_PRODUCT} från din dator."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Denna guide kommer att avinstallera ${MUI_PRODUCT} från din dator."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Avinstallerar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Var vänlig vänta medan ${MUI_PRODUCT} avinstalleras."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Avinstallationen genomförd"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Avinstallationen genomfördes korrekt."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Avinstallationen avbruten"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Avinstallationen genomfördes inte korrekt."
  
!insertmacro MUI_LANGUAGEFILE_END