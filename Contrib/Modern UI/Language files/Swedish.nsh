;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.64

;Language: Swedish
;By Magnus Bonnevier.
;e-mail: magnus.bonnevier@telia.com
;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SWEDISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Swedish" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Klicka nästa för att fortsätta."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Klicka Install för att starta installationen."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Välkommen till installations guiden för ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Denna guide tar dig igenom installationen av ${MUI_PRODUCT}.\r\n\r\nDet är Rekomenderat att du avslutar alla program innan du fortsätter installationen. Detta tillåter att installationen kan uppdatera vissa system filer utan att starta om din dator.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licens Avtal"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Var vänlig läs igenom licens vilkoren innan du installerar ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Tryck Page Down för att se resten av licens avtalet."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Om du accepterar vilkoren i avtalet, klicka jag godkänner för att fortsätta. Du måste acceptera avtalet för att installera ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Om du accepterar vilkoren i avtalet, klicka i check rutan nedan. Du måste acceptera avtalet för att installera ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Om du accepterar vilkoren i avtalet, välj det första alternativet nedan. Du måste acceptera avtalet för att installera ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Välj komponenter"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Välj vilka alternativ av ${MUI_PRODUCT} som du vill installera."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Checka dom alternativ du vill installera och checka av dom du inte vill installera."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrivning"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Håll muspekaren över ett alternativ för att se dess beskrivning."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Välj installations väg"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Välj katalog att installera ${MUI_PRODUCT} till."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Guiden kommer att installera ${MUI_PRODUCT} till följande katalog.$\r$\n$\r$\nFör att installera i en annan katalog, klicka på bläddra och välj en annan katalog."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Destinations Katalog"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Installerar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Var vänlig vänta medans ${MUI_PRODUCT} installeras."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Installationen är klar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Guiden avslutades korrekt."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Installationen avbröts"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Guiden geonomfördes inte korrekt."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Finish"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Avslutar installations guiden för ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} har installerats på din dator.\r\n\r\nKlicka på Finsh för att avsluta guiden."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Din dator måste startas om för att fullborda installationen av ${MUI_PRODUCT}. Vill du starta om nu?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Starta om nu"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Jag vill starta om själv senare"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "&Kör ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "&Visa readme"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Välj start meny katalog"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Välj en start meny katalog för programmets genvägar."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Välj start meny katalog i vilken du vill skapa programmets genvägar. Du kan ange ett eget namn för att skapa en ny katalog."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Skapa ej genvägar"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Är du säker på att du vill avbryta installationen av ${MUI_PRODUCT} ?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Klicka på Uninstall för att starta avinstallationen."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Avinstallera ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Ta bort ${MUI_PRODUCT} från din dator."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Denna guide kommer att avinstallera ${MUI_PRODUCT} från din dator."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Avinstallerar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Vänta medans ${MUI_PRODUCT} avinstalleras."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Avinstallation genomförd"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Avinstallationen genomfördes korrekt."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Avinstallationen avbruten"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Avinstallationen genomfördes inte korrekt."
  
!insertmacro MUI_LANGUAGEFILE_END