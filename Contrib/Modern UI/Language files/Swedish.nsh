;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Swedish (1053)
;By Magnus Bonnevier (magnus.bonnevier@telia.com), updated by Rickard Angbratt (r.angbratt@home.se)
;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SWEDISH"

  !define MUI_LANGNAME "Svenska" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Välkommen till installationsguiden för $(^Name)."
  !define MUI_TEXT_WELCOME_INFO_TEXT "Denna guide tar dig igenom installationen av $(^Name).\r\n\r\nDet är rekomenderat att du avslutar alla program innan du fortsätter installationen. Detta tillåter att installationen kan uppdatera vissa systemfiler utan att starta om din dator.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Licensavtal"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Var vänlig läs igenom licensvilkoren innan du installerar $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Tryck Page Down för att se resten av licensavtalet."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Om du accepterar vilkoren i avtalet, klicka Jag Godkänner för att fortsätta. Du måste acceptera avtalet för att installera $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Om du accepterar vilkoren i avtalet, klicka i checkrutan nedan. Du måste acceptera avtalet för att installera $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Om du accepterar vilkoren i avtalet, välj det första alternativet nedan. Du måste acceptera avtalet för att installera $(^Name). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Välj komponenter"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Välj vilka alternativ av $(^Name) som du vill installera."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beskrivning"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Håll muspekaren över ett alternativ för att se dess beskrivning."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Välj installationsväg"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Välj katalog att installera $(^Name) i."
  
  !define MUI_TEXT_INSTALLING_TITLE "Installerar"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Var vänlig vänta medan $(^Name) installeras."
  
  !define MUI_TEXT_FINISH_TITLE "Installationen är klar"
  !define MUI_TEXT_FINISH_SUBTITLE "Guiden avslutades korrekt."
  
  !define MUI_TEXT_ABORT_TITLE "Installationen avbröts"
  !define MUI_TEXT_ABORT_SUBTITLE "Guiden geonomfördes inte korrekt."
  
  !define MUI_BUTTONTEXT_FINISH "&Avsluta"
  !define MUI_TEXT_FINISH_INFO_TITLE "Avslutar installationsguiden för $(^Name)."
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) har installerats på din dator.\r\n\r\nKlicka på Avsluta för att avsluta guiden."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Din dator måste startas om för att fullborda installationen av $(^Name). Vill du starta om nu?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Starta om nu"
  !define MUI_TEXT_FINISH_REBOOTLATER "Jag vill starta om själv senare"
  !define MUI_TEXT_FINISH_RUN "&Kör $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Visa Readme-filen"
  
  !define MUI_TEXT_STARTMENU_TITLE "Välj Startmenykatalog"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Välj en Startmenykatalog för programmets genvägar."
  !define MUI_INNERTEXT_STARTMENU_TOP "Välj startmenykatalog i vilken du vill skapa programmets genvägar. Du kan ange ett eget namn för att skapa en ny katalog."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Skapa ej genvägar"
  
  !define MUI_TEXT_ABORTWARNING "Är du säker på att du vill avbryta installationen av $(^Name)?"  
  
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Avinstallera $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Ta bort $(^Name) från din dator."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Avinstallerar"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Var vänlig vänta medan $(^Name) avinstalleras."
    
  !define MUI_UNTEXT_FINISH_TITLE "Avinstallationen genomförd"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Avinstallationen genomfördes korrekt."
  
  !define MUI_UNTEXT_ABORT_TITLE "Avinstallationen avbruten"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Avinstallationen genomfördes inte korrekt."
  
!insertmacro MUI_LANGUAGEFILE_END