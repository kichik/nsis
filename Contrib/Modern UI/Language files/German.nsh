;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: German (1031)
;By L.King, changes by K. Windszus & R. Bisswanger

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "GERMAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Deutsch" ;Name of the language in the language itself
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Willkommen beim Installations-\r\n\Assistenten für $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Dieser Assistent wird Sie durch die Installation von $(^Name) begleiten.\r\n\r\nEs wird empfohlen vor der Installation alle anderen Programme zu schließen, damit bestimmte Systemdateien ohne Neustart ersetzt werden können.\r\n\r\n"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Lizenzabkommen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Bitte lesen Sie die Lizenzbedingungen durch, bevor Sie mit der Installation fortfahren."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Drücken Sie die Bild-nach-unten Taste um den Rest des Abkommens zu sehen."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Falls Sie alle Bedingungen des Abkommens akzeptieren, klicken Sie auf Annehmen. Sie müssen die Lizenzvereinbarungen anerkennen um $(^Name) zu installieren."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Falls Sie alle Bedingungen des Abkommens akzeptieren, aktivieren Sie das Kästchen. Sie müssen die Lizenzvereinbarungen anerkennen um $(^Name) zu installieren. $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Falls Sie alle Bedingungen des Abkommens akzeptieren, wählen Sie untenstehend die entsprechende Option. Sie müssen die Lizenzvereinbarungen anerkennen um $(^Name) zu installieren. $_CLICK"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Komponenten auswählen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Wählen Sie die Komponenten aus, die Sie installieren möchten."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beschreibung"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Bewegen Sie den Mauszeiger über eine Komponente um ihre Beschreibung zu sehen."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Zielverzeichnis auswählen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Wählen Sie das Verzeichnis aus, in das $(^Name) installiert werden soll."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Installiere..."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Bitte warten Sie, während $(^Name) installiert wird."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Die Installation ist vollständig"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Die Installation wurde erfolgreich abgeschlossen."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_TITLE "Abbruch der Installation"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORT_SUBTITLE "Die Installation wurde nicht vollständig abgeschlossen."  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_BUTTONTEXT_FINISH "&Fertig stellen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Die Installation von $(^Name) wird abgeschlossen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "$(^Name) wurde auf Ihrem Computer installiert.\r\n\r\nKlicken Sie auf Fertig stellen um den Installations-Assistenten zu schließen."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Windows muss neu gestartet werden um die Installation von $(^Name) zu vervollständigen. Möchten Sie Windows jetzt neu starten?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Jetzt neu starten"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Windows später selbst neu starten"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "$(^Name) ausführen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Liesmich anzeigen"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Startmenü Ordner bestimmen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Bestimmen Sie einen Startmenü Ordner für die Programmverknüpfungen."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Wählen Sie den Startmenü Ordner für die Programmverknüpfungen aus. Falls Sie einen neuen Ordner erstellen möchten, geben Sie dessen Namen ein."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Keine Verknüpfungen erstellen"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Sind Sie sicher, dass Sie die Installation von $(^Name) abbrechen wollen?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Klicken Sie auf Deinstallieren um mit der Deinstallation zu beginnen."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_TITLE "Deinstallation von $(^Name)"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONFIRM_SUBTITLE "Entferne $(^Name) von Ihrem Computer."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_TITLE "Komponenten auswählen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_COMPONENTS_SUBTITLE "Wählen Sie die Komponenten aus, die Sie entfernen möchten."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Deinstalliere..."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Bitte warten Sie, während $(^Name) entfernt wird."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Die Deinstallation ist vollständig"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "Die Deinstallation von $(^Name) wurde erfolgreich abgeschlossen."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_TITLE "Abbruch der Deinstallation"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_ABORT_SUBTITLE "Die Deinstallation wurde nicht vollständig abgeschlossen."

!insertmacro MUI_LANGUAGEFILE_END