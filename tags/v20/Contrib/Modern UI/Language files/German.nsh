;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: German (1031)
;By L.King, changes by K. Windszus & R. Bisswanger & M. Simmack

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "GERMAN"

  !define MUI_LANGNAME "Deutsch" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Willkommen beim Installations-\r\nAssistenten für $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Dieser Assistent wird Sie durch die Installation von $(^NameDA) begleiten.\r\n\r\nEs wird empfohlen vor der Installation alle anderen Programme zu schließen, damit bestimmte Systemdateien ohne Neustart ersetzt werden können.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Lizenzabkommen"
  !define MUI_TEXT_LICENSE_SUBTITLE "Bitte lesen Sie die Lizenzbedingungen durch, bevor Sie mit der Installation fortfahren."
  !define MUI_INNERTEXT_LICENSE_TOP "Drücken Sie die Bild-nach-unten Taste, um den Rest des Abkommens zu sehen."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Falls Sie alle Bedingungen des Abkommens akzeptieren, klicken Sie auf Annehmen. Sie müssen die Lizenzvereinbarungen anerkennen, um $(^NameDA) installieren zu können."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Falls Sie alle Bedingungen des Abkommens akzeptieren, aktivieren Sie das Kästchen. Sie müssen die Lizenzvereinbarungen anerkennen, um $(^NameDA) installieren zu können. $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Falls Sie alle Bedingungen des Abkommens akzeptieren, wählen Sie unten die entsprechende Option. Sie müssen die Lizenzvereinbarungen anerkennen, um $(^NameDA) installieren zu können. $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Komponenten auswählen"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Wählen Sie die Komponenten aus, die Sie installieren möchten."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beschreibung"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Bewegen Sie den Mauszeiger über eine Komponente, um ihre Beschreibung zu sehen."

  !define MUI_TEXT_DIRECTORY_TITLE "Zielverzeichnis auswählen"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Wählen Sie das Verzeichnis aus, in das $(^NameDA) installiert werden soll."

  !define MUI_TEXT_INSTALLING_TITLE "Installiere..."
  !define MUI_TEXT_INSTALLING_SUBTITLE "Bitte warten Sie, während $(^NameDA) installiert wird."

  !define MUI_TEXT_FINISH_TITLE "Die Installation ist vollständig"
  !define MUI_TEXT_FINISH_SUBTITLE "Die Installation wurde erfolgreich abgeschlossen."

  !define MUI_TEXT_ABORT_TITLE "Abbruch der Installation"
  !define MUI_TEXT_ABORT_SUBTITLE "Die Installation wurde nicht vollständig abgeschlossen."

  !define MUI_BUTTONTEXT_FINISH "&Fertig stellen"
  !define MUI_TEXT_FINISH_INFO_TITLE "Die Installation von $(^NameDA) wird abgeschlossen"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) ist auf Ihrem Computer installiert worden.\r\n\r\nKlicken Sie auf Fertig stellen, um den Installations-Assistenten zu schließen."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Windows muss neu gestartet werden, um die Installation von $(^NameDA) zu vervollständigen. Möchten Sie Windows jetzt neu starten?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Jetzt neu starten"
  !define MUI_TEXT_FINISH_REBOOTLATER "Windows später selbst neu starten"
  !define MUI_TEXT_FINISH_RUN "$(^NameDA) ausführen"
  !define MUI_TEXT_FINISH_SHOWREADME "Liesmich anzeigen"

  !define MUI_TEXT_STARTMENU_TITLE "Startmenü-Ordner bestimmen"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Bestimmen Sie einen Startmenü-Ordner für die Programmverknüpfungen."
  !define MUI_INNERTEXT_STARTMENU_TOP "Wählen Sie den Startmenü-Ordner für die Programmverknüpfungen aus. Falls Sie einen neuen Ordner erstellen möchten, geben Sie dessen Namen ein."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Keine Verknüpfungen erstellen"

  !define MUI_TEXT_ABORTWARNING "Sind Sie sicher, dass Sie die Installation von $(^Name) abbrechen wollen?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Willkommen beim Deinstallations-\r\n\Assistenten für $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Dieser Assistent wird Sie durch die Deinstallation von $(^NameDA) begleiten.\r\n\r\nBitte beenden Sie $(^NameDA), bevor Sie mit der Deinstallation fortfahren.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Deinstallation von $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "$(^NameDA) wird von Ihrem Computer entfernt."

  !define MUI_UNTEXT_LICENSE_TITLE "Lizenzabkommen"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Bitte lesen Sie die Lizenzbedingungen durch, bevor Sie mit der Deinstallation von $(^NameDA) fortfahren."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Falls Sie alle Bedingungen des Abkommens akzeptieren, klicken Sie auf Annehmen. Sie müssen die Lizenzvereinbarungen anerkennen, um $(^NameDA) deinstallieren zu können."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Falls Sie alle Bedingungen des Abkommens akzeptieren, aktivieren Sie das Kästchen. Sie müssen die Lizenzvereinbarungen anerkennen, um $(^NameDA) deinstallieren zu können. $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Falls Sie alle Bedingungen des Abkommens akzeptieren, wählen Sie unten die entsprechende Option. Sie müssen die Lizenzvereinbarungen anerkennen, um $(^NameDA) deinstallieren zu können. $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Komponenten auswählen"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Wählen Sie die Komponenten aus, die Sie entfernen möchten."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Verzeichnis für Deinstallation auswählen"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Wählen Sie das Verzeichnis aus, aus dem $(^NameDA) entfernt werden soll."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Deinstalliere..."
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Bitte warten Sie, während $(^NameDA) entfernt wird."

  !define MUI_UNTEXT_FINISH_TITLE "Die Deinstallation ist vollständig"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Die Deinstallation wurde erfolgreich abgeschlossen."

  !define MUI_UNTEXT_ABORT_TITLE "Abbruch der Deinstallation"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Die Deinstallation wurde nicht vollständig abgeschlossen."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Die Deinstallation von $(^NameDA) wird abgeschlossen"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) ist von ihrem Computer entfernt worden.\r\n\r\nKlicken Sie auf Beenden, um den Assistenten zu schließen."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Windows muss neu gestartet werden, um die Deinstallation von $(^NameDA) zu vervollständigen. Möchten Sie Windows jetzt neu starten?"

  !define MUI_UNTEXT_ABORTWARNING "Sind Sie sicher, dass Sie die Deinstallation von $(^Name) abbrechen möchten?"

!insertmacro MUI_LANGUAGEFILE_END