;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: German (1031)
;By L.King

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "GERMAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Deutsch" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Klicken Sie auf Weiter um fortzufahren."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Klicken Sie auf Installieren um mit der Installation zu beginnen."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Lizenzabkommen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Bitte lesen Sie die Lizenzbedingungen durch, bevor Sie mit der Installation fortfahren."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Drücken Sie die Bild-nach-unten Taste um den Rest des Abkommens zu sehen."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Falls Sie alle Bedingungen des Abkommens akzeptieren, klicken Sie auf Annehmen. Sie müssen die Lizenzvereinbarungen anerkennen um ${MUI_PRODUCT} zu installieren."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Wählen Sie die Komponenten aus"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Wählen Sie die Komponenten aus, die Sie installieren möchten."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS "Wählen Sie die Komponenten aus, die Sie installieren möchten und wählen Sie diejenigen ab, die Sie nicht installieren wollen."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Beschreibung"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Bewegen Sie den Mauszeiger über eine Komponente um ihre Beschreibung zu sehen."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Wählen Sie das Zielverzeichnis aus"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Wählen Sie das Verzeichnis aus, in dem ${MUI_PRODUCT} installiert werden soll."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "${MUI_PRODUCT} wird in das unten angegebene Verzeichnis installiert.$\r$\n$\r$\nFalls Sie in ein anderes Verzeichnis installieren möchten, klicken Sie auf Durchsuchen und wählen Sie ein anderes Verzeichnis aus."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Zielverzeichnis"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Installiere..."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Bitte warten Sie, während ${MUI_PRODUCT} installiert wird."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_TITLE "Fertig"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_SUBTITLE "Die Installation wurde erfolgreich abgeschlossen."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE "Startmenü Ordner"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Startmenü Ordner bestimmen"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Bestimmen Sie einen Startmenü Ordner für die Programmverknüpfungen."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU "Wählen Sie den Startmenü Ordner für die Programmverknüpfungen aus. Falls einen neuen Ordner erstellen möchten, geben Sie dessen Namen ein. Klicken Sie auf Installieren um mit der Installation zu beginnen."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Sind Sie sicher, dass Sie die Installation von ${MUI_PRODUCT} abbrechen wollen?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Deinstallation von ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Entferne ${MUI_PRODUCT} von ihrem System."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Deinstalliert ${MUI_PRODUCT} von ihrem System."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Deinstalliere..."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Bitte warten Sie, während ${MUI_PRODUCT} entfernt wird."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Fertig"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Die Deinstallation von ${MUI_PRODUCT} wurde erfolgreich abgeschlossen."

!insertmacro MUI_LANGUAGEFILE_END