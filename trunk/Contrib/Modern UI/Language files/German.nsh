;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.4

;Language: German (1031)
;By L.King

;--------------------------------
!verbose 3

!ifndef MUI_GERMAN_USED

!define MUI_GERMAN_USED

  !define MUI_GERMAN_LANGNAME "Deutsch" ;Name of the language in the language itself (English, Deutsch, Français etc.)

  ;INSTALLER

  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_GERMAN} "Drücken Sie die Bild-nach-unten Taste um den Rest des Abkommens zu sehen."
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_GERMAN} "Lizenzabkommen"
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_GERMAN} "Bitte lesen Sie die Lizenzbedingungen durch, bevor Sie mit der Installation fortfahren."
     LangString MUI_INNERTEXT_LICENSE ${LANG_GERMAN} "Falls Sie alle Bedingungen des Abkommens akzeptieren, klicken Sie auf Annehmen. Sie müssen die Lizenzvereinbarungen anerkennen um ${NAME} zu installieren."
  !endif

  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_GERMAN} "Wählen Sie die Komponenten aus, die Sie installieren möchten und wählen Sie diejenigen ab, die Sie nicht installieren wollen. Klicken Sie auf Weiter um fortzufahren."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_GERMAN} "Wählen Sie die Komponenten aus"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_GERMAN} "Wählen Sie die Komponenten aus, die Sie installieren möchten."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_GERMAN} "Beschreibung"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_GERMAN} "Bewegen Sie den Mauszeiger über eine Komponente um ihre Beschreibung zu sehen."
  !endif

  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_GERMAN} "${NAME} wird in das unten angegebene Verzeichnis installiert.$\r$\n$\r$\nKlicken Sie auf Installieren um fortzufahren. Falls Sie in ein anderes Verzeichnis installieren möchten, klicken Sie auf Durchsuchen und wählen Sie ein anderes Verzeichnis aus." " "
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_GERMAN} "Wählen Sie das Zielverzeichnis aus"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_GERMAN} "Wählen Sie das Verzeichnis aus, in dem ${NAME} installiert werden soll."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_GERMAN} "Zielverzeichnis"
  !endif

  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_GERMAN} "Installiere..."
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_GERMAN} "Bitte warten Sie, während ${NAME} installiert wird."

  LangString MUI_TEXT_FINISHED_TITLE ${LANG_GERMAN} "Fertig"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_GERMAN} "Die Installation wurde erfolgreich abgeschlossen."

  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_GERMAN} "Sind Sie sicher, dass Sie die Installation von ${NAME} abbrechen wollen?"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_GERMAN} "${NAME} ${VERSION} Installation"
  !endif
  

  ;UNINSTALLER

  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_GERMAN} "Deinstalliert ${NAME} von ihrem System."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_GERMAN} "Deinstallation von ${NAME}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_GERMAN} "Entferne ${NAME} von ihrem System."

    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_GERMAN} "Deinstalliere..."
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_GERMAN} "Bitte warten Sie, während ${NAME} entfernt wird."

    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_GERMAN} "Fertig"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_GERMAN} "Die Deinstallation von ${NAME} wurde erfolgreich abgeschlossen."
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_GERMAN} "${NAME} ${VERSION} Installation"
  !endif

!endif

!verbose 4