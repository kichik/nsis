;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: German (1031)
;By L.King

;--------------------------------

!ifndef MUI_GERMAN_USED

!define MUI_GERMAN_USED

  ;INSTALLER

  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_GERMAN} "Drücken Sie die Bild-nach-unten Taste um den Rest des Abkommens zu sehen."
  !endif

  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_GERMAN} "Wählen Sie die Komponenten aus, die Sie installieren möchten und wählen Sie diejenigen ab, die Sie nicht installieren wollen. Klicken Sie auf 'Weiter' um fortzufahren." " "
  !endif

  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_GERMAN} "${NAME} wird in das unten angegebene Verzeichnis installiert.$\r$\n$\r$\nKlicken Sie auf 'Installieren' um fortzufahren. Falls Sie in ein anderes Verzeichnis installieren möchten, klicken Sie auf 'Durchsuchen' und wählen Sie ein anderes Verzeichnis aus." " "
  !endif

  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_GERMAN} "Weiter >"
  !endif

  LangString MUI_TEXT_LICENSE_TITLE ${LANG_GERMAN} "Lizenzabkommen"
  LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_GERMAN} "Bitte lesen Sie die Lizenzbedingungen durch, bevor Sie mit der Installation fortfahren."
  LangString MUI_INNERTEXT_LICENSE ${LANG_GERMAN} "Falls Sie alle Bedingungen des Abkommens akzeptieren, klicken Sie auf 'Ich stimme zu'. Sie müssen die Lizenzvereinbarungen anerkennen um ${NAME} zu installieren."

  LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_GERMAN} "Wählen Sie die Komponenten aus"
  LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_GERMAN} "Wählen Sie die Komponenten aus, die Sie installieren möchten."
  LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_GERMAN} "Beschreibung"
  LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_GERMAN} "Bewegen Sie den Mauszeiger über eine Komponente um ihre Beschreibung zu sehen."

  LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_GERMAN} "Wählen Sie das Zielverzeichnis aus"
  LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_GERMAN} "Wählen Sie das Verzeichnis aus, in dem ${NAME} installiert werden soll."
  LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_GERMAN} "Zielverzeichnis"

  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_GERMAN} "Installiere..."
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_GERMAN} "Bitte warten Sie, während ${NAME} installiert wird."

  LangString MUI_TEXT_FINISHED_TITLE ${LANG_GERMAN} "Fertig"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_GERMAN} "Die Installation wurde erfolgreich abgeschlossen."

  LangString MUI_MSGTEXT_ABORTWARNING ${LANG_GERMAN} "Sind Sie sicher, dass Sie die Installation von ${NAME} abbrechen wollen?"

  LangString MUI_BUTTONTEXT_BACK ${LANG_GERMAN} "< Zurück"
  LangString MUI_BUTTONTEXT_NEXT ${LANG_GERMAN} "Weiter >"
  LangString MUI_BUTTONTEXT_CANCEL ${LANG_GERMAN} "Abbrechen"
  LangString MUI_BUTTONTEXT_INSTALL ${LANG_GERMAN} "Installieren"


  ;UNINSTALLER

  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_GERMAN} "Deinstalliert ${NAME} von ihrem System."
  !endif

  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_GERMAN} "Weiter >"
  !endif

  LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_GERMAN} "Deinstallation von ${NAME}"
  LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_GERMAN} "Entferne ${NAME} von ihrem System."

  LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_GERMAN} "Deinstalliere..."
  LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_GERMAN} "Bitte warten Sie, während ${NAME} entfernt wird."

  LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_GERMAN} "Fertig"
  LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_GERMAN} "Die Deinstallation von ${NAME} wurde erfolgreich abgeschlossen."

  LangString un.MUI_BUTTONTEXT_BACK ${LANG_GERMAN} "< Zurück"
  LangString un.MUI_BUTTONTEXT_NEXT ${LANG_GERMAN} "Weiter >"
  LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_GERMAN} "Abbrechen"
  LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_GERMAN} "Deinstallieren"

!endif