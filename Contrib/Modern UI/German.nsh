;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: German (1031)
;By Jann Röder <jann.roeder@web.de>

;--------------------------------

!ifndef MUI_GERMAN_USED

!define MUI_GERMAN_USED

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_GERMAN} "Mit der Nach-Unten-Taste können Sie den Rest des Vertrags anzeigen."
  !endif
  
  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_GERMAN} "Markieren Sie die Komponenten die installiert werden sollen.$\r$\nEntfernen Sie die Markierung um eine Komponente von der Installation auszuschließen." " "
  !endif
  
  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_GERMAN} "${NAME} wird in folgendes Verzeichnis installiert:$\r$\n$\r$\nKlicken sie auf Installieren um ${NAME} in dieses Verzeichnis zu installieren. Oder klicken Sie auf Durchsuchen um ein anderes Verzeichnis zu wählen." " "
  !endif
  
  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_GERMAN} "Weiter >"
  !endif
   
  LangString MUI_TEXT_LICENSE_TITLE ${LANG_GERMAN} "Lizenzvertrag "  
  LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_GERMAN} "Bitte lesen Sie den folgenden Lizenzvertrag bevor Sie ${NAME} installieren."
  LangString MUI_INNERTEXT_LICENSE ${LANG_GERMAN} "Für die Installation von ${NAME} müssen Sie diesem Lizenzvertrag zustimmen. Wenn Sie mit dem Vertrag einverstanden sind, klicken Sie auf Akzeptieren."
  
  LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_GERMAN} "Komponenten wählen"
  LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_GERMAN} "Wählen Sie die zu installierenden Komponenten."
  LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_GERMAN} "Beschreibung:"
  LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_GERMAN} "Bewegen Sie die Maus über eine Komponente um die Beschreibung zu sehen."
  
  LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_GERMAN} "Zielverzeichnis"
  LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_GERMAN} "Wählen Sie das Verzeichnis, in welches ${NAME} installiert werden soll."
  LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_GERMAN} "Zielverzeichnis:"
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_GERMAN} "Installation durchführen"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_GERMAN} "Bitte warten Sie während ${NAME} installiert wird."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_GERMAN} "Installation beendet"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_GERMAN} "Die Installation wurde erfolgreich abgeschlossen."
  
  LangString MUI_MSGTEXT_ABORTWARNING ${LANG_GERMAN} "Wollen Sie ${NAME} Setup wirklich beenden?"

  LangString MUI_BUTTONTEXT_BACK ${LANG_GERMAN} "< Zurück"
  LangString MUI_BUTTONTEXT_NEXT ${LANG_GERMAN} "Weiter >"
  LangString MUI_BUTTONTEXT_CANCEL ${LANG_GERMAN} "Abbrechen"
  LangString MUI_BUTTONTEXT_INSTALL ${LANG_GERMAN} "Installieren"

  
  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_GERMAN} "${NAME} wird von ihrem PC entfernt."
  !endif
  
  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_GERMAN} "Weiter >"
  !endif
  
  LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_GERMAN} "${NAME} deinstallieren"
  LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_GERMAN} "${NAME} aus dem System entfernen"
  
  LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_GERMAN} "Deinstallieren"
  LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_GERMAN} "Bitte warten Sie während ${NAME} deinstalliert wird."
  
  LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_GERMAN} "Abgeschlossen"
  LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_GERMAN} "Deinstallation erfolgreich abgeschlossen."
  
  LangString un.MUI_BUTTONTEXT_BACK ${LANG_GERMAN} "< Zurück"
  LangString un.MUI_BUTTONTEXT_NEXT ${LANG_GERMAN} "Weiter >"
  LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_GERMAN} "Abbrechen"
  LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_GERMAN} "Deinstallieren"
    
!endif
