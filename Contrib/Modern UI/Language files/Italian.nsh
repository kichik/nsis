;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: Italian (1040)
;By SANFACE Software <sanface@sanface.com>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ITALIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Italiano" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licenza d'uso"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Per favore esamina i termini della licenza d'uso prima di installare ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Seleziona Page Down per vedere il resto della licenza d'uso."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Se accetti i termini della licenza d'uso seleziona Accetto per continuare. Devi accettare i termini della licenza d'uso per installare ${MUI_PRODUCT}."  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Seleziona i componenti"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Seleziona i componenti che vuoi installare."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS "Seleziona i componenti che vuoi installare e deseleziona i componenti che non vuoi installare. Per continuare clicca su Sucessivo."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descrizione"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Per leggere una descrizione più approfondita dei componenti posiziona il puntatore del mouse sul componente di tuo interesse."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Scegli la cartella di installazione"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Scegli la cartella dove installare ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Il programma d'installazione installerà ${MUI_PRODUCT} nella cartella selezionata.$\r$\n$\r$\nPer installare nella cartella selezionata clicca su Installa. Per installare in una cartella differente, clicca su Sfoglia e seleziona un'altra cartella."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Cartella d'installazione"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Installazione in corso"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Per favore attendi che ${MUI_PRODUCT} sia completamente installato."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_TITLE "Installazione completata"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_SUBTITLE "L'installazione è stata completata con successo."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Sei sicuro di voler terminare l'installazione di ${MUI_PRODUCT} ?"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WINDOWTITLE "Installazione di ${MUI_NAME}"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Disinstalla ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Rimuovi ${MUI_PRODUCT} dal vostro computer."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Questo programma rimuoverà ${MUI_PRODUCT} dal vosto computer."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Disinstallazione in corso"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Per favore attendi che ${MUI_PRODUCT} sia completamente disinstallato."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Disinstallazione completata"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "La disinstallazione è stata completata con successo."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_WINDOWTITLE "Rimozione di ${MUI_NAME}"
   
!insertmacro MUI_LANGUAGEFILE_END