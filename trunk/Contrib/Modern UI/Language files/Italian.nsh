;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.65

;Language: Italian (1040)
;By SANFACE Software <sanface@sanface.com>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ITALIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Italiano" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Seleziona Seguente per continuare." 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Seleziona Installa per iniziare l'installazione."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Benvenuto nel progamma di installazione di ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Questo programma installerà ${MUI_PRODUCT} nel vostro computer.\r\n\r\nSi raccomanda di chiudere tutte le vostre applicazioni, prima di iniziare l'installazione. Questo permetterà al programma di installazione di aggiornare i parametri di sistema senza dover riavviare il vostro computer.\r\n\r\n"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licenza d'uso"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Per favore esamina i termini della licenza d'uso prima di installare ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Seleziona Page Down per vedere il resto della licenza d'uso."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Se accetti i termini della licenza d'uso seleziona Accetto per continuare. Devi accettare i termini della licenza d'uso per installare ${MUI_PRODUCT}."  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se accetti i termini della licenza d'uso, seleziona il check box sottostante. Deviaccettare i termini della licenza d'uso per installare ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se accetti i termini della licenza d'uso, seleziona la prima opzione sottostante. Deviaccettare i termini della licenza d'uso per installare ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Seleziona i componenti"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Seleziona i componenti di ${MUI_PRODUCT} che vuoi installare."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Seleziona i componenti che vuoi installare e deseleziona i componenti che non vuoi installare."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descrizione"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Per leggere una descrizione più approfondita dei componenti posiziona il puntatore del mouse sul componente di tuo interesse."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Scegli la cartella di installazione"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Scegli la cartella dove installare ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Il programma d'installazione installerà ${MUI_PRODUCT} nella cartella selezionata.$\r$\n$\r$\nPer installare in una cartella differente, clicca su Sfoglia e seleziona un'altra cartella."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Cartella d'installazione"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Installazione in corso"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Per favore attendi che ${MUI_PRODUCT}  sia completamente installato."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Installazione completata"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "L'installazione è stata completata con successo."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Fine"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Installazione di ${MUI_PRODUCT} completata."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} è stato installato sul vostro computer.\r\n\r\nSelezionate Fine per chiudere il programma di installazione."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Il vostro computer deve essere riavviato per completare l'installazione di ${MUI_PRODUCT}. Desiderate riavviarlo ora ?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Riavvio immediato"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Preferisco riavviarlo più tardi"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Esegui ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Mostra il file Readme"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Seleziona la cartella del menù Start" 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Scegli una cartella del menù Start per i tasti di accesso rapido al programma." 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Scegli la cartella del menù Start in cui creare i tasti di accesso rapido al programma. Puoi introdurre un nome per creare una nuova cartella."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Non creare tasti di accesso rapido al programma."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Sei sicuro di voler terminare l'installazione di ${MUI_PRODUCT} ?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Seleziona Disintalla per iniziare la disinstallazione."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Disintalla ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Rimuovi ${MUI_PRODUCT} dal tuo computer."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Questo programma rimuoverà ${MUI_PRODUCT} dal tuo computer."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Disinstallazione in corso"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Per favore attendi che ${MUI_PRODUCT} sia completamente disinstallato."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Disinstallazione completata"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "La disinstallazione è stata completata con successo."

!insertmacro MUI_LANGUAGEFILE_END
