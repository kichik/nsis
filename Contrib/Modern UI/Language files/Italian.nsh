;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Italian (1040)
;By SANFACE Software <sanface@sanface.com>
;Review and update from v1.65 to v1.66 by Alessandro Staltari < staltari (a) geocities.com >

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ITALIAN"

  !define MUI_LANGNAME "Italiano" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Benvenuti nel programma di installazione di $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Questo programma installerà $(^Name) nel vostro computer.\r\n\r\nSi raccomanda di chiudere tutte le altre applicazioni prima di iniziare l'installazione. Questo permetterà al programma di installazione di aggiornare i file di  sistema senza dover riavviare il computer.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Licenza d'uso"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Prego leggere le condizioni della licenza d'uso prima di installare $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Premere Page Down per vedere il resto della licenza d'uso."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Se si accettano i termini della licenza d'uso scegliere Accetto per continuare. É necessario accettare i termini della licenza d'uso per installare $(^Name)."  
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se si accettano i termini della licenza d'uso, selezionare la casella sottostante. É necessario accettare i termini della licenza d'uso per installare $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se si accettano i termini della licenza d'uso, selezionare la prima opzione sottostante. É necessario accettare i termini della licenza d'uso per installare $(^Name)."

  !define MUI_TEXT_COMPONENTS_TITLE "Selezione dei componenti"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Selezionare i componenti di $(^Name) che si desidera installare."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descrizione"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Posizionare il puntatore del mouse sul componente per vederne la descrizione."

  !define MUI_TEXT_DIRECTORY_TITLE "Scelta della cartella di installazione"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Scegliere la cartella nella quale installare $(^Name)."

  !define MUI_TEXT_INSTALLING_TITLE "Installazione in corso"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Prego attendere mentre $(^Name)  viene installato."

  !define MUI_TEXT_FINISH_TITLE "Installazione completata"
  !define MUI_TEXT_FINISH_SUBTITLE "L'installazione è stata completata con successo."

  !define MUI_TEXT_ABORT_TITLE "Installazione interrotta"
  !define MUI_TEXT_ABORT_SUBTITLE "L'installazione non é stata completata correttamente."
  
  !define MUI_BUTTONTEXT_FINISH "&Fine"
  !define MUI_TEXT_FINISH_INFO_TITLE "Completamento dell'installazione di $(^Name)."
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) è stato installato sul vostro computer.\r\n\r\nScegliere Fine per chiudere il programma di installazione."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Il computer deve essere riavviato per completare l'installazione di $(^Name). Si desidera riavviarlo adesso?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Riavvia adesso"
  !define MUI_TEXT_FINISH_REBOOTLATER "Preferisco riavviarlo manualmente più tardi"
  !define MUI_TEXT_FINISH_RUN "Esegui $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Mostra il file Readme"

  !define MUI_TEXT_STARTMENU_TITLE "Scelta della cartella del menù Start" 
  !define MUI_TEXT_STARTMENU_SUBTITLE "Scegliere una cartella del menù Start per i collegamenti del programma." 
  !define MUI_INNERTEXT_STARTMENU_TOP "Scegliere la cartella del menù Start in cui verranno crati i collegamenti del programma. É possibile inserire un nome per creare una nuova cartella."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Non creare i collegamenti al programma."

  !define MUI_TEXT_ABORTWARNING "Se sicuro di voler terminare l'installazione di $(^Name) ?"


  !define MUI_UNTEXT_CONFIRM_TITLE "Disintalla $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Rimuove $(^Name) dal computer."
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Selezione componenti"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Scegliere i componenti di $(^Name) che si desidera disinstallare."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Disinstallazione in corso"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Prego attendere mentre $(^Name) viene disinstallato."

  !define MUI_UNTEXT_FINISH_TITLE "Disinstallazione completata"
  !define MUI_UNTEXT_FINISH_SUBTITLE "La disinstallazione è stata completata con successo."

  !define MUI_UNTEXT_ABORT_TITLE "Disinstallazione interrotta"
  !define MUI_UNTEXT_ABORT_SUBTITLE "La disintallazione non e' stata completata correttamente."
  
!insertmacro MUI_LANGUAGEFILE_END
