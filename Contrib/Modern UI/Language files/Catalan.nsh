;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: Catalan (1027)
;By falanko

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "CATALAN"

  !define MUI_LANGNAME "Català" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Benvingut a l'Assistent d'Instal·lació de $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Aquest programa instal·larà $(^Name) en el seu ordinador.\r\n\r\nEs recomana tancar tota la resta d'aplicacions abans d'iniciar la Instal·lació. Això permetrà a la Instal·lació actualitzar certs arxius del sistema sense haver de reiniciar el seu ordinador.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Acord de Llicència"
  !define MUI_TEXT_LICENSE_SUBTITLE "Per favor revisi els termes de la llicència abans d'instal·lar $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Premi AvPàg per veure la resta de l'acord."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Si accepta tots els termes de l'acord, premi Accepto per continuar. Ha d'acceptar l'acord per poder instal·lar $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si accepta tots els termes de l'acord, marqui la casella de sota. Ha d'acceptar l'acord per poder instal·lar $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepta tots els termes de l'acord, seleccioni la primera opció de sota. Ha d'acceptar l'acord per poder instal·lar $(^Name). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Selecció de components"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Seleccioni quines característiques de $(^Name) desitja instal·lar."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descripció"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Situï el ratolí al damunt d'un component per veure'n la seva descripció."

  !define MUI_TEXT_DIRECTORY_TITLE "Escollir el lloc d'instal·lació"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Esculli la carpeta per instal·lar $(^Name)."

  !define MUI_TEXT_INSTALLING_TITLE "Instal·lant"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Per favor esperi mentre $(^Name) s'instal·la."

  !define MUI_TEXT_FINISH_TITLE "Instal·lació Completada"
  !define MUI_TEXT_FINISH_SUBTITLE "La instal·lació s'ha realitzat correctament."

  !define MUI_TEXT_ABORT_TITLE "Instal·lació Anul·lada"
  !define MUI_TEXT_ABORT_SUBTITLE "La instal·lació no s'ha realitzat correctament."

  !define MUI_BUTTONTEXT_FINISH_BUTTON "&Finalitzar"
  !define MUI_TEXT_FINISH_INFO_TITLE "Completant l'Assistent d'Instal·lació de $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) s'ha instal·lat al sistema.\r\n\r\nPremi Finalitzar per tancar aquest assistent."
  !define MUI_TEXT_FINISH_INFO_REBOOT "El sistema ha de ser reiniciat per a que pugui completar-se la instal·lació de $(^Name). Desitja reiniciar-lo ara?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reiniciar ara"
  !define MUI_TEXT_FINISH_REBOOTLATER "Reiniciar més tard manualment"
  !define MUI_TEXT_FINISH_RUN "Executar $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Veure LLEGEIXME.TXT"

  !define MUI_TEXT_STARTMENU_TITLE "Escollir Carpeta del Menú d'Inici"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Esculli una Carpeta del Menú d'Inici per als accessos directes del programa."
  !define MUI_INNERTEXT_STARTMENU_TOP "Seleccioni la carpeta del Menú d'Inici en la que hi vulgui crear els accessos directes del programa. Pot introduir un altre nom si vol crear una carpeta nova."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "No crear accessos directes"

  !define MUI_TEXT_ABORTWARNING "Està segur que desitja sortir de la instal·lació de $(^Name)?"


  !define MUI_UNTEXT_CONFIRM_TITLE "Desinstal·lar $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Elimina $(^Name) del seu sistema."

  !define MUI_UNTEXT_LICENSE_TITLE "Acord de Llicència"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Per favor revisi els termes de la llicència abans de desinstal·lar $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Si accepta tots els termes de l'acord, premi Accepto per continuar. Ha d'acceptar l'acord per poder desinstal·lar $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si accepta tots els termes de l'acord, marqui la casella de sota. Ha d'acceptar l'acord per poder desinstal·lar $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepta tots els termes de l'acord, seleccioni la primera opció de sota. Ha d'acceptar l'acord per poder desinstal·lar $(^Name). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Selecció de components"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Seleccioni quines característiques de $(^Name) desitja desinstal·lar."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Escollir el lloc de desinstal·lació"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Esculli la carpeta des d'on desinstal·lar $(^Name)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Desinstal·lant"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Per favor, esperi mentre $(^Name) es desinstal·la."

  !define MUI_UNTEXT_FINISH_TITLE "Desinstal·lació Completada"
  !define MUI_UNTEXT_FINISH_SUBTITLE "La desinstal·lació s'ha realitzat correctament."

  !define MUI_UNTEXT_ABORT_TITLE "Desinstal·lació Anul·lada"
  !define MUI_UNTEXT_ABORT_SUBTITLE "La desinstal·lació no s'ha realitzat correctament."

!insertmacro MUI_LANGUAGEFILE_END