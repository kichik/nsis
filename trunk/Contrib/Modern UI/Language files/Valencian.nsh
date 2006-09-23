;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Valencian (33280)
;By Bernardo Arlandis Mañó

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Valencian"

  !define MUI_LANGNAME "Valencià" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Benvingut a l'Assistent d'Instalacio de $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Este programa instalarà $(^NameDA) en el seu ordenador.\r\n\r\nEs recomana que tanque totes les demes aplicacions abans d'iniciar l'instalacio. Aixina es podran actualisar archius relacionats en el sistema sense haver de reiniciar el seu ordenador.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Acort de llicencia"
  !define MUI_TEXT_LICENSE_SUBTITLE "Per favor, revise els termens de la llicencia abans d'instalar $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Pulse Avançar Pagina per a vore lo restant de l'acort."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Si accepta tots els termens de l'acort, seleccione Accepte per a continuar. Deu d'acceptar l'acort per a instalar $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si accepta els termens de l'acort, marque la casella avall. Deu d'acceptar els termens per a instalar $(^NameDA). $_CLICK" 
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepta els termens de l'acort, seleccione la primera opcio avall. Deu d'acceptar els termens per a instalar $(^NameDA). $_CLICK" 
  
  !define MUI_TEXT_COMPONENTS_TITLE "Seleccio de components"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Seleccione quins components de $(^NameDA) vol instalar."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descripcio"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Situe el ratoli damunt d'un component per a vore la seua descripcio."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Seleccione un component per a vore la seua descripcio."
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Trie el lloc d'instalacio"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Trie el directori per a instalar $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Instalant"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Per favor, espere mentres $(^NameDA) s'instala."
  
  !define MUI_TEXT_FINISH_TITLE "Instalacio Completa"
  !define MUI_TEXT_FINISH_SUBTITLE "L'instalacio s'ha completat correctament."

  !define MUI_TEXT_ABORT_TITLE "Instalacio Anulada"
  !define MUI_TEXT_ABORT_SUBTITLE "L'instalacio no s'ha completat correctament."

  !define MUI_BUTTONTEXT_FINISH "&Acabar"
  !define MUI_TEXT_FINISH_INFO_TITLE "Finalisant l'Assistent d'Instalacio de $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) s'ha instalat en son sistema.\r\n\r\nPulse Acabar per a tancar est assistent."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Son sistema es deu de reiniciar per a que puga completar-se l'instalacio de $(^NameDA). ¿Vol reiniciar ara?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reiniciar ara"
  !define MUI_TEXT_FINISH_REBOOTLATER "Vullc reiniciar manualment despres en acabant"
  !define MUI_TEXT_FINISH_RUN "&Eixecutar $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Vore Lligga'm"

  !define MUI_TEXT_STARTMENU_TITLE "Trie una Carpeta del Menu Inici"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Trie una Carpeta del Menu Inici per als llançadors de $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Seleccione una carpeta del Menu Inici a on vullga crear els llançadors del programa. Tambe pot introduir un nom per a crear-ne una nova carpeta."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "No crear llançadors"
  
  !define MUI_TEXT_ABORTWARNING "¿Està segur que vol eixir de l'instalacio de $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Benvingut a l'Assistent de Desinstalacio de $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Est assistent el guiarà durant la desinstalacio de $(^NameDA).\r\n\r\nAbans de començar la desinstalacio, assegure's que $(^NameDA) no s'està eixecutant.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Desinstalar $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Elimina $(^NameDA) de son sistema."

  !define MUI_UNTEXT_LICENSE_TITLE "Acort de llicencia"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Per favor, revise els termens de la llicencia abans de desinstalar $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Si accepta tots els termens de l'acort, seleccione Accepte per a continuar. Deu d'acceptar l'acort per a desinstalar $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si accepta els termens de l'acort, marque la casella avall. Deu d'acceptar els termens per a desinstalar $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepta els termens de l'acort, seleccione la primera opcio avall. Deu d'acceptar els termens per a desinstalar $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Seleccio de components"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Seleccione quins components de $(^NameDA) vol desinstalar."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Triar lloc de desinstalacio"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Trie el directori des d'a on es desinstalarà $(^NameDA)."
     
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalant"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Per favor, espere mentres $(^NameDA) es desinstala."
  
  !define MUI_UNTEXT_FINISH_TITLE "Desinstalacio Completa"
  !define MUI_UNTEXT_FINISH_SUBTITLE "La desinstalacio s'ha completat correctament."

  !define MUI_UNTEXT_ABORT_TITLE "Desinstalacio Anulada"
  !define MUI_UNTEXT_ABORT_SUBTITLE "La desinstalacio no s'ha completat correctament."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Finalisant l'Assistent de Desinstalacio de $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) s'ha desinstalat de son sistema.\r\n\r\nPulse Acabar per a tancar est assistent."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "El seu ordenador deu de reiniciar-se per a completar la desinstalacio de $(^NameDA). ¿Vol reiniciar ara?"
  !define MUI_UNTEXT_ABORTWARNING "¿Està segur de voler eixir de la desinstalacio de $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END