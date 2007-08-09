;Language: Valencian (33280)
;By Bernardo Arlandis Mañó

!insertmacro LANGFILE "Valencian" "Valencià"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Benvingut a l'Assistent d'Instalacio de $(^NameDA)"
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TEXT "Este programa instalarà $(^NameDA) en el seu ordenador.$\r$\n$\r$\nEs recomana que tanque totes les demes aplicacions abans d'iniciar l'instalacio. Aixina es podran actualisar archius relacionats en el sistema sense haver de reiniciar el seu ordenador.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TITLE "Benvingut a l'Assistent de Desinstalacio de $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TEXT "Est assistent el guiarà durant la desinstalacio de $(^NameDA).$\r$\n$\r$\nAbans de començar la desinstalacio, assegure's que $(^NameDA) no s'està eixecutant.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_WELCOMEPAGE | MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Acabar"
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Acort de llicencia"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Per favor, revise els termens de la llicencia abans d'instalar $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Si accepta tots els termens de l'acort, seleccione Accepte per a continuar. Deu d'acceptar l'acort per a instalar $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si accepta els termens de l'acort, marque la casella avall. Deu d'acceptar els termens per a instalar $(^NameDA). $_CLICK"
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepta els termens de l'acort, seleccione la primera opcio avall. Deu d'acceptar els termens per a instalar $(^NameDA). $_CLICK"
!endif

!ifdef MUI_UNLICENSEPAGE
  ${LangFileString} MUI_UNTEXT_LICENSE_TITLE "Acort de llicencia"
  ${LangFileString} MUI_UNTEXT_LICENSE_SUBTITLE "Per favor, revise els termens de la llicencia abans de desinstalar $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM "Si accepta tots els termens de l'acort, seleccione Accepte per a continuar. Deu d'acceptar l'acort per a desinstalar $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si accepta els termens de l'acort, marque la casella avall. Deu d'acceptar els termens per a desinstalar $(^NameDA). $_CLICK"
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepta els termens de l'acort, seleccione la primera opcio avall. Deu d'acceptar els termens per a desinstalar $(^NameDA). $_CLICK"
!endif

!ifdef MUI_LICENSEPAGE | MUI_UNLICENSEPAGE
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Pulse Avançar Pagina per a vore lo restant de l'acort."
!endif

!ifdef MUI_COMPONENTSPAGE
  ${LangFileString} MUI_TEXT_COMPONENTS_TITLE "Seleccio de components"
  ${LangFileString} MUI_TEXT_COMPONENTS_SUBTITLE "Seleccione quins components de $(^NameDA) vol instalar."
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descripcio"
!endif

!ifdef MUI_UNCOMPONENETSPAGE
  ${LangFileString} MUI_UNTEXT_COMPONENTS_TITLE "Seleccio de components"
  ${LangFileString} MUI_UNTEXT_COMPONENTS_SUBTITLE "Seleccione quins components de $(^NameDA) vol desinstalar."
!endif

!ifdef MUI_COMPONENTSPAGE | MUI_UNCOMPONENTSPAGE
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Situe el ratoli damunt d'un component per a vore la seua descripcio."
  !else
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Situe el ratoli damunt d'un component per a vore la seua descripcio."
  !endif
!endif

!ifdef MUI_DIRECTORYPAGE
  ${LangFileString} MUI_TEXT_DIRECTORY_TITLE "Trie el lloc d'instalacio"
  ${LangFileString} MUI_TEXT_DIRECTORY_SUBTITLE "Trie el directori per a instalar $(^NameDA)."
!endif

!ifdef MUI_UNDIRECTORYSPAGE
  ${LangFileString} MUI_UNTEXT_DIRECTORY_TITLE "Triar lloc de desinstalacio"
  ${LangFileString} MUI_UNTEXT_DIRECTORY_SUBTITLE "Trie el directori des d'a on es desinstalarà $(^NameDA)."
!endif

!ifdef MUI_INSTFILESPAGE
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Instalant"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Per favor, espere mentres $(^NameDA) s'instala."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Instalacio Completa"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "L'instalacio s'ha completat correctament."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Instalacio Anulada"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "L'instalacio no s'ha completat correctament."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalant"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Per favor, espere mentres $(^NameDA) es desinstala."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Desinstalacio Completa"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "La desinstalacio s'ha completat correctament."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Desinstalacio Anulada"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "La desinstalacio no s'ha completat correctament."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Finalisant l'Assistent d'Instalacio de $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) s'ha instalat en son sistema.$\r$\n$\r$\nPulse Acabar per a tancar est assistent."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Son sistema es deu de reiniciar per a que puga completar-se l'instalacio de $(^NameDA). ¿Vol reiniciar ara?"
!endif

!ifdef MUI_UNFINISHPAGE
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TITLE "Finalisant l'Assistent de Desinstalacio de $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) s'ha desinstalat de son sistema.$\r$\n$\r$\nPulse Acabar per a tancar est assistent."
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_REBOOT "El seu ordenador deu de reiniciar-se per a completar la desinstalacio de $(^NameDA). ¿Vol reiniciar ara?"
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Reiniciar ara"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Vullc reiniciar manualment despres en acabant"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Eixecutar $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "&Vore Lligga'm"
!endif

!ifdef MUI_STARTMENUPAGE
  ${LangFileString} MUI_TEXT_STARTMENU_TITLE "Trie una Carpeta del Menu Inici"
  ${LangFileString} MUI_TEXT_STARTMENU_SUBTITLE "Trie una Carpeta del Menu Inici per als llançadors de $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_TOP "Seleccione una carpeta del Menu Inici a on vullga crear els llançadors del programa. Tambe pot introduir un nom per a crear-ne una nova carpeta."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_CHECKBOX "No crear llançadors"
!endif

!ifdef MUI_UNCONFIRMPAGE
  ${LangFileString} MUI_UNTEXT_CONFIRM_TITLE "Desinstalar $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_CONFIRM_SUBTITLE "Elimina $(^NameDA) de son sistema."
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "¿Està segur que vol eixir de l'instalacio de $(^Name)?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "¿Està segur de voler eixir de la desinstalacio de $(^Name)?"
!endif
