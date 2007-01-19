;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Galician (1110)
;Ramon Flores <fa2ramon@usc.es>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Galician"

  !define MUI_LANGNAME "Galego" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Benvindo ao Asistente de Instalación do $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Este asistente vai axudá-lo durante a instalación do $(^NameDA).\r\n\r\nRecomenda-se fechar todas as outras aplicacións antes de iniciar a instalación. Isto posibilita actualizar os ficheiros do sistema relevantes sen ter que reiniciar o computador.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Contrato de licenza"
  !define MUI_TEXT_LICENSE_SUBTITLE "Por favor, verifique os termos da licenza antes de instalar o $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Prema Page Down para ver o restante da licenza."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Se aceitar os termos da licenza, clique en 'Aceito' para continuar. Cumpre aceitar o contrato para instalar o $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se aceitar os termos da licenza, clique na caixa de selección abaixo. Cumpre aceitar o contrato para instalar o $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se aceitar os termos da licenza, seleccione a primeira opción abaixo. Cumpre aceitar o contrato para instalar o $(^NameDA). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Escolla de componentes"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Escolla que características do $(^NameDA) que desexa instalar."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descrición"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Posicione o rato sobre un componente para ver a sua descrición."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Seleccione un componente para ver a sua descrición."
  !endif

  !define MUI_TEXT_DIRECTORY_TITLE "Escolla do local da instalación"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Escolla a directória na cal desexa instalar o $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Instalando"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Por favor, agarde entanto o $(^NameDA) está sendo instalado."

  !define MUI_TEXT_FINISH_TITLE "Instalación completa"
  !define MUI_TEXT_FINISH_SUBTITLE "A instalación concluiu con suceso."

  !define MUI_TEXT_ABORT_TITLE "Instalación Abortada"
  !define MUI_TEXT_ABORT_SUBTITLE "A instalación concluiu sen suceso."

  !define MUI_BUTTONTEXT_FINISH "&Rematar"
  !define MUI_TEXT_FINISH_INFO_TITLE "Concluindo o Asistente de instalación do $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "Instalou-se o $(^NameDA) no seu computador.\r\n\r\nClique en Rematar para fechar este asistente."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Cumpre reiniciar o seu computador para concluír a instalación do $(^NameDA). Desexa reiniciar agora?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reiniciar agora"
  !define MUI_TEXT_FINISH_REBOOTLATER "Prefiro reinicia-lo manualmente despois"
  !define MUI_TEXT_FINISH_RUN "&Executar $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Mostrar Leame"

  !define MUI_TEXT_STARTMENU_TITLE "Escolla un cartafol do Menu Iniciar"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Escolla un cartafol do Menu Iniciar para os atallos do programa."
  !define MUI_INNERTEXT_STARTMENU_TOP "Seleccione o cartafol do Menu Iniciar no que desexa criar os atallos do programa. Tamén é posíbel dixitar un nome para criar un novo cartafol. "
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Non criar atallos"

  !define MUI_TEXT_ABORTWARNING "Realmente desexa cancelar a instalación do $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Benvindo ao Asistente de desinstalación do $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Este asistente vai axudá-lo durante a desinstalación do $(^NameDA).\r\n\r\nAntes de iniciar a desinstalación, certifique-se de que o $(^NameDA) non está a executar-se.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Desinstalar $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Eliminar o $(^NameDA) do seu computador."

  !define MUI_UNTEXT_LICENSE_TITLE "Contrato de licenza"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Por favor, verifique os termos da licenza antes de desinstalar o $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM  "Se aceitar os termos da licenza, clique en 'Aceito' para continuar. Cumpre aceitar o contrato para desinstalar o $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se aceitar os termos da licenza, clique na caixa de selección abaixo. Cumpre aceitar o contrato para desinstalar o $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se aceitar os termos da licenza, seleccione a primeira opción abaixo. Cumpre aceitar o contrato para desinstalar o $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Escoller componentes"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Escolla que características do $(^NameDA) desexa desinstalar."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Escolla o Local de desinstalación"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Escolla a directória de onde pretende desinstalar o $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalando"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Por favor, agarde entanto se desinstala o $(^NameDA)."

  !define MUI_UNTEXT_FINISH_TITLE "Desinstalación completa"
  !define MUI_UNTEXT_FINISH_SUBTITLE "A desinstalación concluiu con suceso."

  !define MUI_UNTEXT_ABORT_TITLE "Desinstalación abortada"
  !define MUI_UNTEXT_ABORT_SUBTITLE "A desinstalación non concluiu con suceso"

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Concluíndo o asistente de desinstalación do $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "Eliminou-se $(^NameDA) do seu computador.\r\n\r\nClique em Rematar para fechar este asistente."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Cumpre reiniciar o seu computador para concluír a desinstalación do $(^NameDA). Desexa reiniciá-lo agora?"

  !define MUI_UNTEXT_ABORTWARNING "Realmente desexa cancelar a desinstalación do $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END
