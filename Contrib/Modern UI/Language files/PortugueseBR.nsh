;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Brazilian Portuguese (1046)
;By Diego Marcos <jump@osite.com.br>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "PortugueseBR"

  !define MUI_LANGNAME "Português (do Brasil)" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Bem-vindo ao Assistente de Instalação do $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Este assistente o guiará durante a instalação do $(^NameDA).\r\n\r\nÉ recomendado que você feche todas as outras aplicações antes de iniciar a Instalação. Isto possibilitará fazer update dos arquivos do sistema sem reiniciar o computador.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Contrato de Licença"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Por favor, verifique os termos da licença antes de instalar o $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Tecle Page Down para ver o restante da licença."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Se você aceitar os termos da licença, clique em Concordo para continuar. Você deve aceitar o contrato para instalar o $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se você aceitar os termos da licença, clique na caixa de seleção abaixo. Você deve aceitar o contrato para instalar o $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se você aceitar os termos da licença, selecione a primeira opção abaixo. Você deve aceitar o contrato para instalar o $(^NameDA). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Escolha de Componentes"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Escolha quais características do $(^NameDA) que você deseja instalar."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descrição"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Passe o ponteiro do mouse sobre um componente para ver sua descrição."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Selecione um componente para ver sua descrição."
  !endif

  !define MUI_TEXT_DIRECTORY_TITLE "Escolha do Local da Instalação"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Escolha a pasta na qual deseja instalar o $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Instalando"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Por favor, aguarde enquanto o $(^NameDA) está sendo instalado."
  
  !define MUI_TEXT_FINISH_TITLE "Instalação Completada"
  !define MUI_TEXT_FINISH_SUBTITLE "A instalação foi concluída com sucesso."
  
  !define MUI_TEXT_ABORT_TITLE "Instalação Abortada"
  !define MUI_TEXT_ABORT_SUBTITLE "A instalação não foi concluída com sucesso."

  !define MUI_BUTTONTEXT_FINISH "&Terminar"
  !define MUI_TEXT_FINISH_INFO_TITLE "Concluindo o Assistente de Instalação do $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) foi instalado no seu computador.\r\n\r\nClique em Terminar para fechar este assistente."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Seu computador deve ser reiniciado para concluír a instalação do $(^NameDA). Você quer reiniciar agora?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reiniciar Agora"
  !define MUI_TEXT_FINISH_REBOOTLATER "Eu quero reiniciar manualmente depois"
  !define MUI_TEXT_FINISH_RUN "&Executar $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Mostrar Leiame"
  
  !define MUI_TEXT_STARTMENU_TITLE "Escolha uma Pasta do Menu Iniciar"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Escolha uma pasta do Menu Iniciar para os atalhos do programa."
  !define MUI_INNERTEXT_STARTMENU_TOP "Selecione uma pasta do Menu Iniciar em que deseja criar os atalhos do programa. Você pode também digitar um nome para criar uma nova pasta. "
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Não criar atalhos"  
  
  !define MUI_TEXT_ABORTWARNING "Você deseja realmente finalizar a instalação do $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Bem-vindo ao Assistente de Desinstalação do $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Este assistente o guiará durante a desinstalação do $(^NameDA).\r\n\r\nAntes de iniciar a desinstalação, tenha certeza que o $(^NameDA) não está sendo executado.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Desinstalar o $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Remover o $(^NameDA) do seu computador."
  
  !define MUI_UNTEXT_LICENSE_TITLE "Contrato de Licença"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Por favor, verifique os termos da licença antes de desinstalar o $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Se você aceitar os termos da licença, clique em Concordo para continuar. Você deve aceitar o contrato para desinstalar o $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se você aceitar os termos da licença, clique na caixa de seleção abaixo. Você deve aceitar o contrato para desinstalar o $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se você aceitar os termos da licença, selecione a primeira opção abaixo. Você deve aceitar o contrato para desinstalar o $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Escolher Componentes"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Escolha quais qualidades do $(^NameDA) quer desinstalar."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Escolha do Local da Desinstalação"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Escolha a pasta na qual deseja desinstalar o $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalando"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Por favor, aguarde enquanto o $(^NameDA) está sendo desinstalado."
  
  !define MUI_UNTEXT_FINISH_TITLE "Desinstalação Completada"
  !define MUI_UNTEXT_FINISH_SUBTITLE "A desinstalação foi concluída com sucesso."
  
  !define MUI_UNTEXT_ABORT_TITLE "Desinstalação Abortada"
  !define MUI_UNTEXT_ABORT_SUBTITLE "A desinstalação não foi concluída com sucesso"

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Concluindo o Assistente de Desinstalação do $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) foi desinstalado do seu computador.\r\n\r\nClique em Terminar para fechar este assistente."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Seu computador tem que ser reiniciado para completar a desinstalação do $(^NameDA). Quer reiniciar agora?"

  !define MUI_UNTEXT_ABORTWARNING "Você deseja realmente finalizar a desinstalação do $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END
