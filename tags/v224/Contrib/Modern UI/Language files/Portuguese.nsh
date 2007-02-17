;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Portuguese (2070)
;By Ramon <ramon@netcabo.pt>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Portuguese"

  !define MUI_LANGNAME "Português" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Bem vindo ao Assistente de Instalação do $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Este assistente ajudá-lo-á durante a instalação do $(^NameDA).\r\n\r\nÉ recomendado que feche todas as outras aplicações antes de iniciar a Instalação. Isto permitirá que o Instalador actualize ficheiros relacionados com o sistema sem necessidade de reiniciar o computador.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Contrato de Licença"
  !define MUI_TEXT_LICENSE_SUBTITLE "Por favor, verifique os termos da licença antes de instalar o $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Tecle Page Down para ver o restante da licença."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Se aceitar os termos da licença, clique em 'Aceito' para continuar. Deverá aceitar o contrato para instalar o $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se aceitar os termos da licença, clique na caixa de seleção abaixo. Deverá aceitar o contrato para instalar o $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se aceitar os termos da licença, selecione a primeira opção abaixo. Você deve aceitar o contrato para instalar o $(^NameDA). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Escolha de Componentes"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Escolha quais as características do $(^NameDA) que deseja instalar."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descrição"
!ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Posicione o rato sobre um componente para ver a sua descrição."
  !else
    !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Seleccione um componente para ver a sua descrição."
  !endif
  
  !define MUI_TEXT_DIRECTORY_TITLE "Escolha do Local da Instalação"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Escolha a pasta na qual deseja instalar o $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "Instalando"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Por favor, aguarde enquanto o $(^NameDA) está sendo instalado."
  
  !define MUI_TEXT_FINISH_TITLE "Instalação Completa"
  !define MUI_TEXT_FINISH_SUBTITLE "A instalação foi concluída com sucesso."
  
  !define MUI_TEXT_ABORT_TITLE "Instalação Abortada"
  !define MUI_TEXT_ABORT_SUBTITLE "A instalação não foi concluída com sucesso."

  !define MUI_BUTTONTEXT_FINISH "&Terminar"
  !define MUI_TEXT_FINISH_INFO_TITLE "Concluindo o Assistente de Instalação do $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) foi instalado no seu computador.\r\n\r\nClique em Terminar para fechar este assistente."
  !define MUI_TEXT_FINISH_INFO_REBOOT "O seu computador deve ser reiniciado para concluír a instalação do $(^NameDA). Deseja reiniciar agora?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reiniciar Agora"
  !define MUI_TEXT_FINISH_REBOOTLATER "Eu quero reiniciar manualmente depois"
  !define MUI_TEXT_FINISH_RUN "&Executar $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Mostrar Leiame"
  
  !define MUI_TEXT_STARTMENU_TITLE "Escolha uma Pasta do Menu Iniciar"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Escolha uma pasta do Menu Iniciar para os atalhos do programa."
  !define MUI_INNERTEXT_STARTMENU_TOP "Selecione uma pasta do Menu Iniciar em que deseja criar os atalhos do programa. Você pode também digitar um nome para criar uma nova pasta. "
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Não criar atalhos"  
  
  !define MUI_TEXT_ABORTWARNING "Deseja realmente cancelar a instalação do $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Bem vindo ao Assistente de desinstalação do $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Este assistente ajudá-lo-á durante a desinstalação do $(^NameDA).\r\n\r\nAntes de iniciar a desinstalação, certifique-se de que o $(^NameDA) não está em execução.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Desinstalar $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Remover o $(^NameDA) do seu computador."
 
  !define MUI_UNTEXT_LICENSE_TITLE "Contrato de Licença"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Por favor, verifique os termos da licença antes de desinstalar o $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM  "Se aceitar os termos da licença, clique em 'Aceito' para continuar. Deverá aceitar o contrato para desinstalar o $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se aceitar os termos da licença, clique na caixa de seleção abaixo. Deverá aceitar o contrato para desinstalar o $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se aceitar os termos da licença, selecione a primeira opção abaixo. Você deve aceitar o contrato para desinstalar o $(^NameDA). $_CLICK"
    
  !define MUI_UNTEXT_COMPONENTS_TITLE "Escolher Componentes"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Escolha quais as características do $(^NameDA) que deseja desinstalar."
    
  !define MUI_UNTEXT_DIRECTORY_TITLE "Escolha o Local de desinstalação"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Escolha a pasta de onde pretende desinstalar o $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalando"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Por favor, aguarde enquanto o $(^NameDA) está sendo desinstalado."
  
  !define MUI_UNTEXT_FINISH_TITLE "Desinstalação Completa"
  !define MUI_UNTEXT_FINISH_SUBTITLE "A desinstalação foi concluída com sucesso."
  
  !define MUI_UNTEXT_ABORT_TITLE "Desinstalação Abortada"
  !define MUI_UNTEXT_ABORT_SUBTITLE "A desinstalação não foi concluída com sucesso"

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Concluíndo o assistente de desisntalação do $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) foi removido do seu computador.\r\n\r\nClique em Terminar para fechar este assistente."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "O seu computador deve ser reiniciado para concluír a desinstalação do $(^NameDA). Deseja reiniciar agora?"
  
  !define MUI_UNTEXT_ABORTWARNING "Deseja realmente cancelar a desinstalação do $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END