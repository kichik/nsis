;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.67

;Language: Portuguese (2070)
;By Ramon <ramon@netcabo.pt>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "PORTUGUESE"

  !define MUI_LANGNAME "Português" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Bem vindo ao Assistente de Instalação do $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Este assistente ajudá-lo-á durante a instalação do $(^Name).\r\n\r\nÉ recomendado que feche todas as outras aplicações antes de iniciar a Instalação. Isto permitirá que o Instalador actualize ficheiros de sistema sem reiniciar o computador.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "Contrato de Licença"
  !define MUI_TEXT_LICENSE_SUBTITLE "Por favor, verifique os termos da licença antes de instalar o $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Tecle Page Down para ver o restante da licença."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Se aceitar os termos da licença, clique em 'Aceito' para continuar. Deverá aceitar o contrato para instalar o $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se aceitar os termos da licença, clique na caixa de seleção abaixo. Deverá aceitar o contrato para instalar o $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se aceitar os termos da licença, selecione a primeira opção abaixo. Você deve aceitar o contrato para instalar o $(^Name). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Escolha de Componentes"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Escolha quais as características do $(^Name) que deseja instalar."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descrição"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Passe o ponteiro do rato sobre um componente para ver a sua descrição."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Escolha do Local da Instalação"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Escolha a pasta na qual deseja instalar o $(^Name)."

  !define MUI_TEXT_INSTALLING_TITLE "Instalando"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Por favor, aguarde enquanto o $(^Name) está sendo instalado."
  
  !define MUI_TEXT_FINISH_TITLE "Instalação Completa"
  !define MUI_TEXT_FINISH_SUBTITLE "A instalação foi concluída com sucesso."
  
  !define MUI_TEXT_ABORT_TITLE "Instalação Abortada"
  !define MUI_TEXT_ABORT_SUBTITLE "A instalação não foi concluída com sucesso."

  !define MUI_BUTTONTEXT_FINISH "&Terminar"
  !define MUI_TEXT_FINISH_INFO_TITLE "Concluindo o Assistente de Instalação do $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) foi instalado no seu computador.\r\n\r\nClique em Terminar para fechar este assistente."
  !define MUI_TEXT_FINISH_INFO_REBOOT "O seu computador deve ser reiniciado para concluír a instalação do $(^Name). Deseja reiniciar agora?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reiniciar Agora"
  !define MUI_TEXT_FINISH_REBOOTLATER "Eu quero reiniciar manualmente depois"
  !define MUI_TEXT_FINISH_RUN "&Executar $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Mostrar Leiame"
  
  !define MUI_TEXT_STARTMENU_TITLE "Escolha uma Pasta do Menu Iniciar"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Escolha uma pasta do Menu Iniciar para os atalhos do programa."
  !define MUI_INNERTEXT_STARTMENU_TOP "Selecione uma pasta do Menu Iniciar em que deseja criar os atalhos do programa. Você pode também digitar um nome para criar uma nova pasta. "
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Não criar atalhos"  
  
  !define MUI_TEXT_ABORTWARNING "Deseja realmente cancelar a instalação do $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Bem vindo ao Assistente de desinstalação do $(^Name)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Este assistente ajudá-lo-á durante a desinstalação do $(^Name).\r\n\r\nAntes de iniciar a desinstalação, certifique-se de que o $(^Name) não está em execução.\r\n\r\n$_CLICK
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Desinstalar $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Remover o $(^Name) do seu computador."
 
  !define MUI_UNTEXT_LICENSE_TITLE "Contrato de Licença"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Por favor, verifique os termos da licença antes de desinstalar o $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM  "Se aceitar os termos da licença, clique em 'Aceito' para continuar. Deverá aceitar o contrato para desinstalar o $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se aceitar os termos da licença, clique na caixa de seleção abaixo. Deverá aceitar o contrato para desinstalar o $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se aceitar os termos da licença, selecione a primeira opção abaixo. Você deve aceitar o contrato para desinstalar o $(^Name). $_CLICK"
    
  !define MUI_UNTEXT_COMPONENTS_TITLE "Escolher Componentes"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Escolha quais as características do $(^Name) que deseja desinstalar."
    
  !define MUI_UNTEXT_DIRECTORY_TITLE "Escolha o Local de desinstalação"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Escolha a pasta de onde pretende desinstalar o $(^Name)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalando"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Por favor, aguarde enquanto o $(^Name) está sendo desinstalado."
  
  !define MUI_UNTEXT_FINISH_TITLE "Desinstalação Completa"
  !define MUI_UNTEXT_FINISH_SUBTITLE "A desinstalação foi concluída com sucesso."
  
  !define MUI_UNTEXT_ABORT_TITLE "Desinstalação Abortada"
  !define MUI_UNTEXT_ABORT_SUBTITLE "A desinstalação não foi concluída com sucesso"

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Concluíndo o assistente de desisntalação do $(^Name)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name) foi removido do seu computador.\r\n\r\nClique em Terminar para fechar este assistente."
  
  !define MUI_UNTEXT_ABORTWARNING "Deseja realmente cancelar a desinstalação do $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END