;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.64

;Language: Brazilian Portuguese (1046)
;By Diego Marcos <jump@osite.com.br>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "PORTUGUESEBR"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Português (do Brasil)" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Clique em Avançar para continuar."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Clique em Instalar para iniciar a instalação."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Bem vindo ao Assistente de Instalação do ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Este assistente te guiará durante a instalação do ${MUI_PRODUCT}.\r\n\r\nÉ recomendado que você feche todas as outras aplicações antes de iniciar a Instalação. Isto permitirá que o Instalador faça update dos arquivos de sistema sem reiniciar o computador.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Contrato de Licença"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Por favor, verifique os termos da licença antes de instalar o ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Tecle Page Down para ver o restante da licença."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Se você aceitar os termos da licença, clique em Concordo para continuar. Você deve aceitar o contrato para instalar o ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se você aceitar os termos da licença, clique na caixa de seleção abaixo. Você deve aceitar o contrato para instalar o ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se você aceitar os termos da licença, selecione a primeira opção abaixo. Você deve aceitar o contrato para instalar o ${MUI_PRODUCT}."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Escolha de Componentes"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Escolha quais características do ${MUI_PRODUCT} que você deseja instalar."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Marque os componentes que você deseja instalar e desmarque os componentes que você não deseja instalar."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descrição"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Passe o ponteiro do mouse sobre um componente para ver sua descrição."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Escolha do Local da Instalação"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Escolha a pasta na qual deseja instalar o ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "O instalador instalará o ${MUI_PRODUCT} na pasta abaixo.$\r$\n$\r$\nPara instalar em uma pasta diferente, clique em Procurar e selecione outra pasta."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Pasta de destino"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Instalando"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Por favor, aguarde enquanto o ${MUI_PRODUCT} está sendo instalado."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Instalação Concluída"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "A instalação foi finalizada com sucesso."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Terminar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Concluindo o Assistente de Instalação do ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} foi instalado no seu computador.\r\n\r\nClique em Terminar para fechar este assistente."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Seu computador deve ser reiniciado para concluír a instalação do ${MUI_PRODUCT}. Você quer reiniciar agora?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Reiniciar Agora"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Eu quero reiniciar manualmente depois"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "&Executar ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "&Mostrar Leiame"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Escolha uma Pasta do Menu Iniciar"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Escolha uma pasta do Menu Iniciar para os atalhos do programa."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Selecione uma pasta do Menu Iniciar em que deseja criar os atalhos do programa. Você pode também digitar um nome para criar uma nova pasta. "
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Não criar atalhos"  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Você deseja realmente finalizar a instalação do ${MUI_PRODUCT}?"


  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Clique em Desinstalar para começar a desinstalação."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Desinstalar o ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Remover o ${MUI_PRODUCT} do seu computador."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Este assistente desinstalará o ${MUI_PRODUCT} do seu computador."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalando"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Por favor, aguarde enquanto o ${MUI_PRODUCT} está sendo desinstalado."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_TITLE "Desinstalação Concluída"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISH_SUBTITLE "A desinstalação foi finalizada com sucesso."

!insertmacro MUI_LANGUAGEFILE_END