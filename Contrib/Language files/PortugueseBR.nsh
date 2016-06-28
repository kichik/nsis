;Language: Brazilian Portuguese (1046)
;By Diego Pedroso and Felipe

!insertmacro LANGFILE "PortugueseBR" "Brazilian Portuguese" "Português Brasileiro" "Portugues Brasileiro"

!ifdef MUI_WELCOMEPAGE
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TITLE "Bem-vindo ao instalador do $(^NameDA)"
  ${LangFileString} MUI_TEXT_WELCOME_INFO_TEXT "O assistente guiará você pela instalação do $(^NameDA).$\r$\n$\r$\nÉ recomendado que você feche todos os outros aplicativos antes de iniciar o instalador. Isto tornará possível atualizar os arquivos de sistema relevantes sem ter que reiniciar seu computador.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_UNWELCOMEPAGE
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TITLE "Bem-vindo ao desinstalador do $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_WELCOME_INFO_TEXT "O assistente guiará você pela desinstalação do $(^NameDA).$\r$\n$\r$\nAntes de iniciar a desinstalação, tenha certeza de que o $(^NameDA) não está em execução.$\r$\n$\r$\n$_CLICK"
!endif

!ifdef MUI_LICENSEPAGE
  ${LangFileString} MUI_TEXT_LICENSE_TITLE "Acordo de licença"
  ${LangFileString} MUI_TEXT_LICENSE_SUBTITLE "Por favor reveja os termos da licença antes de instalar o $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM "Se você aceita os termos do acordo, clique em Eu Concordo pra continuar. Você deve aceitar o acordo para instalar o $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se você aceita os termos do acordo, clique na caixa de seleção abaixo. Você deve aceitar o acordo para instalar o $(^NameDA). $_CLICK"
  ${LangFileString} MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se você aceita os termos do acordo, selecione a primeira opção abaixo. Você deve aceitar o acordo para instalar o $(^NameDA). $_CLICK"
!endif

!ifdef MUI_UNLICENSEPAGE
  ${LangFileString} MUI_UNTEXT_LICENSE_TITLE "Acordo de licença"
  ${LangFileString} MUI_UNTEXT_LICENSE_SUBTITLE "Por favor reveja os termos da licença antes de desinstalar o $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM "Se você aceita os termos do acordo, clique em Eu Concordo pra continuar. Você deve aceitar o acordo para desinstalar o $(^NameDA)."
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Se você aceita os termos do acordo, clique na caixa de seleção abaixo. Você deve aceitar o acordo para desinstalar o $(^NameDA). $_CLICK"
  ${LangFileString} MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Se você aceita os termos do acordo, selecione a primeira opção abaixo. Você deve aceitar o acordo para desinstalar o $(^NameDA). $_CLICK"
!endif

!ifdef MUI_LICENSEPAGE | MUI_UNLICENSEPAGE
  ${LangFileString} MUI_INNERTEXT_LICENSE_TOP "Pressione Page Down pra ver o resto do acordo."
!endif

!ifdef MUI_COMPONENTSPAGE
  ${LangFileString} MUI_TEXT_COMPONENTS_TITLE "Escolher componentes"
  ${LangFileString} MUI_TEXT_COMPONENTS_SUBTITLE "Escolha quais funções do $(^NameDA) você quer instalar."
!endif

!ifdef MUI_UNCOMPONENTSPAGE
  ${LangFileString} MUI_UNTEXT_COMPONENTS_TITLE "Escolher componentes"
  ${LangFileString} MUI_UNTEXT_COMPONENTS_SUBTITLE "Escolha quais funções do $(^NameDA) você quer desinstalar."
!endif

!ifdef MUI_COMPONENTSPAGE | MUI_UNCOMPONENTSPAGE
  ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descrição"
  !ifndef NSIS_CONFIG_COMPONENTPAGE_ALTERNATIVE
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Posicione a seta sobre um componente pra ver sua descrição."
  !else
    ${LangFileString} MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Selecione um componente pra ver sua descrição."
  !endif
!endif

!ifdef MUI_DIRECTORYPAGE
  ${LangFileString} MUI_TEXT_DIRECTORY_TITLE "Escolher o local da instalação"
  ${LangFileString} MUI_TEXT_DIRECTORY_SUBTITLE "Escolha a pasta na qual instalar o $(^NameDA)."
!endif

!ifdef MUI_UNDIRECTORYPAGE
  ${LangFileString} MUI_UNTEXT_DIRECTORY_TITLE "Escolher o local da desinstalação"
  ${LangFileString} MUI_UNTEXT_DIRECTORY_SUBTITLE "Escolha a pasta da qual desinstalar o $(^NameDA)."
!endif

!ifdef MUI_INSTFILESPAGE
  ${LangFileString} MUI_TEXT_INSTALLING_TITLE "Instalando"
  ${LangFileString} MUI_TEXT_INSTALLING_SUBTITLE "Por favor espere enquanto o $(^NameDA) está sendo instalado."
  ${LangFileString} MUI_TEXT_FINISH_TITLE "Instalação completa"
  ${LangFileString} MUI_TEXT_FINISH_SUBTITLE "O instalador concluiu com sucesso."
  ${LangFileString} MUI_TEXT_ABORT_TITLE "Instalação anulada"
  ${LangFileString} MUI_TEXT_ABORT_SUBTITLE "O instalador não concluiu com sucesso."
!endif

!ifdef MUI_UNINSTFILESPAGE
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalando"
  ${LangFileString} MUI_UNTEXT_UNINSTALLING_SUBTITLE "Por favor espere enquanto o $(^NameDA) está sendo desinstalado."
  ${LangFileString} MUI_UNTEXT_FINISH_TITLE "Desinstalação completa"
  ${LangFileString} MUI_UNTEXT_FINISH_SUBTITLE "A desinstalação concluiu com sucesso."
  ${LangFileString} MUI_UNTEXT_ABORT_TITLE "Desinstalação anulada"
  ${LangFileString} MUI_UNTEXT_ABORT_SUBTITLE "A desinstalação não concluiu com sucesso."
!endif

!ifdef MUI_FINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_INFO_TITLE "Completando a instalação do $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_INFO_TEXT "O $(^NameDA) foi instalado no seu computador.$\r$\n$\r$\nClique em Concluir pra fechar o instalador."
  ${LangFileString} MUI_TEXT_FINISH_INFO_REBOOT "Seu computador deve ser reiniciado para completar a instalação do $(^NameDA). Você quer reiniciar agora?"
!endif

!ifdef MUI_UNFINISHPAGE
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TITLE "Concluindo a desinstalação do $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_TEXT "O $(^NameDA) foi desinstalado do seu computador.$\r$\n$\r$\nClique em Concluir pra fechar o instalador."
  ${LangFileString} MUI_UNTEXT_FINISH_INFO_REBOOT "Seu computador deve ser reiniciado para completar a desinstalação do $(^NameDA). Você quer reiniciar agora?"
!endif

!ifdef MUI_FINISHPAGE | MUI_UNFINISHPAGE
  ${LangFileString} MUI_TEXT_FINISH_REBOOTNOW "Reiniciar agora"
  ${LangFileString} MUI_TEXT_FINISH_REBOOTLATER "Eu quero reiniciar manualmente depois"
  ${LangFileString} MUI_TEXT_FINISH_RUN "&Executar o $(^NameDA)"
  ${LangFileString} MUI_TEXT_FINISH_SHOWREADME "&Mostrar o leiame"
  ${LangFileString} MUI_BUTTONTEXT_FINISH "&Concluir"
!endif

!ifdef MUI_STARTMENUPAGE
  ${LangFileString} MUI_TEXT_STARTMENU_TITLE "Escolher a pasta do menu Iniciar"
  ${LangFileString} MUI_TEXT_STARTMENU_SUBTITLE "Escolher uma pasta do menu Iniciar para os atalhos do $(^NameDA)."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_TOP "Selecione a pasta do menu Iniciar na qual você gostaria de criar os atalhos do programa. Você pode também digitar um nome para criar uma nova pasta."
  ${LangFileString} MUI_INNERTEXT_STARTMENU_CHECKBOX "Não criar atalhos"
!endif

!ifdef MUI_UNCONFIRMPAGE
  ${LangFileString} MUI_UNTEXT_CONFIRM_TITLE "Desinstalar o $(^NameDA)"
  ${LangFileString} MUI_UNTEXT_CONFIRM_SUBTITLE "Remover o $(^NameDA) do seu computador."
!endif

!ifdef MUI_ABORTWARNING
  ${LangFileString} MUI_TEXT_ABORTWARNING "Você tem certeza que você quer sair do instalador do $(^Name)?"
!endif

!ifdef MUI_UNABORTWARNING
  ${LangFileString} MUI_UNTEXT_ABORTWARNING "Você tem certeza que você quer sair do desinstalador do $(^Name)?"
!endif

!ifdef MULTIUSER_INSTALLMODEPAGE
  ${LangFileString} MULTIUSER_TEXT_INSTALLMODE_TITLE "Escolher usuários"
  ${LangFileString} MULTIUSER_TEXT_INSTALLMODE_SUBTITLE "Escolha para quais usuários você quer instalar o $(^NameDA)."
  ${LangFileString} MULTIUSER_INNERTEXT_INSTALLMODE_TOP "Selecione se você quer instalar o $(^NameDA) só pra si mesmo ou pra todos os usuários deste computador. $(^ClickNext)"
  ${LangFileString} MULTIUSER_INNERTEXT_INSTALLMODE_ALLUSERS "Instalar pra qualquer um usando este computador"
  ${LangFileString} MULTIUSER_INNERTEXT_INSTALLMODE_CURRENTUSER "Instalar apenas pra mim"
!endif
