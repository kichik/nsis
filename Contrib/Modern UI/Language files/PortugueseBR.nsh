;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: Portuguese (1046)
;By Layout do Brasil www.layoutdobrasil.com

;--------------------------------

!ifndef MUI_PORTUGUESEBR_USED

!define MUI_PORTUGUESEBR_USED

  !define MUI_PORTUGUESEBR_LANGNAME "Portuguese (Brasil)" ;Name of the language in the language itself (English, Deutsch, Français etc.)

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_PORTUGUESEBR} "Pressione Page Down para ver o restante da licença."
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_PORTUGUESEBR} "Contrato de Licença"  
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_PORTUGUESEBR} "Favor verificar os termos da licença antes de instalar ${NAME}."
     LangString MUI_INNERTEXT_LICENSE ${LANG_PORTUGUESEBR} "Se você aceitar todos os termos da licença, escolha Concordo para continuar. Você deve aceitar o contrato para instalar ${NAME}."
  !endif
  
  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_PORTUGUESEBR} "Marque os componentes que você deseja instalar e desmarque os componentes que você não deseja instalar. Clique em Avançar para continuar."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_PORTUGUESEBR} "Escolhendo Componentes"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_PORTUGUESEBR} "Escolha as características do ${NAME} que você deseja instalar."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_PORTUGUESEBR} "Descrição"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_PORTUGUESEBR} "Repouse o ponteiro do mouse em cima de um componente para ver sua descrição."
  !endif
  
  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_PORTUGUESEBR} "O instalador instalará o ${NAME} na pasta abaixo.$\r$\n$\r$\nPara prosseguir e instalar nesta pasta, clique em Instalar. Para instalar em uma pasta diferente, clique Procurar... e selecione outra pasta." " "
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_PORTUGUESEBR} "Escolhendo o Local da Instalação"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_PORTUGUESEBR} "Escolha a pasta na qual deseja instalar ${NAME}."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_PORTUGUESEBR} "Pasta de destino"
  !endif
  
  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_PORTUGUESEBR} "&Próximo >"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_PORTUGUESEBR} "Instalando"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_PORTUGUESEBR} "Por favor, aguarde enquanto ${NAME} está sendo instalado."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_PORTUGUESEBR} "Finalizando"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_PORTUGUESEBR} "A instalação foi finalizada com sucesso."
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_MSGTEXT_ABORTWARNING ${LANG_PORTUGUESEBR} "Você deseja realmente finalizar a instalação ${NAME}?"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_BUTTONTEXT_BACK ${LANG_PORTUGUESEBR} "< &Voltar"
    LangString MUI_BUTTONTEXT_NEXT ${LANG_PORTUGUESEBR} "&Avançar >"
    LangString MUI_BUTTONTEXT_CANCEL ${LANG_PORTUGUESEBR} "Cancelar"
    LangString MUI_BUTTONTEXT_INSTALL ${LANG_PORTUGUESEBR} "&Instalar"
  !endif


  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_PORTUGUESEBR} "Isto desinstalará ${NAME} de seu sistema."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_PORTUGUESEBR} "Desinstalar ${NAME}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_PORTUGUESEBR} "Remover ${NAME} de seu sistema."
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_PORTUGUESEBR} "Desinstalando"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_PORTUGUESEBR} "Por favor, aguarde enquanto ${NAME} está sendo desinstalado."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_PORTUGUESEBR} "Finalizando"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_PORTUGUESEBR} "A desinstalação foi finalizada com sucesso."
  !endif
  
  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_PORTUGUESEBR} "&Próximo >"
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_BUTTONTEXT_BACK ${LANG_PORTUGUESEBR} "< &Voltar"
    LangString un.MUI_BUTTONTEXT_NEXT ${LANG_PORTUGUESEBR} "&Avançar >"
    LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_PORTUGUESEBR} "Cancelar"
    LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_PORTUGUESEBR} "&Desinstalar"
  !endif  
    
!endif