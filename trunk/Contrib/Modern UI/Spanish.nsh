;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: Spanish (1034)
;By MoNKi

;--------------------------------

!ifndef MUI_SPANISH_USED

!define MUI_SPANISH_USED

  !define MUI_SPANISH_LANGNAME "Español" ;Name of the language in the language itself (English, Deutsch, Français etc.)

  ;INSTALLER
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_SPANISH} "Presione Avanzar Página para ver el resto del acuerdo."
  !endif
  
  !ifdef MUI_COMPONENTPAGE
    ComponentText /LANG=${LANG_SPANISH} "Marque los componentes que desea instalar y desmarque los componentes que no desea instalar. Presione Siguiente para continuar."
  !endif
  
  !ifdef MUI_DIRSELECTPAGE
    DirText /LANG=${LANG_SPANISH} "Se instalará ${NAME} en el siguiente directorio.$\r$\n$\r$\nPara instalar en este directorio, presione Instalar. Para instalar en un directorio distinto, presione Buscar y seleccione otro directorio." " "
  !endif
  
  !ifdef MUI_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_SPANISH} "Siguiente >"
  !endif
   
  LangString MUI_TEXT_LICENSE_TITLE ${LANG_SPANISH} "Acuerdo de licencia"  
  LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_SPANISH} "Por favor revise los términos de la licencia antes de instalar ${NAME}."
  LangString MUI_INNERTEXT_LICENSE ${LANG_SPANISH} "Si acepta todos los términos del acuerdo, seleccione Acepto para continuar. Debe aceptar el acuerdo para instalar ${NAME}."
  
  LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_SPANISH} "Selección de componentes"
  LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_SPANISH} "Seleccione que características de ${NAME} desea instalar."
  LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_SPANISH} "Descripción"
  LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_SPANISH} "Situe el ratón encima de un componente para ver su descripción."
  
  LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_SPANISH} "Elegir lugar de instalación"
  LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_SPANISH} "Elija el directorio en el que instalar ${NAME}."
  LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_SPANISH} "Directorio de destino"
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_SPANISH} "Instalando"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_SPANISH} "Por favor espere mientas ${NAME} se instala."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_SPANISH} "Completado"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_SPANISH} "La instalación se ha completado correctamente."
  
  LangString MUI_MSGTEXT_ABORTWARNING ${LANG_SPANISH} "¿Está seguro de que desea salir de la instalación de ${NAME}?"

  LangString MUI_BUTTONTEXT_BACK ${LANG_SPANISH} "< Atrás"
  LangString MUI_BUTTONTEXT_NEXT ${LANG_SPANISH} "Siguiente >"
  LangString MUI_BUTTONTEXT_CANCEL ${LANG_SPANISH} "Cancelar"
  LangString MUI_BUTTONTEXT_INSTALL ${LANG_SPANISH} "Instalar"

  
  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_SPANISH} "Esto desinstalará ${NAME} de su sistema."
  !endif
  
  !ifdef MUI_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_SPANISH} "Siguiente >"
  !endif
  
  LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_SPANISH} "Desinstalar ${NAME}"
  LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_SPANISH} "Elimina ${NAME} de su sistema."
  
  LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_SPANISH} "Desinstalando"
  LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_SPANISH} "Por favor espere mientras ${NAME} se desinstala."
  
  LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_SPANISH} "Completado"
  LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_SPANISH} "La desinstalación se ha completado correctamente."
  
  LangString un.MUI_BUTTONTEXT_BACK ${LANG_SPANISH} "< Atrás"
  LangString un.MUI_BUTTONTEXT_NEXT ${LANG_SPANISH} "Siguiente >"
  LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_SPANISH} "Cancelar"
  LangString un.MUI_BUTTONTEXT_UNINSTALL ${LANG_SPANISH} "Desinstalar"
    
!endif