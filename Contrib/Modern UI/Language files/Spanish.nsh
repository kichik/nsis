;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.4

;Language: Spanish (1034)
;By MoNKi

;--------------------------------
!verbose 3

!ifndef MUI_SPANISH_USED

!define MUI_SPANISH_USED

  LoadLanguageFile "${NSISDIR}\Contrib\Language files\Spanish.nlf"

  !define MUI_SPANISH_LANGNAME "Español" ;Name of the language in the language itself (English, Deutsch, Français etc.)

  ;INSTALLER
  Name /LANG=${LANG_SPANISH} "${MUI_NAME}"
  
  !ifdef MUI_LICENSEPAGE
    LicenseText /LANG=${LANG_SPANISH} "Presione Avanzar Página para ver el resto del acuerdo."
    LangString MUI_TEXT_LICENSE_TITLE ${LANG_SPANISH} "Acuerdo de licencia"  
    LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_SPANISH} "Por favor revise los términos de la licencia antes de instalar ${MUI_PRODUCT}."
    LangString MUI_INNERTEXT_LICENSE ${LANG_SPANISH} "Si acepta todos los términos del acuerdo, seleccione Acepto para continuar. Debe aceptar el acuerdo para instalar ${MUI_PRODUCT}."
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_SPANISH} "Marque los componentes que desea instalar y desmarque los componentes que no desea instalar. Presione Siguiente para continuar."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_SPANISH} "Selección de componentes"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_SPANISH} "Seleccione que características de ${MUI_PRODUCT} desea instalar."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_SPANISH} "Descripción"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_SPANISH} "Situe el ratón encima de un componente para ver su descripción."
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_SPANISH} "Se instalará ${MUI_PRODUCT} en el siguiente directorio.$\r$\n$\r$\nPara instalar en este directorio, presione Instalar. Para instalar en un directorio distinto, presione Examinar y seleccione otro directorio."
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_SPANISH} "Elegir lugar de instalación"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_SPANISH} "Elija el directorio en el que instalar ${MUI_PRODUCT}."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_SPANISH} "Directorio de destino"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_SPANISH} "Instalando"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_SPANISH} "Por favor espere mientas ${MUI_PRODUCT} se instala."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_SPANISH} "Completado"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_SPANISH} "La instalación se ha completado correctamente."
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_SPANISH} "¿Está seguro de que desea salir de la instalación de ${MUI_PRODUCT}?"
  !endif

  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_SPANISH} "Instalación de ${MUI_PRODUCT} ${MUI_VERSION}"
  !endif

  
  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_SPANISH} "Esto desinstalará ${MUI_PRODUCT} de su sistema."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_SPANISH} "Desinstalar ${MUI_PRODUCT}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_SPANISH} "Elimina ${MUI_PRODUCT} de su sistema."
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_SPANISH} "Desinstalando"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_SPANISH} "Por favor espere mientras ${MUI_PRODUCT} se desinstala."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_SPANISH} "Completado"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_SPANISH} "La desinstalación se ha completado correctamente."
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_SPANISH} "Instalación de ${MUI_PRODUCT} ${MUI_VERSION}"
  !endif
    
!endif

!verbose 4