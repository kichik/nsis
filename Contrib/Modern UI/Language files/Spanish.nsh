;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: Spanish (1034)
;By MoNKi

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SPANISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Español" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Acuerdo de licencia"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Por favor revise los términos de la licencia antes de instalar ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Presione Avanzar Página para ver el resto del acuerdo."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Si acepta todos los términos del acuerdo, seleccione Acepto para continuar. Debe aceptar el acuerdo para instalar ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Selección de componentes"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Seleccione que características de ${MUI_PRODUCT} desea instalar."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS "Marque los componentes que desea instalar y desmarque los componentes que no desea instalar. Presione Siguiente para continuar."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descripción"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Situe el ratón encima de un componente para ver su descripción."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Elegir lugar de instalación"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Elija el directorio en el que instalar ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Se instalará ${MUI_PRODUCT} en el siguiente directorio.$\r$\n$\r$\nPara instalar en este directorio, presione Instalar. Para instalar en un directorio distinto, presione Examinar y seleccione otro directorio."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Directorio de destino"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Instalando"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Por favor espere mientas ${MUI_PRODUCT} se instala."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_TITLE "Completado"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_SUBTITLE "La instalación se ha completado correctamente."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "¿Está seguro de que desea salir de la instalación de ${MUI_PRODUCT}?"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WINDOWTITLE "Instalación de ${MUI_NAME}"

  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Desinstalar ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Elimina ${MUI_PRODUCT} de su sistema."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Esto desinstalará ${MUI_PRODUCT} de su sistema."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalando"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Por favor espere mientras ${MUI_PRODUCT} se desinstala."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Completado"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "La desinstalación se ha completado correctamente."
   
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_WINDOWTITLE "Desinstalación de ${MUI_NAME}"
    
!insertmacro MUI_LANGUAGEFILE_END