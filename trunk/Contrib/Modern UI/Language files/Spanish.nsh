;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Spanish (1034)
;By MoNKi & Lobo Lunar

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "SPANISH"

  !define MUI_LANGNAME "Español" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Bienvenido al Asistente de Instalación de $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Este programa instalará $(^NameDA) en su ordenador.\r\n\r\nSe recomienda que cierre todas las demás aplicaciones antes de iniciar la instalación. Esto hará posible actualizar archivos relacionados con el sistema sin tener que reiniciar su ordenador.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Acuerdo de licencia"
  !define MUI_TEXT_LICENSE_SUBTITLE "Por favor revise los términos de la licencia antes de instalar $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Presione Avanzar Página para ver el resto del acuerdo."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Si acepta todos los términos del acuerdo, seleccione Acepto para continuar. Debe aceptar el acuerdo para instalar $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si acepta los términos del acuerdo, marque abajo la casilla. Debe aceptar los términos para instalar $(^NameDA). $_CLICK" 
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si acepta los términos del acuerdo, seleccione abajo la primera opción. Debe aceptar los términos para instalar $(^NameDA). $_CLICK" 
  
  !define MUI_TEXT_COMPONENTS_TITLE "Selección de componentes"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Seleccione qué características de $(^NameDA) desea instalar."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descripción"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Sitúe el ratón encima de un componente para ver su descripción."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Elegir lugar de instalación"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Elija el directorio para instalar $(^NameDA)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Instalando"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Por favor espere mientras $(^NameDA) se instala."
  
  !define MUI_TEXT_FINISH_TITLE "Instalación Completada"
  !define MUI_TEXT_FINISH_SUBTITLE "La instalación se ha completado correctamente."

  !define MUI_TEXT_ABORT_TITLE "Instalación Anulada"
  !define MUI_TEXT_ABORT_SUBTITLE "La instalación no se completó correctamente."

  !define MUI_BUTTONTEXT_FINISH "&Terminar"
  !define MUI_TEXT_FINISH_INFO_TITLE "Completando el Asistente de Instalación de $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) ha sido instalado en su sistema.\r\n\r\nPresione Terminar para cerrar este asistente."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Su sistema debe ser reiniciado para que pueda completarse la instalación de $(^NameDA). ¿Desea reiniciar ahora?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reiniciar ahora"
  !define MUI_TEXT_FINISH_REBOOTLATER "Deseo reiniciar manualmente más tarde"
  !define MUI_TEXT_FINISH_RUN "&Ejecutar $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Ver Léame"

  !define MUI_TEXT_STARTMENU_TITLE "Elegir Carpeta del Menú Inicio"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Elija una Carpeta del Menú Inicio para los accesos directos de $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Seleccione una carpeta del Menú Inicio en la que quiera crear los accesos directos del programa. También puede introducir un nombre para crear una nueva carpeta."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "No crear accesos directos"
  
  !define MUI_TEXT_ABORTWARNING "¿Está seguro de que desea salir de la instalación de $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Bienvenido al Asistente de Desinstalación de $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Este asistente le guiará durante la desinstalación de $(^NameDA).\r\n\r\nAntes de comenzar la desinstalación, asegúrese de que $(^NameDA) no se está ejecutando.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Desinstalar $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Elimina $(^NameDA) de su sistema."

  !define MUI_UNTEXT_LICENSE_TITLE "Acuerdo de licencia"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Por favor revise los términos de la licencia antes de desinstalar $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Si acepta todos los términos del acuerdo, seleccione Acepto para continuar. Debe aceptar el acuerdo para desinstalar $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si acepta los términos del acuerdo, marque abajo la casilla. Debe aceptar los términos para desinstalar $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si acepta los términos del acuerdo, seleccione abajo la primera opción. Debe aceptar los términos para desinstalar $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Selección de componentes"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Seleccione qué características de $(^NameDA) desea desinstalar."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Elegir lugar de desinstalación"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Elija el directorio desde el cual se desinstalará $(^NameDA)."
     
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Desinstalando"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Por favor espere mientras $(^NameDA) se desinstala."
  
  !define MUI_UNTEXT_FINISH_TITLE "Desinstalación Completada"
  !define MUI_UNTEXT_FINISH_SUBTITLE "La desinstalación se ha completado correctamente."

  !define MUI_UNTEXT_ABORT_TITLE "Desinstalación Anulada"
  !define MUI_UNTEXT_ABORT_SUBTITLE "La desinstalación no se completó correctamente."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "Completando el Asistente de Desinstalación de $(^NameDA)"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) ha sido desinstalado de su sistema.\r\n\r\nPresione Terminar para cerrar este asistente."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Su ordenador debe ser reiniciado para completar la desinstalación de $(^NameDA). ¿Desea reiniciar ahora?"
  !define MUI_UNTEXT_ABORTWARNING "¿Está seguro de que desea salir de la desinstalación de $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END