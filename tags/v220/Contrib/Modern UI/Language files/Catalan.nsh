;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.68

;Language: Catalan (1027)
;By falanko, corrections by Toni Hermoso Pulido

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "Catalan"

  !define MUI_LANGNAME "Català" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Benvinguts a l'auxiliar d'instal·lació de l'aplicació $(^NameDA)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Aquest auxiliar us guiarà durant el procés d'instal·lació de l'aplicació $(^NameDA).\r\n\r\nEs recomana tancar la resta d'aplicacions abans de començar la instal·lació. Això permetrà al programa d'instal·ació actualitzar fitxers del sistema rellevants sense haver de reiniciar l'ordinador.\r\n\r\n$_CLICK"

  !define MUI_TEXT_LICENSE_TITLE "Acord de Llicència"
  !define MUI_TEXT_LICENSE_SUBTITLE "Reviseu els termes de la llicència abans d'instal·lar l'aplicació $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_TOP "Premeu AvPàg per a veure la resta de l'acord."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Si accepteu tots els termes de l'acord, premeu Hi estic d'acord per a continuar. Heu d'acceptar l'acord per a poder instal·lar l'aplicació $(^NameDA)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si accepteu tots els termes de l'acord, activeu la casella de sota. Heu d'acceptar l'acord per poder instal·lar l'aplicació $(^NameDA). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepteu tots els termes de l'acord, seleccioneu la primera opció de sota. Heu d'acceptar l'acord per a poder instal·lar $(^NameDA). $_CLICK"

  !define MUI_TEXT_COMPONENTS_TITLE "Selecció de components"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Seleccioneu quines característiques de l'aplicació $(^NameDA) desitgeu instal·lar."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descripció"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Situeu el ratolí damunt d'un component per a veure'n la descripció."

  !define MUI_TEXT_DIRECTORY_TITLE "Trieu una ubicació d'instal·lació"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Trieu la carpeta on instal·lar-hi l'aplicació $(^NameDA)."

  !define MUI_TEXT_INSTALLING_TITLE "S'està instal·lant"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Si us plau, espereu mentre l'aplicació $(^NameDA) s'instal·la."

  !define MUI_TEXT_FINISH_TITLE "S'ha acabat la instal·lació"
  !define MUI_TEXT_FINISH_SUBTITLE "La instal·lació ha acabat correctament."

  !define MUI_TEXT_ABORT_TITLE "S'ha abandonat la instal·lació"
  !define MUI_TEXT_ABORT_SUBTITLE "La instal·lació no ha acabat correctament."

  !define MUI_BUTTONTEXT_FINISH "&Finalitza"
  !define MUI_TEXT_FINISH_INFO_TITLE "S'està acabant l'auxiliar d'instal·lació de l'aplicació $(^NameDA)"
  !define MUI_TEXT_FINISH_INFO_TEXT "L'aplicació $(^NameDA) s'ha instal·lat a l'ordinador.\r\n\r\nFeu clic a Finalitza per a tancar aquest auxiliar."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Cal reiniciar l'ordinador perquè pugui acabar-se la instal·lació de l'aplicació $(^NameDA). Voleu reiniciar-lo ara?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reinicia ara"
  !define MUI_TEXT_FINISH_REBOOTLATER "Reinicia més tard manualment"
  !define MUI_TEXT_FINISH_RUN "Executa l'aplicació $(^NameDA)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Mostra el Llegeix-me"

  !define MUI_TEXT_STARTMENU_TITLE "Tria la carpeta del menú Inicia"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Tria una carpeta del menú Inicia per a les dreceres de l'aplicació $(^NameDA)."
  !define MUI_INNERTEXT_STARTMENU_TOP "Seleccioneu la carpeta del Menú Inicia en la que hi vulgueu crear les dreceres del programa. Podeu introduir-hi un altre nom si voleu crear una carpeta nova."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "No creïs les dreceres"

  !define MUI_TEXT_ABORTWARNING "Esteu segur que voleu sortir del programa d'instal·lació de l'aplicació $(^Name)?"


  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Benvinguts a l'auxiliar de desinstal·lació de l'aplicació $(^NameDA)"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "Aquest auxiliar us guiarà a través de la desinstal·lació de l'aplicació $(^NameDA).\r\n\rAbans de començar la desinstal·lació, assegureu-vos que l'aplicació $(^NameDA) no s'està executant.\r\n\r\n$_CLICK"

  !define MUI_UNTEXT_CONFIRM_TITLE "Desinstal·la l'aplicació $(^NameDA)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Suprimeix l'aplicació $(^NameDA) de l'ordinador."

  !define MUI_UNTEXT_LICENSE_TITLE "Acord de llicència"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Reviseu els termes de la llicència abans de desinstal·lar l'aplicació $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Si accepteu tots els termes de l'acord, premeu Hi estic d'Acord per a continuar. Heu d'acceptar l'acord per a poder desinstal·lar l'aplicació $(^NameDA)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si accepteu tots els termes de l'acord, activeu la casella de sota. Heu d'acceptar l'acord per a poder desinstal·lar l'aplicació $(^NameDA). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si accepteu tots els termes de l'acord, seleccioneu la primera opció de sota. Heu d'acceptar l'acord per a poder desinstal·lar l'aplicació $(^NameDA). $_CLICK"

  !define MUI_UNTEXT_COMPONENTS_TITLE "Selecció de components"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Seleccioneu quines característiques de l'aplicació $(^NameDA) desitgeu desinstal·lar."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Trieu la ubicació de desinstal·lació"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Trieu la carpeta d'on desinstal·lar l'aplicació $(^NameDA)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "S'està desinstal·lant"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Si us plau, espereu mentre l'aplicació $(^NameDA) es desinstal·la."

  !define MUI_UNTEXT_FINISH_TITLE "S'ha acabat la desinstal·lació"
  !define MUI_UNTEXT_FINISH_SUBTITLE "La desinstal·lació s'ha realitzat correctament."

  !define MUI_UNTEXT_ABORT_TITLE "S'ha abandonat la desinstal·lació"
  !define MUI_UNTEXT_ABORT_SUBTITLE "La desinstal·lació no ha acabat correctament."

  !define MUI_UNTEXT_FINISH_INFO_TITLE "S'està acabant l'auxiliar de desinstal·lació de l'aplicació $(^NameDA)."
  !define MUI_UNTEXT_FINISH_INFO_TEXT "L'aplicació $(^NameDA) s'ha desinstal·lat de l'ordinador.\r\n\r\nFeu clic a Finalitza per a tancar aquest auxiliar."
  !define MUI_UNTEXT_FINISH_INFO_REBOOT "Cal reiniciar l'ordinador perquè pugui acabar-se la desinstal·lació de l'aplicació $(^NameDA). Voleu reiniciar-lo ara?"

  !define MUI_UNTEXT_ABORTWARNING "Esteu segur que voleu sortir del programa de desinstal·lació de l'aplicació $(^Name)?"

!insertmacro MUI_LANGUAGEFILE_END
