;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: French (1036)
;By Sébastien Delahaye <seb@delahaye.net>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "FRENCH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Français" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Cliquez sur Suivant pour continuer." 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Cliquez sur Installer pour démarrer l'installation."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licence utilisateur"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Veuillez examiner les termes de la licence avant d'installer ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Appuyez sur Page Down pour lire le reste de la licence utilisateur."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Si vous acceptez tous les termes de la licence utilisateur, cliquez sur J'accepte pour continuer. Vous devez accepter la licence utilisateur pour installer ${MUI_PRODUCT}."  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Choisissez les composants"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Choisissez les composants que vous souhaitez installer."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS "Cochez les composants que vous souhaitez installer, et décochez ceux que vous ne voulez pas installer."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Description"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Passer le curseur de votre souris sur un composant pour voir sa description."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Choisissez le dossier d'installation"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Choisissez le dossier dans lequel installer ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Le programme d'installation va maintenant installer ${MUI_PRODUCT} dans le dossier suivant.$\r$\n$\r$\nPour l'installer dans un autre dossier, cliquez sur Parcourir et choisissez un autre dossier."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Dossier d'installation"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Installation en cours"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Veuillez patienter pendant que ${MUI_PRODUCT} est en train d'être installé sur votre ordinateur."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_TITLE "Installation terminée"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_SUBTITLE "L'installation s'est terminée avec succès."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE "Dossier du menu Démarrer" 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Choisissez un dossier dans le menu Démarrer" 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Choisissez un dossier dans le menu Démarrer pour les raccourcis de l'application." 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU "Choisissez le dossier du menu Démarrer dans lequel vous voulez placer les raccourcis du programme. Vous pouvez également entrer un nouveau nom pour créer un nouveau dossier."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Êtes-vous sûr de vouloir quitter l'installation de ${MUI_PRODUCT} ?"
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Désinstaller ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Supprimer ${MUI_PRODUCT} de votre ordinateur."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Ce programme va supprimer ${MUI_PRODUCT} de votre ordinateur."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Désinstallation en cours"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Veuillez patienter pendant que ${MUI_PRODUCT} est en train d'être supprimé de votre ordinateur."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Désinstallation terminée"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "La désinstallation s'est terminée avec succès."
    
!insertmacro MUI_LANGUAGEFILE_END