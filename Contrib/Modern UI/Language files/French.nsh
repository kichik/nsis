;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: French (1036)
;By Sébastien Delahaye <seb@delahaye.net>

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "FRENCH"

  !define MUI_LANGNAME "Français" ;Use only ASCII characters (if this is not possible, use the English name)

  !define MUI_TEXT_WELCOME_INFO_TITLE "Bienvenue dans le programme d'installation de $(^Name)"
  !define MUI_TEXT_WELCOME_INFO_TEXT "Vous êtes sur le point d'installer $(^Name) sur votre ordinateur.\r\n\r\nAvant de débuter l'installation, il est recommandé de fermer toutes les autres applications. Cela permettra au programme d'installation de mettre à jour certains fichiers système sans avoir besoin de redémarrer votre ordinateur.\r\n\r\n"

  !define MUI_TEXT_LICENSE_TITLE "Licence utilisateur"
  !define MUI_TEXT_LICENSE_SUBTITLE "Veuillez examiner les termes de la licence avant d'installer $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Appuyez sur Page Down pour lire le reste de la licence utilisateur."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "Si vous acceptez les termes de la licence utilisateur, cliquez sur J'accepte pour continuer. Vous devez accepter la licence utilisateur pour installer $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si vous acceptez les termes de la licence utilisateur, cochez la case ci-dessous. Vous devez accepter la licence utilisateur pour installer $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si vous acceptez les termes de la licence utilisateur, sélectionnez le premier choix ci-dessous. Vous devez accepter la licence utilisateur pour installer $(^Name)."

  !define MUI_TEXT_COMPONENTS_TITLE "Choisissez les composants"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Choisissez les composants de $(^Name) que vous souhaitez installer."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Description"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Passer le curseur de votre souris sur un composant pour voir sa description."

  !define MUI_TEXT_DIRECTORY_TITLE "Choisissez le dossier d'installation"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Choisissez le dossier dans lequel installer $(^Name)."
  !define MUI_TEXT_INSTALLING_TITLE "Installation en cours"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Veuillez patienter pendant que $(^Name) est en train d'être installé."

  !define MUI_TEXT_FINISH_TITLE "Installation terminée"
  !define MUI_TEXT_FINISH_SUBTITLE "L'installation s'est terminée avec succès."

  !define MUI_TEXT_ABORT_TITLE "Installation interrompue"
  !define MUI_TEXT_ABORT_SUBTITLE "L'installation n'a pas été terminée."

  !define MUI_BUTTONTEXT_FINISH "&Fermer"
  !define MUI_TEXT_FINISH_INFO_TITLE "Fin de l'installation de $(^Name)"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) a été installé sur votre ordinateur.\r\n\r\nCliquez sur Fermer pour quitter le programme d'installation."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Votre ordinateur doit être redémarré afin de compléter l'installation de $(^Name). Souhaitez-vous redémarrer maintenant ?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Redémarrer maintenant"
  !define MUI_TEXT_FINISH_REBOOTLATER "Je souhaite redémarrer moi-même plus tard"
  !define MUI_TEXT_FINISH_RUN "Lancer $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "Afficher le fichier Readme"

  !define MUI_TEXT_STARTMENU_TITLE "Choisissez un dossier dans le menu Démarrer"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Choisissez un dossier dans le menu Démarrer pour les raccourcis de l'application."
  !define MUI_INNERTEXT_STARTMENU_TOP "Choisissez le dossier du menu Démarrer dans lequel vous voulez placer les raccourcis du programme. Vous pouvez également entrer un nouveau nom pour créer un nouveau dossier."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Ne pas créer de raccourcis"

  !define MUI_TEXT_ABORTWARNING "Êtes-vous sûr de vouloir quitter l'installation de $(^Name) ?"


  !define MUI_UNTEXT_CONFIRM_TITLE "Désinstaller $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Supprimer $(^Name) de votre ordinateur."

  !define MUI_UNTEXT_LICENSE_TITLE "Licence utilisateur"
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Veuillez examiner les termes de la licence avant de désinstaller $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "Si vous acceptez les termes de la licence utilisateur, cliquez sur J'accepte pour continuer. Vous devez accepter la licence utilisateur pour désinstaller $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "Si vous acceptez les termes de la licence utilisateur, cochez la case ci-dessous. Vous devez accepter la licence utilisateur pour dàsintaller $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "Si vous acceptez les termes de la licence utilisateur, sélectionnez le premier choix ci-dessous. Vous devez accepter la licence utilisateur pour désinstaller $(^Name)."

  !define MUI_UNTEXT_UNINSTALLING_TITLE "Désinstallation en cours"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Veuillez patienter pendant que $(^Name) est en train d'être supprimé de votre ordinateur."

  !define MUI_UNTEXT_COMPONENTS_TITLE "Choisissez les composants"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Cochez les composants de $(^Name) que vous souhaitez désinstaller."

  !define MUI_UNTEXT_DIRECTORY_TITLE "Choisissez le dossier d'installation"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Choisissez le dossier dans lequel vous voulez désinstaller $(^Name)."

  !define MUI_UNTEXT_FINISH_TITLE "Désinstallation terminée"
  !define MUI_UNTEXT_FINISH_SUBTITLE "La désinstallation s'est terminée avec succès."

  !define MUI_UNTEXT_ABORT_TITLE "Désinstallation interrompue"
  !define MUI_UNTEXT_ABORT_SUBTITLE "La désinstallation n'a pas été terminée."

!insertmacro MUI_LANGUAGEFILE_END