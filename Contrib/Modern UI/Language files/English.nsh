;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.66

;Language: English (1033)
;By Joost Verburg

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ENGLISH"

  !define MUI_LANGNAME "English" ;Use only ASCII characters (if this is not possible, use the English name)
  
  !define MUI_TEXT_WELCOME_INFO_TITLE "Welcome to the $(^Name) Setup Wizard"
  !define MUI_TEXT_WELCOME_INFO_TEXT "This wizard will guide you through the installation of $(^Name).\r\n\r\nIt is recommended that you close all other applications before starting Setup. This will allow Setup to update certain system files without rebooting your computer.\r\n\r\n$_CLICK"
  
  !define MUI_TEXT_LICENSE_TITLE "License Agreement"  
  !define MUI_TEXT_LICENSE_SUBTITLE "Please review the license terms before installing $(^Name)."
  !define MUI_INNERTEXT_LICENSE_TOP "Press Page Down to see the rest of the agreement."
  !define MUI_INNERTEXT_LICENSE_BOTTOM "If you accept the terms of the agreement, click I Agree to continue. You must accept the agreement to install $(^Name)."
  !define MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "If you accept the terms of the agreement, click the check box below. You must accept the agreement to install $(^Name). $_CLICK"
  !define MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "If you accept the terms of the agreement, select the first option below. You must accept the agreement to install $(^Name). $_CLICK"
  
  !define MUI_TEXT_COMPONENTS_TITLE "Choose Components"
  !define MUI_TEXT_COMPONENTS_SUBTITLE "Choose which features of $(^Name) you want to install."
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Description"
  !define MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Hover your mouse over a component to see its description."
  
  !define MUI_TEXT_DIRECTORY_TITLE "Choose Install Location"
  !define MUI_TEXT_DIRECTORY_SUBTITLE "Choose the folder in which to install $(^Name)."
  
  !define MUI_TEXT_INSTALLING_TITLE "Installing"
  !define MUI_TEXT_INSTALLING_SUBTITLE "Please wait while $(^Name) is being installed."
  
  !define MUI_TEXT_FINISH_TITLE "Installation Complete"
  !define MUI_TEXT_FINISH_SUBTITLE "Setup was completed successfully."
  
  !define MUI_TEXT_ABORT_TITLE "Installation Aborted"
  !define MUI_TEXT_ABORT_SUBTITLE "Setup was not completed successfully."
  
  !define MUI_BUTTONTEXT_FINISH "&Finish"
  !define MUI_TEXT_FINISH_INFO_TITLE "Completing the $(^Name) Setup Wizard"
  !define MUI_TEXT_FINISH_INFO_TEXT "$(^Name) has been installed on your computer.\r\n\r\nClick Finish to close this wizard."
  !define MUI_TEXT_FINISH_INFO_REBOOT "Your computer must be restarted in order to complete the installation of $(^Name). Do you want to reboot now?"
  !define MUI_TEXT_FINISH_REBOOTNOW "Reboot now"
  !define MUI_TEXT_FINISH_REBOOTLATER "I want to manually reboot later"
  !define MUI_TEXT_FINISH_RUN "&Run $(^Name)"
  !define MUI_TEXT_FINISH_SHOWREADME "&Show Readme"
  
  !define MUI_TEXT_STARTMENU_TITLE "Choose Start Menu Folder"
  !define MUI_TEXT_STARTMENU_SUBTITLE "Choose a Start Menu folder for the $(^Name) shortcuts."
  !define MUI_INNERTEXT_STARTMENU_TOP "Select the Start Menu folder in which you would like to create the program's shortcuts. You can also enter a name to create a new folder."
  !define MUI_INNERTEXT_STARTMENU_CHECKBOX "Do not create shortcuts"
  
  !define MUI_TEXT_ABORTWARNING "Are you sure you want to quit $(^Name) Setup?"  
  
  
  !define MUI_UNTEXT_WELCOME_INFO_TITLE "Welcome to the $(^Name) Uninstall Wizard"
  !define MUI_UNTEXT_WELCOME_INFO_TEXT "This wizard will guide you through the uninstallation of $(^Name).\r\n\r\nBefore starting the uninstallation, make sure $(^Name) is not running.\r\n\r\n$_CLICK"
  
  !define MUI_UNTEXT_CONFIRM_TITLE "Uninstall $(^Name)"
  !define MUI_UNTEXT_CONFIRM_SUBTITLE "Remove $(^Name) from your computer."
  
  !define MUI_UNTEXT_LICENSE_TITLE "License Agreement"  
  !define MUI_UNTEXT_LICENSE_SUBTITLE "Please review the license terms before uninstalling $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM "If you accept the terms of the agreement, click I Agree to continue. You must accept the agreement to uninstall $(^Name)."
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "If you accept the terms of the agreement, click the check box below. You must accept the agreement to uninstall $(^Name). $_CLICK"
  !define MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "If you accept the terms of the agreement, select the first option below. You must accept the agreement to uninstall $(^Name). $_CLICK"
  
  !define MUI_UNTEXT_COMPONENTS_TITLE "Choose Components"
  !define MUI_UNTEXT_COMPONENTS_SUBTITLE "Choose which features of $(^Name) you want to uninstall."
  
  !define MUI_UNTEXT_DIRECTORY_TITLE "Choose Uninstall Location"
  !define MUI_UNTEXT_DIRECTORY_SUBTITLE "Choose the folder from which to uninstall $(^Name)."
  
  !define MUI_UNTEXT_UNINSTALLING_TITLE "Uninstalling"
  !define MUI_UNTEXT_UNINSTALLING_SUBTITLE "Please wait while $(^Name) is being uninstalled."
    
  !define MUI_UNTEXT_FINISH_TITLE "Uninstallation Complete"
  !define MUI_UNTEXT_FINISH_SUBTITLE "Uninstall was completed successfully."
  
  !define MUI_UNTEXT_ABORT_TITLE "Uninstallation Aborted"
  !define MUI_UNTEXT_ABORT_SUBTITLE "Uninstall was not completed successfully."
  
  !define MUI_UNTEXT_FINISH_INFO_TITLE "Completing the $(^Name) Uninstall Wizard"
  !define MUI_UNTEXT_FINISH_INFO_TEXT "$(^Name) has been uninstalled from your computer.\r\n\r\nClick Finish to close this wizard."
  
  !define MUI_UNTEXT_ABORTWARNING "Are you sure you want to quit $(^Name) Uninstall?"  
  
!insertmacro MUI_LANGUAGEFILE_END