;This file contains default strings. These strings will be used when no string exists in one of the language files.
;Only for backwards compatibility.

;--------------------------------

  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_WELCOME_INFO_TITLE "Welcome to the $(^NameDA) Setup Wizard"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_WELCOME_INFO_TEXT "This wizard will guide you through the installation of $(^NameDA).\r\n\r\nIt is recommended that you close all other applications before starting Setup. This will make it possible to update relevant system files without having to reboot your computer.\r\n\r\n$_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_LICENSE_TITLE "License Agreement"  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_LICENSE_SUBTITLE "Please review the license terms before installing $(^NameDA)."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_INNERTEXT_LICENSE_TOP "Press Page Down to see the rest of the agreement."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_INNERTEXT_LICENSE_BOTTOM "If you accept the terms of the agreement, click I Agree to continue. You must accept the agreement to install $(^NameDA)."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX "If you accept the terms of the agreement, click the check box below. You must accept the agreement to install $(^NameDA). $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "If you accept the terms of the agreement, select the first option below. You must accept the agreement to install $(^NameDA). $_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_COMPONENTS_TITLE "Choose Components"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_COMPONENTS_SUBTITLE "Choose which features of $(^NameDA) you want to install."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Description"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Hover your mouse over a component to see its description."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_DIRECTORY_TITLE "Choose Install Location"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_DIRECTORY_SUBTITLE "Choose the folder in which to install $(^NameDA)."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_INSTALLING_TITLE "Installing"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_INSTALLING_SUBTITLE "Please wait while $(^NameDA) is being installed."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_FINISH_TITLE "Installation Complete"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_FINISH_SUBTITLE "Setup was completed successfully."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_ABORT_TITLE "Installation Aborted"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_ABORT_SUBTITLE "Setup was not completed successfully."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_BUTTONTEXT_FINISH "&Finish"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_FINISH_INFO_TITLE "Completing the $(^NameDA) Setup Wizard"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_FINISH_INFO_TEXT "$(^NameDA) has been installed on your computer.\r\n\r\nClick Finish to close this wizard."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_FINISH_INFO_REBOOT "Your computer must be restarted in order to complete the installation of $(^NameDA). Do you want to reboot now?"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_FINISH_REBOOTNOW "Reboot now"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_FINISH_REBOOTLATER "I want to manually reboot later"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_FINISH_RUN "&Run $(^NameDA)"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_FINISH_SHOWREADME "&Show Readme"
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_STARTMENU_TITLE "Choose Start Menu Folder"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_STARTMENU_SUBTITLE "Choose a Start Menu folder for the $(^NameDA) shortcuts."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_INNERTEXT_STARTMENU_TOP "Select the Start Menu folder in which you would like to create the program's shortcuts. You can also enter a name to create a new folder."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_INNERTEXT_STARTMENU_CHECKBOX "Do not create shortcuts"
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_TEXT_ABORTWARNING "Are you sure you want to quit $(^Name) Setup?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_WELCOME_INFO_TITLE "Welcome to the $(^NameDA) Uninstall Wizard"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_WELCOME_INFO_TEXT "This wizard will guide you through the uninstallation of $(^NameDA).\r\n\r\nBefore starting the uninstallation, make sure $(^NameDA) is not running.\r\n\r\n$_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_CONFIRM_TITLE "Uninstall $(^NameDA)"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_CONFIRM_SUBTITLE "Remove $(^NameDA) from your computer."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_LICENSE_TITLE "License Agreement"  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_LICENSE_SUBTITLE "Please review the license terms before uninstalling $(^NameDA)."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNINNERTEXT_LICENSE_BOTTOM "If you accept the terms of the agreement, click I Agree to continue. You must accept the agreement to uninstall $(^NameDA)."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX "If you accept the terms of the agreement, click the check box below. You must accept the agreement to uninstall $(^NameDA). $_CLICK"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS "If you accept the terms of the agreement, select the first option below. You must accept the agreement to uninstall $(^NameDA). $_CLICK"
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_COMPONENTS_TITLE "Choose Components"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_COMPONENTS_SUBTITLE "Choose which features of $(^NameDA) you want to uninstall."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_DIRECTORY_TITLE "Choose Uninstall Location"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_DIRECTORY_SUBTITLE "Choose the folder from which to uninstall $(^NameDA)."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_UNINSTALLING_TITLE "Uninstalling"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_UNINSTALLING_SUBTITLE "Please wait while $(^NameDA) is being uninstalled."
    
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_FINISH_TITLE "Uninstallation Complete"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_FINISH_SUBTITLE "Uninstall was completed successfully."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_ABORT_TITLE "Uninstallation Aborted"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_ABORT_SUBTITLE "Uninstall was not completed successfully."
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_FINISH_INFO_TITLE "Completing the $(^NameDA) Uninstall Wizard"
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_FINISH_INFO_TEXT "$(^NameDA) has been uninstalled from your computer.\r\n\r\nClick Finish to close this wizard."
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_FINISH_INFO_REBOOT "Your computer must be restarted in order to complete the uninstallation of $(^NameDA). Do you want to reboot now?"
  
  !insertmacro MUI_LANGUAGEFILE_DEFAULT MUI_UNTEXT_ABORTWARNING "Are you sure you want to quit $(^Name) Uninstall?"