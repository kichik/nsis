;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.5

;Language: English (1033)
;By Joost Verburg

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ENGLISH"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "English" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "License Agreement"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Please review the license terms before installing ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Press Page Down to see the rest of the agreement."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "If you accept all the terms of the agreement, choose I Agree to continue. You must accept the agreement to install ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Choose Components"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Choose which features of ${MUI_PRODUCT} you want to install."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS "Check the components you want to install and uncheck the components you don't want to install. Click Next to continue."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Description"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Hover your mouse over a component to see it's description."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Choose Install Location"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Choose the folder in which to install ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Setup will install ${MUI_PRODUCT} in the following folder.$\r$\n$\r$\nTo install in this folder, click Install. To install in a different folder, click Browse and select another folder."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Destination Folder"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "Installing"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Please wait while ${MUI_PRODUCT} is being installed."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_TITLE "Finished"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISHED_SUBTITLE "Setup was completed successfully."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_WINDOWTITLE "Start Menu Folder"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Choose Start Menu Folder"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Choose a Start Menu folder for the program's shortcuts."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU "Select the Start Menu folder in which you would like to create the program's shotcuts. You can also enter a name to create a new folder. Click Install to start the installation."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Are you sure you want to quit ${MUI_PRODUCT} Setup?"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WINDOWTITLE "${MUI_NAME} Setup"
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Uninstall ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Remove ${MUI_PRODUCT} from your system."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "This will uninstall ${MUI_PRODUCT} from your system."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "Uninstalling"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Please wait while ${MUI_PRODUCT} is being uninstalled."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Finished"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Uninstall was completed successfully."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_WINDOWTITLE "${MUI_NAME} Uninstall"
  
!insertmacro MUI_LANGUAGEFILE_END