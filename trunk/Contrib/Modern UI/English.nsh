;Modern UI Language File
;version 1 - Compatible with Modern UI 1.3

;Language: English (1033)
;By Joost Verburg

;--------------------------------

!ifndef MUI_ENGLISH_USED

!define MUI_ENGLISH_USED

  ;INSTALLER
  
  !macro MUI_ENGLISH_LICENSETEXT
     LicenseText /LANG=${LANG_ENGLISH} "Press Page Down to see the rest of the agreement."
  !macroend
  
  !macro MUI_ENGLISH_COMPONENTTEXT
    ComponentText /LANG=${LANG_ENGLISH} "Check the components you want to install and uncheck the components you don't want to install. Click Next to continue." " "
  !macroend
  
  !macro MUI_ENGLISH_DIRTEXT
    DirText /LANG=${LANG_ENGLISH} "Setup will install ${NAME} in the following folder.$\r$\n$\r$\nTo install in this folder, click Install. To install in a different folder, click Browse and select another folder." " "
  !macroend
  
  !macro MUI_ENGLISH_INSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_ENGLISH} "Next >"
  !macroend
   
  LangString MUI_TEXT_LICENSE_TITLE ${LANG_ENGLISH} "License Agreement"  
  LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_ENGLISH} "Please review the license terms before installing ${NAME}."
  LangString MUI_INNERTEXT_LICENSE ${LANG_ENGLISH} "If you accept all the terms of the agreement, choose I Agree to continue. If you choose Cancel, Setup will close. You must accept the agreement to install ${NAME}."
  
  LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_ENGLISH} "Choose Components"
  LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_ENGLISH} "Choose the components you want to install."
  LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_ENGLISH} "Description"
  LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_ENGLISH} "Hover your mouse over a component to see it's description."
  
  LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_ENGLISH} "Choose Install Location"
  LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_ENGLISH} "Choose the folder in which to install ${NAME}."
  LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_ENGLISH} "Destination Folder"
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_ENGLISH} "Installing"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_ENGLISH} "Please wait while ${NAME} is being installed."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_ENGLISH} "Finished"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_ENGLISH} "Setup was completed successfully."
  
  LangString MUI_MSGTEXT_ABORTWARNING ${LANG_ENGLISH} "Are you sure you want to quit ${NAME} Setup?"

  LangString MUI_BUTTONTEXT_BACK ${LANG_ENGLISH} "< Back"
  LangString MUI_BUTTONTEXT_NEXT ${LANG_ENGLISH} "Next > "
  LangString MUI_BUTTONTEXT_CANCEL ${LANG_ENGLISH} "Cancel"
  LangString MUI_BUTTONTEXT_INSTALL ${LANG_ENGLISH} "Install"

  
  ;UNINSTALLER
  
  !macro MUI_ENGLISH_UNINSTALLTEXT
    UninstallText /LANG=${LANG_ENGLISH} "This will uninstall ${NAME} from your system."
  !macroend
  
  !macro MUI_ENGLISH_UNINSTALLBUTTONTEXT_NEXT
    InstallButtonText /LANG=${LANG_ENGLISH} "Next >"
  !macroend
  
  LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_ENGLISH} "Uninstall ${NAME}"
  LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_ENGLISH} "Remove ${NAME} from your system."
  
  LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_ENGLISH} "Uninstalling"
  LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_ENGLISH} "Please wait while ${NAME} is being uninstalled."
  
  LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_ENGLISH} "Finished"
  LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_ENGLISH} "Uninstall was completed successfully."
  
  LangString un.MUI_BUTTONTEXT_BACK ${LANG_ENGLISH} "Back > "
  LangString un.MUI_BUTTONTEXT_NEXT ${LANG_ENGLISH} "Next > "
  LangString un.MUI_BUTTONTEXT_CANCEL ${LANG_ENGLISH} "Cancel"
  LangString un.MUI_BUTTONTEXT_INSTALL ${LANG_ENGLISH} "Install"
    
!endif