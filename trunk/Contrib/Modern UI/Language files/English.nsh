;NSIS Modern User Interface - Language File
;version 2 - Compatible with Modern UI 1.4

;Language: English (1033)
;By Joost Verburg

;--------------------------------
!verbose 3

!ifndef MUI_ENGLISH_USED

!define MUI_ENGLISH_USED

  LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"

  !define MUI_ENGLISH_LANGNAME "English" ;Name of the language in the language itself (English, Deutsch, Français etc.)

  ;INSTALLER
  Name /LANG=${LANG_ENGLISH} "${MUI_NAME}"
  
  !ifdef MUI_LICENSEPAGE
     LicenseText /LANG=${LANG_ENGLISH} "Press Page Down to see the rest of the agreement."
     LangString MUI_TEXT_LICENSE_TITLE ${LANG_ENGLISH} "License Agreement"  
     LangString MUI_TEXT_LICENSE_SUBTITLE ${LANG_ENGLISH} "Please review the license terms before installing ${MUI_PRODUCT}."
     LangString MUI_INNERTEXT_LICENSE ${LANG_ENGLISH} "If you accept all the terms of the agreement, choose I Agree to continue. You must accept the agreement to install ${MUI_PRODUCT}."
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    ComponentText /LANG=${LANG_ENGLISH} "Check the components you want to install and uncheck the components you don't want to install. Click Next to continue."
    LangString MUI_TEXT_COMPONENTS_TITLE ${LANG_ENGLISH} "Choose Components"
    LangString MUI_TEXT_COMPONENTS_SUBTITLE ${LANG_ENGLISH} "Choose which features of ${MUI_PRODUCT} you want to install."
    LangString MUI_INNERTEXT_DESCRIPTION_TITLE ${LANG_ENGLISH} "Description"
    LangString MUI_INNERTEXT_DESCRIPTION_INFO ${LANG_ENGLISH} "Hover your mouse over a component to see it's description."
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    DirText /LANG=${LANG_ENGLISH} "Setup will install ${MUI_PRODUCT} in the following folder.$\r$\n$\r$\nTo install in this folder, click Install. To install in a different folder, click Browse and select another folder."
    LangString MUI_TEXT_DIRSELECT_TITLE ${LANG_ENGLISH} "Choose Install Location"
    LangString MUI_TEXT_DIRSELECT_SUBTITLE ${LANG_ENGLISH} "Choose the folder in which to install ${MUI_PRODUCT}."
    LangString MUI_INNERTEXT_DESTINATIONFOLDER ${LANG_ENGLISH} "Destination Folder"
  !endif
  
  LangString MUI_TEXT_INSTALLING_TITLE ${LANG_ENGLISH} "Installing"
  LangString MUI_TEXT_INSTALLING_SUBTITLE ${LANG_ENGLISH} "Please wait while ${MUI_PRODUCT} is being installed."
  
  LangString MUI_TEXT_FINISHED_TITLE ${LANG_ENGLISH} "Finished"
  LangString MUI_TEXT_FINISHED_SUBTITLE ${LANG_ENGLISH} "Setup was completed successfully."
  
  !ifdef MUI_ABORTWARNING
    LangString MUI_TEXT_ABORTWARNING ${LANG_ENGLISH} "Are you sure you want to quit ${MUI_PRODUCT} Setup?"
  !endif
  
  !ifdef MUI_INSTALLOPTIONS
    LangString MUI_TEXT_SETUPCAPTION ${LANG_ENGLISH} "${MUI_NAME} Setup"
  !endif
  
  
  ;UNINSTALLER
  
  !ifdef MUI_UNINSTALLER
    UninstallText /LANG=${LANG_ENGLISH} "This will uninstall ${MUI_PRODUCT} from your system."
    LangString un.MUI_UNTEXT_INTRO_TITLE ${LANG_ENGLISH} "Uninstall ${MUI_PRODUCT}"
    LangString un.MUI_UNTEXT_INTRO_SUBTITLE ${LANG_ENGLISH} "Remove ${MUI_PRODUCT} from your system."
  
    LangString un.MUI_UNTEXT_UNINSTALLING_TITLE ${LANG_ENGLISH} "Uninstalling"
    LangString un.MUI_UNTEXT_UNINSTALLING_SUBTITLE ${LANG_ENGLISH} "Please wait while ${MUI_PRODUCT} is being uninstalled."
  
    LangString un.MUI_UNTEXT_FINISHED_TITLE ${LANG_ENGLISH} "Finished"
    LangString un.MUI_UNTEXT_FINISHED_SUBTITLE ${LANG_ENGLISH} "Uninstall was completed successfully."
  !endif
  
  !ifdef MUI_UNINSTALLOPTIONS
    LangString un.MUI_UNTEXT_SETUPCAPTION ${LANG_ENGLISH} "${MUI_NAME} Uninstall"
  !endif
  
!endif

!verbose 4