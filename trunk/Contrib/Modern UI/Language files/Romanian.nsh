;NSIS Modern User Interface - Language File
;Compatible with Modern UI 1.63

;Language: Romanian (1048)
;Translated by Cristian Pirvu (pcristip@yahoo.com)
;verion 0.1 (any comments and suggestions are welcomed)

;--------------------------------

!insertmacro MUI_LANGUAGEFILE_BEGIN "ROMANIAN"

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_LANGNAME "Romana" ;Name of the language in the language itself

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_NAME "${MUI_PRODUCT} ${MUI_VERSION}"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_NEXT "Alege Inainte pentru a continua."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_CONTINUE_INSTALL "Alege Instalare pentru a incepe instalarea."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TITLE "Bine ai venit la instalarea produsului ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_WELCOME_INFO_TEXT "Acum ${MUI_PRODUCT} incepe instalarea.\r\n\r\nE recomandat sa inchizi toate aplicatiile inainte. Aceasta va permite setup-ului sa modifice anumite fisiere de sistem fara repornirea calculatorului.\r\n\r\n"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_TITLE "Licenta de utilizare"  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_LICENSE_SUBTITLE "Citeste cu atentie termenii licentei inaintea instalarii ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_TOP "Apasa Page Down pentru a vedea restul licentei."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_LICENSE_BOTTOM "Daca accepti termenii licentei, alege De Acord pentru a continua. Trebuie sa accepti licenta ca sa instalezi ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_TITLE "Alege componente"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_COMPONENTS_SUBTITLE "Alege componentele produsului ${MUI_PRODUCT} pe care vrei sa le instalezi."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_TOP "Selecteaza componentele de instalat si deselecteaza-le pe celelalte." 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE "Descriere"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO "Misca mouse-ul deasupra unei componente pentru a vedea descrierea."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_TITLE "Alege locatia instalarii" 
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_DIRECTORY_SUBTITLE "Alege directorul de instalat al produsului ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_TOP "Setup-ul va instala ${MUI_PRODUCT} in urmatorul director.$\r$\n$\r$\nPentru a instala in alt director, apasa Alege pentru a alege alt director."

  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_DIRECTORY_DESTINATION "Directorul destinatie"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_TITLE "In process de instalare"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_INSTALLING_SUBTITLE "Asteapta, ${MUI_PRODUCT} se instaleaza."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_TITLE "Instalare completa"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SUBTITLE "Instalarea s-a completat cu succes."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_BUTTON "&Termina"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TITLE "Terminarea instalarii pentru ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_TEXT "${MUI_PRODUCT} a fost instalat.\r\n\r\nApasa Termina pentru a incheia instalarea."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_INFO_REBOOT "Trebuie sa restartezi calculatorul pentru a termina instalarea. Vrei sa restartezi acum?"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTNOW "Restarteaza acum"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_REBOOTLATER "Vreau sa restartez ulterior"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_RUN "Executa ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_FINISH_SHOWREADME "Arata Readme-ul"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_TITLE "Alege directorul din Start Menu"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_STARTMENU_SUBTITLE "Alege un director din Start Menu pentru shortcut-urile aplicatiei."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_TOP "Selecteaza directorul din Start Menu in care sa creez shortcut-urile programului. Poti de asemenea sa creezi un director nou scriind un nume nou."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_INNERTEXT_STARTMENU_CHECKBOX "Nu crea shortcut-uri"
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_TEXT_ABORTWARNING "Esti sigur(a) ca vrei sa opresti instalarea pentru ${MUI_PRODUCT} ?"  
  
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_CONTINUE_UNINSTALL "Apasa butonul Dezinstalare pentru a porni dezinstalarea."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_TITLE "Dezinstaleaza ${MUI_PRODUCT}"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_INTRO_SUBTITLE "Sterge ${MUI_PRODUCT}."
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNINNERTEXT_INTRO "Acest program v-a dezinstala ${MUI_PRODUCT}."
  
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_TITLE "In proces de dezinstalare"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_UNINSTALLING_SUBTITLE "Asteapta pana ${MUI_PRODUCT} este dezinstalat."
    
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_TITLE "Terminat"
  !insertmacro MUI_LANGUAGEFILE_STRING MUI_UNTEXT_FINISHED_SUBTITLE "Dezinstalarea s-a terminat cu succes."
  
!insertmacro MUI_LANGUAGEFILE_END