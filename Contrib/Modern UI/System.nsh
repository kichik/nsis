;NSIS Modern User Interface version 1.6
;Macro System
;Written by Joost Verburg

;See the scripts in the 'Examples\Modern UI' directory for examples of usage.

;--------------------------------
!verbose 3

!ifndef MUI_MACROS_USED

!define MUI_MACROS_USED

!include "${NSISDIR}\Examples\WinMessages.nsh"

!define MUI_TEMP1 $R0
!define MUI_TEMP2 $R1
!define MUI_TEMP3 $R2

!macro MUI_INTERFACE

  !verbose 3

  ;User interface
  
  !ifndef MUI_UI
    !define MUI_UI "${NSISDIR}\Contrib\UIs\modern.exe"
  !endif

  !ifndef MUI_ICON
    !define MUI_ICON "${NSISDIR}\Contrib\Icons\modern-install.ico"
  !endif

  !ifndef MUI_UNICON
    !define MUI_UNICON "${NSISDIR}\Contrib\Icons\modern-uninstall.ico"
  !endif

  !ifndef MUI_CHECKBITMAP
    !define MUI_CHECKBITMAP "${NSISDIR}\Contrib\Icons\modern.bmp"
  !endif

  !ifndef MUI_FONT
    !define MUI_FONT "MS Shell Dlg"
  !endif

  !ifndef MUI_INSTALLCOLORS
    !define MUI_INSTALLCOLORS "/windows"
  !endif

  !ifndef MUI_PROGRESSBAR
    !define MUI_PROGRESSBAR "smooth"
  !endif

  !ifndef MUI_BRANDINGTEXT
    !define MUI_BRANDINGTEXT "" ;Default value
  !endif
  
  !ifndef MUI_WIZARDINI
    !define MUI_WIZARDINI "${NSISDIR}\Contrib\Modern UI\ioWizard.ini"
  !endif
  
  !ifndef MUI_WIZARDBITMAP
    !define MUI_WIZARDBITMAP "${NSISDIR}\Contrib\Icons\modern-wizard.bmp"
  !endif
  
  !ifdef MUI_FINISHPAGE
    !ifndef MUI_FINISHPAGE_NOAUTOCLOSE
      AutoCloseWindow true
    !endif
  !endif

  XPStyle On

  ChangeUI all "${MUI_UI}"
  Icon "${MUI_ICON}"
  UninstallIcon "${MUI_UNICON}"
  CheckBitmap "${MUI_CHECKBITMAP}"
  SetFont "${MUI_FONT}" 8
  InstallColors "${MUI_INSTALLCOLORS}"
  InstProgressFlags "${MUI_PROGRESSBAR}"
  BrandingText /TRIMRIGHT "${MUI_BRANDINGTEXT}"

  !verbose 4

!macroend

!macro MUI_INNERDIALOG_TEXT CONTROL TEXT

  !verbose 3

  ;Set text on inner dialogs component
  Push ${MUI_TEMP1}

    FindWindow ${MUI_TEMP1} "#32770" "" $HWNDPARENT
    GetDlgItem ${MUI_TEMP1} ${MUI_TEMP1} ${CONTROL}
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"

  Pop ${MUI_TEMP1}

  !verbose 4

!macroend

!macro MUI_HEADER_TEXT TEXT SUBTEXT

  !verbose 3

  ;Set text on the white rectangle
  Push ${MUI_TEMP1}

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1037
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1038
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${SUBTEXT}"

  Pop ${MUI_TEMP1}

  !verbose 4

!macroend

!macro MUI_DESCRIPTION_BEGIN

  !verbose 3

  Push ${MUI_TEMP1}

  FindWindow ${MUI_TEMP1} "#32770" "" $HWNDPARENT
  GetDlgItem ${MUI_TEMP1} ${MUI_TEMP1} 1043

  !verbose 4

!macroend

!macro MUI_DESCRIPTION_TEXT VAR TEXT

  !verbose 3

  ;Set text on the Description frame

  StrCmp $0 ${VAR} "" +3
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
    Goto done

  !verbose 4

!macroend

!macro MUI_DESCRIPTION_END

  !verbose 3

  done:
  Pop ${MUI_TEMP1}

  !verbose 4

!macroend

!macro MUI_FINISHHEADER

  !verbose 3

  ;Finish text on the header (white rectangle)
  !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_FINISH_TITLE) $(MUI_TEXT_FINISH_SUBTITLE)

  !verbose 4

!macroend

!macro MUI_UNFINISHHEADER

  !verbose 3

  ;Finish text on the header (white rectangle)
  !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_FINISHED_TITLE) $(MUI_UNTEXT_FINISHED_SUBTITLE)

  !verbose 4

!macroend

!macro MUI_ABORTWARNING

  !verbose 3

  ;Warning when Cancel button is pressed

  MessageBox MB_YESNO|MB_ICONEXCLAMATION "$(MUI_TEXT_ABORTWARNING)" IDYES quit
    Abort
    quit:

  !verbose 4

!macroend

!macro MUI_GUIINIT

  !verbose 3
  
  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_WELCOMEFINISHPAGE_INIT
  !endif
  
  !ifndef MUI_WELCOMEPAGE
    !ifdef MUI_FINISHPAGE
      !insertmacro MUI_WELCOMEFINISHPAGE_INIT
    !endif
  !endif

  Push ${MUI_TEMP1}
  Push ${MUI_TEMP2}

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1037
  CreateFont ${MUI_TEMP2} "Tahoma" 10 700
  SendMessage ${MUI_TEMP1} ${WM_SETFONT} ${MUI_TEMP2} 0
  SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1038
  SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1034
  SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1039
  SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF

  Pop ${MUI_TEMP2}
  Pop ${MUI_TEMP1}
  
  !verbose 4
  
!macroend

!macro MUI_UNGUIINIT

  !verbose 3

  Push ${MUI_TEMP1}
  Push ${MUI_TEMP2}

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1037
  CreateFont ${MUI_TEMP2} "Tahoma" 10 700
  SendMessage ${MUI_TEMP1} ${WM_SETFONT} ${MUI_TEMP2} 0
  SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1038
  SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1034
  SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1039
  SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF

  Pop ${MUI_TEMP2}
  Pop ${MUI_TEMP1}
  
  !verbose 4
  
!macroend

!macro MUI_WELCOMEFINISHPAGE_INIT

  ;Extract Install Options INI Files
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT_CUSTOMNAME "${MUI_WIZARDINI}" "ioWizard.ini"
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT_CUSTOMNAME "${MUI_WIZARDBITMAP}" "modern-wizard.bmp"   
  
  ;Write bitmap location
  !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 1" "Text" "$PLUGINSDIR\modern-wizard.bmp"
  
  ;Write Welcome text
  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 2" "Text" "$(MUI_TEXT_WELCOME_TITLE)"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 3" "Text" "$(MUI_TEXT_WELCOME_INFO)"
  !endif
  
!macroend

!macro MUI_LANGUAGE LANGUAGE

  !verbose 3

  !include "${NSISDIR}\Contrib\Modern UI\Language files\${LANGUAGE}.nsh"
  
  !verbose 4
  
!macroend

!macro MUI_STARTMENU_WRITE_BEGIN

  Push ${MUI_TEMP1}
  
    StrCpy ${MUI_TEMP1} ${MUI_STARTMENU_VARIABLE} 1
    StrCmp ${MUI_TEMP1} ">" no_startmenu_shortcuts

  Pop ${MUI_TEMP1}

!macroend

!macro MUI_STARTMENU_WRITE_END

  no_startmenu_shortcuts:

!macroend

;--------------------------------
;PAGE COMMANDS

!macro MUI_PAGECOMMANDS
 
  !verbose 3
  
  !ifndef MUI_CUSTOMPAGECOMMANDS

    !insertmacro MUI_PAGECOMMAND_WELCOME
    !insertmacro MUI_PAGECOMMAND_LICENSE
    !insertmacro MUI_PAGECOMMAND_COMPONENTS
    !insertmacro MUI_PAGECOMMAND_DIRECTORY
    !insertmacro MUI_PAGECOMMAND_STARTMENU
    !insertmacro MUI_PAGECOMMAND_INSTFILES
    !insertmacro MUI_PAGECOMMAND_FINISH
  
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_PAGECOMMAND_WELCOME

  !verbose 3

  !ifdef MUI_WELCOMEPAGE
    Page custom SetWelcome "" "MUI_INSTALLBUTTON_WELCOME"
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_PAGECOMMAND_LICENSE

  !verbose 3

  !ifdef MUI_LICENSEPAGE
    Page license SetLicense SetLicenseDialog
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_PAGECOMMAND_COMPONENTS

  !verbose 3

  !ifdef MUI_COMPONENTSPAGE
    Page components SetComponents SetComponentsDialog "MUI_INSTALLBUTTON_COMPONENTS"
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_PAGECOMMAND_DIRECTORY

  !verbose 3

  !ifdef MUI_DIRECTORYPAGE
    Page directory SetDirectory SetDirectoryDialog "MUI_INSTALLBUTTON_DIRECTORY"
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_PAGECOMMAND_STARTMENU

  !verbose 3

  !ifdef MUI_STARTMENUPAGE
    Page custom SetStartmenu "$(MUI_TEXT_STARTMENU_WINDOWTITLE)" "MUI_INSTALLBUTTON_STARTMENU"
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_PAGECOMMAND_INSTFILES

  !verbose 3

  Page instfiles SetInstFiles
   
  !verbose 4
   
!macroend

!macro MUI_PAGECOMMAND_FINISH

  !verbose 3

  !ifdef MUI_FINISHPAGE
    Page custom SetFinish "$(MUI_TEXT_FINISH_WINDOWTITLE)"
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_UNPAGECOMMANDS
 
  !verbose 3
  
  !ifndef MUI_UNCUSTOMPAGECOMMANDS

    !insertmacro MUI_UNPAGECOMMAND_CONFIRM
    !insertmacro MUI_UNPAGECOMMAND_INSTFILES
    
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_UNPAGECOMMAND_CONFIRM

  !verbose 3

  UninstPage uninstConfirm un.SetUninstConfirm
   
  !verbose 4
   
!macroend

!macro MUI_UNPAGECOMMAND_INSTFILES

  !verbose 3

  UninstPage instfiles un.SetInstFiles
   
  !verbose 4
   
!macroend

;--------------------------------
;INSTALL OPTIONS

!macro MUI_INSTALLOPTIONS_EXTRACT FILE

  !verbose 3

  ;Init plugin system
  !ifndef MUI_INSTALLOPTIONS_INITPLUGINS
    !define MUI_INSTALLOPTIONS_INITPLUGINS
    InitPluginsDir
  !endif

  File "/oname=$PLUGINSDIR\${FILE}" "${FILE}"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_EXTRACT_CUSTOMNAME FILE FILENAME

  !verbose 3

  ;Init plugin system
  !ifndef MUI_INSTALLOPTIONS_INITPLUGINS
    !define MUI_INSTALLOPTIONS_INITPLUGINS
    InitPluginsDir
  !endif

  File "/oname=$PLUGINSDIR\${FILENAME}" "${FILE}"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_UNEXTRACT FILE

  !verbose 3

  ;Init plugin system
  !ifndef MUI_INSTALLOPTIONS_UNINITPLUGINS
    !define MUI_INSTALLOPTIONS_UNINITPLUGINS
    InitPluginsDir
  !endif

  File /oname=$PLUGINSDIR\${FILE} "${FILE}"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_DISPLAY FILE

  !verbose 3
  
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop ${MUI_TEMP1}

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_INITDIALOG FILE

  !verbose 3

  InstallOptions::initDialog /NOUNLOAD "$PLUGINSDIR\${FILE}"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_SHOW

  !verbose 3S

  Push ${MUI_TEMP1}

  InstallOptions::show
  Pop ${MUI_TEMP1}

  StrCmp ${MUI_TEMP1} "cancel" "" +2
    Quit

  StrCmp ${MUI_TEMP1} "back" "" +3
    Pop ${MUI_TEMP1}
    Abort

  Pop ${MUI_TEMP1}

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_READ VAR FILE SECTION KEY

  !verbose 3

  ReadIniStr ${VAR} "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_WRITE FILE SECTION KEY VALUE

  !verbose 3

  WriteIniStr "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}" "${VALUE}"

  !verbose 4

!macroend

;--------------------------------
;SECTIONS

!macro MUI_SECTIONS_FINISHHEADER

  !verbose 3

  Section ""

    ;Invisible section to display the Finish header
    !insertmacro MUI_FINISHHEADER

  SectionEnd
  
  !verbose 4
  
!macroend

;--------------------------------
;FUNCTIONS

!macro MUI_FUNCTIONS_GUIINIT

  !verbose 3
  
  !ifndef MUI_CUSTOMGUIINIT

    Function .onGUIInit
      !insertmacro MUI_GUIINIT
    FunctionEnd
    
  !endif

  !verbose 4

!macroend

!macro MUI_FUNCTIONS_PAGES

  !verbose 3

  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_FUNCTIONS_WELCOMEPAGE SetWelcome
  !endif

  !ifdef MUI_LICENSEPAGE
    !insertmacro MUI_FUNCTIONS_LICENSEPAGE SetLicense SetLicenseDialog
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    !insertmacro MUI_FUNCTIONS_COMPONENTSPAGE SetComponents SetComponentsDialog
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    !insertmacro MUI_FUNCTIONS_DIRECTORYPAGE SetDirectory SetDirectoryDialog
  !endif
  
  !ifdef MUI_STARTMENUPAGE
    !insertmacro MUI_FUNCTIONS_STARTMENUPAGE SetStartmenu
  !endif
  
  !insertmacro MUI_FUNCTIONS_INSTFILESPAGE SetInstFiles
    
  !ifdef MUI_FINISHPAGE
    !insertmacro MUI_FUNCTIONS_FINISHPAGE SetFinish
  !endif

  !verbose 4

!macroend

!macro MUI_FUNCTIONS_WELCOMEPAGE SETWELCOME

  !verbose 3

  Function "${SETWELCOME}"
  
    Push ${MUI_TEMP1}
    Push ${MUI_TEMP2}
    Push ${MUI_TEMP3}

      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1028
      ShowWindow ${MUI_TEMP1} ${SW_HIDE}

      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1035
      ShowWindow ${MUI_TEMP1} ${SW_HIDE}
      
      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1045
      ShowWindow ${MUI_TEMP1} ${SW_NORMAL}

      !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ioWizard.ini"
      
        Pop ${MUI_TEMP1}
        
        SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF
      
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1201
        SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
        CreateFont ${MUI_TEMP3} "Verdana" 12 1000
        SendMessage ${MUI_TEMP2} ${WM_SETFONT} ${MUI_TEMP3} 0
        
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1202
        SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
  
      !insertmacro MUI_INSTALLOPTIONS_SHOW
      
      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1028
      ShowWindow ${MUI_TEMP1} ${SW_NORMAL}

      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1035
      ShowWindow ${MUI_TEMP1} ${SW_NORMAL}
      
      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1045
      ShowWindow ${MUI_TEMP1} ${SW_HIDE}

    Pop ${MUI_TEMP3}
    Pop ${MUI_TEMP2}
    Pop ${MUI_TEMP1}
    
  FunctionEnd
  
  !verbose 4
  
!macroend

!macro MUI_FUNCTIONS_LICENSEPAGE SETLICENSE SETLICENSEDIALOG

  !verbose 3

  Function "${SETLICENSE}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
  FunctionEnd

  Function "${SETLICENSEDIALOG}"
    !insertmacro MUI_INNERDIALOG_TEXT 1040 $(MUI_INNERTEXT_LICENSE_BOTTOM)
  FunctionEnd
  
  !verbose 4
    
!macroend

!macro MUI_FUNCTIONS_COMPONENTSPAGE SETCOMPONENTS SETCOMPONENTSDIALOG

  !verbose 3

  Function "${SETCOMPONENTS}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_COMPONENTS_TITLE) $(MUI_TEXT_COMPONENTS_SUBTITLE)
  FunctionEnd

  Function "${SETCOMPONENTSDIALOG}"
    !insertmacro MUI_INNERDIALOG_TEXT 1042 $(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE)
    !insertmacro MUI_INNERDIALOG_TEXT 1043 $(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO)
  FunctionEnd
  
  !verbose 4
    
!macroend

!macro MUI_FUNCTIONS_DIRECTORYPAGE SETDIRECTORY SETDIRECTORYDIALOG

  !verbose 3

  Function "${SETDIRECTORY}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_DIRECTORY_TITLE) $(MUI_TEXT_DIRECTORY_SUBTITLE)
  FunctionEnd

  Function "${SETDIRECTORYDIALOG}"
    !insertmacro MUI_INNERDIALOG_TEXT 1041 $(MUI_INNERTEXT_DIRECTORY_DESTINATION)
  FunctionEnd
  
  !verbose 4
  
!macroend

!macro MUI_FUNCTIONS_STARTMENUPAGE SETSTARTMENU

  !verbose 3
  
  ;Check defines
  !ifndef MUI_STARTMENU_VARIABLE
    !define MUI_STARTMENU_VARIABLE "$9"
  !endif
  !ifndef MUI_STARTMENU_DEFAULTFOLDER
    !define MUI_STARTMENU_DEFAULTFOLDER "${MUI_PRODUCT}"
  !endif

  Function "${SETSTARTMENU}"
  
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_STARTMENU_TITLE) $(MUI_TEXT_STARTMENU_SUBTITLE)
    
    StartMenu::Select /noicon /autoadd /text "$(MUI_INNERTEXT_STARTMENU_TOP)" /lastused "${MUI_STARTMENU_VARIABLE}" /checknoshortcuts "$(MUI_INNERTEXT_STARTMENU_CHECKBOX)" "${MUI_STARTMENU_DEFAULTFOLDER}"
    Pop "${MUI_STARTMENU_VARIABLE}"
    
  FunctionEnd

  !verbose 4
  
!macroend

!macro MUI_FUNCTIONS_INSTFILESPAGE SETINSTFILES

  !verbose 3

  Function "${SETINSTFILES}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_INSTALLING_TITLE) $(MUI_TEXT_INSTALLING_SUBTITLE)
  FunctionEnd
  
  !verbose 4
  
!macroend

!macro MUI_FUNCTIONS_FINISHPAGE SETFINISH

  !verbose 3

  Function "${SETFINISH}"
  
    Push ${MUI_TEMP1}
    Push ${MUI_TEMP2}
    Push ${MUI_TEMP3}
    
    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1028
    ShowWindow ${MUI_TEMP1} ${SW_HIDE}

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1035
    ShowWindow ${MUI_TEMP1} ${SW_HIDE}
      
    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1045
    ShowWindow ${MUI_TEMP1} ${SW_NORMAL}
    
     ;Write Finish text
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 2" "Text" "$(MUI_TEXT_FINISH_TITLE)"
    
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 3" "Bottom" "150"
    
      !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
    
        IfRebootFlag "" noreboot_init
      
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 3" "Text" "$(MUI_TEXT_FINISH_INFO_REBOOT)"
      
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Settings" "Numfields" "5"
          
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Type" "RadioButton"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_REBOOTNOW)"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Left" "190"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Right" "475"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Top" "160"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Bottom" "175"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "State" "1"
          
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Type" "RadioButton"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Text" "$(MUI_TEXT_FINISH_REBOOTLATER)"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Left" "190"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Right" "475"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Top" "190"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Bottom" "205"
      
          Goto init
      
        noreboot_init:
      
      !endif
       
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 3" "Text" "$(MUI_TEXT_FINISH_INFO)"
      
        !ifdef MUI_FINISHPAGE_RUN
        
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_INFO)"
        
          !ifndef MUI_FINISHPAGE_SHOWREADME
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Settings" "Numfields" "4"
          !endif
        
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Type" "CheckBox"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_RUN)"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Left" "190"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Right" "475"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Top" "160"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Bottom" "175"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "State" "1"
          
          !ifdef MUI_FINISHPAGE_SHOWREADME
          
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Settings" "Numfields" "5"
            
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Type" "CheckBox"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Text" "$(MUI_TEXT_FINISH_SHOWREADME)"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Left" "190"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Right" "475"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Top" "190"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 5" "Bottom" "205"
            
          !endif

        !endif
        
        !ifndef MUI_FINISHPAGE_RUN
        
          !ifdef MUI_FINISHPAGE_SHOWREADME
            
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Settings" "Numfields" "4"
            
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Type" "CheckBox"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_SHOWREADME)"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Left" "190"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Right" "475"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Top" "160"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "Bottom" "175"
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioWizard.ini" "Field 4" "State" "1"
            
          !endif
          
        !endif
      
      !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
        init:
      !endif

      !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ioWizard.ini"
      
        Pop ${MUI_TEMP1}
        
        SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF
      
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1201
        SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
        CreateFont ${MUI_TEMP3} "Verdana" 12 1000
        SendMessage ${MUI_TEMP2} ${WM_SETFONT} ${MUI_TEMP3} 0
        
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1202
        SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
        
        !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
        
          IfRebootFlag "" noreboot_show
        
            GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1203
            SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
            
            GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1204
            SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
            
            Goto show
        
          noreboot_show:
          
        !endif
        
          !ifdef MUI_FINISHPAGE_RUN
          
            GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1203
            SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
            
            !ifdef MUI_FINISHPAGE_SHOWREADME
            
              GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1204
              SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
              
            !endif
          
          !endif
          
          !ifndef MUI_FINISHPAGE_RUN
          
            !ifdef MUI_FINISHPAGE_SHOWREADME
            
              GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1203
              SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
              
            !endif
            
          !endif
        
        !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
          show:
        !endif

      !insertmacro MUI_INSTALLOPTIONS_SHOW
      
      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1028
      ShowWindow ${MUI_TEMP1} ${SW_NORMAL}

      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1035
      ShowWindow ${MUI_TEMP1} ${SW_NORMAL}
      
      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1045
      ShowWindow ${MUI_TEMP1} ${SW_HIDE}
      
      !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
      
        IfRebootFlag "" noreboot_end
      
          !insertmacro MUI_INSTALLOPTIONS_READ ${MUI_TEMP1} "ioWizard.ini" "Field 4" "State"
        
            StrCmp ${MUI_TEMP1} "1" "" +2
              Reboot
            
            Goto done
      
        noreboot_end:
        
      !endif
      
        !ifdef MUI_FINISHPAGE_RUN
      
          !insertmacro MUI_INSTALLOPTIONS_READ ${MUI_TEMP1} "ioWizard.ini" "Field 4" "State"
          
           StrCmp ${MUI_TEMP1} "1" "" +2
             Exec '"${MUI_FINISHPAGE_RUN}"'
             
           !ifdef MUI_FINISHPAGE_SHOWREADME
          
             !insertmacro MUI_INSTALLOPTIONS_READ ${MUI_TEMP1} "ioWizard.ini" "Field 5" "State"
            
             StrCmp ${MUI_TEMP1} "1" "" +2
               ExecShell "open" '"${MUI_FINISHPAGE_SHOWREADME}"'
               
           !endif
             
        !endif
        
        !ifndef MUI_FINISHPAGE_RUN
        
          !ifdef MUI_FINISHPAGE_SHOWREADME
          
            !insertmacro MUI_INSTALLOPTIONS_READ ${MUI_TEMP1} "ioWizard.ini" "Field 4" "State"
            
             StrCmp ${MUI_TEMP1} "1" "" +2
               ExecShell "open" '"${MUI_FINISHPAGE_SHOWREADME}"'
               
          !endif
          
        !endif
        
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
      done:
    !endif

    Pop ${MUI_TEMP3}
    Pop ${MUI_TEMP2}
    Pop ${MUI_TEMP1}
    
  FunctionEnd
  
  !verbose 4
  
!macroend

!macro MUI_FUNCTIONS_DESCRIPTION_BEGIN

  !verbose 3

  Function .onMouseOverSection
    !insertmacro MUI_DESCRIPTION_BEGIN

  !verbose 4

!macroend

!macro MUI_FUNCTIONS_DESCRIPTION_END

  !verbose 3

    !insertmacro MUI_DESCRIPTION_END
  FunctionEnd

  !verbose 4

!macroend

!macro MUI_FUNCTIONS_ABORTWARNING

  !verbose 3

  !ifdef MUI_ABORTWARNING
    Function .onUserAbort
      !insertmacro MUI_ABORTWARNING
    FunctionEnd
  !endif

  !verbose 4

!macroend

!macro MUI_UNFUNCTIONS_GUIINIT

  !verbose 3
  
  !ifndef MUI_UNCUSTOMGUIINIT

    Function un.onGUIInit
      !insertmacro MUI_UNGUIINIT
    FunctionEnd
    
  !endif

  !verbose 4

!macroend

!macro MUI_UNFUNCTIONS_PAGES
  
  !insertmacro MUI_UNFUNCTIONS_CONFIRMPAGE un.SetUninstConfirm
  !insertmacro MUI_UNFUNCTIONS_INSTFILESPAGE un.SetInstFiles
  
!macroend

!macro MUI_UNFUNCTIONS_CONFIRMPAGE UNSETUNINSTCONFIRM

  !verbose 3

  Function "${UNSETUNINSTCONFIRM}"
    !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_INTRO_TITLE) $(MUI_UNTEXT_INTRO_SUBTITLE)
  FunctionEnd
  
  !verbose 4
  
!macroend

!macro MUI_UNFUNCTIONS_INSTFILESPAGE UNSETINSTFILES

  !verbose 3

  Function ${UNSETINSTFILES}
    !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_UNINSTALLING_TITLE) $(MUI_UNTEXT_UNINSTALLING_SUBTITLE)
  FunctionEnd
  
  !verbose 4
  
!macroend

;--------------------------------
;RESERVE FILES

!macro MUI_RESERVEFILE_INSTALLOPTIONS
  ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
!macroend

!macro MUI_RESERVEFILE_WIZARDINI
  ReserveFile "${NSISDIR}\Contrib\Modern UI\ioWizard.ini"
!macroend

!macro MUI_RESERVEFILE_WIZARDBITMAP
  ReserveFile "${NSISDIR}\Contrib\Icons\modern-wizard.bmp"
!macroend

;--------------------------------
;BASIC MACRO'S

!macro MUI_SYSTEM

  !verbose 3
  
  !insertmacro MUI_INTERFACE
  !insertmacro MUI_BASIC
  !insertmacro MUI_UNBASIC
  
  !verbose 4
  
!macroend

!macro MUI_BASIC

  !verbose 3

  !insertmacro MUI_FUNCTIONS_PAGES
  !insertmacro MUI_FUNCTIONS_GUIINIT
  !insertmacro MUI_FUNCTIONS_ABORTWARNING
  
  !verbose 4

!macroend

!macro MUI_UNBASIC

  !verbose 3
  
  !ifdef MUI_UNINSTALLER

    !insertmacro MUI_UNFUNCTIONS_PAGES
    !insertmacro MUI_UNFUNCTIONS_GUIINIT
  
  !endif

  !verbose 4

!macroend

;--------------------------------
;LANGUAGE FILES

!macro MUI_LANGUAGEFILE_BEGIN LANGUAGE

  !verbose 3
  
  !define MUI_LANGUAGEFILE_CURRENT "${LANGUAGE}"
  
  !ifndef "MUI_LANGUAGEFILE_${LANGUAGE}_USED"
  
    !define "MUI_LANGUAGEFILE_${LANGUAGE}_USED"

    LoadLanguageFile "${NSISDIR}\Contrib\Language files\${LANGUAGE}.nlf"

  !else

    !error "${LANGUAGE} included twice!"

  !endif
  
  !verbose 4
  
!macroend

!macro MUI_LANGUAGEFILE_STRING STRING VALUE

  !verbose 3

  !ifndef "${STRING}"
    !define "${STRING}" "${VALUE}"
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING NAME VALUE

  !verbose 3

  LangString "${NAME}" "${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE}"
  !undef "${NAME}"
  
  !verbose 4
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_CONTINUE NAME VALUE INSTALLBUTTON

  !verbose 3
 
  !ifndef "${INSTALLBUTTON}"
    !ifdef MUI_TEXT_CONTINUE_NEXT
      LangString "${NAME}" "${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE}${MUI_TEXT_CONTINUE_NEXT}"
    !endif
    !ifndef MUI_TEXT_CONTINUE_NEXT
      LangString "${NAME}" "${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE}"
    !endif
  !endif
  
  !ifdef "${INSTALLBUTTON}"
    !ifdef MUI_TEXT_CONTINUE_INSTALL
      LangString "${NAME}" "${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE}${MUI_TEXT_CONTINUE_INSTALL}"
    !endif
    !ifndef MUI_TEXT_CONTINUE_INSTALL
      LangString "${NAME}" "${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE}"
    !endif
  !endif

  !undef "${NAME}"
  
  !verbose 4
  
!macroend

!macro MUI_LANGUAGEFILE_UNLANGSTRING NAME VALUE

  !verbose 3

  LangString "un.${NAME}" "${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE}"
  !undef "${NAME}"
  
  !verbose 4
  
!macroend

!macro MUI_LANGUAGEFILE_NSISCOMMAND COMMAND NAME VALUE

  !verbose 3

  "${COMMAND}" "/LANG=${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE}"
  !undef "${NAME}"
  
  !verbose 4

!macroend

!macro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER COMMAND NAME VALUE

  !verbose 3

  "${COMMAND}" "/LANG=${LANG_${MUI_LANGUAGEFILE_CURRENT}}" ${VALUE}
  !undef "${NAME}"
  
  !verbose 4

!macroend

!macro MUI_LANGUAGEFILE_NSISCOMMAND_CONTINUE COMMAND NAME VALUE INSTALLBUTTON

  !verbose 3

  !ifndef "${INSTALLBUTTON}"
    !ifdef MUI_TEXT_CONTINUE_NEXT
      "${COMMAND}" "/LANG=${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE} ${MUI_TEXT_CONTINUE_NEXT}"
    !endif
    !ifndef MUI_TEXT_CONTINUE_NEXT
      "${COMMAND}" "/LANG=${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE}"
    !endif
  !endif

  !ifdef "${INSTALLBUTTON}"
    !ifdef MUI_TEXT_CONTINUE_INSTALL
      "${COMMAND}" "/LANG=${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE} ${MUI_TEXT_CONTINUE_INSTALL}"
    !endif
    !ifndef MUI_TEXT_CONTINUE_INSTALL
      "${COMMAND}" "/LANG=${LANG_${MUI_LANGUAGEFILE_CURRENT}}" "${VALUE}"
    !endif
  !endif
  
  !undef "${NAME}"
  
  !verbose 4

!macroend

!macro MUI_LANGUAGEFILE_DEFINE DEFINE NAME VALUE

  !verbose 3

  !ifndef "${DEFINE}"
    !define "${DEFINE}" "${VALUE}"
  !endif
  !undef "${NAME}"
  
  !verbose 4

!macroend

!macro MUI_LANGUAGEFILE_END

  !verbose 3
    
  !insertmacro MUI_LANGUAGEFILE_DEFINE "MUI_${LANGUAGE}_LANGNAME" "MUI_LANGNAME" "${MUI_LANGNAME}"

  !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND Name MUI_NAME "${MUI_NAME}"

  !ifdef MUI_STARTMENUPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_STARTMENU_WINDOWTITLE" "${MUI_TEXT_STARTMENU_WINDOWTITLE}"
  !endif
  
  !ifdef MUI_FINISHPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_WINDOWTITLE" "${MUI_TEXT_FINISH_WINDOWTITLE}"
  !endif

  !ifndef MUI_CUSTOMPAGECOMMANDS
    !ifndef MUI_PAGECOMMANDS
      !define MUI_PAGECOMMANDS
      !insertmacro MUI_PAGECOMMANDS
    !endif
  !endif
  
  !ifdef MUI_UNINSTALLER
    !ifndef MUI_UNCUSTOMPAGECOMMANDS
      !ifndef MUI_UNPAGECOMMANDS
        !define MUI_UNPAGECOMMANDS
        !insertmacro MUI_UNPAGECOMMANDS
      !endif
    !endif
  !endif

  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_WELCOME_TITLE" "${MUI_TEXT_WELCOME_TITLE}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CONTINUE "MUI_TEXT_WELCOME_INFO" "${MUI_TEXT_WELCOME_INFO}" "MUI_INSTALLBUTTON_WELCOME"
  !endif

  !ifdef MUI_LICENSEPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_LICENSE_TITLE" "${MUI_TEXT_LICENSE_TITLE}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_LICENSE_SUBTITLE" "${MUI_TEXT_LICENSE_SUBTITLE}"
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND "LicenseText" "MUI_INNERTEXT_LICENSE_TOP" "${MUI_INNERTEXT_LICENSE_TOP}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_LICENSE_BOTTOM" "${MUI_INNERTEXT_LICENSE_BOTTOM}"
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_COMPONENTS_TITLE" "${MUI_TEXT_COMPONENTS_TITLE}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_COMPONENTS_SUBTITLE" "${MUI_TEXT_COMPONENTS_SUBTITLE}"
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_CONTINUE "ComponentText" "MUI_INNERTEXT_COMPONENTS_TOP" "${MUI_INNERTEXT_COMPONENTS_TOP} " "MUI_INSTALLBUTTON_COMPONENTS"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE" "${MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO" "${MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO}"
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_DIRECTORY_TITLE" "${MUI_TEXT_DIRECTORY_TITLE}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_DIRECTORY_SUBTITLE" "${MUI_TEXT_DIRECTORY_SUBTITLE}"
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_CONTINUE "DirText" "MUI_INNERTEXT_DIRECTORY_TOP" "${MUI_INNERTEXT_DIRECTORY_TOP} " "MUI_INSTALLBUTTON_DIRECTORY"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_INNERTEXT_DIRECTORY_DESTINATION "${MUI_INNERTEXT_DIRECTORY_DESTINATION}"
  !endif
  
  !ifdef MUI_STARTMENUPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_STARTMENU_TITLE" "${MUI_TEXT_STARTMENU_TITLE}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_STARTMENU_SUBTITLE" "${MUI_TEXT_STARTMENU_SUBTITLE}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CONTINUE "MUI_INNERTEXT_STARTMENU_TOP" "${MUI_INNERTEXT_STARTMENU_TOP} " "MUI_INSTALLBUTTON_STARTMENU"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_STARTMENU_CHECKBOX" "${MUI_INNERTEXT_STARTMENU_CHECKBOX}"
  !endif
  
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_INSTALLING_TITLE" "${MUI_TEXT_INSTALLING_TITLE}"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_INSTALLING_SUBTITLE" "${MUI_TEXT_INSTALLING_SUBTITLE}"
  
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_TITLE" "${MUI_TEXT_FINISH_TITLE}"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_SUBTITLE" "${MUI_TEXT_FINISH_SUBTITLE}"
  !ifdef MUI_FINISHPAGE
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "MiscButtonText" "MUI_TEXT_FINISH_BUTTON" '"" "" "" "${MUI_TEXT_FINISH_BUTTON}"'
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_INFO "${MUI_TEXT_FINISH_INFO}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_INFO_REBOOT "${MUI_TEXT_FINISH_INFO_REBOOT}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_REBOOTNOW "${MUI_TEXT_FINISH_REBOOTNOW}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_REBOOTLATER "${MUI_TEXT_FINISH_REBOOTLATER}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_RUN "${MUI_TEXT_FINISH_RUN}"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_SHOWREADME "${MUI_TEXT_FINISH_SHOWREADME}"
  !endif
  
  !ifdef MUI_ABORTWARNING
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_ABORTWARNING" "${MUI_TEXT_ABORTWARNING}"
  !endif
  
  
  !ifdef MUI_UNINSTALLER
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_INTRO_TITLE" "${MUI_UNTEXT_INTRO_TITLE}"
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_INTRO_SUBTITLE" "${MUI_UNTEXT_INTRO_SUBTITLE}"
  
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND "UninstallText" "MUI_UNINNERTEXT_INTRO" "${MUI_UNINNERTEXT_INTRO}"
  
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_UNINSTALLING_TITLE" "${MUI_UNTEXT_UNINSTALLING_TITLE}"
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_UNINSTALLING_SUBTITLE" "${MUI_UNTEXT_UNINSTALLING_SUBTITLE}"
     
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_FINISHED_TITLE" "${MUI_UNTEXT_FINISHED_TITLE}"
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_FINISHED_SUBTITLE" "${MUI_UNTEXT_FINISHED_SUBTITLE}"
  !endif
  
  !ifdef MUI_TEXT_CONTINUE_NEXT
    !undef MUI_TEXT_CONTINUE_NEXT
  !endif

  !ifdef MUI_TEXT_CONTINUE_INSTALL
    !undef MUI_TEXT_CONTINUE_INSTALL
  !endif
  
  !undef MUI_LANGUAGEFILE_CURRENT
  
  !verbose 4

!macroend

;--------------------------------
;END

!endif

!verbose 4