;NSIS Modern User Interface version 1.62
;Macro System
;Written by Joost Verburg

;Have a look the scripts in the 'Examples\Modern UI'
;directory for examples of usage.

;--------------------------------

!verbose 3

!ifndef MUI_MACROS_USED

!define MUI_MACROS_USED

!include "${NSISDIR}\Include\WinMessages.nsh"

!define MUI_TEMP1 $R0
!define MUI_TEMP2 $R1
!define MUI_TEMP3 $R2

!macro MUI_INTERFACE

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

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
  
  !ifndef MUI_FONTSIZE
    !define MUI_FONTSIZE "8"
  !endif

  !ifndef MUI_FONT_HEADER
    !define MUI_FONT_HEADER "MS Sans Serif"
  !endif
    
  !ifndef MUI_FONTSIZE_HEADER
    !define MUI_FONTSIZE_HEADER "8"
  !endif
  
  !ifndef MUI_FONTSTYLE_HEADER
    !define MUI_FONTSTYLE_HEADER "700"
  !endif

  !ifndef MUI_FONT_TITLE
    !define MUI_FONT_TITLE "Verdana"
  !endif
  
  !ifndef MUI_FONTSIZE_TITLE
    !define MUI_FONTSIZE_TITLE "12"
  !endif
  
  !ifndef MUI_FONTSTYLE_TITLE
    !define MUI_FONTSTYLE_TITLE "700"
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
  
  !ifndef MUI_SPECIALINI
    !define MUI_SPECIALINI "${NSISDIR}\Contrib\Modern UI\ioSpecial.ini"
  !endif
  
  !ifndef MUI_SPECIALBITMAP
    !define MUI_SPECIALBITMAP "${NSISDIR}\Contrib\Icons\modern-wizard.bmp"
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
  SetFont "${MUI_FONT}" "${MUI_FONTSIZE}"
  InstallColors ${MUI_INSTALLCOLORS}
  InstProgressFlags ${MUI_PROGRESSBAR}
  BrandingText /TRIMRIGHT "${MUI_BRANDINGTEXT}"

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INNERDIALOG_TEXT CONTROL TEXT

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  ;Set text on inner dialogs component
  Push ${MUI_TEMP1}

    FindWindow ${MUI_TEMP1} "#32770" "" $HWNDPARENT
    GetDlgItem ${MUI_TEMP1} ${MUI_TEMP1} ${CONTROL}
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"

  Pop ${MUI_TEMP1}

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_HEADER_TEXT TEXT SUBTEXT

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  ;Set text on the white rectangle
  Push ${MUI_TEMP1}

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1037
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1038
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${SUBTEXT}"

  Pop ${MUI_TEMP1}

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_DESCRIPTION_BEGIN

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Push ${MUI_TEMP1}

  FindWindow ${MUI_TEMP1} "#32770" "" $HWNDPARENT
  GetDlgItem ${MUI_TEMP1} ${MUI_TEMP1} 1043

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_DESCRIPTION_TEXT VAR TEXT

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  ;Set text on the Description frame

  StrCmp $0 ${VAR} "" +3
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
    Goto done

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_DESCRIPTION_END

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  done:
  Pop ${MUI_TEMP1}

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_FINISHHEADER

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  ;Finish text on the header (white rectangle)
  !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_FINISH_TITLE) $(MUI_TEXT_FINISH_SUBTITLE)

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_UNFINISHHEADER

  !ifndef MUI_NOVERBOSE
    !verbose 3
    !define MUI_NOVERBOSE
  !endif

  ;Finish text on the header (white rectangle)
  !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_FINISHED_TITLE) $(MUI_UNTEXT_FINISHED_SUBTITLE)

  !undef MUI_NOVERBOSE
  !verbose 4

!macroend

!macro MUI_ABORTWARNING

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  ;Warning when Cancel button is pressed

  MessageBox MB_YESNO|MB_ICONEXCLAMATION "$(MUI_TEXT_ABORTWARNING)" IDYES quit
    Abort
    quit:

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_GUIINIT

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif
  
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
  CreateFont ${MUI_TEMP2} "${MUI_FONT_HEADER}" "${MUI_FONTSIZE_HEADER}" "${MUI_FONTSTYLE_HEADER}"
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
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_UNGUIINIT

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Push ${MUI_TEMP1}
  Push ${MUI_TEMP2}

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1037
  CreateFont ${MUI_TEMP2} "${MUI_FONT_HEADER}" "${MUI_FONTSIZE_HEADER}" "${MUI_FONTSTYLE_HEADER}"
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
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_WELCOMEFINISHPAGE_INIT

  ;Extract Install Options INI Files
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT_CUSTOMNAME "${MUI_SPECIALINI}" "ioSpecial.ini"
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT_CUSTOMNAME "${MUI_SPECIALBITMAP}" "modern-wizard.bmp"   
  
  ;Write bitmap location
  !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 1" "Text" "$PLUGINSDIR\modern-wizard.bmp"
  
  ;Write Welcome text
  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 2" "Text" "$(MUI_TEXT_WELCOME_INFO_TITLE)"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_TEXT_WELCOME_INFO_TEXT)"
  !endif
  
!macroend

!macro MUI_LANGUAGE LANGUAGE

  !verbose 3

  !include "${NSISDIR}\Contrib\Modern UI\Language files\${LANGUAGE}.nsh"
  
  !verbose 4
  
!macroend

!macro MUI_LANGDLL_PUSH LANGUAGE

  !verbose 3

  Push "${LANG_${LANGUAGE}}"
  Push "${MUI_${LANGUAGE}_LANGNAME}"
  
  !verbose 4
  
!macroend

!macro MUI_STARTMENU_WRITE_BEGIN

  !verbose 3

  Push ${MUI_TEMP1}
  
    StrCpy ${MUI_TEMP1} ${MUI_STARTMENU_VARIABLE} 1
    StrCmp ${MUI_TEMP1} ">" no_startmenu_shortcuts

  Pop ${MUI_TEMP1}
  
  !verbose 4

!macroend

!macro MUI_STARTMENU_WRITE_END

  !verbose 3

  no_startmenu_shortcuts:
  
  !verbose 4

!macroend

;--------------------------------
;PAGE COMMANDS

!macro MUI_PAGECOMMANDS
 
  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif
  
  !ifndef MUI_CUSTOMPAGECOMMANDS

    !insertmacro MUI_PAGECOMMAND_WELCOME
    !insertmacro MUI_PAGECOMMAND_LICENSE
    !insertmacro MUI_PAGECOMMAND_COMPONENTS
    !insertmacro MUI_PAGECOMMAND_DIRECTORY
    !insertmacro MUI_PAGECOMMAND_STARTMENU
    !insertmacro MUI_PAGECOMMAND_INSTFILES
    !insertmacro MUI_PAGECOMMAND_FINISH
  
  !endif
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_WELCOME

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  !ifdef MUI_WELCOMEPAGE
    Page custom SetWelcome "" "MUI_INSTALLBUTTON_WELCOME"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_LICENSE

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  !ifdef MUI_LICENSEPAGE
    Page license SetLicense SetLicenseDialog
  !endif
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_COMPONENTS

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  !ifdef MUI_COMPONENTSPAGE
    Page components SetComponents SetComponentsDialog "MUI_INSTALLBUTTON_COMPONENTS"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_DIRECTORY

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  !ifdef MUI_DIRECTORYPAGE
    Page directory SetDirectory SetDirectoryDialog "MUI_INSTALLBUTTON_DIRECTORY"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_STARTMENU

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  !ifdef MUI_STARTMENUPAGE
    Page custom SetStartmenu "$(MUI_TEXT_STARTMENU_WINDOWTITLE)" "MUI_INSTALLBUTTON_STARTMENU"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_INSTFILES

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Page instfiles SetInstFiles
   
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
   
!macroend

!macro MUI_PAGECOMMAND_FINISH

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  !ifdef MUI_FINISHPAGE
    Page custom SetFinish "$(MUI_TEXT_FINISH_WINDOWTITLE)"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_UNPAGECOMMANDS
 
  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif
  
  !ifndef MUI_UNCUSTOMPAGECOMMANDS
    !insertmacro MUI_UNPAGECOMMAND_CONFIRM
    !insertmacro MUI_UNPAGECOMMAND_INSTFILES
  !endif
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_UNPAGECOMMAND_CONFIRM

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  !ifdef MUI_UNCONFIRMPAGE
    UninstPage uninstConfirm un.SetUninstConfirm "" "MUI_UNINSTALLBUTTON_CONFIRM"
  !endif
   
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
   
!macroend

!macro MUI_UNPAGECOMMAND_INSTFILES

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  UninstPage instfiles un.SetInstFiles
   
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
   
!macroend

;--------------------------------
;INSTALL OPTIONS

!macro MUI_INSTALLOPTIONS_EXTRACT FILE

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  ;Init plugin system
  InitPluginsDir

  File "/oname=$PLUGINSDIR\${FILE}" "${FILE}"

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_EXTRACT_CUSTOMNAME FILE FILENAME

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  ;Init plugin system
  InitPluginsDir

  File "/oname=$PLUGINSDIR\${FILENAME}" "${FILE}"

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_DISPLAY FILE

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif
  
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop ${MUI_TEMP1}

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_INITDIALOG FILE

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  InstallOptions::initDialog /NOUNLOAD "$PLUGINSDIR\${FILE}"

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_SHOW

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Push ${MUI_TEMP1}

  InstallOptions::show
  Pop ${MUI_TEMP1}

  StrCmp ${MUI_TEMP1} "cancel" "" +2
    Quit

  StrCmp ${MUI_TEMP1} "back" "" +3
    Pop ${MUI_TEMP1}
    Abort

  Pop ${MUI_TEMP1}

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_READ VAR FILE SECTION KEY

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  ReadIniStr ${VAR} "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}"

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_WRITE FILE SECTION KEY VALUE

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  WriteIniStr "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}" "${VALUE}"

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

;--------------------------------
;SECTIONS

!macro MUI_SECTIONS_FINISHHEADER

  !ifndef MUI_NOVERBOSE
    !verbose 3
    !define MUI_NOVERBOSE
  !endif

  Section ""

    ;Invisible section to display the Finish header
    !insertmacro MUI_FINISHHEADER

  SectionEnd
  
  !undef MUI_NOVERBOSE
  !verbose 4
  
!macroend

;--------------------------------
;FUNCTIONS

!macro MUI_FUNCTIONS_GUIINIT

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif
  
  !ifndef MUI_CUSTOMGUIINIT

    Function .onGUIInit
      !insertmacro MUI_GUIINIT
    FunctionEnd
    
  !endif

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_FUNCTIONS_PAGES

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

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

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_FUNCTIONS_WELCOMEPAGE SETWELCOME

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

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

      !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ioSpecial.ini"
      
        Pop ${MUI_TEMP1}
        
        SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF
      
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1201
        SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
        CreateFont ${MUI_TEMP3} "${MUI_FONT_TITLE}" "${MUI_FONTSIZE_TITLE}" "${MUI_FONTSTYLE_TITLE}"
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
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_FUNCTIONS_LICENSEPAGE SETLICENSE SETLICENSEDIALOG

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Function "${SETLICENSE}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
  FunctionEnd

  Function "${SETLICENSEDIALOG}"
    !insertmacro MUI_INNERDIALOG_TEXT 1040 $(MUI_INNERTEXT_LICENSE_BOTTOM)
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
    
!macroend

!macro MUI_FUNCTIONS_COMPONENTSPAGE SETCOMPONENTS SETCOMPONENTSDIALOG

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Function "${SETCOMPONENTS}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_COMPONENTS_TITLE) $(MUI_TEXT_COMPONENTS_SUBTITLE)
  FunctionEnd

  Function "${SETCOMPONENTSDIALOG}"
    !insertmacro MUI_INNERDIALOG_TEXT 1042 $(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE)
    !insertmacro MUI_INNERDIALOG_TEXT 1043 $(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO)
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
    
!macroend

!macro MUI_FUNCTIONS_DIRECTORYPAGE SETDIRECTORY SETDIRECTORYDIALOG

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Function "${SETDIRECTORY}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_DIRECTORY_TITLE) $(MUI_TEXT_DIRECTORY_SUBTITLE)
  FunctionEnd

  Function "${SETDIRECTORYDIALOG}"
    !insertmacro MUI_INNERDIALOG_TEXT 1041 $(MUI_INNERTEXT_DIRECTORY_DESTINATION)
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_FUNCTIONS_STARTMENUPAGE SETSTARTMENU

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif
  
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

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_FUNCTIONS_INSTFILESPAGE SETINSTFILES

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Function "${SETINSTFILES}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_INSTALLING_TITLE) $(MUI_TEXT_INSTALLING_SUBTITLE)
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_FUNCTIONS_FINISHPAGE SETFINISH

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

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
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 2" "Text" "$(MUI_TEXT_FINISH_INFO_TITLE)"
    
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Bottom" "80"
    
      !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
    
        IfRebootFlag "" noreboot_init
      
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_TEXT_FINISH_INFO_REBOOT)"
      
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "5"
          
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Type" "RadioButton"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_REBOOTNOW)"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Left" "120"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Right" "321"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Top" "85"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Bottom" "95"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "State" "1"
          
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Type" "RadioButton"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Text" "$(MUI_TEXT_FINISH_REBOOTLATER)"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Left" "120"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Right" "321"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Top" "105"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Bottom" "115"
      
          Goto init
      
        noreboot_init:
      
      !endif
       
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_TEXT_FINISH_INFO_TEXT)"
      
      !ifdef MUI_FINISHPAGE_RUN
        
        !ifndef MUI_FINISHPAGE_SHOWREADME
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "4"
        !endif
        
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Type" "CheckBox"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_RUN)"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Left" "120"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Right" "315"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Top" "85"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Bottom" "95"
        !ifndef MUI_FINISHPAGE_RUN_NOTCHECKED
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "State" "1"
        !endif
          
        !ifdef MUI_FINISHPAGE_SHOWREADME
          
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "5"
            
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Type" "CheckBox"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Text" "$(MUI_TEXT_FINISH_SHOWREADME)"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Left" "120"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Right" "315"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Top" "105"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Bottom" "115"
          !ifndef MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
            !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "State" "1"
          !endif
            
        !endif

      !else ifdef MUI_FINISHPAGE_SHOWREADME
      
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "4"
            
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Type" "CheckBox"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_SHOWREADME)"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Left" "120"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Right" "315"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Top" "85"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Bottom" "95"
        !ifndef MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "State" "1"
        !endif
          
      !endif
      
      !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
        init:
      !endif

      !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ioSpecial.ini"
      
        Pop ${MUI_TEMP1}
        
        SetStaticBkColor ${MUI_TEMP1} 0x00FFFFFF
      
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1201
        SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
        CreateFont ${MUI_TEMP3} "${MUI_FONT_TITLE}" "${MUI_FONTSIZE_TITLE}" "${MUI_FONTSTYLE_TITLE}"
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
          
          !else ifdef MUI_FINISHPAGE_SHOWREADME
            
              GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1203
              SetStaticBkColor ${MUI_TEMP2} 0x00FFFFFF
            
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
      
          !insertmacro MUI_INSTALLOPTIONS_READ ${MUI_TEMP1} "ioSpecial.ini" "Field 4" "State"
        
            StrCmp ${MUI_TEMP1} "1" "" +2
              Reboot
            
            Goto done
      
        noreboot_end:
        
      !endif
      
        !ifdef MUI_FINISHPAGE_RUN
      
          !insertmacro MUI_INSTALLOPTIONS_READ ${MUI_TEMP1} "ioSpecial.ini" "Field 4" "State"
          
           StrCmp ${MUI_TEMP1} "1" "" +3
             !ifndef MUI_FINISHPAGE_RUN_PARAMETERS
               StrCpy ${MUI_TEMP1} "$\"${MUI_FINISHPAGE_RUN}$\""
             !else
               StrCpy ${MUI_TEMP1} "$\"${MUI_FINISHPAGE_RUN}$\" ${MUI_FINISHPAGE_RUN_PARAMETERS}"
             !endif
             Exec "${MUI_TEMP1}"
             
           !ifdef MUI_FINISHPAGE_SHOWREADME
          
             !insertmacro MUI_INSTALLOPTIONS_READ ${MUI_TEMP1} "ioSpecial.ini" "Field 5" "State"
            
             StrCmp ${MUI_TEMP1} "1" "" +2
               ExecShell "open" "${MUI_FINISHPAGE_SHOWREADME}"
               
           !endif
             
        !else ifdef MUI_FINISHPAGE_SHOWREADME
          
            !insertmacro MUI_INSTALLOPTIONS_READ ${MUI_TEMP1} "ioSpecial.ini" "Field 4" "State"
            
             StrCmp ${MUI_TEMP1} "1" "" +2
               ExecShell "open" "${MUI_FINISHPAGE_SHOWREADME}"
                              
          !endif
          
        !endif
        
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
      done:
    !endif

    Pop ${MUI_TEMP3}
    Pop ${MUI_TEMP2}
    Pop ${MUI_TEMP1}
    
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_FUNCTIONS_DESCRIPTION_BEGIN

  !ifndef MUI_NOVERBOSE
    !verbose 3
    !define MUI_NOVERBOSE
  !endif

  Function .onMouseOverSection
    !insertmacro MUI_DESCRIPTION_BEGIN

  !undef MUI_NOVERBOSE
  !verbose 4
  
!macroend

!macro MUI_FUNCTIONS_DESCRIPTION_END

  !ifndef MUI_NOVERBOSE
    !verbose 3
    !define MUI_NOVERBOSE
  !endif

    !insertmacro MUI_DESCRIPTION_END
  FunctionEnd

  !undef MUI_NOVERBOSE
  !verbose 4

!macroend

!macro MUI_FUNCTIONS_ABORTWARNING

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  !ifdef MUI_ABORTWARNING
    Function .onUserAbort
      !insertmacro MUI_ABORTWARNING
    FunctionEnd
  !endif

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_UNFUNCTIONS_GUIINIT

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif
  
  !ifndef MUI_UNCUSTOMGUIINIT

    Function un.onGUIInit
      !insertmacro MUI_UNGUIINIT
    FunctionEnd
    
  !endif

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_UNFUNCTIONS_PAGES
  
  !ifdef MUI_UNCONFIRMPAGE
    !insertmacro MUI_UNFUNCTIONS_CONFIRMPAGE un.SetUninstConfirm
  !endif
    
  !insertmacro MUI_UNFUNCTIONS_INSTFILESPAGE un.SetInstFiles
  
!macroend

!macro MUI_UNFUNCTIONS_CONFIRMPAGE UNSETUNINSTCONFIRM

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Function "${UNSETUNINSTCONFIRM}"
    !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_INTRO_TITLE) $(MUI_UNTEXT_INTRO_SUBTITLE)
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_UNFUNCTIONS_INSTFILESPAGE UNSETINSTFILES

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  Function ${UNSETINSTFILES}
    !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_UNINSTALLING_TITLE) $(MUI_UNTEXT_UNINSTALLING_SUBTITLE)
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif
  
!macroend

;--------------------------------
;RESERVE FILES

!macro MUI_RESERVEFILE_INSTALLOPTIONS

  !verbose 3

  ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
  
  !verbose 4
  
!macroend

!macro MUI_RESERVEFILE_SPECIALINI

  !verbose 3

  ReserveFile "${NSISDIR}\Contrib\Modern UI\ioSpecial.ini"
  
  !verbose 4
  
!macroend

!macro MUI_RESERVEFILE_SPECIALBITMAP

  !verbose 3

  ReserveFile "${NSISDIR}\Contrib\Icons\modern-wizard.bmp"
  
  !verbose 4
  
!macroend

;--------------------------------
;BASIC MACRO'S

!macro MUI_SYSTEM

  !verbose 3
  
  !define MUI_NOVERBOSE
  
  !ifndef MUI_LANGUAGEFILE_INSERTED
    !error "No Modern UI language file inserted!"
  !endif
  
  !insertmacro MUI_INTERFACE
  !insertmacro MUI_BASIC
  !insertmacro MUI_UNBASIC
  
  !undef MUI_NOVERBOSE
  
  !verbose 4
  
!macroend

!macro MUI_BASIC

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif

  !insertmacro MUI_FUNCTIONS_PAGES
  !insertmacro MUI_FUNCTIONS_GUIINIT
  !insertmacro MUI_FUNCTIONS_ABORTWARNING
  
  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_UNBASIC

  !ifndef MUI_NOVERBOSE
    !verbose 3
  !endif
  
  !ifdef MUI_UNINSTALLER

    !insertmacro MUI_UNFUNCTIONS_PAGES
    !insertmacro MUI_UNFUNCTIONS_GUIINIT
  
  !endif

  !ifndef MUI_NOVERBOSE
    !verbose 4
  !endif

!macroend

;--------------------------------
;LANGUAGE FILES

!macro MUI_LANGUAGEFILE_BEGIN LANGUAGE
  
  !ifndef "MUI_LANGUAGEFILE_${LANGUAGE}_USED"
  
    !define "MUI_LANGUAGEFILE_${LANGUAGE}_USED"

    LoadLanguageFile "${NSISDIR}\Contrib\Language files\${LANGUAGE}.nlf"

  !else

    !error "Modern UI language file ${LANGUAGE} included twice!"

  !endif
  
!macroend

!macro MUI_LANGUAGEFILE_STRING NAME VALUE

  !ifndef "${NAME}"
    !define "${NAME}" "${VALUE}"
  !endif

!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING NAME

  LangString "${NAME}" 0 "${${NAME}}"
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_CONTINUE NAME INSTALLBUTTON

  !ifndef "${INSTALLBUTTON}"
  
    !ifdef MUI_TEXT_CONTINUE_NEXT
      LangString "${NAME}" 0 "${${NAME}}${MUI_TEXT_CONTINUE_NEXT}"
    !else
      LangString "${NAME}" 0 "${${NAME}}"
    !endif
    
  !else
  
    !ifdef MUI_TEXT_CONTINUE_INSTALL
      LangString "${NAME}" 0 "${${NAME}}${MUI_TEXT_CONTINUE_INSTALL}"
    !else
      LangString "${NAME}" 0 "${${NAME}}"
    !endif
    
  !endif

  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_UNLANGSTRING NAME

  LangString "un.${NAME}" 0 "${${NAME}}"
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_NSISCOMMAND COMMAND NAME

  "${COMMAND}" "${${NAME}}"
  !undef "${NAME}"

!macroend

!macro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER COMMAND NAME VALUE

  "${COMMAND}" ${VALUE}
  !undef "${NAME}"

!macroend

!macro MUI_LANGUAGEFILE_NSISCOMMAND_CONTINUE COMMAND NAME INSTALLBUTTON

  !ifndef "${INSTALLBUTTON}"
  
    !ifdef MUI_TEXT_CONTINUE_NEXT
      "${COMMAND}" "${${NAME}} ${MUI_TEXT_CONTINUE_NEXT}"
    !else
      "${COMMAND}" "${${NAME}}"
    !endif
    
  !else
  
    !ifdef MUI_TEXT_CONTINUE_INSTALL
      "${COMMAND}" "${${NAME}} ${MUI_TEXT_CONTINUE_INSTALL}"
    !else
      "${COMMAND}" "${${NAME}}"
    !endif
    
  !endif
  
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_UNNSISCOMMAND_CONTINUE COMMAND NAME INSTALLBUTTON

  !ifndef "${INSTALLBUTTON}"
  
    !ifdef MUI_TEXT_CONTINUE_NEXT
      "${COMMAND}" "${${NAME}} ${MUI_TEXT_CONTINUE_NEXT}"
    !else
      "${COMMAND}" "${${NAME}}"
    !endif
    
  !else
  
    !ifdef MUI_UNTEXT_CONTINUE_UNINSTALL
      "${COMMAND}" "${${NAME}} ${MUI_UNTEXT_CONTINUE_UNINSTALL}"
    !else
      "${COMMAND}" "${${NAME}}"
    !endif
    
  !endif
  
  !undef "${NAME}"
  
!macroend


!macro MUI_LANGUAGEFILE_DEFINE DEFINE NAME

  !ifndef "${DEFINE}"
    !define "${DEFINE}" "${${NAME}}"
  !endif
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_END

  !ifdef MUI_FINISHPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_WINDOWTITLE"
  !endif
  
  !ifdef MUI_STARTMENUPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_STARTMENU_WINDOWTITLE"
  !endif

  !ifndef MUI_LANGUAGEFILE_INSERTED
  
    !define MUI_LANGUAGEFILE_INSERTED
    
    !define MUI_NOVERBOSE
    
    !insertmacro MUI_PAGECOMMANDS
    
    !ifdef MUI_UNINSTALLER
      !insertmacro MUI_UNPAGECOMMANDS
    !endif
    
    !undef MUI_NOVERBOSE
    
  !endif

  !insertmacro MUI_LANGUAGEFILE_DEFINE "MUI_${LANGUAGE}_LANGNAME" "MUI_LANGNAME"

  !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND Name MUI_NAME

  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_WELCOME_INFO_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CONTINUE "MUI_TEXT_WELCOME_INFO_TEXT" "MUI_INSTALLBUTTON_WELCOME"
  !endif

  !ifdef MUI_LICENSEPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_LICENSE_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_LICENSE_SUBTITLE"
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND "LicenseText" "MUI_INNERTEXT_LICENSE_TOP"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_LICENSE_BOTTOM"
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_COMPONENTS_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_COMPONENTS_SUBTITLE"
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_CONTINUE "ComponentText" "MUI_INNERTEXT_COMPONENTS_TOP" "MUI_INSTALLBUTTON_COMPONENTS"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO"
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_DIRECTORY_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_DIRECTORY_SUBTITLE"
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_CONTINUE "DirText" "MUI_INNERTEXT_DIRECTORY_TOP" "MUI_INSTALLBUTTON_DIRECTORY"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_DIRECTORY_DESTINATION"
  !endif
  
  !ifdef MUI_STARTMENUPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_STARTMENU_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_STARTMENU_SUBTITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CONTINUE "MUI_INNERTEXT_STARTMENU_TOP" "MUI_INSTALLBUTTON_STARTMENU"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_STARTMENU_CHECKBOX"
  !endif
  
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_INSTALLING_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_INSTALLING_SUBTITLE"
  
  !ifdef MUI_TEXT_FINISH_INFO_TITLE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_TITLE"
  !else
    LangString "MUI_TEXT_FINISH_TITLE" 0 "${MUI_TEXT_FINISH_TITLE}"
  !endif
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_SUBTITLE"
  
  !ifdef MUI_FINISHPAGE
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "MiscButtonText" "MUI_TEXT_FINISH_BUTTON" '"" "" "" "${MUI_TEXT_FINISH_BUTTON}"'
    !ifndef MUI_TEXT_FINISH_INFO_TITLE
      LangString "MUI_TEXT_FINISH_INFO_TITLE" 0 "${MUI_TEXT_FINISH_TITLE}"
      !undef MUI_TEXT_FINISH_TITLE
    !else
      !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_INFO_TITLE
    !endif
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_INFO_TEXT
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_INFO_REBOOT
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_REBOOTNOW
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_REBOOTLATER
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_RUN
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING MUI_TEXT_FINISH_SHOWREADME
  !endif
  
  !ifdef MUI_ABORTWARNING
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_ABORTWARNING"
  !endif
  
  
  !ifdef MUI_UNINSTALLER
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_INTRO_TITLE"
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_INTRO_SUBTITLE"
  
    !ifdef MUI_UNCONFIRMPAGE
      !insertmacro MUI_LANGUAGEFILE_UNNSISCOMMAND_CONTINUE "UninstallText" "MUI_UNINNERTEXT_INTRO" "MUI_UNINSTALLBUTTON_CONFIRM"
    !endif
  
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_UNINSTALLING_TITLE"
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_UNINSTALLING_SUBTITLE"
     
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_FINISHED_TITLE"
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_FINISHED_SUBTITLE"
  !endif
  
  !ifdef MUI_TEXT_CONTINUE_NEXT
    !undef MUI_TEXT_CONTINUE_NEXT
  !endif

  !ifdef MUI_TEXT_CONTINUE_INSTALL
    !undef MUI_TEXT_CONTINUE_INSTALL
  !endif
  
  !ifdef MUI_UNTEXT_CONTINUE_UNINSTALL
    !undef MUI_UNTEXT_CONTINUE_UNINSTALL
  !endif
    
!macroend

;--------------------------------
;END

!endif

!verbose 4