;NSIS Modern User Interface version 1.62
;Macro System
;Written by Joost Verburg

;Have a look the scripts in the 'Examples\Modern UI'
;directory for examples of usage.

;--------------------------------

!ifndef MUI_MANUALVERBOSE
  !verbose 3
!endif

!ifndef MUI_MACROS_USED

!define MUI_MACROS_USED

!include "${NSISDIR}\Include\WinMessages.nsh"

!define MUI_TEMP1 $R0
!define MUI_TEMP2 $R1
!define MUI_TEMP3 $R2

!macro MUI_INTERFACE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
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

  !ifndef MUI_SPECIALINI
    !define MUI_SPECIALINI "${NSISDIR}\Contrib\Modern UI\ioSpecial.ini"
  !endif
  
  !ifndef MUI_SPECIALBITMAP
    !define MUI_SPECIALBITMAP "${NSISDIR}\Contrib\Icons\modern-wizard.bmp"
  !endif
  
  !ifndef MUI_BGCOLOR
    !define MUI_BGCOLOR "0xFFFFFF"
  !endif
  
  !ifdef MUI_FINISHPAGE
    !ifndef MUI_FINISHPAGE_NOAUTOCLOSE
      AutoCloseWindow true
    !endif
  !endif

  XPStyle On

  ChangeUI all "${MUI_UI}"
  Icon "${MUI_ICON}"
  
  !ifdef MUI_UNINSTALLER
    UninstallIcon "${MUI_UNICON}"
  !endif
  
  CheckBitmap "${MUI_CHECKBITMAP}"
  SetFont "${MUI_FONT}" "${MUI_FONTSIZE}"
  InstallColors ${MUI_INSTALLCOLORS}
  InstProgressFlags ${MUI_PROGRESSBAR}

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_INNERDIALOG_TEXT CONTROL TEXT

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ;Set text on inner dialogs component
  Push ${MUI_TEMP1}

    FindWindow ${MUI_TEMP1} "#32770" "" $HWNDPARENT
    GetDlgItem ${MUI_TEMP1} ${MUI_TEMP1} ${CONTROL}
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"

  Pop ${MUI_TEMP1}

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_HEADER_TEXT TEXT SUBTEXT

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ;Set text on the white rectangle
  Push ${MUI_TEMP1}

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1037
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1038
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${SUBTEXT}"

  Pop ${MUI_TEMP1}

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_DESCRIPTION_BEGIN

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Push ${MUI_TEMP1}

  FindWindow ${MUI_TEMP1} "#32770" "" $HWNDPARENT
  GetDlgItem ${MUI_TEMP1} ${MUI_TEMP1} 1043

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_DESCRIPTION_TEXT VAR TEXT

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ;Set text on the Description frame

  StrCmp $0 ${VAR} "" +3
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
    Goto done

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_DESCRIPTION_END

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  done:
  Pop ${MUI_TEMP1}

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_FINISHHEADER

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ;Finish text on the header (white rectangle)
  !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_FINISH_TITLE) $(MUI_TEXT_FINISH_SUBTITLE)

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_UNFINISHHEADER

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  !define MUI_NOVERBOSE
  
  ;Finish text on the header (white rectangle)
  !insertmacro MUI_HEADER_TEXT $(un.MUI_UNTEXT_FINISHED_TITLE) $(un.MUI_UNTEXT_FINISHED_SUBTITLE)

  !undef MUI_NOVERBOSE

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_ABORTWARNING

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ;Warning when Cancel button is pressed

  MessageBox MB_YESNO|MB_ICONEXCLAMATION "$(MUI_TEXT_ABORTWARNING)" IDYES quit
    Abort
    quit:

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_GUIINIT
  
  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_WELCOMEFINISHPAGE_INIT
  !endif
  
  !ifndef MUI_WELCOMEPAGE
    !ifdef MUI_FINISHPAGE
      !insertmacro MUI_WELCOMEFINISHPAGE_INIT
    !endif
  !endif

  !insertmacro MUI_GUIINIT_BASIC
  
!macroend

!macro MUI_UNGUIINIT

  !insertmacro MUI_GUIINIT_BASIC
  
!macroend

!macro MUI_GUIINIT_BASIC

  Push ${MUI_TEMP1}
  Push ${MUI_TEMP2}

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1037
    CreateFont ${MUI_TEMP2} "${MUI_FONT_HEADER}" "${MUI_FONTSIZE_HEADER}" "${MUI_FONTSTYLE_HEADER}"
    SendMessage ${MUI_TEMP1} ${WM_SETFONT} ${MUI_TEMP2} 0
    SetStaticBkColor ${MUI_TEMP1} "${MUI_BGCOLOR}"

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1038
    SetStaticBkColor ${MUI_TEMP1} "${MUI_BGCOLOR}"

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1034
    SetStaticBkColor ${MUI_TEMP1} "${MUI_BGCOLOR}"

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1039
    SetStaticBkColor ${MUI_TEMP1} "${MUI_BGCOLOR}"

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1028
    SetStaticBkColor ${MUI_TEMP1} -1
    GetWindowText ${MUI_TEMP2} ${MUI_TEMP1}
    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1256
    SetStaticBkColor ${MUI_TEMP1} -1
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} ${NSIS_MAX_STRLEN} "STR:${MUI_TEMP2}"

  Pop ${MUI_TEMP2}
  Pop ${MUI_TEMP1}

!macroend

!macro MUI_WELCOMEFINISHPAGE_INIT

  ;Extract InstallOptions INI Files
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS "${MUI_SPECIALINI}" "ioSpecial.ini"
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS "${MUI_SPECIALBITMAP}" "modern-wizard.bmp"   

  ;Write bitmap location
  !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 1" "Text" "$PLUGINSDIR\modern-wizard.bmp"

  ;Write Welcome text
  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 2" "Text" "$(MUI_TEXT_WELCOME_INFO_TITLE)"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_TEXT_WELCOME_INFO_TEXT)"
  !endif

!macroend

!macro MUI_LANGUAGE LANGUAGE

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  !include "${NSISDIR}\Contrib\Modern UI\Language files\${LANGUAGE}.nsh"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_LANGDLL_PUSH LANGUAGE

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  Push "${LANG_${LANGUAGE}}"
  Push "${MUI_${LANGUAGE}_LANGNAME}"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
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
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
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
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_WELCOME

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !ifdef MUI_WELCOMEPAGE
    Page custom mui.Welcome "" "MUI_INSTALLBUTTON_WELCOME"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_LICENSE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !ifdef MUI_LICENSEPAGE
    Page license mui.LicensePre mui.LicenseShow mui.LicenseLeave
  !endif
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_COMPONENTS

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !ifdef MUI_COMPONENTSPAGE
    Page components mui.ComponentsPre mui.ComponentsShow mui.ComponentsLeave "MUI_INSTALLBUTTON_COMPONENTS"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_DIRECTORY

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !ifdef MUI_DIRECTORYPAGE
    Page directory mui.DirectoryPre mui.DirectoryShow mui.DirectoryLeave "MUI_INSTALLBUTTON_DIRECTORY"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_STARTMENU

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !ifdef MUI_STARTMENUPAGE
    Page custom mui.Startmenu "$(MUI_TEXT_STARTMENU_WINDOWTITLE)" "MUI_INSTALLBUTTON_STARTMENU"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_PAGECOMMAND_INSTFILES

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Page instfiles mui.InstFilesPre mui.InstFilesShow mui.InstFilesLeave
   
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
   
!macroend

!macro MUI_PAGECOMMAND_FINISH

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !ifdef MUI_FINISHPAGE
    Page custom mui.Finish "$(MUI_TEXT_FINISH_WINDOWTITLE)"
  !endif
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_UNPAGECOMMANDS
 
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif
  
  !ifndef MUI_UNCUSTOMPAGECOMMANDS
    !insertmacro MUI_UNPAGECOMMAND_CONFIRM
    !insertmacro MUI_UNPAGECOMMAND_INSTFILES
  !endif
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_UNPAGECOMMAND_CONFIRM

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !ifdef MUI_UNCONFIRMPAGE
    UninstPage uninstConfirm un.mui.ConfirmPre un.mui.ConfirmShow un.mui.ConfirmLeave "MUI_UNINSTALLBUTTON_CONFIRM"
  !endif
   
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
   
!macroend

!macro MUI_UNPAGECOMMAND_INSTFILES

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  UninstPage instfiles un.mui.InstFilesPre un.mui.InstFilesShow un.mui.InstFilesLeave
   
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
   
!macroend

;--------------------------------
;INSTALL OPTIONS

!macro MUI_INSTALLOPTIONS_EXTRACT FILE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ;Init plugin system
  InitPluginsDir

  File "/oname=$PLUGINSDIR\${FILE}" "${FILE}"

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_EXTRACT_AS FILE FILENAME

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ;Init plugin system
  InitPluginsDir

  File "/oname=$PLUGINSDIR\${FILENAME}" "${FILE}"

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_DISPLAY FILE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif
  
  Push ${MUI_TEMP1}
  
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop ${MUI_TEMP1}
  
  Pop ${MUI_TEMP1}

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_INITDIALOG FILE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif
  
  InstallOptions::initDialog /NOUNLOAD "$PLUGINSDIR\${FILE}"

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_SHOW

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  InstallOptions::show

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_READ VAR FILE SECTION KEY

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ReadIniStr ${VAR} "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}"

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_WRITE FILE SECTION KEY VALUE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  WriteIniStr "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}" "${VALUE}"

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

;--------------------------------
;SECTIONS

!macro MUI_SECTIONS_FINISHHEADER

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  !define MUI_NOVERBOSE

  Section ""

    ;Invisible section to display the Finish header
    !insertmacro MUI_FINISHHEADER

  SectionEnd
  
  !undef MUI_NOVERBOSE
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

;--------------------------------
;FUNCTIONS

!macro MUI_FUNCTIONS_GUIINIT

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif
  
  Function .onGUIInit
     
  !insertmacro MUI_GUIINIT
  
  !ifdef MUI_CUSTOMFUNCTION_GUIINIT
    Call "${MUI_CUSTOMFUNCTION_GUIINIT}"
  !endif

  FunctionEnd

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_FUNCTIONS_PAGES

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_FUNCTIONS_WELCOMEPAGE mui.Welcome
  !endif

  !ifdef MUI_LICENSEPAGE
    !insertmacro MUI_FUNCTIONS_LICENSEPAGE mui.LicensePre mui.LicenseShow mui.LicenseLeave
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    !insertmacro MUI_FUNCTIONS_COMPONENTSPAGE mui.ComponentsPre mui.ComponentsShow mui.ComponentsLeave
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    !insertmacro MUI_FUNCTIONS_DIRECTORYPAGE mui.DirectoryPre mui.DirectoryShow mui.DirectoryLeave
  !endif
  
  !ifdef MUI_STARTMENUPAGE
    !insertmacro MUI_FUNCTIONS_STARTMENUPAGE mui.Startmenu
  !endif
  
  !insertmacro MUI_FUNCTIONS_INSTFILESPAGE mui.InstFilesPre mui.InstFilesShow mui.InstFilesLeave
    
  !ifdef MUI_FINISHPAGE
    !insertmacro MUI_FUNCTIONS_FINISHPAGE mui.Finish
  !endif

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_FUNCTIONS_WELCOMEPAGE FUNCTION

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Function "${FUNCTION}"

    !ifdef MUI_CUSTOMFUNCTION_WELCOME_PRE
      Call "${MUI_CUSTOMFUNCTION_WELCOME_PRE}"
    !endif
  
    Push ${MUI_TEMP1}
    Push ${MUI_TEMP2}
    Push ${MUI_TEMP3}

      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1028
      ShowWindow ${MUI_TEMP1} ${SW_HIDE}

      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1256
      ShowWindow ${MUI_TEMP1} ${SW_HIDE}

      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1035
      ShowWindow ${MUI_TEMP1} ${SW_HIDE}
      
      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1045
      ShowWindow ${MUI_TEMP1} ${SW_NORMAL}

      !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ioSpecial.ini"
      
        Pop ${MUI_TEMP1}
        
        SetStaticBkColor ${MUI_TEMP1} "${MUI_BGCOLOR}"
      
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1201
        SetStaticBkColor ${MUI_TEMP2} "${MUI_BGCOLOR}"
        CreateFont ${MUI_TEMP3} "${MUI_FONT_TITLE}" "${MUI_FONTSIZE_TITLE}" "${MUI_FONTSTYLE_TITLE}"
        SendMessage ${MUI_TEMP2} ${WM_SETFONT} ${MUI_TEMP3} 0
        
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1202
        SetStaticBkColor ${MUI_TEMP2} "${MUI_BGCOLOR}"

	!ifdef MUI_CUSTOMFUNCTION_WELCOME_SHOW
          Call "${MUI_CUSTOMFUNCTION_WELCOME_SHOW}"
        !endif
  
      !insertmacro MUI_INSTALLOPTIONS_SHOW
      
      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1028
      ShowWindow ${MUI_TEMP1} ${SW_NORMAL}

      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1256
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
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_FUNCTIONS_LICENSEPAGE PRE SHOW LEAVE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Function "${PRE}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
    !ifdef MUI_CUSTOMFUNCTION_LICENSE_PRE
      Call "${MUI_CUSTOMFUNCTION_LICENSE_PRE}"
    !endif
  FunctionEnd

  Function "${SHOW}"
    !insertmacro MUI_INNERDIALOG_TEXT 1040 $(MUI_INNERTEXT_LICENSE_BOTTOM)
    !ifdef MUI_CUSTOMFUNCTION_LICENSE_SHOW
      Call "${MUI_CUSTOMFUNCTION_LICENSE_SHOW}"
    !endif
  FunctionEnd
  
  Function "${LEAVE}"
    !ifdef MUI_CUSTOMFUNCTION_LICENSE_LEAVE
      Call "${MUI_CUSTOMFUNCTION_LICENSE_LEAVE}"
    !endif
  FunctionEnd

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
    
!macroend

!macro MUI_FUNCTIONS_COMPONENTSPAGE PRE SHOW LEAVE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Function "${PRE}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_COMPONENTS_TITLE) $(MUI_TEXT_COMPONENTS_SUBTITLE)
    !ifdef MUI_CUSTOMFUNCTION_COMPONENTS_PRE
      Call "${MUI_CUSTOMFUNCTION_COMPONENTS_PRE}"
    !endif
  FunctionEnd

  Function "${SHOW}"
    !insertmacro MUI_INNERDIALOG_TEXT 1042 $(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE)
    !insertmacro MUI_INNERDIALOG_TEXT 1043 $(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO)
    !ifdef MUI_CUSTOMFUNCTION_COMPONENTS_SHOW
      Call "${MUI_CUSTOMFUNCTION_COMPONENTS_SHOW}"
    !endif
  FunctionEnd

  Function "${LEAVE}"
    !ifdef MUI_CUSTOMFUNCTION_COMPONENTS_LEAVE
      Call "${MUI_CUSTOMFUNCTION_COMPONENTS_LEAVE}"
    !endif
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
    
!macroend

!macro MUI_FUNCTIONS_DIRECTORYPAGE PRE SHOW LEAVE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Function "${PRE}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_DIRECTORY_TITLE) $(MUI_TEXT_DIRECTORY_SUBTITLE)
    !ifdef MUI_CUSTOMFUNCTION_DIRECTORY_PRE
      Call "${MUI_CUSTOMFUNCTION_DIRECTORY_PRE}"
    !endif
  FunctionEnd

  Function "${SHOW}"
    !insertmacro MUI_INNERDIALOG_TEXT 1041 $(MUI_INNERTEXT_DIRECTORY_DESTINATION)
    !ifdef MUI_CUSTOMFUNCTION_DIRECTORY_SHOW
      Call "${MUI_CUSTOMFUNCTION_DIRECTORY_SHOW}"
    !endif
  FunctionEnd
  
  Function "${LEAVE}"
    !ifdef MUI_CUSTOMFUNCTION_DIRECTORY_LEAVE
      Call "${MUI_CUSTOMFUNCTION_DIRECTORY_LEAVE}"
    !endif
  FunctionEnd

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_FUNCTIONS_STARTMENUPAGE FUNCTION

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif
  
  ;Check defines
  !ifndef MUI_STARTMENU_VARIABLE
    !define MUI_STARTMENU_VARIABLE "$9"
  !endif
  !ifndef MUI_STARTMENU_DEFAULTFOLDER
    !define MUI_STARTMENU_DEFAULTFOLDER "${MUI_PRODUCT}"
  !endif
  
  Function "${FUNCTION}"

  Push ${MUI_TEMP1}
  
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_STARTMENU_TITLE) $(MUI_TEXT_STARTMENU_SUBTITLE)
    
    !ifdef MUI_STARTMENU_REGISTRY_ROOT
      !ifdef MUI_STARTMENU_REGISTRY_KEY
        !ifdef MUI_STARTMENU_REGISTRY_VALUENAME
        
          StrCmp "${MUI_STARTMENU_VARIABLE}" "" "" +4

            ReadRegStr ${MUI_TEMP1} "${MUI_STARTMENU_REGISTRY_ROOT}" "${MUI_STARTMENU_REGISTRY_KEY}" "${MUI_STARTMENU_REGISTRY_VALUENAME}"
            StrCmp ${MUI_TEMP1} "" +2
              StrCpy "${MUI_STARTMENU_VARIABLE}" ${MUI_TEMP1}

        !endif
      !endif
    !endif

    StartMenu::Select /noicon /autoadd /text "$(MUI_INNERTEXT_STARTMENU_TOP)" /lastused "${MUI_STARTMENU_VARIABLE}" /checknoshortcuts "$(MUI_INNERTEXT_STARTMENU_CHECKBOX)" "${MUI_STARTMENU_DEFAULTFOLDER}"
      
    Pop ${MUI_TEMP1}
    
    StrCmp ${MUI_TEMP1} "success" 0 +2
      Pop "${MUI_STARTMENU_VARIABLE}"

  Pop ${MUI_TEMP1}

  !ifdef MUI_CUSTOMFUNCTION_START
    Call "${MUI_CUSTOMFUNCTION_START}"
  !endif

  FunctionEnd

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_FUNCTIONS_INSTFILESPAGE PRE SHOW LEAVE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Function "${PRE}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_INSTALLING_TITLE) $(MUI_TEXT_INSTALLING_SUBTITLE)
    !ifdef MUI_CUSTOMFUNCTION_INSTFILES_PRE
      Call "${MUI_CUSTOMFUNCTION_INSTFILES_PRE}"
    !endif
  FunctionEnd

  Function "${SHOW}"
    !ifdef MUI_CUSTOMFUNCTION_INSTFILES_SHOW
      Call "${MUI_CUSTOMFUNCTION_INSTFILES_SHOW}"
    !endif
  FunctionEnd

  Function "${LEAVE}"
    !ifdef MUI_CUSTOMFUNCTION_INSTFILES_LEAVE
      Call "${MUI_CUSTOMFUNCTION_INSTFILES_LEAVE}"
    !endif
  FunctionEnd

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_FUNCTIONS_FINISHPAGE FUNCTION

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Function "${FUNCTION}"
  
    !ifdef MUI_CUSTOMFUNCTION_FINISH_PRE
      Call "${MUI_CUSTOMFUNCTION_FINISH_PRE}"
    !endif

    Push ${MUI_TEMP1}
    Push ${MUI_TEMP2}
    Push ${MUI_TEMP3}
    
    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1028
    ShowWindow ${MUI_TEMP1} ${SW_HIDE}

    GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1256
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
        
        SetStaticBkColor ${MUI_TEMP1} "${MUI_BGCOLOR}"
      
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1201
        SetStaticBkColor ${MUI_TEMP2} "${MUI_BGCOLOR}"
        CreateFont ${MUI_TEMP3} "${MUI_FONT_TITLE}" "${MUI_FONTSIZE_TITLE}" "${MUI_FONTSTYLE_TITLE}"
        SendMessage ${MUI_TEMP2} ${WM_SETFONT} ${MUI_TEMP3} 0
        
        GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1202
        SetStaticBkColor ${MUI_TEMP2} "${MUI_BGCOLOR}"
        
        !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
        
          IfRebootFlag "" noreboot_show
        
            GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1203
            SetStaticBkColor ${MUI_TEMP2} "${MUI_BGCOLOR}"
            
            GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1204
            SetStaticBkColor ${MUI_TEMP2} "${MUI_BGCOLOR}"
            
            Goto show
        
          noreboot_show:
          
        !endif
        
          !ifdef MUI_FINISHPAGE_RUN
          
            GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1203
            SetStaticBkColor ${MUI_TEMP2} "${MUI_BGCOLOR}"
            
            !ifdef MUI_FINISHPAGE_SHOWREADME
            
              GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1204
              SetStaticBkColor ${MUI_TEMP2} "${MUI_BGCOLOR}"
              
            !endif
          
          !else ifdef MUI_FINISHPAGE_SHOWREADME
            
              GetDlgItem ${MUI_TEMP2} ${MUI_TEMP1} 1203
              SetStaticBkColor ${MUI_TEMP2} "${MUI_BGCOLOR}"
            
          !endif
        
        !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
          show:
        !endif

      !ifdef MUI_CUSTOMFUNCTION_FINISH_SHOW
        Call "${MUI_CUSTOMFUNCTION_FINISH_SHOW}"
      !endif

      !insertmacro MUI_INSTALLOPTIONS_SHOW
      
      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1028
      ShowWindow ${MUI_TEMP1} ${SW_NORMAL}

      GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1256
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

    !ifdef MUI_CUSTOMFUNCTION_FINISH
      Call "${MUI_CUSTOMFUNCTION_FINISH}"
    !endif
    
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_FUNCTIONS_DESCRIPTION_BEGIN

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  !define MUI_NOVERBOSE

  Function .onMouseOverSection
    !insertmacro MUI_DESCRIPTION_BEGIN

  !undef MUI_NOVERBOSE
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_FUNCTIONS_DESCRIPTION_END

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

    !insertmacro MUI_DESCRIPTION_END
  FunctionEnd

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_FUNCTIONS_ABORTWARNING

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !ifdef MUI_ABORTWARNING
    Function .onUserAbort
      !insertmacro MUI_ABORTWARNING
    FunctionEnd
  !endif

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_UNFUNCTION_GUIINIT

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif
  
  Function un.onGUIInit
    
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !insertmacro MUI_UNGUIINIT
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
  !ifdef MUI_CUSTOMFUNCTION_UNGUIINIT
    Call "${MUI_CUSTOMFUNCTION_UNGUIINIT}
  !endif
  
  FunctionEnd

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_UNFUNCTION_PAGES
  
  !ifdef MUI_UNCONFIRMPAGE
    !insertmacro MUI_UNFUNCTION_CONFIRMPAGE un.mui.ConfirmPre un.mui.ConfirmShow un.mui.ConfirmLeave
  !endif
    
  !insertmacro MUI_UNFUNCTION_INSTFILESPAGE un.mui.InstFilesPre un.mui.InstFilesShow un.mui.InstFilesLeave
  
!macroend

!macro MUI_UNFUNCTION_CONFIRMPAGE PRE SHOW LEAVE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Function "${PRE}"
    !insertmacro MUI_HEADER_TEXT $(un.MUI_UNTEXT_INTRO_TITLE) $(un.MUI_UNTEXT_INTRO_SUBTITLE)
    !ifdef MUI_UNCUSTOMFUNCTION_CONFIRM_PRE
      Call "{MUI_UNCUSTOMFUNCTION_CONFIRM_PRE}"
    !endif
  FunctionEnd
  
  Function "${SHOW}"
    !ifdef MUI_UNCUSTOMFUNCTION_CONFIRM_SHOW
      Call "{MUI_UNCUSTOMFUNCTION_CONFIRM_SHOW}"
    !endif
  FunctionEnd
  
  Function "${LEAVE}"
    !ifdef MUI_UNCUSTOMFUNCTION_CONFIRM_LEAVE
      Call "{MUI_UNCUSTOMFUNCTION_CONFIRM_LEAVE}"
    !endif
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_UNFUNCTION_INSTFILESPAGE PRE SHOW LEAVE

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  Function ${PRE}
    !insertmacro MUI_HEADER_TEXT $(un.MUI_UNTEXT_UNINSTALLING_TITLE) $(un.MUI_UNTEXT_UNINSTALLING_SUBTITLE)
    !ifdef MUI_UNCUSTOMFUNCTION_INSTFILES_PRE
      Call "{MUI_UNCUSTOMFUNCTION_INSTFILES_PRE}"
    !endif
  FunctionEnd

  Function "${SHOW}"
    !ifdef MUI_UNCUSTOMFUNCTION_INSTFILES_SHOW
      Call "{MUI_UNCUSTOMFUNCTION_INSTFILES_SHOW}"
    !endif
  FunctionEnd
  
  Function "${LEAVE}"
    !ifdef MUI_UNCUSTOMFUNCTION_INSTFILES_LEAVE
      Call "{MUI_UNCUSTOMFUNCTION_INSTFILES_LEAVE}"
    !endif
  FunctionEnd
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

;--------------------------------
;RESERVE FILES

!macro MUI_RESERVEFILE_WELCOMEFINISHPAGE

  !verbose 3
  
    !define MUI_NOVERBOSE

    !insertmacro MUI_RESERVEFILE_SPECIALINI
    !insertmacro MUI_RESERVEFILE_SPECIALBITMAP
    !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS
    
    !undef MUI_NOVERBOSE
    
  !verbose 4
    
!macroend

!macro MUI_RESERVEFILE_INSTALLOPTIONS

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_RESERVEFILE_SPECIALINI

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ReserveFile "${NSISDIR}\Contrib\Modern UI\ioSpecial.ini"
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_RESERVEFILE_SPECIALBITMAP

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  ReserveFile "${NSISDIR}\Contrib\Icons\modern-wizard.bmp"
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif
  
!macroend

!macro MUI_RESERVEFILE_LANGDLL

  !verbose 3
  
  ReserveFile "${NSISDIR}\Plugins\LangDLL.dll"
  
  !verbose 4
  
!macroend

;--------------------------------
;BASIC MACRO'S

!macro MUI_SYSTEM

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  !define MUI_NOVERBOSE

  !insertmacro MUI_INTERFACE  
  !insertmacro MUI_BASIC
  !insertmacro MUI_UNBASIC
  
  !undef MUI_NOVERBOSE
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_BASIC

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif

  !insertmacro MUI_PAGECOMMANDS
  !insertmacro MUI_FUNCTIONS_PAGES
  !insertmacro MUI_FUNCTIONS_GUIINIT
  !insertmacro MUI_FUNCTIONS_ABORTWARNING
  
  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
  !endif

!macroend

!macro MUI_UNBASIC

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 3
    !endif
  !endif
  
  !ifdef MUI_UNINSTALLER

    !insertmacro MUI_UNPAGECOMMANDS
    !insertmacro MUI_UNFUNCTION_PAGES
    !insertmacro MUI_UNFUNCTION_GUIINIT
  
  !endif

  !ifndef MUI_NOVERBOSE
    !ifndef MUI_MANUALVERBOSE
      !verbose 4
    !endif
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
    LangString "${NAME}" 0 "${${NAME}}${MUI_TEXT_CONTINUE_NEXT}"
  !else
    LangString "${NAME}" 0 "${${NAME}}${MUI_TEXT_CONTINUE_INSTALL}"
  !endif

  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_UNLANGSTRING NAME

  LangString "un.${NAME}" 0 "${${NAME}}"
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_NSISCOMMAND COMMAND NAME

  ${COMMAND} "${${NAME}}"
  !undef "${NAME}"

!macroend

!macro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER COMMAND NAME VALUE

  ${COMMAND} ${VALUE}
  !undef "${NAME}"

!macroend

!macro MUI_LANGUAGEFILE_NSISCOMMAND_CONTINUE COMMAND NAME INSTALLBUTTON

  !ifndef "${INSTALLBUTTON}"
     ${COMMAND} "${${NAME}} ${MUI_TEXT_CONTINUE_NEXT}"
  !else
    ${COMMAND} "${${NAME}} ${MUI_TEXT_CONTINUE_INSTALL}"
  !endif
  
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_UNNSISCOMMAND_CONTINUE COMMAND NAME INSTALLBUTTON

  !ifndef "${INSTALLBUTTON}"
    ${COMMAND} "${${NAME}} ${MUI_TEXT_CONTINUE_NEXT}"
  !else
    !ifdef MUI_UNTEXT_CONTINUE_UNINSTALL
      ${COMMAND} "${${NAME}} ${MUI_UNTEXT_CONTINUE_UNINSTALL}"
    !else
      ;Modern UI 1.61 Language File compatibility
      ${COMMAND} "${${NAME}}"
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

  !insertmacro MUI_LANGUAGEFILE_DEFINE "MUI_${LANGUAGE}_LANGNAME" "MUI_LANGNAME"

  !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND "Name" "MUI_NAME"
  
  !ifndef MUI_BRANDINGTEXT
    !define MUI_BRANDINGTEXT ""
  !endif
  !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND "BrandingText" "MUI_BRANDINGTEXT"

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
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_STARTMENU_WINDOWTITLE"
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
    ;Modern UI 1.61 Language File compatibility 
    LangString "MUI_TEXT_FINISH_TITLE" 0 "${MUI_TEXT_FINISH_TITLE}"
  !endif
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_SUBTITLE"
  
  !ifdef MUI_FINISHPAGE
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "MiscButtonText" "MUI_TEXT_FINISH_BUTTON" '"" "" "" "${MUI_TEXT_FINISH_BUTTON}"'
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_WINDOWTITLE"
    !ifdef MUI_TEXT_FINISH_INFO_TITLE
      !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_INFO_TITLE"
    !else
      ;Modern UI 1.61 Language File compatibility 
      LangString "MUI_TEXT_FINISH_INFO_TITLE" 0 "${MUI_TEXT_FINISH_TITLE}"
      !undef MUI_TEXT_FINISH_TITLE
    !endif
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_INFO_TEXT"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_INFO_REBOOT"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_REBOOTNOW"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_REBOOTLATER"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_RUN"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_SHOWREADME"
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
  
  !undef MUI_TEXT_CONTINUE_NEXT
  !undef MUI_TEXT_CONTINUE_INSTALL
  
  !ifdef MUI_UNTEXT_CONTINUE_UNINSTALL
    !undef MUI_UNTEXT_CONTINUE_UNINSTALL
  !endif
    
!macroend

;--------------------------------
;END

!endif

!ifndef MUI_MANUALVERBOSE
  !verbose 4
!endif