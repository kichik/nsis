;NSIS Modern User Interface version 1.4
;Macro System
;Written by Joost Verburg

;See Basic.nsi, Multilanguage.nsi and InstallOptions.nsi
;in the Examples\Modern UI directory for examples of usage.

;--------------------------------
!verbose 3

!ifndef MUI_MACROS_USED

!define MUI_MACROS_USED

!include "${NSISDIR}\Examples\WinMessages.nsh"

!define MUI_TEMP1 $R0
!define MUI_TEMP2 $R1

!macro MUI_INTERFACE

  !verbose 3

  ;User interface

  !ifndef MUI_ICON
    !define MUI_ICON "${NSISDIR}\Contrib\Icons\modern-install.ico"
  !endif

  !ifndef MUI_UNICON
    !define MUI_UNICON "${NSISDIR}\Contrib\Icons\modern-uninstall.ico"
  !endif

  !ifndef MUI_CHECKBITMAP
    !define MUI_CHECKBITMAP "${NSISDIR}\Contrib\Icons\modern.bmp"
  !endif

  !ifndef MUI_UI
    !define MUI_UI "${NSISDIR}\Contrib\UIs\modern.exe"
  !endif

  !ifndef MUI_FONT
    !define MUI_FONT "Tahoma"
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

  XPStyle On

  Icon "${MUI_ICON}"
  UninstallIcon "${MUI_UNICON}"
  CheckBitmap "${MUI_CHECKBITMAP}"
  ChangeUI all "${MUI_UI}"
  SetFont "${MUI_FONT}" 8
  InstallColors "${MUI_INSTALLCOLORS}"
  InstProgressFlags "${MUI_PROGRESSBAR}"
  BrandingText /TRIMRIGHT "${MUI_BRANDINGTEXT}"

  !verbose 4

!macroend

!macro MUI_INTERFACE_ALLRES UI ICON UNICON CHECKS PROGRESSBAR FONT
  !define MUI_UI ${UI}
  !define MUI_ICON ${ICON}
  !define MUI_UNICON ${ICON}
  !define MUI_CHECKBITMAP ${CHECKS}
  !define MUI_PROGRESSBAR ${PROGRESSBAR}
  !define MUI_FONT ${FONT}
  !insertmacro MUI_INTERFACE
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

!macro MUI_DESCRIPTION_INIT

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
  !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_FINISHED_TITLE) $(MUI_TEXT_FINISHED_SUBTITLE)

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

!macro MUI_PAGECOMMANDS
 
  !verbose 3
  
  !ifndef MUI_CUSTOMPAGECOMMANDS

    !insertmacro MUI_PAGECOMMAND_LICENSE
    !insertmacro MUI_PAGECOMMAND_COMPONENTS
    !insertmacro MUI_PAGECOMMAND_DIRECTORY
    !insertmacro MUI_PAGECOMMAND_INSTFILES
  
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
    Page components SetComponents SetComponentsDialog
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_PAGECOMMAND_DIRECTORY

  !verbose 3

  !ifdef MUI_DIRECTORYPAGE
    Page directory SetDirectory SetDirectoryDialog
  !endif
  
  !verbose 4
  
!macroend

!macro MUI_PAGECOMMAND_INSTFILES

  !verbose 3

  Page instfiles SetInstFiles
   
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
    Call Initialize_____Plugins
    SetDetailsPrint both
  !endif

  File /oname=$PLUGINSDIR\${FILE} "${FILE}"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_UNEXTRACT FILE

  !verbose 3

  ;Init plugin system
  !ifndef MUI_INSTALLOPTIONS_UNINITPLUGINS
    !define MUI_INSTALLOPTIONS_UNINITPLUGINS
    Call un.Initialize_____Plugins
    SetDetailsPrint both
  !endif

  File /oname=$PLUGINSDIR\${FILE} "${FILE}"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_SHOW FILE

  !verbose 3

  Push ${MUI_TEMP1}

  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
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
;FUNCTIONS

!macro MUI_FUNCTIONS_GUIINIT

  !verbose 3

  Function .onGUIInit
    !insertmacro MUI_GUIINIT
  FunctionEnd

  !verbose 4

!macroend

!macro MUI_FUNCTIONS_PAGES

  !verbose 3

  !ifdef MUI_LICENSEPAGE
    !insertmacro MUI_FUNCTIONS_LICENSEPAGE SetLicense SetLicenseDialog
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    !insertmacro MUI_FUNCTIONS_COMPONENTSPAGE SetComponents SetComponentsDialog
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    !insertmacro MUI_FUNCTIONS_DIRECTORYPAGE SetDirectory SetDirectoryDialog
  !endif
  
  !insertmacro MUI_FUNCTIONS_INSTFILESPAGE SetInstFiles

  !verbose 4

!macroend

!macro MUI_FUNCTIONS_LICENSEPAGE SETLICENSE SETLICENSEDIALOG

  !verbose 3

  Function "${SETLICENSE}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
  FunctionEnd

  Function "${SETLICENSEDIALOG}"
    !insertmacro MUI_INNERDIALOG_TEXT 1040 $(MUI_INNERTEXT_LICENSE)
  FunctionEnd
  
  !verbose 4
    
!macroend

!macro MUI_FUNCTIONS_COMPONENTSPAGE SETCOMPONENTS SETCOMPONENTSDIALOG

  !verbose 3

  Function "${SETCOMPONENTS}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_COMPONENTS_TITLE) $(MUI_TEXT_COMPONENTS_SUBTITLE)
  FunctionEnd

  Function "${SETCOMPONENTSDIALOG}"
    !insertmacro MUI_INNERDIALOG_TEXT 1042 $(MUI_INNERTEXT_DESCRIPTION_TITLE)
    !insertmacro MUI_INNERDIALOG_TEXT 1043 $(MUI_INNERTEXT_DESCRIPTION_INFO)
  FunctionEnd
  
  !verbose 4
    
!macroend

!macro MUI_FUNCTIONS_DIRECTORYPAGE SETDIRECTORYPAGE SETDIRECTORYDIALOGPAGE

  !verbose 3

  Function "${SETDIRECTORYPAGE}"
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_DIRSELECT_TITLE) $(MUI_TEXT_DIRSELECT_SUBTITLE)
  FunctionEnd

  Function "${SETDIRECTORYDIALOGPAGE}"
    !insertmacro MUI_INNERDIALOG_TEXT 1041 $(MUI_INNERTEXT_DESTINATIONFOLDER)
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

!macro MUI_FUNCTIONS_DESCRIPTION_START

  !verbose 3

  Function .onMouseOverSection
    !insertmacro MUI_DESCRIPTION_INIT

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

  Function un.onGUIInit
    !insertmacro MUI_GUIINIT
  FunctionEnd

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
;BASIC FUNCTIONS

!macro MUI_FUNCTIONS_BASIC

  !verbose 3

  !insertmacro MUI_PAGECOMMANDS
  !insertmacro MUI_FUNCTIONS_PAGES
  !insertmacro MUI_FUNCTIONS_GUIINIT
  !insertmacro MUI_FUNCTIONS_ABORTWARNING
  
  !verbose 4

!macroend

!macro MUI_UNFUNCTIONS_BASIC

  !verbose 3

  !insertmacro MUI_UNPAGECOMMANDS
  !insertmacro MUI_UNFUNCTIONS_PAGES
  !insertmacro MUI_UNFUNCTIONS_GUIINIT

  !verbose 4

!macroend

!endif

!verbose 4
;--------------------------------