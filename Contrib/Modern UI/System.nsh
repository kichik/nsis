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
  
  !ifndef MUI_CURRENTPAGEVAR
    !define MUI_CURRENTPAGEVAR "$9"
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
  
  !define CURRENTPAGE ${MUI_CURRENTPAGEVAR}
  
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
;BASIC FUNCTIONS

!macro MUI_BASICFUNCTIONS
  
  !ifdef MUI_LICENSEPAGE
    Page license SetLicense SetLicenseDialog
  !endif
  !ifdef MUI_COMPONENTSPAGE
    Page components SetComponents SetComponentsDialog
  !endif
  !ifdef MUI_DIRECTORYPAGE
    Page directory SetDirectory SetDirectoryDialog
  !endif
  Page instfiles SetInstFiles

  !ifdef MUI_LICENSEPAGE
  Function SetLicense
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
  FunctionEnd
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
  Function SetComponents
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_COMPONENTS_TITLE) $(MUI_TEXT_COMPONENTS_SUBTITLE)
  FunctionEnd
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
  Function SetDirectory
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_DIRSELECT_TITLE) $(MUI_TEXT_DIRSELECT_SUBTITLE)
  FunctionEnd
  !endif
  
  Function SetInstFiles
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_INSTALLING_TITLE) $(MUI_TEXT_INSTALLING_SUBTITLE)
  FunctionEnd
          
  Function SetLicenseDialog
    !insertmacro MUI_INNERDIALOG_TEXT 1040 $(MUI_INNERTEXT_LICENSE)
  FunctionEnd
  
  Function SetComponentsDialog
    !insertmacro MUI_INNERDIALOG_TEXT 1042 $(MUI_INNERTEXT_DESCRIPTION_TITLE)
    !insertmacro MUI_INNERDIALOG_TEXT 1043 $(MUI_INNERTEXT_DESCRIPTION_INFO)
  FunctionEnd
  
  Function SetDirectoryDialog
    !insertmacro MUI_INNERDIALOG_TEXT 1041 $(MUI_INNERTEXT_DESTINATIONFOLDER)
  FunctionEnd
  
  Function .onGUIInit
    !insertmacro MUI_GUIINIT
  FunctionEnd

!verbose 4

!macroend

!macro MUI_FUNCTION_DESCRIPTION_START

  !verbose 3

  Function .onMouseOverSection
    !insertmacro MUI_DESCRIPTION_INIT

  !verbose 4

!macroend

!macro MUI_FUNCTION_DESCRIPTION_END

  !verbose 3

    !insertmacro MUI_DESCRIPTION_END
  FunctionEnd

  !verbose 4  

!macroend

!macro MUI_FUNCTION_ABORTWARNING

  !verbose 3

  Function .onUserAbort
    !insertmacro MUI_ABORTWARNING
  FunctionEnd

  !verbose 4

!macroend

!macro MUI_UNBASICFUNCTIONS

!verbose 3

UninstPage uninstConfirm un.SetUninstConfirm
UninstPage instfiles un.SetInstFiles

  Function un.SetUninstConfirm
    !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_INTRO_TITLE) $(MUI_UNTEXT_INTRO_SUBTITLE)
  FunctionEnd
 
  Function un.SetInstFiles
    !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_UNINSTALLING_TITLE) $(MUI_UNTEXT_UNINSTALLING_SUBTITLE)
  FunctionEnd
  
  Function un.onGUIInit
    !insertmacro MUI_GUIINIT
  FunctionEnd

!verbose 4

!macroend

!endif

!verbose 4
;--------------------------------