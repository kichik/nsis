;Modern UI Macro System 1.3
;Written by Joost Verburg

;See Basic.nsi, Multilanguage.nsi and InstallOptions.nsi
;in the Examples\Modern UI directory for examples of usage.

;--------------------------------

!ifndef MUI_MACROS_USED

!define MUI_MACROS_USED

!include "${NSISDIR}\Examples\WinMessages.nsh"

!define IO_DIRECTION_NEXT 1
!define IO_DIRECTION_PREV 2

!define IO_INITPLUGINS_1 "Call Initialize_____Plugins"
!define IO_INITPLUGINS_2 "SetDetailsPrint both"
!define IO_UNINITPLUGINS_1 "Call un.Initialize_____Plugins"
!define IO_UNINITPLUGINS_2 "SetDetailsPrint both"

!define MUI_TEMP1 $R0
!define MUI_TEMP2 $R1

!macro MUI_INTERFACE UI ICON UNICON CHECKS PROGRESSBAR FONT CURRENTPAGEVAR

  ;User interface
  
  Icon "${NSISDIR}\Contrib\Icons\${ICON}"
  UninstallIcon "${NSISDIR}\Contrib\Icons\${UNICON}"
  XPStyle On
  ChangeUI all "${NSISDIR}\Contrib\UIs\${UI}"
  SetFont "${FONT}" 8
  CheckBitmap "${NSISDIR}\Contrib\Icons\${CHECKS}"
  InstallColors /windows
  InstProgressFlags "${PROGRESSBAR}"
  BrandingText /TRIMRIGHT
  !define CURRENTPAGE ${CURRENTPAGEVAR}

!macroend

!macro MUI_INTERFACE_ABSOLUTEPATH UI ICON UNICON CHECKS PROGRESSBAR CURRENTPAGEVAR

  ;User interface
  
  Icon "${ICON}"
  UninstallIcon "${UNICON}"
  XPStyle On
  ChangeUI all "${UI}"
  SetFont Tahoma 8
  CheckBitmap "${CHECKS}"
  InstallColors /windows
  InstProgressFlags "${PROGRESSBAR}"
  BrandingText /TRIMRIGHT
  !define CURRENTPAGE ${CURRENTPAGEVAR}

!macroend

!macro MUI_FINISHHEADER CALL

  ;Finish text on the header (white rectangle)
  
  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1
  Call ${CALL}

!macroend

!macro MUI_INNERDIALOG_INIT

  Push ${MUI_TEMP1}

!macroend

!macro MUI_INNERDIALOG_START PAGE

  StrCmp ${CURRENTPAGE} ${PAGE} "" done_${PAGE}
  
!macroend

!macro MUI_INNERDIALOG_TEXT CONTROL TEXT

   ;Text on inner dialogs components
   
   FindWindow ${MUI_TEMP1} "#32770" "" $HWNDPARENT
   GetDlgItem ${MUI_TEMP1} ${MUI_TEMP1} ${CONTROL}
   SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
	
!macroend

!macro MUI_INNERDIALOG_STOP PAGE

  Goto done
  done_${PAGE}:
  
!macroend

!macro MUI_INNERDIALOG_END

  done:
  Pop ${MUI_TEMP1}

!macroend

!macro MUI_NEXTPAGE CALL

  ;Set backgrounds & fonts for the outer dialog (only once)
  StrCmp ${CURRENTPAGE} "" "" no_first_run

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

  no_first_run:

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1

  Call "${CALL}"
   
!macroend

!macro MUI_PREVPAGE CALL

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} - 1

  Call "${CALL}"
  
!macroend

!macro MUI_PAGE_INIT

  Push ${MUI_TEMP1}

!macroend

!macro MUI_PAGE_START PAGE

   StrCmp ${CURRENTPAGE} ${PAGE} "" done_${PAGE}
  
!macroend

!macro MUI_HEADER_TEXT TEXT SUBTEXT

  ;Text on the white rectangle

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1037
  SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1038
  SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${SUBTEXT}"

!macroend

!macro MUI_PAGE_STOP PAGE

  Goto done
  done_${PAGE}:
  
!macroend

!macro MUI_PAGE_END
  
  done:
  Pop ${MUI_TEMP1}
  
!macroend

!macro MUI_DESCRIPTION_INIT

  Push ${MUI_TEMP1}

  FindWindow ${MUI_TEMP1} "#32770" "" $HWNDPARENT
  GetDlgItem ${MUI_TEMP1} ${MUI_TEMP1} 1043

!macroend

!macro MUI_DESCRIPTION_TEXT VAR TEXT

  ;Set text on the Description frame

  StrCmp $0 ${VAR} "" +3
    SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
    Goto done
    
!macroend

!macro MUI_DESCRIPTION_END

  done:
  Pop ${MUI_TEMP1}
  
!macroend

!macro MUI_ABORTWARNING

  ;Warning when Cancel button is pressed

  MessageBox MB_YESNO|MB_ICONEXCLAMATION "$(MUI_MSGTEXT_ABORTWARNING)" IDYES quit
    Abort
    quit:

!macroend

;INSTALL OPTIONS

!macro MUI_INSTALLOPTIONS DIRECTIONVAR NOSETDIRECTIONVAR

  !define IO_NOSETDIRECTION ${DIRECTIONVAR}
  !define IO_DIRECTION ${NOSETDIRECTIONVAR}
  
!macroend

!macro MUI_INSTALLOPTIONS_EXTRACT FILE

  ;Init plugin system
  ${IO_INITPLUGINS_1}
  ${IO_INITPLUGINS_2}
  !undef IO_INITPLUGINS_1
  !undef IO_INITPLUGINS_2
  !define IO_INITPLUGINS_1 ""
  !define IO_INITPLUGINS_2 ""
  
  File /oname=$PLUGINSDIR\${FILE} "${FILE}"
  
!macroend

!macro MUI_INSTALLOPTIONS_UNEXTRACT FILE

  ;Init plugin system
  ${IO_UNINITPLUGINS_1}
  ${IO_UNINITPLUGINS_2}
  !undef IO_UNINITPLUGINS_1
  !undef IO_UNINITPLUGINS_2
  !define IO_UNINITPLUGINS_1 ""
  !define IO_UNINITPLUGINS_2 ""
  
  File /oname=$PLUGINSDIR\${FILE} "${FILE}"
  
!macroend

!macro MUI_INSTALLOPTIONS_SETDIRECTION DIRECTION

  StrCpy ${IO_DIRECTION} "${DIRECTION}"

!macroend

!macro MUI_INSTALLOPTIONS_NEXTPAGE

  StrCmp ${IO_NOSETDIRECTION} "1" no_setdirection
    !insertmacro MUI_INSTALLOPTIONS_SETDIRECTION ${IO_DIRECTION_NEXT}
  no_setdirection:
  StrCpy ${IO_NOSETDIRECTION} "0"
  
!macroend

!macro MUI_INSTALLOPTIONS_PREVPAGE

  StrCmp ${IO_NOSETDIRECTION} "1" no_setdirection
    !insertmacro MUI_INSTALLOPTIONS_SETDIRECTION ${IO_DIRECTION_PREV}
  no_setdirection:
  StrCpy ${IO_NOSETDIRECTION} "0"
  
!macroend

!macro MUI_INSTALLOPTIONS_SHOW PAGE FILE IOBACK IONEXT
        
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop ${MUI_TEMP1}

  StrCmp ${MUI_TEMP1} "cancel" "" +2
    Quit

  StrCmp ${MUI_TEMP1} "back" "" noback_${PAGE}
    !insertmacro MUI_INSTALLOPTIONS_${IOBACK}BACK
  noback_${PAGE}:

  !insertmacro MUI_INSTALLOPTIONS_${IONEXT}NEXT
		
!macroend

!macro MUI_INSTALLOPTIONS_UNSHOW PAGE FILE IOBACK IONEXT
        
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop ${MUI_TEMP1}

  StrCmp ${MUI_TEMP1} "cancel" "" +2
    Quit
    
  StrCmp ${MUI_TEMP1} "back" "" noback_${PAGE}
    !insertmacro MUI_INSTALLOPTIONS_UN${IOBACK}BACK
  noback_${PAGE}:

  !insertmacro MUI_INSTALLOPTIONS_UN${IONEXT}NEXT
		
!macroend

!macro MUI_INSTALLOPTIONS_BACK

  StrCmp ${IO_DIRECTION} "${IO_DIRECTION_NEXT}" "" +3
    Call .onPrevPage
    Abort
  StrCmp ${IO_DIRECTION} "${IO_DIRECTION_PREV}" "" +3
    Call .onPrevPage
    Goto done
            
!macroend

!macro MUI_INSTALLOPTIONS_NEXT

  StrCmp ${IO_DIRECTION} ${IO_DIRECTION_NEXT} "" +3
    Call .onNextPage
    Goto done
  StrCmp ${IO_DIRECTION} ${IO_DIRECTION_PREV} "" +3
    Call .onNextPage
    Abort
   
!macroend

!macro MUI_INSTALLOPTIONS_IOBACK
  
  StrCpy ${IO_NOSETDIRECTION} "1"
  Call .onPrevPage
  Goto done
            
!macroend

!macro MUI_INSTALLOPTIONS_IONEXT

  StrCpy ${IO_NOSETDIRECTION} "1"
  Call .onNextPage
  Goto done
	
!macroend

!macro MUI_INSTALLOPTIONS_UNBACK

  StrCmp ${IO_DIRECTION} "${IO_DIRECTION_NEXT}" "" +3
    Call un.onPrevPage
    Abort
  StrCmp ${IO_DIRECTION} "${IO_DIRECTION_PREV}" "" +3
    Call un.onPrevPage
    Goto done
            
!macroend

!macro MUI_INSTALLOPTIONS_UNNEXT

  StrCmp ${IO_DIRECTION} ${IO_DIRECTION_NEXT} "" +3
    Call un.onNextPage
    Goto done
  StrCmp ${IO_DIRECTION} ${IO_DIRECTION_PREV} "" +3
    Call un.onNextPage
    Abort
   
!macroend

!macro MUI_INSTALLOPTIONS_UNIOBACK
  
  StrCpy ${IO_NOSETDIRECTION} "1"
  Call .onPrevPage
  Goto done
            
!macroend

!macro MUI_INSTALLOPTIONS_UNIONEXT

  StrCpy ${IO_NOSETDIRECTION} "1"
  Call .onNextPage
  Goto done
	
!macroend

!endif