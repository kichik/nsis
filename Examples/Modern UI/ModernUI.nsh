;Modern UI Header File version 1.19e - WORKING ON INSTALLOPTIONS INTEGRATION
;Written by Joost Verburg

;See Example.nsi & Multilanguage.nsi for an example of usage

!ifndef MUI_MACROS_USED

!define MUI_MACROS_USED

!define IO_DIRECTION_NEXT 1
!define IO_DIRECTION_PREV 2

!macro MUI_INTERFACE UI ICON UNICON CHECKS PROGRESSBAR

  ;User interface
  
  Icon "${NSISDIR}\Contrib\Icons\${ICON}"
  UninstallIcon "${NSISDIR}\Contrib\Icons\${UNICON}"
  XPStyle On
  ChangeUI all "${NSISDIR}\Contrib\UIs\${UI}"
  SetFont Tahoma 8
  CheckBitmap "${NSISDIR}\Contrib\Icons\${CHECKS}"
  InstallColors /windows
  InstProgressFlags "${PROGRESSBAR}"
  BrandingText /TRIMRIGHT

!macroend

!macro MUI_FINISHHEADER CALL

  ;Finish text on the header (white rectangle)
  
  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1
  Call ${CALL}

!macroend

!macro MUI_INNERDIALOG_INIT

  Push ${TEMP1}

!macroend

!macro MUI_INNERDIALOG_START PAGE

  StrCmp ${CURRENTPAGE} ${PAGE} "" done_${PAGE}
  
!macroend

!macro MUI_INNERDIALOG_TEXT LANGID CONTROL TEXT

 ;Text on inner dialogs components

  StrCmp $LANGUAGE ${LANGID} "" +4
    FindWindow ${TEMP1} "#32770" "" $HWNDPARENT
    GetDlgItem ${TEMP1} ${TEMP1} ${CONTROL}
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
	
!macroend

!macro MUI_INNERDIALOG_STOP PAGE

  Goto done
  done_${PAGE}:
  
!macroend

!macro MUI_INNERDIALOG_END

  done:
  Pop ${TEMP1}

!macroend

!macro MUI_NEXTPAGE_OUTER

  Push ${TEMP1}
  Push ${TEMP2}  

  ;Set backgrounds & fonts for the outer dialog (only once)
  StrCmp ${CURRENTPAGE} "" "" no_first_run
   
    GetDlgItem ${TEMP1} $HWNDPARENT 1037
    CreateFont ${TEMP2} "Tahoma" 10 700
    SendMessage ${TEMP1} ${WM_SETFONT} ${TEMP2} 0
    SetStaticBkColor ${TEMP1} 0x00FFFFFF
 
    GetDlgItem ${TEMP1} $HWNDPARENT 1038
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    GetDlgItem ${TEMP1} $HWNDPARENT 1034
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    GetDlgItem ${TEMP1} $HWNDPARENT 1039
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    no_first_run:
    
  Pop ${TEMP2}  
  Pop ${TEMP1}
  
!macroend

!macro MUI_NEXTPAGE CALL

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1

  Call "${CALL}"

!macroend

!macro MUI_PREVPAGE CALL

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} - 1

  Call "${CALL}"
  
!macroend

!macro MUI_PAGE_INIT

  Push ${TEMP1}

!macroend

!macro MUI_PAGE_START PAGE

   StrCmp ${CURRENTPAGE} ${PAGE} "" done_${PAGE}
  
!macroend

!macro MUI_HEADER_TEXT LANGID TEXT SUBTEXT

  ;Text on the white rectangle

  StrCmp $LANGUAGE ${LANGID} "" +5
    GetDlgItem ${TEMP1} $HWNDPARENT 1037
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
    GetDlgItem ${TEMP1} $HWNDPARENT 1038
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "STR:${SUBTEXT}"

!macroend

!macro MUI_PAGE_STOP PAGE

  Goto done
  done_${PAGE}:
  
!macroend

!macro MUI_PAGE_END
  
  done:
  Pop ${TEMP1}
  
!macroend

!macro MUI_DESCRIPTION_INIT

  Push ${TEMP1}

  FindWindow ${TEMP1} "#32770" "" $HWNDPARENT
  GetDlgItem ${TEMP1} ${TEMP1} 1043

!macroend

!macro MUI_DESCRIPTION_TEXT LANGID VAR TEXT

  ;Set text on the Description frame

  StrCmp $LANGUAGE ${LANGID} "" +4
    StrCmp $0 ${VAR} "" +3
      SendMessage ${TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
      Goto done
    
!macroend

!macro MUI_DESCRIPTION_END

  done:
  Pop ${TEMP1}
  
!macroend

!macro MUI_ABORTWARNING LANGID TEXT

  ;Warning when Cancel button is pressed

  StrCmp $LANGUAGE ${LANGID} "" +3
    MessageBox MB_YESNO|MB_ICONEXCLAMATION "${TEXT}" IDYES quit
    Abort

!macroend

!macro MUI_ABORTWARNING_END

  quit:
  
!macroend

!macro MUI_INSTALLOPTIONS_INIT

  Call Initialize_____Plugins
  
!macroend

!macro MUI_INSTALLOPTIONS_UNINIT

  Call un.Initialize_____Plugins
  
!macroend

!macro MUI_INSTALLOPTIONS_EXTRACT FILE

  File /oname=$PLUGINSDIR\${FILE} "${FILE}"
  
!macroend

!macro MUI_INSTALLOPTIONS_SETDIRECTION DIRECTION

  StrCpy ${IO_DIRECTION} "${DIRECTION}"

!macroend

!macro MUI_INSTALLOPTIONS_SHOW FILE
        
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop ${TEMP1}

  StrCmp ${TEMP1} "cancel" "" +2
    Quit

  StrCmp ${TEMP1} "back" "" +7
    !insertmacro MUI_INSTALLOPTIONS_BACK

  !insertmacro MUI_INSTALLOPTIONS_NEXT
		
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

!endif