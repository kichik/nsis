;NSIS Modern User Interface version 1.3
;Macro System
;Written by Joost Verburg

;See Basic.nsi, Multilanguage.nsi and InstallOptions.nsi
;in the Examples\Modern UI directory for examples of usage.

;--------------------------------
!verbose 3

!ifndef MUI_MACROS_USED

!define MUI_MACROS_USED

!include "${NSISDIR}\Examples\WinMessages.nsh"

!define MUI_INSTALLOPTIONS_DIRECTION_NEXT 1
!define MUI_INSTALLOPTIONS_DIRECTION_PREV 2

!define MUI_TEMP1 $R0
!define MUI_TEMP2 $R1

!macro MUI_INTERFACE

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

!macroend

!macro MUI_FINISHHEADER

  ;Finish text on the header (white rectangle)
  
  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1
  
  !ifndef MUI_SETPAGE_FUNCTIONNAME
    !error "Modern UI Error: SetPage function name (MUI_SETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_SETPAGE_FUNCTIONNAME}"

!macroend

!macro MUI_UNFINISHHEADER

  ;Finish text on the header (white rectangle)
  
  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1
  
  !ifndef MUI_UNSETPAGE_FUNCTIONNAME
    !error "Modern UI Error: Uninstall SetPage function name (MUI_UNSETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_UNSETPAGE_FUNCTIONNAME}"

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

!macro MUI_NEXTPAGE

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

  !ifndef MUI_SETPAGE_FUNCTIONNAME
    !error "Modern UI Error: SetPage function name (MUI_SETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_SETPAGE_FUNCTIONNAME}"
   
!macroend

!macro MUI_UNNEXTPAGE

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

  !ifndef MUI_UNSETPAGE_FUNCTIONNAME
    !error "Modern UI Error: Uninstall SetPage function name (MUI_UNSETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_UNSETPAGE_FUNCTIONNAME}"
   
!macroend

!macro MUI_PREVPAGE

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} - 1

  !ifndef MUI_SETPAGE_FUNCTIONNAME
    !error "Modern UI Error: SetPage function name (MUI_SETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_SETPAGE_FUNCTIONNAME}"
  
!macroend

!macro MUI_UNPREVPAGE

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} - 1

  !ifndef MUI_UNSETPAGE_FUNCTIONNAME
    !error "Modern UI Error: Uninstall SetPage function name (MUI_UNSETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_UNSETPAGE_FUNCTIONNAME}"
  
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

;--------------------------------
;INSTALL OPTIONS

!macro MUI_INSTALLOPTIONS DIRECTIONVAR NOSETDIRECTIONVAR

  !define IO_NOSETDIRECTION ${DIRECTIONVAR}
  !define IO_DIRECTION ${NOSETDIRECTIONVAR}
  
!macroend

!macro MUI_INSTALLOPTIONS_EXTRACT FILE

  ;Init plugin system
  !ifndef MUI_INSTALLOPTIONS_INITPLUGINS
    !define MUI_INSTALLOPTIONS_INITPLUGINS
    Call Initialize_____Plugins
    SetDetailsPrint both
  !endif  
  
  File /oname=$PLUGINSDIR\${FILE} "${FILE}"
  
!macroend

!macro MUI_INSTALLOPTIONS_UNEXTRACT FILE

  ;Init plugin system
  !ifndef MUI_INSTALLOPTIONS_UNINITPLUGINS
    !define MUI_INSTALLOPTIONS_UNINITPLUGINS
    Call un.Initialize_____Plugins
    SetDetailsPrint both
  !endif
  
  File /oname=$PLUGINSDIR\${FILE} "${FILE}"
  
!macroend

!macro MUI_INSTALLOPTIONS_SETDIRECTION DIRECTION

  StrCpy ${IO_DIRECTION} "${DIRECTION}"

!macroend

!macro MUI_INSTALLOPTIONS_NEXTPAGE

  StrCmp ${IO_NOSETDIRECTION} "1" no_setdirection
    !insertmacro MUI_INSTALLOPTIONS_SETDIRECTION ${MUI_INSTALLOPTIONS_DIRECTION_NEXT}
  no_setdirection:
  StrCpy ${IO_NOSETDIRECTION} "0"
  
!macroend

!macro MUI_INSTALLOPTIONS_PREVPAGE

  StrCmp ${IO_NOSETDIRECTION} "1" no_setdirection
    !insertmacro MUI_INSTALLOPTIONS_SETDIRECTION ${MUI_INSTALLOPTIONS_DIRECTION_PREV}
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

  StrCmp ${IO_DIRECTION} "${MUI_INSTALLOPTIONS_DIRECTION_NEXT}" "" +3
    Call .onPrevPage
    Abort
  StrCmp ${IO_DIRECTION} "${MUI_INSTALLOPTIONS_DIRECTION_PREV}" "" +3
    Call .onPrevPage
    Goto done
            
!macroend

!macro MUI_INSTALLOPTIONS_NEXT

  StrCmp ${IO_DIRECTION} ${MUI_INSTALLOPTIONS_DIRECTION_NEXT} "" +3
    Call .onNextPage
    Goto done
  StrCmp ${IO_DIRECTION} ${MUI_INSTALLOPTIONS_DIRECTION_PREV} "" +3
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

  StrCmp ${IO_DIRECTION} "${MUI_INSTALLOPTIONS_DIRECTION_NEXT}" "" +3
    Call un.onPrevPage
    Abort
  StrCmp ${IO_DIRECTION} "${MUI_INSTALLOPTIONS_DIRECTION_PREV}" "" +3
    Call un.onPrevPage
    Goto done
            
!macroend

!macro MUI_INSTALLOPTIONS_UNNEXT

  StrCmp ${IO_DIRECTION} ${MUI_INSTALLOPTIONS_DIRECTION_NEXT} "" +3
    Call un.onNextPage
    Goto done
  StrCmp ${IO_DIRECTION} ${MUI_INSTALLOPTIONS_DIRECTION_PREV} "" +3
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

!macro MUI_INSTALLOPTIONS_READ VAR FILE SECTION KEY

  ReadIniStr ${VAR} "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}"

!macroend

!macro MUI_INSTALLOPTIONS_WRITE FILE SECTION KEY VALUE

  WriteIniStr "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}" "${VALUE}"

!macroend

;--------------------------------
;BASIC FUNCTIONS

!macro MUI_BASICFUNCTIONS_INIT

  !define MUI_SETPAGE_FUNCTIONNAME "SetPage"
  !define MUI_UNSETPAGE_FUNCTIONNAME "un.SetPage"
  
!macroend

!macro MUI_BASICFUNCTIONS

Function .onNextPage
  !insertmacro MUI_NEXTPAGE
FunctionEnd

Function .onPrevPage
  !insertmacro MUI_PREVPAGE
FunctionEnd

Function .onInitDialog

  !insertmacro MUI_INNERDIALOG_INIT
  
    StrCpy ${MUI_TEMP1} 0

    !ifdef MUI_LICENSEPAGE
       IntOp ${MUI_TEMP1} ${MUI_TEMP1} + 1
       StrCmp ${CURRENTPAGE} ${MUI_TEMP1} "" done_licensepage
         !insertmacro MUI_INNERDIALOG_TEXT 1040 $(MUI_INNERTEXT_LICENSE)
         Goto done
       done_licensepage:
    !endif

    !ifdef MUI_COMPONENTPAGE
       IntOp ${MUI_TEMP1} ${MUI_TEMP1} + 1
       StrCmp ${CURRENTPAGE} ${MUI_TEMP1} "" done_componentpage
         !insertmacro MUI_INNERDIALOG_TEXT 1042 $(MUI_INNERTEXT_DESCRIPTION_TITLE)
         !insertmacro MUI_INNERDIALOG_TEXT 1043 $(MUI_INNERTEXT_DESCRIPTION_INFO)
         Goto done
       done_componentpage:
    !endif

	!ifdef MUI_DIRSELECTPAGE
	   IntOp ${MUI_TEMP1} ${MUI_TEMP1} + 1
       StrCmp ${CURRENTPAGE} ${MUI_TEMP1} "" done_dirselectpage
         !insertmacro MUI_INNERDIALOG_TEXT 1041 $(MUI_INNERTEXT_DESTINATIONFOLDER)
         Goto done
       done_dirselectpage:
    !endif

  !insertmacro MUI_INNERDIALOG_END
  
FunctionEnd

Function SetPage

  !insertmacro MUI_PAGE_INIT

	StrCpy ${MUI_TEMP1} 0

    !ifdef MUI_LICENSEPAGE
      IntOp ${MUI_TEMP1} ${MUI_TEMP1} + 1
      StrCmp ${CURRENTPAGE} ${MUI_TEMP1} "" done_licensepage
        !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
        Goto done
      done_licensepage:
    !endif
     
     !ifdef MUI_COMPONENTPAGE
       IntOp ${MUI_TEMP1} ${MUI_TEMP1} + 1
       StrCmp ${CURRENTPAGE} ${MUI_TEMP1} "" done_componentpage
         !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_COMPONENTS_TITLE) $(MUI_TEXT_COMPONENTS_SUBTITLE)
         Goto done
       done_componentpage:
     !endif
     
     !ifdef MUI_DIRSELECTPAGE
       IntOp ${MUI_TEMP1} ${MUI_TEMP1} + 1
         StrCmp ${CURRENTPAGE} ${MUI_TEMP1} "" done_dirselectpage
           !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_DIRSELECT_TITLE) $(MUI_TEXT_DIRSELECT_SUBTITLE)
           Goto done
         done_dirselectpage:
     !endif

      IntOp ${MUI_TEMP1} ${MUI_TEMP1} + 1
        StrCmp ${CURRENTPAGE} ${MUI_TEMP1} "" done_installingpage
          !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_INSTALLING_TITLE) $(MUI_TEXT_INSTALLING_SUBTITLE)
          Goto done
        done_installingpage:
          
      IntOp ${MUI_TEMP1} ${MUI_TEMP1} + 1
        StrCmp ${CURRENTPAGE} ${MUI_TEMP1} "" done_finishedpage
          !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_FINISHED_TITLE) $(MUI_TEXT_FINISHED_SUBTITLE)
          Goto done
        done_finishedpage:

  !insertmacro MUI_PAGE_END

FunctionEnd

!macroend

!macro MUI_FUNCTION_DESCRIPTION_START
  Function .onMouseOverSection
    !insertmacro MUI_DESCRIPTION_INIT
!macroend

!macro MUI_FUNCTION_DESCRIPTION_END
    !insertmacro MUI_DESCRIPTION_END
  FunctionEnd
!macroend

!macro MUI_FUNCTION_ABORTWARNING
  Function .onUserAbort
    !insertmacro MUI_ABORTWARNING
  FunctionEnd
!macroend

!macro MUI_UNBASICFUNCTIONS

Function un.onNextPage

  !insertmacro MUI_UNNEXTPAGE
  
FunctionEnd

Function un.SetPage
  
  !insertmacro MUI_PAGE_INIT
    
    !insertmacro MUI_PAGE_START 1
      !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_INTRO_TITLE) $(MUI_UNTEXT_INTRO_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 1

    !insertmacro MUI_PAGE_START 2
      !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_UNINSTALLING_TITLE) $(MUI_UNTEXT_UNINSTALLING_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 2

    !insertmacro MUI_PAGE_START 3
      !insertmacro MUI_HEADER_TEXT $(MUI_UNTEXT_FINISHED_TITLE) $(MUI_UNTEXT_FINISHED_SUBTITLE)
    !insertmacro MUI_PAGE_STOP 3

  !insertmacro MUI_PAGE_END

FunctionEnd

!macroend

!endif

!verbose 4
;--------------------------------