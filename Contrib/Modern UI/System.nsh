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

!macro MUI_INNERDIALOG_INIT

  !verbose 3

  Push ${MUI_TEMP1}
  
  !verbose 4

!macroend

!macro MUI_INNERDIALOG_START PAGE
  
  !verbose 3

  StrCmp ${CURRENTPAGE} ${PAGE} "" done_${PAGE}
  
  !verbose 4
  
!macroend

!macro MUI_INNERDIALOG_TEXT CONTROL TEXT

  !verbose 3

  ;Text on inner dialogs components
   
  FindWindow ${MUI_TEMP1} "#32770" "" $HWNDPARENT
  GetDlgItem ${MUI_TEMP1} ${MUI_TEMP1} ${CONTROL}
  SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
	
  !verbose 4
	
!macroend

!macro MUI_INNERDIALOG_STOP PAGE

  !verbose 3

  Goto done
  done_${PAGE}:
  
  !verbose 4
  
!macroend

!macro MUI_INNERDIALOG_END

  !verbose 3

  done:
  Pop ${MUI_TEMP1}
  
  !verbose 4

!macroend

!macro MUI_NEXTPAGE

  !verbose 3

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
  
  !verbose 4
   
!macroend

!macro MUI_UNNEXTPAGE

  !verbose 3

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
  
  !verbose 4
   
!macroend

!macro MUI_PREVPAGE

  !verbose 3

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} - 1

  !ifndef MUI_SETPAGE_FUNCTIONNAME
    !error "Modern UI Error: SetPage function name (MUI_SETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_SETPAGE_FUNCTIONNAME}"
  
  !verbose 4
  
!macroend

!macro MUI_UNPREVPAGE

  !verbose 3

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} - 1

  !ifndef MUI_UNSETPAGE_FUNCTIONNAME
    !error "Modern UI Error: Uninstall SetPage function name (MUI_UNSETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_UNSETPAGE_FUNCTIONNAME}"
  
  !verbose 4
  
!macroend

!macro MUI_PAGE_INIT

  !verbose 3

  Push ${MUI_TEMP1}
  
  !verbose 4

!macroend

!macro MUI_PAGE_START PAGE

  !verbose 3

  StrCmp ${CURRENTPAGE} ${PAGE} "" done_${PAGE}
   
  !verbose 4
  
!macroend

!macro MUI_HEADER_TEXT TEXT SUBTEXT

  !verbose 3

  ;Text on the white rectangle

  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1037
  SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${TEXT}"
  GetDlgItem ${MUI_TEMP1} $HWNDPARENT 1038
  SendMessage ${MUI_TEMP1} ${WM_SETTEXT} 0 "STR:${SUBTEXT}"
  
  !verbose 4

!macroend

!macro MUI_PAGE_STOP PAGE

  !verbose 3

  Goto done
  done_${PAGE}:
  
  !verbose 4
  
!macroend

!macro MUI_PAGE_END

  !verbose 3
  
  done:
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
  
  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1
  
  !ifndef MUI_SETPAGE_FUNCTIONNAME
    !error "Modern UI Error: SetPage function name (MUI_SETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_SETPAGE_FUNCTIONNAME}"

  !verbose 4

!macroend

!macro MUI_UNFINISHHEADER

  !verbose 3

  ;Finish text on the header (white rectangle)
  
  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1
  
  !ifndef MUI_UNSETPAGE_FUNCTIONNAME
    !error "Modern UI Error: Uninstall SetPage function name (MUI_UNSETPAGE_FUNCTIONNAME) not defined!"
  !endif
  
  Call "${MUI_UNSETPAGE_FUNCTIONNAME}"

  !verbose 4

!macroend

!macro MUI_ABORTWARNING

  !verbose 3

  ;Warning when Cancel button is pressed

  MessageBox MB_YESNO|MB_ICONEXCLAMATION "$(MUI_MSGTEXT_ABORTWARNING)" IDYES quit
    Abort
    quit:

  !verbose 4

!macroend

;--------------------------------
;INSTALL OPTIONS

!macro MUI_INSTALLOPTIONS DIRECTIONVAR NOSETDIRECTIONVAR

  !verbose 3

  !define IO_NOSETDIRECTION ${DIRECTIONVAR}
  !define IO_DIRECTION ${NOSETDIRECTIONVAR}

  !verbose 4

!macroend

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

!macro MUI_INSTALLOPTIONS_SETDIRECTION DIRECTION

  !verbose 3

  StrCpy ${IO_DIRECTION} "${DIRECTION}"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_NEXTPAGE

  !verbose 3

  StrCmp ${IO_NOSETDIRECTION} "1" no_setdirection
    !insertmacro MUI_INSTALLOPTIONS_SETDIRECTION ${MUI_INSTALLOPTIONS_DIRECTION_NEXT}
  no_setdirection:
  StrCpy ${IO_NOSETDIRECTION} "0"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_PREVPAGE

  !verbose 3

  StrCmp ${IO_NOSETDIRECTION} "1" no_setdirection
    !insertmacro MUI_INSTALLOPTIONS_SETDIRECTION ${MUI_INSTALLOPTIONS_DIRECTION_PREV}
  no_setdirection:
  StrCpy ${IO_NOSETDIRECTION} "0"

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_SHOW PAGE FILE IOBACK IONEXT

  !verbose 3

  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop ${MUI_TEMP1}

  StrCmp ${MUI_TEMP1} "cancel" "" +2
    Quit

  StrCmp ${MUI_TEMP1} "back" "" noback_${PAGE}
    !insertmacro MUI_INSTALLOPTIONS_${IOBACK}BACK
  noback_${PAGE}:

  !insertmacro MUI_INSTALLOPTIONS_${IONEXT}NEXT

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_UNSHOW PAGE FILE IOBACK IONEXT

  !verbose 3

  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop ${MUI_TEMP1}

  StrCmp ${MUI_TEMP1} "cancel" "" +2
    Quit
    
  StrCmp ${MUI_TEMP1} "back" "" noback_${PAGE}
    !insertmacro MUI_INSTALLOPTIONS_UN${IOBACK}BACK
  noback_${PAGE}:

  !insertmacro MUI_INSTALLOPTIONS_UN${IONEXT}NEXT

  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_BACK

  !verbose 3

  StrCmp ${IO_DIRECTION} "${MUI_INSTALLOPTIONS_DIRECTION_NEXT}" "" +3
    Call .onPrevPage
    Abort
  StrCmp ${IO_DIRECTION} "${MUI_INSTALLOPTIONS_DIRECTION_PREV}" "" +3
    Call .onPrevPage
    Goto done

  !verbose 4
          
!macroend

!macro MUI_INSTALLOPTIONS_NEXT

  !verbose 3

  StrCmp ${IO_DIRECTION} ${MUI_INSTALLOPTIONS_DIRECTION_NEXT} "" +3
    Call .onNextPage
    Goto done
  StrCmp ${IO_DIRECTION} ${MUI_INSTALLOPTIONS_DIRECTION_PREV} "" +3
    Call .onNextPage
    Abort

  !verbose 4
 
!macroend

!macro MUI_INSTALLOPTIONS_IOBACK

  !verbose 3

  StrCpy ${IO_NOSETDIRECTION} "1"
  Call .onPrevPage
  Goto done

  !verbose 4
          
!macroend

!macro MUI_INSTALLOPTIONS_IONEXT

  !verbose 3

  StrCpy ${IO_NOSETDIRECTION} "1"
  Call .onNextPage
  Goto done
  
  !verbose 4

!macroend

!macro MUI_INSTALLOPTIONS_UNBACK

  !verbose 3

  StrCmp ${IO_DIRECTION} "${MUI_INSTALLOPTIONS_DIRECTION_NEXT}" "" +3
    Call un.onPrevPage
    Abort
  StrCmp ${IO_DIRECTION} "${MUI_INSTALLOPTIONS_DIRECTION_PREV}" "" +3
    Call un.onPrevPage
    Goto done

  !verbose 4
          
!macroend

!macro MUI_INSTALLOPTIONS_UNNEXT

  !verbose 3

  StrCmp ${IO_DIRECTION} ${MUI_INSTALLOPTIONS_DIRECTION_NEXT} "" +3
    Call un.onNextPage
    Goto done
  StrCmp ${IO_DIRECTION} ${MUI_INSTALLOPTIONS_DIRECTION_PREV} "" +3
    Call un.onNextPage
    Abort

  !verbose 4
 
!macroend

!macro MUI_INSTALLOPTIONS_UNIOBACK

  !verbose 3

  StrCpy ${IO_NOSETDIRECTION} "1"
  Call .onPrevPage
  Goto done

  !verbose 4
          
!macroend

!macro MUI_INSTALLOPTIONS_UNIONEXT

  !verbose 3

  StrCpy ${IO_NOSETDIRECTION} "1"
  Call .onNextPage
  Goto done

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

!macro MUI_BASICFUNCTIONS_INIT

  !verbose 3

  !define MUI_SETPAGE_FUNCTIONNAME "SetPage"
  !define MUI_UNSETPAGE_FUNCTIONNAME "un.SetPage"

  !verbose 4

!macroend

!macro MUI_BASICFUNCTIONS

!verbose 3

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

!verbose 4

!macroend

!endif

!verbose 4
;--------------------------------