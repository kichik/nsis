;NSIS Modern User Interface version 1.65
;Macro System
;Written by Joost Verburg

;Copyright © 2002-2003 Joost Verburg

;Documentation: Readme.html
;License: License.txt
;Examples: Examples\Modern UI

;--------------------------------

!echo "NSIS Modern User Interface version 1.65 - © 2002-2003 Joost Verburg"

!ifndef MUI_MANUALVERBOSE
  !verbose 3
!endif

!ifndef MUI_INCLUDED

!define MUI_INCLUDED

!include "WinMessages.nsh"

!macro MUI_DEFINEVARS

  Var MUI_TEMP1
  Var MUI_TEMP2
  
  !ifdef MUI_WELCOMEPAGE | MUI_FINISHPAGE
    Var MUI_TEMP3
    Var MUI_HWND
  !endif

!macroend

!macro MUI_INTERFACE
  
  !ifndef MUI_UI
    !define MUI_UI "${NSISDIR}\Contrib\UIs\modern.exe"
  !endif
  
  !ifndef MUI_UI_HEADERBITMAP
    !define MUI_UI_HEADERBITMAP "${NSISDIR}\Contrib\UIs\modern_headerbmp.exe"
  !endif

  !ifndef MUI_UI_HEADERBITMAP_RIGHT
    !define MUI_UI_HEADERBITMAP_RIGHT "${NSISDIR}\Contrib\UIs\modern_headerbmpr.exe"
  !endif
  
  !ifndef MUI_UI_SMALLDESCRIPTION
    !define MUI_UI_SMALLDESCRIPTION "${NSISDIR}\Contrib\UIs\modern_smalldesc.exe"
  !endif

  !ifndef MUI_UI_NODESCRIPTION
    !define MUI_UI_NODESCRIPTION "${NSISDIR}\Contrib\UIs\modern_nodesc.exe"
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

  !ifdef MUI_FONT
    !error "Use SetFont to change the dialog font"
  !endif
  
  !ifndef MUI_LICENSEBKCOLOR
    !define MUI_LICENSEBKCOLOR "/windows"
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

  !ifndef MUI_RTL_UI
    ChangeUI all "${MUI_UI}"
    !ifdef MUI_HEADERBITMAP
      !ifndef MUI_HEADERBITMAP_RIGHT
        ChangeUI IDD_INST "${MUI_UI_HEADERBITMAP}"
      !else
        ChangeUI IDD_INST "${MUI_UI_HEADERBITMAP_RIGHT}"
      !endif
    !endif
    !ifdef MUI_COMPONENTSPAGE_SMALLDESC
      ChangeUI IDD_SELCOM "${MUI_UI_SMALLDESCRIPTION}"
    !else ifdef MUI_COMPONENTSPAGE_NODESC
      ChangeUI IDD_SELCOM "${MUI_UI_NODESCRIPTION}"
    !endif
  !else
    ChangeUI /RTL all "${MUI_UI}"
    !ifndef MUI_HEADERBITMAP_RIGHT
      ChangeUI /RTL IDD_INST "${MUI_UI_HEADERBITMAP}"
    !else
      ChangeUI /RTL IDD_INST "${MUI_UI_HEADERBITMAP_RIGHT}"
    !endif
    !ifdef MUI_COMPONENTSPAGE_SMALLDESC
      ChangeUI /RTL IDD_SELCOM "${MUI_UI_SMALLDESCRIPTION}"
    !else ifdef MUI_COMPONENTSPAGE_NODESC
      ChangeUI /RTL IDD_SELCOM "${MUI_UI_NODESCRIPTION}"
    !endif
  !endif
  
  Icon "${MUI_ICON}"
  
  !ifdef MUI_LICENSEPAGE
    !ifdef MUI_LICENSEPAGE_CHECKBOX
      LicenseForceSelection checkbox
    !else ifdef MUI_LICENSEPAGE_RADIOBUTTONS
      LicenseForceSelection radiobuttons
    !endif
  !endif
  
  !ifdef MUI_UNINSTALLER
    UninstallIcon "${MUI_UNICON}"
  !endif
  
  CheckBitmap "${MUI_CHECKBITMAP}"
  LicenseBkColor "${MUI_LICENSEBKCOLOR}"
  InstallColors ${MUI_INSTALLCOLORS}
  InstProgressFlags ${MUI_PROGRESSBAR}

!macroend

!macro MUI_INNERDIALOG_TEXT CONTROL TEXT

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  FindWindow $MUI_TEMP1 "#32770" "" $HWNDPARENT
  GetDlgItem $MUI_TEMP1 $MUI_TEMP1 ${CONTROL}
  SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:${TEXT}"

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_HEADER_TEXT TEXT SUBTEXT

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1037
  SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:${TEXT}"
  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1038
  SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:${SUBTEXT}"

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_DESCRIPTION_BEGIN

  FindWindow $MUI_TEMP1 "#32770" "" $HWNDPARENT
  GetDlgItem $MUI_TEMP1 $MUI_TEMP1 1043

  StrCmp $0 -1 0 mui.description_begin_done
    SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:"
    EnableWindow $MUI_TEMP1 0
    SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:$(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO)"
    Goto mui.description_done
  mui.description_begin_done:

!macroend

!macro MUI_DESCRIPTION_TEXT VAR TEXT

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  StrCmp $0 ${VAR} 0 mui.description_${VAR}_done
    SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:"
    EnableWindow $MUI_TEMP1 1
    SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:${TEXT}"
    Goto mui.description_done
  mui.description_${VAR}_done:

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_DESCRIPTION_END

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  mui.description_done:

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_FINISHHEADER

  IfAbort mui.finishheader_abort
  
  !ifndef MUI_FINISHPAGE
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_FINISH_TITLE) $(MUI_TEXT_FINISH_SUBTITLE)
  !else ifdef MUI_FINISHPAGE_NOAUTOCLOSE
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_FINISH_TITLE) $(MUI_TEXT_FINISH_SUBTITLE)
  !endif

  Goto mui.finishheader_done
  
  mui.finishheader_abort:
  !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_ABORT_TITLE) $(MUI_TEXT_ABORT_SUBTITLE)
  
  mui.finishheader_done:
  
!macroend

!macro MUI_UNFINISHHEADER
  
  IfAbort mui.finishheader_abort
  
  !insertmacro MUI_HEADER_TEXT $(un.MUI_UNTEXT_FINISH_TITLE) $(un.MUI_UNTEXT_FINISH_SUBTITLE)
  Goto mui.finishheader_done

  mui.finishheader_abort:
  !insertmacro MUI_HEADER_TEXT $(un.MUI_UNTEXT_ABORT_TITLE) $(un.MUI_UNTEXT_ABORT_SUBTITLE)
  
  mui.finishheader_done:

!macroend

!macro MUI_ABORTWARNING

  MessageBox MB_YESNO|MB_ICONEXCLAMATION "$(MUI_TEXT_ABORTWARNING)" IDYES quit
    Abort
    quit:

!macroend

!macro MUI_GUIINIT
  
  !insertmacro MUI_WELCOMEFINISHPAGE_INIT
  !insertmacro MUI_HEADERBITMAP_INIT

  !insertmacro MUI_GUIINIT_BASIC
  
!macroend

!macro MUI_UNGUIINIT

  !insertmacro MUI_HEADERBITMAP_INIT

  !insertmacro MUI_UNGUIINIT_BASIC
  
!macroend

!macro MUI_GUIINIT_BASIC

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1037
  CreateFont $MUI_TEMP2 "$(MUI_FONT_HEADER)" "$(MUI_FONTSIZE_HEADER)" "$(MUI_FONTSTYLE_HEADER)"
  SendMessage $MUI_TEMP1 ${WM_SETFONT} $MUI_TEMP2 0
  SetBkColor $MUI_TEMP1 "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1038
  SetBkColor $MUI_TEMP1 "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1034
  SetBkColor $MUI_TEMP1 "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1039
  SetBkColor $MUI_TEMP1 "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
  SetBkColor $MUI_TEMP1 -1
  GetWindowText $MUI_TEMP2 $MUI_TEMP1
  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
  SetBkColor $MUI_TEMP1 -1
  SendMessage $MUI_TEMP1 ${WM_SETTEXT} ${NSIS_MAX_STRLEN} "STR:$MUI_TEMP2"

!macroend

!macro MUI_UNGUIINIT_BASIC

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1037
  CreateFont $MUI_TEMP2 "$(un.MUI_FONT_HEADER)" "$(un.MUI_FONTSIZE_HEADER)" "$(un.MUI_FONTSTYLE_HEADER)"
  SendMessage $MUI_TEMP1 ${WM_SETFONT} $MUI_TEMP2 0
  SetBkColor $MUI_TEMP1 "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1038
  SetBkColor $MUI_TEMP1 "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1034
  SetBkColor $MUI_TEMP1 "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1039
  SetBkColor $MUI_TEMP1 "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
  SetBkColor $MUI_TEMP1 -1
  GetWindowText $MUI_TEMP2 $MUI_TEMP1
  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
  SetBkColor $MUI_TEMP1 -1
  SendMessage $MUI_TEMP1 ${WM_SETTEXT} ${NSIS_MAX_STRLEN} "STR:$MUI_TEMP2"

!macroend

!macro MUI_WELCOMEFINISHPAGE_INIT

  !ifdef MUI_WELCOMEPAGE | MUI_FINISHPAGE

    !insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS "${MUI_SPECIALINI}" "ioSpecial.ini"
    !insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS "${MUI_SPECIALBITMAP}" "modern-wizard.bmp"   
    
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 1" "Text" "$PLUGINSDIR\modern-wizard.bmp"

    !ifdef MUI_WELCOMEPAGE
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 2" "Text" "$(MUI_TEXT_WELCOME_INFO_TITLE)"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_TEXT_WELCOME_INFO_TEXT)"
    !endif
    
    !ifdef MUI_SPECIALBITMAP_NOSTRETCH
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 1" "Flags" ""
    !endif
    
  !endif

!macroend

!macro MUI_HEADERBITMAP_INIT

  !ifdef MUI_HEADERBITMAP
    InitPluginsDir
    File "/oname=$PLUGINSDIR\modern-header.bmp" "${MUI_HEADERBITMAP}"
    !ifndef MUI_HEADERBITMAP_NOSTRETCH
      SetBrandingImage /IMGID=1046 /RESIZETOFIT "$PLUGINSDIR\modern-header.bmp"
    !else
      SetBrandingImage /IMGID=1046 "$PLUGINSDIR\modern-header.bmp"
    !endif
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

!macro MUI_STARTMENU_INITDEFINES

  !ifndef MUI_STARTMENUPAGE_VARIABLE
    Var MUI_STARTMENU_FOLDER
    !define MUI_STARTMENUPAGE_VARIABLE "$MUI_STARTMENU_FOLDER"
  !endif
  
  !ifndef MUI_STARTMENUPAGE_DEFAULTFOLDER
    !define MUI_STARTMENUPAGE_DEFAULTFOLDER "${MUI_PRODUCT}"
  !endif

!macroend

!macro MUI_STARTMENU_GETFOLDER VAR

  !ifdef MUI_STARTMENUPAGE_REGISTRY_ROOT & MUI_STARTMENUPAGE_REGISTRY_KEY & MUI_STARTMENUPAGE_REGISTRY_VALUENAME

    ReadRegStr $MUI_TEMP1 "${MUI_STARTMENUPAGE_REGISTRY_ROOT}" "${MUI_STARTMENUPAGE_REGISTRY_KEY}" "${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}"
      StrCmp $MUI_TEMP1 "" +3
        StrCpy "${VAR}" $MUI_TEMP1
        Goto +2
      
        StrCpy "${VAR}" "${MUI_STARTMENUPAGE_DEFAULTFOLDER}"
 
   !else
   
     StrCpy "${VAR}" "${MUI_STARTMENUPAGE_DEFAULTFOLDER}"
  
   !endif
  
!macroend

!macro MUI_STARTMENU_GETFOLDER_IFEMPTY VAR
  
  StrCmp ${VAR} "" 0 mui.startmenu_writebegin_notempty

    !insertmacro MUI_STARTMENU_GETFOLDER ${VAR}
    
  mui.startmenu_writebegin_notempty:
  
!macroend

!macro MUI_STARTMENU_WRITE_BEGIN

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  StrCpy $MUI_TEMP1 ${MUI_STARTMENUPAGE_VARIABLE} 1
  StrCmp $MUI_TEMP1 ">" mui.startmenu_write_done
    
  !insertmacro MUI_STARTMENU_GETFOLDER_IFEMPTY ${MUI_STARTMENUPAGE_VARIABLE}
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_STARTMENU_WRITE_END

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  !ifdef MUI_STARTMENUPAGE_REGISTRY_ROOT & MUI_STARTMENUPAGE_REGISTRY_KEY & MUI_STARTMENUPAGE_REGISTRY_VALUENAME
    WriteRegStr "${MUI_STARTMENUPAGE_REGISTRY_ROOT}" "${MUI_STARTMENUPAGE_REGISTRY_KEY}" "${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}" "${MUI_STARTMENUPAGE_VARIABLE}"
  !endif

  mui.startmenu_write_done:
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_LANGDLL_DISPLAY

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  !ifndef MUI_TEXT_LANGDLL_WINDOWTITLE
    !define MUI_TEXT_LANGDLL_WINDOWTITLE "Installer Language"
  !endif

  !ifndef MUI_TEXT_LANGDLL_INFO
    !define MUI_TEXT_LANGDLL_INFO "Please select a language."
  !endif
  
  !ifdef MUI_LANGDLL_REGISTRY_ROOT & MUI_LANGDLL_REGISTRY_KEY & MUI_LANGDLL_REGISTRY_VALUENAME
    
    ReadRegStr $MUI_TEMP1 "${MUI_LANGDLL_REGISTRY_ROOT}" "${MUI_LANGDLL_REGISTRY_KEY}" "${MUI_LANGDLL_REGISTRY_VALUENAME}"
    StrCmp $MUI_TEMP1 "" showlangdialog
      StrCpy $LANGUAGE $MUI_TEMP1
      !ifndef MUI_LANGDLL_ALWAYSSHOW
        Goto mui.langdll_done
      !endif
    showlangdialog:
  
  !endif
  
  LangDLL::LangDialog "${MUI_TEXT_LANGDLL_WINDOWTITLE}" "${MUI_TEXT_LANGDLL_INFO}" A ${MUI_LANGDLL_PUSHLIST} ""

  Pop $LANGUAGE
  StrCmp $LANGUAGE "cancel" 0 +2
    Abort
  
  !ifndef MUI_LANGDLL_ALWAYSSHOW
    !ifdef MUI_LANGDLL_REGISTRY_ROOT & MUI_LANGDLL_REGISTRY_KEY & MUI_LANGDLL_REGISTRY_VALUENAME
      mui.langdll_done:
    !endif
  !endif
    
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
    
!macroend

!macro MUI_LANGDLL_SAVELANGUAGE

  !ifdef MUI_LANGDLL_REGISTRY_ROOT & MUI_LANGDLL_REGISTRY_KEY & MUI_LANGDLL_REGISTRY_VALUENAME
    WriteRegStr "${MUI_LANGDLL_REGISTRY_ROOT}" "${MUI_LANGDLL_REGISTRY_KEY}" "${MUI_LANGDLL_REGISTRY_VALUENAME}" $LANGUAGE
  !endif
  
!macroend

!macro MUI_UNGETLANGUAGE

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

  !ifdef MUI_LANGDLL_REGISTRY_ROOT & MUI_LANGDLL_REGISTRY_KEY & MUI_LANGDLL_REGISTRY_VALUENAME
  
    ReadRegStr $MUI_TEMP1 "${MUI_LANGDLL_REGISTRY_ROOT}" "${MUI_LANGDLL_REGISTRY_KEY}" "${MUI_LANGDLL_REGISTRY_VALUENAME}"
    StrCmp $MUI_TEMP1 "" 0 mui.ungetlanguage_setlang
  
  !endif
    
  !insertmacro MUI_LANGDLL_DISPLAY
      
  !ifdef MUI_LANGDLL_REGISTRY_ROOT & MUI_LANGDLL_REGISTRY_KEY & MUI_LANGDLL_REGISTRY_VALUENAME
  
    Goto mui.ungetlanguage_done
   
    mui.ungetlanguage_setlang:
      StrCpy $LANGUAGE $MUI_TEMP1
        
    mui.ungetlanguage_done:

  !endif
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

;--------------------------------
;PAGE COMMANDS

!macro MUI_PAGE_WELCOME

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  !ifndef MUI_WELCOMEPAGE
    !define MUI_WELCOMEPAGE
  !endif
  
  Page custom mui.WelcomePre mui.WelcomeLeave "" "MUI_INSTALLBUTTON_WELCOME"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGE_LICENSE

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  !ifndef MUI_LICENSEPAGE
    !define MUI_LICENSEPAGE
  !endif
  
  Page license mui.LicensePre mui.LicenseShow mui.LicenseLeave "MUI_INSTALLBUTTON_LICENSE"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGE_COMPONENTS

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  !ifndef MUI_COMPONENTSPAGE
    !define MUI_COMPONENTSPAGE
  !endif
  
  Page components mui.ComponentsPre mui.ComponentsShow mui.ComponentsLeave "MUI_INSTALLBUTTON_COMPONENTS"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGE_DIRECTORY

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  !ifndef MUI_DIRECTORYPAGE
    !define MUI_DIRECTORYPAGE
  !endif
    
  Page directory mui.DirectoryPre mui.DirectoryShow mui.DirectoryLeave "MUI_INSTALLBUTTON_DIRECTORY"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGE_STARTMENU

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  !ifndef MUI_STARTMENUPAGE
    !define MUI_STARTMENUPAGE
  !endif
  
  Page custom mui.StartmenuPre mui.StartmenuLeave "" "MUI_INSTALLBUTTON_STARTMENU"
  
  !ifndef MUI_NOVERBOSE & MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_PAGE_INSTFILES

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  Page instfiles mui.InstFilesPre mui.InstFilesShow mui.InstFilesLeave
   
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
   
!macroend

!macro MUI_PAGE_FINISH

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  !ifndef MUI_FINISHPAGE
    !define MUI_FINISHPAGE
  !endif
  
  Page custom mui.FinishPre mui.FinishLeave
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_UNPAGE_CONFIRM

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  !ifndef MUI_UNINSTALLER
    !define MUI_UNINSTALLER
  !endif

  !ifndef MUI_UNCONFIRMPAGE
    !define MUI_UNCONFIRMPAGE
  !endif
  
  UninstPage uninstConfirm un.mui.ConfirmPre un.mui.ConfirmShow un.mui.ConfirmLeave "MUI_UNINSTALLBUTTON_CONFIRM"
  
  !ifndef MUI_NOVERBOSE & MUI_MANUALVERBOSE
    !verbose 4
  !endif
   
!macroend

!macro MUI_UNPAGE_INSTFILES

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  !ifndef MUI_UNINSTALLER
    !define MUI_UNINSTALLER
  !endif

  UninstPage instfiles un.mui.InstFilesPre un.mui.InstFilesShow un.mui.InstFilesLeave
   
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
   
!macroend

;--------------------------------
;INSTALL OPTIONS

!macro MUI_INSTALLOPTIONS_EXTRACT FILE

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  InitPluginsDir

  File "/oname=$PLUGINSDIR\${FILE}" "${FILE}"

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_EXTRACT_AS FILE FILENAME

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  InitPluginsDir

  File "/oname=$PLUGINSDIR\${FILENAME}" "${FILE}"

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_DISPLAY FILE

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop $MUI_TEMP1

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_DISPLAY_RETURN FILE

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_INITDIALOG FILE

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  InstallOptions::initDialog /NOUNLOAD "$PLUGINSDIR\${FILE}"

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_SHOW

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  InstallOptions::show
  Pop $MUI_TEMP1

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_SHOW_RETURN

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  InstallOptions::show

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_READ VAR FILE SECTION KEY

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  ReadIniStr ${VAR} "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}"

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

!macro MUI_INSTALLOPTIONS_WRITE FILE SECTION KEY VALUE

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  WriteIniStr "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}" "${VALUE}"

  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif

!macroend

;--------------------------------
;FUNCTIONS

!macro MUI_FUNCTIONS_GUIINIT

  Function .onGUIInit
     
  !insertmacro MUI_GUIINIT
  
  !ifdef MUI_CUSTOMFUNCTION_GUIINIT
    Call "${MUI_CUSTOMFUNCTION_GUIINIT}"
  !endif

  FunctionEnd

!macroend

!macro MUI_FUNCTIONS_PAGES

  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_FUNCTIONS_WELCOMEPAGE mui.WelcomePre mui.WelcomeLeave
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
    !insertmacro MUI_FUNCTIONS_STARTMENUPAGE mui.StartmenuPre mui.StartmenuLeave
  !endif
  
  !insertmacro MUI_FUNCTIONS_INSTFILESPAGE mui.InstFilesPre mui.InstFilesShow mui.InstFilesLeave
    
  !ifdef MUI_FINISHPAGE
    !insertmacro MUI_FUNCTIONS_FINISHPAGE mui.FinishPre mui.FinishLeave
  !endif

!macroend

!macro MUI_FUNCTIONS_WELCOMEPAGE PRE LEAVE

  Function "${PRE}"

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1035
    ShowWindow $MUI_TEMP1 ${SW_HIDE}
      
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1045
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}
      
    !ifdef MUI_CUSTOMFUNCTION_WELCOME_PRE
      Call "${MUI_CUSTOMFUNCTION_WELCOME_PRE}"
    !endif
      
    !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ioSpecial.ini"
    Pop $MUI_HWND
        
    SetBkColor $MUI_HWND "${MUI_BGCOLOR}"
      
    GetDlgItem $MUI_TEMP2 $MUI_HWND 1201
    SetBkColor $MUI_TEMP2 "${MUI_BGCOLOR}"
    CreateFont $MUI_TEMP3 "$(MUI_FONT_TITLE)" "$(MUI_FONTSIZE_TITLE)" "$(MUI_FONTSTYLE_TITLE)"
    SendMessage $MUI_TEMP2 ${WM_SETFONT} $MUI_TEMP3 0
        
    GetDlgItem $MUI_TEMP2 $MUI_HWND 1202
    SetBkColor $MUI_TEMP2 "${MUI_BGCOLOR}"
        
    GetDlgItem $MUI_TEMP2 $MUI_HWND 1200
    SetBkColor $MUI_TEMP2 "${MUI_BGCOLOR}"

    !ifdef MUI_CUSTOMFUNCTION_WELCOME_SHOW
      Call "${MUI_CUSTOMFUNCTION_WELCOME_SHOW}"
    !endif
  
    !insertmacro MUI_INSTALLOPTIONS_SHOW
     
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1035
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}
      
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1045
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

  FunctionEnd
  
  Function "${LEAVE}"
  
    !ifdef MUI_CUSTOMFUNCTION_WELCOME_LEAVE
      Call "${MUI_CUSTOMFUNCTION_WELCOME_LEAVE}"
    !endif
  
  FunctionEnd
  
!macroend

!macro MUI_FUNCTIONS_LICENSEPAGE PRE SHOW LEAVE

  Function "${PRE}"
    !ifdef MUI_CUSTOMFUNCTION_LICENSE_PRE
      Call "${MUI_CUSTOMFUNCTION_LICENSE_PRE}"
    !endif
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_LICENSE_TITLE) $(MUI_TEXT_LICENSE_SUBTITLE)
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

!macroend

!macro MUI_FUNCTIONS_COMPONENTSPAGE PRE SHOW LEAVE

  Function "${PRE}"
    !ifdef MUI_CUSTOMFUNCTION_COMPONENTS_PRE
      Call "${MUI_CUSTOMFUNCTION_COMPONENTS_PRE}"
    !endif
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_COMPONENTS_TITLE) $(MUI_TEXT_COMPONENTS_SUBTITLE)
  FunctionEnd

  Function "${SHOW}"
    !insertmacro MUI_INNERDIALOG_TEXT 1042 $(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE)
    FindWindow $MUI_TEMP1 "#32770" "" $HWNDPARENT
    GetDlgItem $MUI_TEMP1 $MUI_TEMP1 1043
    EnableWindow $MUI_TEMP1 0
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
    
!macroend

!macro MUI_FUNCTIONS_DIRECTORYPAGE PRE SHOW LEAVE

  Function "${PRE}"
    !ifdef MUI_CUSTOMFUNCTION_DIRECTORY_PRE
      Call "${MUI_CUSTOMFUNCTION_DIRECTORY_PRE}"
    !endif
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_DIRECTORY_TITLE) $(MUI_TEXT_DIRECTORY_SUBTITLE)
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

!macroend

!macro MUI_FUNCTIONS_STARTMENUPAGE PRE LEAVE
  
  Function "${PRE}"
  
    !ifdef MUI_CUSTOMFUNCTION_STARTMENU_PRE
      Call "${MUI_CUSTOMFUNCTION_STARTMENU_PRE}"
    !endif

    !ifdef MUI_STARTMENUPAGE_REGISTRY_ROOT & MUI_STARTMENUPAGE_REGISTRY_KEY & MUI_STARTMENUPAGE_REGISTRY_VALUENAME

      StrCmp "${MUI_STARTMENUPAGE_VARIABLE}" "" 0 +4

      ReadRegStr $MUI_TEMP1 "${MUI_STARTMENUPAGE_REGISTRY_ROOT}" "${MUI_STARTMENUPAGE_REGISTRY_KEY}" "${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}"
        StrCmp $MUI_TEMP1 "" +2
          StrCpy "${MUI_STARTMENUPAGE_VARIABLE}" $MUI_TEMP1
    
    !endif
  
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_STARTMENU_TITLE) $(MUI_TEXT_STARTMENU_SUBTITLE)
    
    !ifndef MUI_STARTMENUPAGE_NODISABLE
      StartMenu::Select /noicon /autoadd /text "$(MUI_INNERTEXT_STARTMENU_TOP)" /lastused "${MUI_STARTMENUPAGE_VARIABLE}" /checknoshortcuts "$(MUI_INNERTEXT_STARTMENU_CHECKBOX)" "${MUI_STARTMENUPAGE_DEFAULTFOLDER}"
    !else
      StartMenu::Select /noicon /autoadd /text "$(MUI_INNERTEXT_STARTMENU_TOP)" /lastused "${MUI_STARTMENUPAGE_VARIABLE}" "${MUI_STARTMENUPAGE_DEFAULTFOLDER}"
    !endif
      
    StrCmp $MUI_TEMP1 "success" 0 +2
      Pop "${MUI_STARTMENUPAGE_VARIABLE}"
      
  FunctionEnd

  Function "${LEAVE}"

    !ifdef MUI_CUSTOMFUNCTION_STARTMENU_LEAVE
      Call "${MUI_CUSTOMFUNCTION_STARTMENU_LEAVE}"
    !endif

  FunctionEnd

!macroend

!macro MUI_FUNCTIONS_INSTFILESPAGE PRE SHOW LEAVE

  Function "${PRE}"
    !ifdef MUI_CUSTOMFUNCTION_INSTFILES_PRE
      Call "${MUI_CUSTOMFUNCTION_INSTFILES_PRE}"
    !endif
    !insertmacro MUI_HEADER_TEXT $(MUI_TEXT_INSTALLING_TITLE) $(MUI_TEXT_INSTALLING_SUBTITLE)
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
      
    !insertmacro MUI_FINISHHEADER
    !insertmacro MUI_LANGDLL_SAVELANGUAGE
    
  FunctionEnd
  
!macroend

!macro MUI_FUNCTIONS_FINISHPAGE PRE LEAVE

  Function "${PRE}"
    
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1035
    ShowWindow $MUI_TEMP1 ${SW_HIDE}
      
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1045
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}
    
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 2" "Text" "$(MUI_TEXT_FINISH_INFO_TITLE)"
    
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Top" "45"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Bottom" "85"
    
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
  
      IfRebootFlag "" mui.finish_noreboot_init
    
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_TEXT_FINISH_INFO_REBOOT)"
    
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "5"
        
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Type" "RadioButton"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_REBOOTNOW)"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Left" "120"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Right" "321"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Top" "90"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Bottom" "100"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "State" "1"
        
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Type" "RadioButton"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Text" "$(MUI_TEXT_FINISH_REBOOTLATER)"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Left" "120"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Right" "321"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Top" "110"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Bottom" "120"
    
        Goto mui.finish_load
     
      mui.finish_noreboot_init:
      
    !endif
       
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_TEXT_FINISH_INFO_TEXT)"
      
    !ifdef MUI_FINISHPAGE_RUN
              
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Type" "CheckBox"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_RUN)"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Left" "120"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Right" "315"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Top" "90"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Bottom" "100"
      !ifndef MUI_FINISHPAGE_RUN_NOTCHECKED
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "State" "1"
      !endif
    
    !endif
          
    !ifdef MUI_FINISHPAGE_SHOWREADME
    
      !ifndef MUI_FINISHPAGE_RUN
        !define MUI_FINISHPAGE_CURFIELD_NO 4
        !define MUI_FINISHPAGE_CURFIELD_TOP 90
        !define MUI_FINISHPAGE_CURFIELD_BOTTOM 100
      !else
        !define MUI_FINISHPAGE_CURFIELD_NO 5
        !define MUI_FINISHPAGE_CURFIELD_TOP 110
        !define MUI_FINISHPAGE_CURFIELD_BOTTOM 120
      !endif
      
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Type" "CheckBox"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Text" "$(MUI_TEXT_FINISH_SHOWREADME)"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Left" "120"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Right" "315"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Top" "${MUI_FINISHPAGE_CURFIELD_TOP}"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Bottom" "${MUI_FINISHPAGE_CURFIELD_BOTTOM}"
      !ifndef MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
         !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "State" "1"
      !endif
            
    !endif

    !ifdef MUI_FINISHPAGE_LINK
    
      !ifdef MUI_FINISHPAGE_CURFIELD_NO
        !undef MUI_FINISHPAGE_CURFIELD_NO
      !endif
    
      !ifdef MUI_FINISHPAGE_RUN & MUI_FINISHPAGE_SHOWREADME
        !define MUI_FINISHPAGE_CURFIELD_NO 6
      !else ifdef MUI_FINISHPAGE_RUN | MUI_FINISHPAGE_SHOWREADME
        !define MUI_FINISHPAGE_CURFIELD_NO 5
      !else
        !define MUI_FINISHPAGE_CURFIELD_NO 4
      !endif
    
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Type" "Link"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Text" "${MUI_FINISHPAGE_LINK}"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Left" "120"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Right" "315"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Top" "175"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Bottom" "185"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "State" "${MUI_FINISHPAGE_LINK_LOCATION}"
            
    !endif
    
    !ifdef MUI_FINISHPAGE_RUN & MUI_FINISHPAGE_SHOWREADME & MUI_FINISHPAGE_LINK
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "6"
    !else ifdef MUI_FINISHPAGE_RUN & MUI_FINISHPAGE_SHOWREADME
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "5"
    !else ifdef MUI_FINISHPAGE_RUN & MUI_FINISHPAGE_LINK
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "5"
    !else ifdef MUI_FINISHPAGE_SHOWREADME & MUI_FINISHPAGE_LINK
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "5"
    !else ifdef MUI_FINISHPAGE_RUN | MUI_FINISHPAGE_SHOWREADME | MUI_FINISHPAGE_LINK
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "4"
    !endif
    
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
       mui.finish_load:
    !endif
      
    !ifdef MUI_CUSTOMFUNCTION_FINISH_PRE
       Call "${MUI_CUSTOMFUNCTION_FINISH_PRE}"
    !endif
      
    !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ioSpecial.ini"
    Pop $MUI_HWND
    
    SetBkColor $MUI_HWND "${MUI_BGCOLOR}"
    
    GetDlgItem $MUI_TEMP2 $MUI_HWND 1201
    SetBkColor $MUI_TEMP2 "${MUI_BGCOLOR}"
    CreateFont $MUI_TEMP3 "$(MUI_FONT_TITLE)" "$(MUI_FONTSIZE_TITLE)" "$(MUI_FONTSTYLE_TITLE)"
    SendMessage $MUI_TEMP2 ${WM_SETFONT} $MUI_TEMP3 0
    
    GetDlgItem $MUI_TEMP2 $MUI_HWND 1202
    SetBkColor $MUI_TEMP2 "${MUI_BGCOLOR}"
    
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
        
      IfRebootFlag "" mui.finish_noreboot_show
    
        GetDlgItem $MUI_TEMP2 $MUI_TEMP1 1203
        SetBkColor $MUI_TEMP2 "${MUI_BGCOLOR}"
        
        GetDlgItem $MUI_TEMP2 $MUI_TEMP1 1204
        SetBkColor $MUI_TEMP2 "${MUI_BGCOLOR}"
          
        Goto mui.finish_show
        
      mui.finish_noreboot_show:
        
    !endif
    
    !ifdef MUI_FINISHPAGE_RUN
      GetDlgItem $MUI_TEMP2 $MUI_HWND 1203
      SetBkColor $MUI_TEMP2 "${MUI_BGCOLOR}"
    !endif
           
    !ifdef MUI_FINISHPAGE_SHOWREADME
      !ifndef MUI_FINISHPAGE_RUN
        GetDlgItem $MUI_TEMP2 $MUI_HWND 1203
      !else
        GetDlgItem $MUI_TEMP2 $MUI_HWND 1204
      !endif
      SetBkColor $MUI_TEMP2 "${MUI_BGCOLOR}"  
    !endif
     
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
      mui.finish_show:
    !endif

    !ifdef MUI_CUSTOMFUNCTION_FINISH_SHOW
      Call "${MUI_CUSTOMFUNCTION_FINISH_SHOW}"
    !endif

    !insertmacro MUI_INSTALLOPTIONS_SHOW_RETURN
    Pop $MUI_TEMP1
    StrCmp $MUI_TEMP1 "success" 0 mui.finish_done
    
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1035
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}
      
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1045
    ShowWindow $MUI_TEMP1 ${SW_HIDE}
      
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
    
      IfRebootFlag "" mui.finish_noreboot_end
      
        !insertmacro MUI_INSTALLOPTIONS_READ $MUI_TEMP1 "ioSpecial.ini" "Field 4" "State"
       
          StrCmp $MUI_TEMP1 "1" 0 +2
            Reboot
            
          Goto mui.finish_done
      
      mui.finish_noreboot_end:
        
    !endif
      
    !ifdef MUI_FINISHPAGE_RUN
  
      !insertmacro MUI_INSTALLOPTIONS_READ $MUI_TEMP1 "ioSpecial.ini" "Field 4" "State"
        
       StrCmp $MUI_TEMP1 "1" 0 mui.finish_norun
         !ifndef MUI_FINISHPAGE_RUN_FUNCTION
           !ifndef MUI_FINISHPAGE_RUN_PARAMETERS
             StrCpy $MUI_TEMP1 "$\"${MUI_FINISHPAGE_RUN}$\""
           !else
             StrCpy $MUI_TEMP1 "$\"${MUI_FINISHPAGE_RUN}$\" ${MUI_FINISHPAGE_RUN_PARAMETERS}"
           !endif
           Exec "$MUI_TEMP1"
         !else
           Call "${MUI_FINISHPAGE_RUN_FUNCTION}"
         !endif
             
         mui.finish_norun:
           
    !endif
             
    !ifdef MUI_FINISHPAGE_SHOWREADME
       
      !ifndef MUI_FINISHPAGE_RUN
        !insertmacro MUI_INSTALLOPTIONS_READ $MUI_TEMP1 "ioSpecial.ini" "Field 4" "State"
      !else
        !insertmacro MUI_INSTALLOPTIONS_READ $MUI_TEMP1 "ioSpecial.ini" "Field 5" "State"
      !endif

      StrCmp $MUI_TEMP1 "1" 0 mui.finish_noshowreadme
        !ifndef MUI_FINISHPAGE_SHOWREADME_FUNCTION
           ExecShell "open" "${MUI_FINISHPAGE_SHOWREADME}"
        !else
          Call "${MUI_FINISHPAGE_SHOWREADME_FUNCTION}"
        !endif
        
        mui.finish_noshowreadme:
               
    !endif
        
    mui.finish_done:

    !ifdef MUI_CUSTOMFUNCTION_FINISH
      Call "${MUI_CUSTOMFUNCTION_FINISH}"
    !endif
    
  FunctionEnd
  
  Function "${LEAVE}"
  
    !ifdef MUI_CUSTOMFUNCTION_FINISH_LEAVE
      Call "${MUI_CUSTOMFUNCTION_FINISH_LEAVE}"
    !endif
  
  FunctionEnd
  
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

  !ifdef MUI_ABORTWARNING
    Function .onUserAbort
      !insertmacro MUI_ABORTWARNING
      !ifdef MUI_CUSTOMFUNCTION_ABORT
        Call "${MUI_CUSTOMFUNCTION_ABORT}"
      !endif
    FunctionEnd
  !endif

!macroend

!macro MUI_UNFUNCTION_GUIINIT
  
  Function un.onGUIInit
  
  !insertmacro MUI_UNGUIINIT
  
  !ifdef MUI_CUSTOMFUNCTION_UNGUIINIT
    Call "${MUI_CUSTOMFUNCTION_UNGUIINIT}"
  !endif
  
  FunctionEnd

!macroend

!macro MUI_UNFUNCTION_PAGES
  
  !ifdef MUI_UNCONFIRMPAGE
    !insertmacro MUI_UNFUNCTION_CONFIRMPAGE un.mui.ConfirmPre un.mui.ConfirmShow un.mui.ConfirmLeave
  !endif
    
  !insertmacro MUI_UNFUNCTION_INSTFILESPAGE un.mui.InstFilesPre un.mui.InstFilesShow un.mui.InstFilesLeave
  
!macroend

!macro MUI_UNFUNCTION_CONFIRMPAGE PRE SHOW LEAVE

  Function "${PRE}"
    !ifdef MUI_UNCUSTOMFUNCTION_CONFIRM_PRE
      Call "${MUI_UNCUSTOMFUNCTION_CONFIRM_PRE}"
    !endif
    !insertmacro MUI_HEADER_TEXT $(un.MUI_UNTEXT_INTRO_TITLE) $(un.MUI_UNTEXT_INTRO_SUBTITLE)
  FunctionEnd
  
  Function "${SHOW}"
    !ifdef MUI_UNCUSTOMFUNCTION_CONFIRM_SHOW
      Call "${MUI_UNCUSTOMFUNCTION_CONFIRM_SHOW}"
    !endif
  FunctionEnd
  
  Function "${LEAVE}"
    !ifdef MUI_UNCUSTOMFUNCTION_CONFIRM_LEAVE
      Call "${MUI_UNCUSTOMFUNCTION_CONFIRM_LEAVE}"
    !endif
  FunctionEnd
  
!macroend

!macro MUI_UNFUNCTION_INSTFILESPAGE PRE SHOW LEAVE

  Function ${PRE}
    !ifdef MUI_UNCUSTOMFUNCTION_INSTFILES_PRE
      Call "${MUI_UNCUSTOMFUNCTION_INSTFILES_PRE}"
    !endif
    !insertmacro MUI_HEADER_TEXT $(un.MUI_UNTEXT_UNINSTALLING_TITLE) $(un.MUI_UNTEXT_UNINSTALLING_SUBTITLE)
  FunctionEnd

  Function "${SHOW}"
    !ifdef MUI_UNCUSTOMFUNCTION_INSTFILES_SHOW
      Call "${MUI_UNCUSTOMFUNCTION_INSTFILES_SHOW}"
    !endif
  FunctionEnd
  
  Function "${LEAVE}"
    !ifdef MUI_UNCUSTOMFUNCTION_INSTFILES_LEAVE
      Call "${MUI_UNCUSTOMFUNCTION_INSTFILES_LEAVE}"
    !endif
    !insertmacro MUI_UNFINISHHEADER
  FunctionEnd
  
!macroend

;--------------------------------
;RESERVE FILES

!macro MUI_RESERVEFILE_WELCOMEFINISHPAGE

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
    !define MUI_NOVERBOSE

    !insertmacro MUI_RESERVEFILE_SPECIALINI
    !insertmacro MUI_RESERVEFILE_SPECIALBITMAP
    !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS
    
    !undef MUI_NOVERBOSE
    
  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
    
!macroend

!macro MUI_RESERVEFILE_INSTALLOPTIONS

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_RESERVEFILE_SPECIALINI

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  ReserveFile "${NSISDIR}\Contrib\Modern UI\ioSpecial.ini"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_RESERVEFILE_SPECIALBITMAP

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif

  ReserveFile "${NSISDIR}\Contrib\Icons\modern-wizard.bmp"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_RESERVEFILE_LANGDLL

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  ReserveFile "${NSISDIR}\Plugins\LangDLL.dll"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

!macro MUI_RESERVEFILE_STARTMENU

  !ifndef MUI_MANUALVERBOSE
    !verbose 3
  !endif
  
  ReserveFile "${NSISDIR}\Plugins\StartMenu.dll"
  
  !ifndef MUI_MANUALVERBOSE
    !verbose 4
  !endif
  
!macroend

;--------------------------------
;INSERT ALL CODE

!macro MUI_INSERT
  
  !ifndef MUI_MANUALVERBOSE
    !define MUI_MANUALVERBOSE_SET
    !define MUI_MANUALVERBOSE
  !endif
  
  !insertmacro MUI_DEFINEVARS

  !ifdef MUI_STARTMENUPAGE
    !insertmacro MUI_STARTMENU_INITDEFINES
  !endif
  
  !insertmacro MUI_INTERFACE
   
  !insertmacro MUI_FUNCTIONS_PAGES
  !insertmacro MUI_FUNCTIONS_GUIINIT
  !insertmacro MUI_FUNCTIONS_ABORTWARNING
  
  !ifdef MUI_UNINSTALLER
    !insertmacro MUI_UNFUNCTION_PAGES
    !insertmacro MUI_UNFUNCTION_GUIINIT
  !endif
  
  !ifdef MUI_MANUALVERBOSE_SET
    !undef MUI_MANUALVERBOSE
  !endif
  
!macroend

;--------------------------------
;LANGUAGE FILES

!macro MUI_LANGUAGEFILE_BEGIN LANGUAGE

  !ifndef MUI_INSERT
    !define MUI_INSERT
    !insertmacro MUI_INSERT
  !endif
  
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
    LangString "${NAME}" 0 "${${NAME}} ${MUI_TEXT_CONTINUE_NEXT}"
  !else
    LangString "${NAME}" 0 "${${NAME}} ${MUI_TEXT_CONTINUE_INSTALL}"
  !endif

  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_CONTINUE_NOSPACE NAME INSTALLBUTTON

  !ifndef "${INSTALLBUTTON}"
    LangString "${NAME}" 0 "${${NAME}}${MUI_TEXT_CONTINUE_NEXT}"
  !else
    LangString "${NAME}" 0 "${${NAME}}${MUI_TEXT_CONTINUE_INSTALL}"
  !endif

  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_CUSTOMDEFINE_NOUNDEF NAME DEFINE

  LangString "${NAME}" 0 "${${DEFINE}}"

!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_CUSTOMDEFINE_CONTINUE NAME DEFINE INSTALLBUTTON

  !ifndef "${INSTALLBUTTON}"
    LangString "${NAME}" 0 "${${DEFINE}} ${MUI_TEXT_CONTINUE_NEXT}"
  !else
    LangString "${NAME}" 0 "${${DEFINE}} ${MUI_TEXT_CONTINUE_INSTALL}"
  !endif

  !undef "${DEFINE}"
  
!macroend

!macro MUI_LANGUAGEFILE_UNLANGSTRING NAME

  LangString "un.${NAME}" 0 "${${NAME}}"
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_UNLANGSTRING_CUSTOMDEFINE_NOUNDEF NAME DEFINE

  LangString "un.${NAME}" 0 "${${DEFINE}}"

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
    ${COMMAND} "${${NAME}} ${MUI_UNTEXT_CONTINUE_UNINSTALL}"
  !endif
  
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_DEFINE DEFINE NAME

  !ifndef "${DEFINE}"
    !define "${DEFINE}" "${${NAME}}"
  !endif
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_INSTFONT NAME DEFAULT

  !ifdef "${NAME}"
    Langstring "${NAME}" 0 "${${NAME}}"
    !undef "${NAME}"
  !else
    Langstring "${NAME}" 0 "${DEFAULT}"
  !endif
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_FONT NAME DEFAULT

  !ifdef "${NAME}"
    Langstring "${NAME}" 0 "${${NAME}}"
    Langstring "un.${NAME}" 0 "${${NAME}}"
    !undef "${NAME}"
  !else
    Langstring "${NAME}" 0 "${DEFAULT}"
    Langstring "un.${NAME}" 0 "${DEFAULT}"
  !endif
  
!macroend

!macro MUI_LANGUAGEFILE_END

  !insertmacro MUI_LANGUAGEFILE_DEFINE "MUI_${LANGUAGE}_LANGNAME" "MUI_LANGNAME"
    
  !ifndef MUI_LANGDLL_PUSHLIST
    !define MUI_LANGDLL_PUSHLIST "'${MUI_${LANGUAGE}_LANGNAME}' ${LANG_${LANGUAGE}} "
  !else
    !ifdef MUI_LANGDLL_PUSHLIST_TEMP
      !undef MUI_LANGDLL_PUSHLIST_TEMP
    !endif
    !define MUI_LANGDLL_PUSHLIST_TEMP "${MUI_LANGDLL_PUSHLIST}"
    !undef MUI_LANGDLL_PUSHLIST
    !define MUI_LANGDLL_PUSHLIST "'${MUI_${LANGUAGE}_LANGNAME}' ${LANG_${LANGUAGE}} ${MUI_LANGDLL_PUSHLIST_TEMP}"
  !endif
  
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_FONT "MUI_FONT_HEADER" "MS Shell Dlg"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_FONT "MUI_FONTSIZE_HEADER" "8"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_FONT "MUI_FONTSTYLE_HEADER" "700"
  
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_INSTFONT "MUI_FONT_TITLE" "Verdana"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_INSTFONT "MUI_FONTSIZE_TITLE" "12"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_INSTFONT "MUI_FONTSTYLE_TITLE" "700"
    
  !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND "Name" "MUI_NAME"
  
  SubCaption 0 " "
  SubCaption 1 " "
  SubCaption 2 " "
  SubCaption 3 " "
  SubCaption 4 " "
  
  UninstallSubCaption 0 " "
  UninstallSubCaption 1 " "
  UninstallSubCaption 2 " "
  
  !ifndef MUI_BRANDINGTEXT
    !define MUI_BRANDINGTEXT ""
  !endif
  !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND "BrandingText" "MUI_BRANDINGTEXT"

  !ifdef MUI_WELCOMEPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_WELCOME_INFO_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CONTINUE_NOSPACE "MUI_TEXT_WELCOME_INFO_TEXT" "MUI_INSTALLBUTTON_WELCOME"
  !endif

  !ifdef MUI_LICENSEPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_LICENSE_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_LICENSE_SUBTITLE"
    !ifndef MUI_BUTTONTEXT_LICENSE
      !define MUI_BUTTONTEXT_LICENSE ""
    !endif
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "LicenseText" "MUI_INNERTEXT_LICENSE_TOP" "$\"${MUI_INNERTEXT_LICENSE_TOP}$\" $\"${MUI_BUTTONTEXT_LICENSE}$\""
    !undef MUI_BUTTONTEXT_LICENSE
    !ifndef MUI_LICENSEPAGE_CHECKBOX & MUI_LICENSEPAGE_RADIOBUTTONS
      !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_LICENSE_BOTTOM"
    !else
      !ifndef MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX | MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS
        !error "The ${LANGUAGE} Modern UI language file does not contain a bottom text for a license pages with a checkbox or radiobuttons. Please update this file and post your language file on the NSIS Project Page, http://nsis.sf.net."
      !endif
      !ifdef MUI_LICENSEPAGE_CHECKBOX
        !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CUSTOMDEFINE_CONTINUE "MUI_INNERTEXT_LICENSE_BOTTOM" "MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX" "MUI_INSTALLBUTTON_LICENSE"
      !else ifdef MUI_LICENSEPAGE_RADIOBUTTONS
        !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CUSTOMDEFINE_CONTINUE "MUI_INNERTEXT_LICENSE_BOTTOM" "MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS" "MUI_INSTALLBUTTON_LICENSE"
      !endif
    !endif
  !endif
  
  !ifdef MUI_COMPONENTSPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_COMPONENTS_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_COMPONENTS_SUBTITLE"
    !ifndef MUI_INNERTEXT_COMPONENTS_SUB1
      !define MUI_INNERTEXT_COMPONENTS_SUB1 ""
    !endif
    !ifndef MUI_INNERTEXT_COMPONENTS_SUB2
      !define MUI_INNERTEXT_COMPONENTS_SUB2 ""
    !endif
    !ifndef MUI_INSTALLBUTTON_COMPONENTS
      !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "ComponentText" "MUI_INNERTEXT_COMPONENTS_TOP" "$\"${MUI_INNERTEXT_COMPONENTS_TOP} ${MUI_TEXT_CONTINUE_NEXT}$\" $\"${MUI_INNERTEXT_COMPONENTS_SUB1}$\" $\"${MUI_INNERTEXT_COMPONENTS_SUB2}$\""
    !else
      !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "ComponentText" "MUI_INNERTEXT_COMPONENTS_TOP" "$\"${MUI_INNERTEXT_COMPONENTS_TOP} ${MUI_TEXT_CONTINUE_INSTALL}$\" $\"${MUI_INNERTEXT_COMPONENTS_SUB1}$\" $\"${MUI_INNERTEXT_COMPONENTS_SUB2}$\""
    !endif
    !undef MUI_INNERTEXT_COMPONENTS_SUB1
    !undef MUI_INNERTEXT_COMPONENTS_SUB2
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO"
  !endif
  
  !ifdef MUI_DIRECTORYPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_DIRECTORY_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_DIRECTORY_SUBTITLE"
    !ifndef MUI_TEXT_DIRECTORY_BROWSE
      !define MUI_TEXT_DIRECTORY_BROWSE ""
    !endif
    !ifndef MUI_INSTALLBUTTON_DIRECTORY
      !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "DirText" "MUI_INNERTEXT_DIRECTORY_TOP" "$\"${MUI_INNERTEXT_DIRECTORY_TOP} ${MUI_TEXT_CONTINUE_NEXT}$\" $\"$\" $\"${MUI_TEXT_DIRECTORY_BROWSE}$\""
    !else
      !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "DirText" "MUI_INNERTEXT_DIRECTORY_TOP" "$\"${MUI_INNERTEXT_DIRECTORY_TOP} ${MUI_TEXT_CONTINUE_INSTALL}$\" $\"$\" $\"${MUI_TEXT_DIRECTORY_BROWSE}$\""
    !endif
    !undef MUI_TEXT_DIRECTORY_BROWSE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_DIRECTORY_DESTINATION"
  !endif
  
  !ifdef MUI_STARTMENUPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_STARTMENU_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_STARTMENU_SUBTITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CONTINUE "MUI_INNERTEXT_STARTMENU_TOP" "MUI_INSTALLBUTTON_STARTMENU"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_INNERTEXT_STARTMENU_CHECKBOX"
  !endif
  
  !ifndef MUI_FINISHPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_SUBTITLE"
  !else ifdef MUI_FINISHPAGE_NOAUTOCLOSE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_SUBTITLE"
  !endif
  
  !ifdef MUI_TEXT_ABORT_TITLE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_ABORT_TITLE"
  !else
    ;1.63 compatibility
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CUSTOMDEFINE_NOUNDEF "MUI_TEXT_ABORT_TITLE" "MUI_TEXT_INSTALLING_TITLE"
  !endif
  
  !ifdef MUI_TEXT_ABORT_SUBTITLE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_ABORT_SUBTITLE"
  !else
    ;1.63 compatibility
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING_CUSTOMDEFINE_NOUNDEF "MUI_TEXT_ABORT_SUBTITLE" "MUI_TEXT_INSTALLING_SUBTITLE"
  !endif
  
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_INSTALLING_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_INSTALLING_SUBTITLE"
  
  !ifndef MUI_BUTTONTEXT_BACK
    !define MUI_BUTTONTEXT_BACK ""
  !endif
  !ifndef MUI_BUTTONTEXT_NEXT
    !define MUI_BUTTONTEXT_NEXT ""
  !endif
  !ifndef MUI_BUTTONTEXT_CANCEL
    !define MUI_BUTTONTEXT_CANCEL ""
  !endif
  
  !ifdef MUI_FINISHPAGE
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "MiscButtonText" "MUI_TEXT_FINISH_BUTTON" "$\${MUI_BUTTONTEXT_BACK}"$\" $\"${MUI_BUTTONTEXT_NEXT}$\" $\"${MUI_BUTTONTEXT_CANCEL}$\" $\"${MUI_TEXT_FINISH_BUTTON}$\""
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_INFO_TITLE"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_INFO_TEXT"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_INFO_REBOOT"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_REBOOTNOW"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_REBOOTLATER"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_RUN"
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_SHOWREADME"
  !else
    !ifndef MUI_BUTTONTEXT_CLOSE
      !define MUI_BUTTONTEXT_CLOSE ""
    !endif
    !insertmacro MUI_LANGUAGEFILE_NSISCOMMAND_MULTIPARAMETER "MiscButtonText" "MUI_TEXT_FINISH_BUTTON" "$\${MUI_BUTTONTEXT_BACK}"$\" $\"${MUI_BUTTONTEXT_NEXT}$\" $\"${MUI_BUTTONTEXT_CANCEL}$\" $\"${MUI_BUTTONTEXT_CLOSE}$\""
    !undef MUI_BUTTONTEXT_CLOSE
  !endif
  
  !undef MUI_BUTTONTEXT_BACK
  !undef MUI_BUTTONTEXT_NEXT
  !undef MUI_BUTTONTEXT_CANCEL
  
  !ifdef MUI_ABORTWARNING
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_ABORTWARNING"
  !endif
  
  
  !ifdef MUI_UNINSTALLER
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_INTRO_TITLE"
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_INTRO_SUBTITLE"
  
    !ifdef MUI_UNCONFIRMPAGE
      !insertmacro MUI_LANGUAGEFILE_UNNSISCOMMAND_CONTINUE "UninstallText" "MUI_UNINNERTEXT_INTRO" "MUI_UNINSTALLBUTTON_CONFIRM"
    !endif
     
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_FINISH_TITLE"
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_FINISH_SUBTITLE"
    
    !ifdef MUI_UNTEXT_ABORT_TITLE
       !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_ABORT_TITLE"
    !else
      ;1.63 compatibility
      !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_CUSTOMDEFINE_NOUNDEF "MUI_UNTEXT_ABORT_TITLE" "MUI_UNTEXT_UNINSTALLING_TITLE"
    !endif
    
    !ifdef MUI_UNTEXT_ABORT_SUBTITLE
      !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_ABORT_SUBTITLE"
    !else
      ;1.63 compatibility
      !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_CUSTOMDEFINE_NOUNDEF "MUI_UNTEXT_ABORT_SUBTITLE" "MUI_UNTEXT_UNINSTALLING_SUBTITLE"
    !endif
    
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_UNINSTALLING_TITLE"
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_UNINSTALLING_SUBTITLE"
  
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