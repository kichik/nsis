;NSIS Modern User Interface version 1.67
;Macro System
;Written by Joost Verburg

;Copyright © 2002-2003 Joost Verburg

;Documentation: Readme.html
;License: License.txt
;Examples: Examples\Modern UI

!echo "NSIS Modern User Interface version 1.67 - © 2002-2003 Joost Verburg"

;--------------------------------

!verbose 3

!ifndef MUI_INCLUDED

!define MUI_INCLUDED

;--------------------------------
;HEADER FILES, DECLARATIONS

!include "WinMessages.nsh"

Var MUI_TEMP1
Var MUI_TEMP2

;--------------------------------
;INSERT CODE

!macro MUI_INSERT
  
  !ifdef MUI_PRODUCT | MUI_VERSION
    !warning "The MUI_PRODUCT and MUI_VERSION defines have been removed. Use a normal Name command now."
  !endif
  
  !ifndef MUI_INSERT_INTERFACE
    !insertmacro MUI_INTERFACE
    !define MUI_INSERT_INTERFACE
  !endif
  
  !insertmacro MUI_FUNCTION_GUIINIT
  !insertmacro MUI_FUNCTION_ABORTWARNING
  
  !ifdef MUI_UNINSTALLER
    !insertmacro MUI_UNFUNCTION_GUIINIT
    !insertmacro MUI_FUNCTION_UNABORTWARNING
  !endif
  
!macroend

;--------------------------------
;INTERFACE - COMPILE TIME SETTINGS

!macro MUI_INTERFACE
  
  !ifdef MUI_INSERT_NSISCONF
    !insertmacro MUI_NSISCONF
  !endif
  
  !ifndef MUI_UI
    !define MUI_UI "${NSISDIR}\Contrib\UIs\modern.exe"
  !endif
  
  !ifndef MUI_UI_HEADERIMAGE
    !define MUI_UI_HEADERIMAGE "${NSISDIR}\Contrib\UIs\modern_headerbmp.exe"
  !endif

  !ifndef MUI_UI_HEADERIMAGE_RIGHT
    !define MUI_UI_HEADERIMAGE_RIGHT "${NSISDIR}\Contrib\UIs\modern_headerbmpr.exe"
  !endif
  
  !ifndef MUI_UI_COMPONENTSPAGE_SMALLDESC
    !define MUI_UI_COMPONENTSPAGE_SMALLDESC "${NSISDIR}\Contrib\UIs\modern_smalldesc.exe"
  !endif

  !ifndef MUI_UI_COMPONENTSPAGE_NODESC
    !define MUI_UI_COMPONENTSPAGE_NODESC "${NSISDIR}\Contrib\UIs\modern_nodesc.exe"
  !endif

  !ifndef MUI_ICON
    !define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install.ico"
  !endif

  !ifndef MUI_UNICON
    !define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall.ico"
  !endif

  !ifndef MUI_COMPONENTSPAGE_CHECKBITMAP
    !define MUI_COMPONENTSPAGE_CHECKBITMAP "${NSISDIR}\Contrib\Graphics\Checks\modern.bmp"
  !endif
  
  !ifndef MUI_LICENSEPAGE_BGCOLOR
    !define MUI_LICENSEPAGE_BGCOLOR "/windows"
  !endif
  
  !ifndef MUI_INSTFILESPAGE_COLORS
    !define MUI_INSTFILESPAGE_COLORS "/windows"
  !endif

  !ifndef MUI_INSTFILESPAGE_PROGRESSBAR
    !define MUI_INSTFILESPAGE_PROGRESSBAR "smooth"
  !endif

  !ifndef MUI_BGCOLOR
    !define MUI_BGCOLOR "FFFFFF"
  !endif

  !ifndef MUI_WELCOMEFINISHPAGE_INI
    !define MUI_WELCOMEFINISHPAGE_INI "${NSISDIR}\Contrib\Modern UI\ioSpecial.ini"
  !endif
  
  !ifndef MUI_WELCOMEFINISHPAGE_INI_3LINES
    !define MUI_WELCOMEFINISHPAGE_INI_3LINES "${NSISDIR}\Contrib\Modern UI\ioSpecial3.ini"
  !endif
  
  !ifndef MUI_UNWELCOMEFINISHPAGE_INI
    !define MUI_UNWELCOMEFINISHPAGE_INI "${NSISDIR}\Contrib\Modern UI\ioSpecial.ini"
  !endif
  
  !ifndef MUI_UNWELCOMEFINISHPAGE_INI_3LINES
    !define MUI_UNWELCOMEFINISHPAGE_INI_3LINES "${NSISDIR}\Contrib\Modern UI\ioSpecial3.ini"
  !endif
  
  !ifndef MUI_WELCOMEFINISHPAGE_BITMAP
    !define MUI_WELCOMEFINISHPAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Wizard\win.bmp"
  !endif
  
  !ifndef MUI_UNWELCOMEFINISHPAGE_BITMAP
    !define MUI_UNWELCOMEFINISHPAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Wizard\win.bmp"
  !endif

  !ifdef MUI_HEADERIMAGE
    !ifndef MUI_HEADERIMAGE_BITMAP
      !define MUI_HEADERIMAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Header\nsis.bmp"
    !endif
    !ifndef MUI_HEADERIMAGE_UNBITMAP
      !define MUI_HEADERIMAGE_UNBITMAP "${MUI_HEADERIMAGE_BITMAP}"
      !ifdef MUI_HEADERIMAGE_NOSTRETCH
        !ifndef MUI_HEADERIMAGE_UNNOSTRETCH
          !define MUI_HEADERIMAGE_UNNOSTRETCH
        !endif
      !endif
    !endif
  !endif

  XPStyle On
  
  ChangeUI all "${MUI_UI}"
  !ifdef MUI_HEADERIMAGE
    !ifndef MUI_HEADERIMAGE_RIGHT
      ChangeUI IDD_INST "${MUI_UI_HEADERIMAGE}"
    !else
      ChangeUI IDD_INST "${MUI_UI_HEADERIMAGE_RIGHT}"
    !endif
  !endif
  !ifdef MUI_COMPONENTSPAGE_SMALLDESC
    ChangeUI IDD_SELCOM "${MUI_UI_COMPONENTSPAGE_SMALLDESC}"
  !else ifdef MUI_COMPONENTSPAGE_NODESC
     ChangeUI IDD_SELCOM "${MUI_UI_COMPONENTSPAGE_NODESC}"
  !endif
  
  Icon "${MUI_ICON}"
  UninstallIcon "${MUI_UNICON}"
  
  CheckBitmap "${MUI_COMPONENTSPAGE_CHECKBITMAP}"
  LicenseBkColor "${MUI_LICENSEPAGE_BGCOLOR}"
  InstallColors ${MUI_INSTFILESPAGE_COLORS}
  InstProgressFlags ${MUI_INSTFILESPAGE_PROGRESSBAR}
  
!macroend

;--------------------------------
;INTERFACE - RUN-TIME

!macro MUI_INNERDIALOG_TEXT CONTROL TEXT

  !verbose push
  !verbose 3

  FindWindow $MUI_TEMP1 "#32770" "" $HWNDPARENT
  GetDlgItem $MUI_TEMP1 $MUI_TEMP1 ${CONTROL}
  SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:${TEXT}"

  !verbose pop

!macroend

!macro MUI_HEADER_TEXT TEXT SUBTEXT

  !verbose push
  !verbose 3

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1037
  SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:${TEXT}"
  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1038
  SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:${SUBTEXT}"

  !verbose pop

!macroend

!macro MUI_HEADER_TEXT_PAGE TEXT SUBTEXT

  !ifdef MUI_PAGE_HEADER_TEXT & MUI_PAGE_HEADER_SUBTEXT
    !insertmacro MUI_HEADER_TEXT "${MUI_PAGE_HEADER_TEXT}" "${MUI_PAGE_HEADER_SUBTEXT}"
    !undef MUI_PAGE_HEADER_TEXT
    !undef MUI_PAGE_HEADER_SUBTEXT
  !else ifdef MUI_PAGE_HEADER_TEXT
    !insertmacro MUI_HEADER_TEXT "${MUI_PAGE_HEADER_TEXT}" "${SUBTEXT}"
    !undef MUI_PAGE_HEADER_TEXT
  !else ifdef MUI_PAGE_HEADER_SUBTEXT
    !insertmacro MUI_HEADER_TEXT "${TEXT}" "${MUI_PAGE_HEADER_SUBTEXT}"
    !undef MUI_PAGE_HEADER_SUBTEXT
  !else
    !insertmacro MUI_HEADER_TEXT "${TEXT}" "${SUBTEXT}"
  !endif

!macroend

!macro MUI_DESCRIPTION_BEGIN

  FindWindow $MUI_TEMP1 "#32770" "" $HWNDPARENT
  GetDlgItem $MUI_TEMP1 $MUI_TEMP1 1043

  StrCmp $0 -1 0 mui.description_begin_done
    SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:"
    EnableWindow $MUI_TEMP1 0
    !insertmacro MUI_INNERDIALOG_TEXT 1043 $MUI_TEXT
    Goto mui.description_done
  mui.description_begin_done:

!macroend

!macro MUI_DESCRIPTION_TEXT VAR TEXT

  !verbose push
  !verbose 3

  StrCmp $0 ${VAR} 0 mui.description_${VAR}_done
    SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:"
    EnableWindow $MUI_TEMP1 1
    SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:${TEXT}"
    Goto mui.description_done
  mui.description_${VAR}_done:

  !verbose pop

!macroend

!macro MUI_DESCRIPTION_END

  !verbose push
  !verbose 3

  mui.description_done:

  !verbose pop

!macroend

!macro MUI_FINISHHEADER

  IfAbort mui.finishheader_abort
  
    !ifdef MUI_INSTFILESPAGE_FINISHHEADER_TEXT
      !ifdef MUI_INSTFILESPAGE_FINISHHEADER_SUBTEXT
        !insertmacro MUI_HEADER_TEXT "${MUI_INSTFILESPAGE_FINISHHEADER_TEXT}" "${MUI_INSTFILESPAGE_FINISHHEADER_SUBTEXT}"
        !undef MUI_INSTFILESPAGE_FINISHHEADER_TEXT
        !undef MUI_INSTFILESPAGE_FINISHHEADER_SUBTEXT
      !else
        !insertmacro MUI_HEADER_TEXT "${MUI_INSTFILESPAGE_FINISHHEADER_TEXT}" "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_FINISH_SUBTITLE)"
        !undef MUI_INSTFILESPAGE_FINISHHEADER_TEXT
      !endif
    !else
      !insertmacro MUI_HEADER_TEXT "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_FINISH_TITLE)" "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_FINISH_SUBTITLE)"
    !endif
  
  Goto mui.finishheader_done
  
  mui.finishheader_abort:
  
  !ifdef MUI_INSTFILESPAGE_ABORTHEADER_TEXT
    !ifdef MUI_INSTFILESPAGE_ABORTHEADER_SUBTEXT
      !insertmacro MUI_HEADER_TEXT "${MUI_INSTFILESPAGE_ABORTHEADER_TEXT}" "${MUI_INSTFILESPAGE_ABORTHEADER_SUBTEXT}"
    !else
      !insertmacro MUI_HEADER_TEXT "${MUI_INSTFILESPAGE_ABORTHEADER_TEXT}" "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_FINISH_SUBTITLE)"
    !endif
  !else
      !insertmacro MUI_HEADER_TEXT "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_ABORT_TITLE)" "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_FINISH_SUBTITLE)"
  !endif
  
  mui.finishheader_done:
  
!macroend

!macro MUI_ABORTWARNING

  !ifdef MUI_FINISHPAGE_ABORTWARNINGCHECK
    StrCmp $MUI_NOABORTWARNING "1" mui.quit
  !endif

  !ifdef MUI_ABORTWARNING_TEXT
    MessageBox MB_YESNO|MB_ICONEXCLAMATION "${MUI_ABORTWARNING_TEXT}" IDYES mui.quit
  !else
    MessageBox MB_YESNO|MB_ICONEXCLAMATION "$(MUI_TEXT_ABORTWARNING)" IDYES mui.quit
  !endif
    
  Abort
  mui.quit:

!macroend

!macro MUI_UNABORTWARNING

  !ifdef MUI_UNABORTWARNING_TEXT
    MessageBox MB_YESNO|MB_ICONEXCLAMATION "${MUI_UNABORTWARNING_TEXT}" IDYES mui.quit
  !else
    MessageBox MB_YESNO|MB_ICONEXCLAMATION "$(MUI_UNTEXT_ABORTWARNING)" IDYES mui.quit
  !endif
    
  Abort
  mui.quit:

!macroend

!macro MUI_GUIINIT
  
  !insertmacro MUI_WELCOMEFINISHPAGE_INIT ""
  !insertmacro MUI_HEADERIMAGE_INIT ""

  !insertmacro MUI_GUIINIT_BASIC
  
!macroend

!macro MUI_UNGUIINIT

  !insertmacro MUI_WELCOMEFINISHPAGE_INIT "UN"
  !insertmacro MUI_HEADERIMAGE_INIT "UN"

  !insertmacro MUI_GUIINIT_BASIC
  
  !ifndef MUI_UNFINISHPAGE_NOAUTOCLOSE
    SetAutoClose true
  !endif
  
!macroend

!macro MUI_GUIINIT_BASIC

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1037
  CreateFont $MUI_TEMP2 "$(MUI_FONT)" "$(MUI_FONTSIZE)" "700"
  SendMessage $MUI_TEMP1 ${WM_SETFONT} $MUI_TEMP2 0
  SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1038
  SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1034
  SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"

  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1039
  SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
  
  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
  SetCtlColors $MUI_TEMP1 /BRANDING
  GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
  SetCtlColors $MUI_TEMP1 /BRANDING
  SendMessage $MUI_TEMP1 ${WM_SETTEXT} 0 "STR:$(^Branding) "

!macroend

!macro MUI_WELCOMEFINISHPAGE_INIT UNINSTALLER

  !ifdef MUI_${UNINSTALLER}WELCOMEPAGE | MUI_${UNINSTALLER}FINISHPAGE
  
    !ifndef MUI_WELCOMEFINISHPAGE_3LINES
      !insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS "${MUI_${UNINSTALLER}WELCOMEFINISHPAGE_INI}" "ioSpecial.ini"
    !else
      !insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS "${MUI_${UNINSTALLER}WELCOMEFINISHPAGE_INI_3LINES}" "ioSpecial.ini"
    !endif
    File "/oname=$PLUGINSDIR\modern-wizard.bmp" "${MUI_${UNINSTALLER}WELCOMEFINISHPAGE_BITMAP}"
    
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 1" "Text" "$PLUGINSDIR\modern-wizard.bmp"
    
    !ifdef MUI_${UNINSTALLER}WELCOMEFINISHPAGE_BITMAP_NOSTRETCH
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 1" "Flags" ""
    !endif
    
  !endif

!macroend

!macro MUI_HEADERIMAGE_INIT UNINSTALLER

  !ifdef MUI_HEADERIMAGE
  
    InitPluginsDir
    File "/oname=$PLUGINSDIR\modern-header.bmp" "${MUI_HEADERIMAGE_${UNINSTALLER}BITMAP}"
    
    !ifndef MUI_HEADERIMAGE_${UNINSTALLER}NOSTRETCH
      SetBrandingImage /IMGID=1046 /RESIZETOFIT "$PLUGINSDIR\modern-header.bmp"
    !else
      SetBrandingImage /IMGID=1046 "$PLUGINSDIR\modern-header.bmp"
    !endif
    
  !endif

!macroend

;--------------------------------
;INTERFACE - FUNCTIONS

!macro MUI_FUNCTION_GUIINIT

  Function .onGUIInit
     
    !insertmacro MUI_GUIINIT
    
    !ifdef MUI_CUSTOMFUNCTION_GUIINIT
      Call "${MUI_CUSTOMFUNCTION_GUIINIT}"
    !endif

  FunctionEnd

!macroend

!macro MUI_FUNCTION_DESCRIPTION_BEGIN

  !verbose push
  !verbose 3
  
  !ifndef MUI_VAR_TEXT
    Var MUI_TEXT
    !define MUI_VAR_TEXT
  !endif

  Function .onMouseOverSection
    !insertmacro MUI_DESCRIPTION_BEGIN
  
  !verbose pop
  
!macroend

!macro MUI_FUNCTION_DESCRIPTION_END

  !verbose push
  !verbose 3

    !insertmacro MUI_DESCRIPTION_END
  FunctionEnd

  !verbose pop
  
!macroend

!macro MUI_UNFUNCTION_DESCRIPTION_BEGIN

  !verbose push
  !verbose 3

  Function un.onMouseOverSection
    !insertmacro MUI_DESCRIPTION_BEGIN
  
  !verbose pop
  
!macroend

!macro MUI_UNFUNCTION_DESCRIPTION_END

  !verbose push
  !verbose 3

    !insertmacro MUI_DESCRIPTION_END
  FunctionEnd

  !verbose pop
  
!macroend

!macro MUI_FUNCTION_ABORTWARNING

  Function .onUserAbort
    !ifdef MUI_ABORTWARNING
      !insertmacro MUI_ABORTWARNING
    !endif
    !ifdef MUI_CUSTOMFUNCTION_ABORT
      Call "${MUI_CUSTOMFUNCTION_ABORT}"
    !endif
  FunctionEnd

!macroend

!macro MUI_FUNCTION_UNABORTWARNING

  Function un.onUserAbort
    !ifdef MUI_UNABORTWARNING
      !insertmacro MUI_UNABORTWARNING
    !endif
    !ifdef MUI_CUSTOMFUNCTION_UNABORT
      Call "${MUI_CUSTOMFUNCTION_UNABORT}"
    !endif
  FunctionEnd

!macroend

!macro MUI_UNFUNCTION_GUIINIT
  
  Function un.onGUIInit
  
  !insertmacro MUI_UNGUIINIT
  
  !ifdef MUI_CUSTOMFUNCTION_UNGUIINIT
    Call "${MUI_CUSTOMFUNCTION_UNGUIINIT}"
  !endif
  
  FunctionEnd

!macroend

!macro MUI_FUNCTIONS_DESCRIPTION_BEGIN

  ;1.65 compatibility

  !warning "Modern UI macro name has changed. Please change MUI_FUNCTIONS_DESCRIPTION_BEGIN to MUI_FUNCTION_DESCRIPTION_BEGIN."

  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  
!macroend

!macro MUI_FUNCTIONS_DESCRIPTION_END

  ;1.65 compatibility

  !warning "Modern UI macro name has changed. Please change MUI_FUNCTIONS_DESCRIPTION_END to MUI_FUNCTION_DESCRIPTION_END."

  !insertmacro MUI_FUNCTION_DESCRIPTION_END
  
!macroend

;--------------------------------
;START MENU FOLDER

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

  !verbose push
  !verbose 3
  
  StrCpy $MUI_TEMP1 "${MUI_STARTMENUPAGE_VARIABLE}" 1
  StrCmp $MUI_TEMP1 ">" mui.startmenu_write_done
    
  !insertmacro MUI_STARTMENU_GETFOLDER_IFEMPTY "${MUI_STARTMENUPAGE_VARIABLE}"
  
  !verbose pop

!macroend

!macro MUI_STARTMENU_WRITE_END

  !verbose push
  !verbose 3
  
  !ifdef MUI_STARTMENUPAGE_REGISTRY_ROOT & MUI_STARTMENUPAGE_REGISTRY_KEY & MUI_STARTMENUPAGE_REGISTRY_VALUENAME
    WriteRegStr "${MUI_STARTMENUPAGE_REGISTRY_ROOT}" "${MUI_STARTMENUPAGE_REGISTRY_KEY}" "${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}" "${MUI_STARTMENUPAGE_VARIABLE}"
  !endif

  mui.startmenu_write_done:
  
  !verbose pop

!macroend

;--------------------------------
;PAGES

!macro MUI_PAGE_INIT

  !ifndef MUI_INSERT_INTERFACE
    !insertmacro MUI_INTERFACE
    !define MUI_INSERT_INTERFACE
  !endif
  
  !ifndef MUI_PAGE_UNINSTALLER
    !define MUI_PAGE_UNINSTALLER ""
  !endif
  
  !ifndef MUI_PAGE_UNINSTALLER_PREFIX
    !define MUI_PAGE_UNINSTALLER_PREFIX ""
  !endif
  
  !ifdef MUI_UNIQUEID
    !undef MUI_UNIQUEID
  !endif
  
  !define MUI_UNIQUEID ${__LINE__}

!macroend

!macro MUI_UNPAGE_INIT

  !ifndef MUI_UNINSTALLER
    !define MUI_UNINSTALLER
  !endif

  !ifdef MUI_PAGE_UNINSTALLER
    !undef MUI_PAGE_UNINSTALLER
  !endif
  
  !ifdef MUI_PAGE_UNINSTALLER_PREFIX
    !undef MUI_PAGE_UNINSTALLER_PREFIX
  !endif
  
  !define MUI_PAGE_UNINSTALLER "UN"
  !define MUI_PAGE_UNINSTALLER_PREFIX "un."

!macroend

!macro MUI_UNPAGE_END

  !undef MUI_PAGE_UNINSTALLER
  !undef MUI_PAGE_UNINSTALLER_PREFIX

!macroend

!macro MUI_PAGE_WELCOME

  !verbose push
  !verbose 3

  !insertmacro MUI_PAGE_INIT

  !ifndef MUI_${MUI_PAGE_UNINSTALLER}WELCOMEPAGE
    !define MUI_${MUI_PAGE_UNINSTALLER}WELCOMEPAGE
  !endif
  
  PageEx ${MUI_PAGE_UNINSTALLER_PREFIX}custom
  
    PageCallbacks ${MUI_PAGE_UNINSTALLER_PREFIX}mui.WelcomePre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.WelcomeLeave_${MUI_UNIQUEID}
    
  PageExEnd
  
  !insertmacro MUI_FUNCTION_WELCOMEPAGE ${MUI_PAGE_UNINSTALLER_PREFIX}mui.WelcomePre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.WelcomeLeave_${MUI_UNIQUEID}

  !verbose pop
  
!macroend

!macro MUI_PAGE_LICENSE LICENSEDATA

  !verbose push
  !verbose 3

  !insertmacro MUI_PAGE_INIT

  !ifndef MUI_${MUI_PAGE_UNINSTALLER}LICENSEPAGE
    !define MUI_${MUI_PAGE_UNINSTALLER}LICENSEPAGE
  !endif
  
  PageEx ${MUI_PAGE_UNINSTALLER_PREFIX}license
  
    PageCallbacks ${MUI_PAGE_UNINSTALLER_PREFIX}mui.LicensePre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.LicenseShow_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.LicenseLeave_${MUI_UNIQUEID}
    
    Caption " "
    
    LicenseData "${LICENSEDATA}"
    
    !ifndef MUI_LICENSEPAGE_TEXT
      !ifndef MUI_LICENSEPAGE_CHECKBOX & MUI_LICENSEPAGE_RADIOBUTTONS
          LicenseText "$(MUI_${MUI_PAGE_UNINSTALLER}INNERTEXT_LICENSE_BOTTOM)"
      !else ifdef MUI_LICENSEPAGE_CHECKBOX
        LicenseText "$(MUI_${MUI_PAGE_UNINSTALLER}INNERTEXT_LICENSE_BOTTOM_CHECKBOX)"
      !else
        LicenseText "$(MUI_${MUI_PAGE_UNINSTALLER}INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS)"
      !endif
    !else
      LicenseText ${MUI_LICENSEPAGE_TEXT}
      !undef MUI_LICENSEPAGE_TEXT
    !endif
    
    !ifdef MUI_LICENSEPAGE_CHECKBOX
      !ifdef MUI_LICENSEPAGE_CHECKBOX_TEXT
        LicenseForceSelection checkbox "${MUI_LICENSEPAGE_CHECKBOX_TEXT}"
        !undef MUI_LICENSEPAGE_CHECKBOX_TEXT
      !else
        LicenseForceSelection checkbox
      !endif
      !undef MUI_LICENSEPAGE_CHECKBOX
    !else ifdef MUI_LICENSEPAGE_RADIOBUTTONS
      !ifdef MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_ACCEPT
        !ifdef MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_DECLINE
          LicenseForceSelection radiobuttons "${MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_ACCEPT}" "${MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_DECLINE}"
          !undef MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_ACCEPT
          !undef MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_DECLINE
        !else
          LicenseForceSelection radiobuttons "${MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_ACCEPT}"
          !undef MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_ACCEPT
        !endif
      !else ifdef MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_DECLINE
        LicenseForceSelection radiobuttons "" "${MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_DECLINE}"
        !undef MUI_LICENSEPAGE_RADIOBUTTONS_TEXT_DECLINE
      !else
        LicenseForceSelection radiobuttons
      !endif
      !undef MUI_LICENSEPAGE_RADIOBUTTONS
    !endif
    
  PageExEnd
  
  !insertmacro MUI_FUNCTION_LICENSEPAGE ${MUI_PAGE_UNINSTALLER_PREFIX}mui.LicensePre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.LicenseShow_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.LicenseLeave_${MUI_UNIQUEID}
  
  !verbose pop
  
!macroend

!macro MUI_PAGE_COMPONENTS

  !verbose push
  !verbose 3

  !insertmacro MUI_PAGE_INIT

  !ifndef MUI_${MUI_PAGE_UNINSTALLER}COMPONENTSPAGE
    !define MUI_${MUI_PAGE_UNINSTALLER}COMPONENTSPAGE
  !endif
  
  PageEx ${MUI_PAGE_UNINSTALLER_PREFIX}components
  
    PageCallbacks ${MUI_PAGE_UNINSTALLER_PREFIX}mui.ComponentsPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.ComponentsShow_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.ComponentsLeave_${MUI_UNIQUEID}
    
    Caption " "
    
    !ifdef MUI_COMPONENTSPAGE_TEXT
      ComponentText ${MUI_COMPONENTSPAGE_TEXT}
      !undef MUI_COMPONENTSPAGE_TEXT
    !endif
  
  PageExEnd
  
  !insertmacro MUI_FUNCTION_COMPONENTSPAGE ${MUI_PAGE_UNINSTALLER_PREFIX}mui.ComponentsPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.ComponentsShow_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.ComponentsLeave_${MUI_UNIQUEID}
  
  !verbose pop
  
!macroend

!macro MUI_PAGE_DIRECTORY

  !verbose push
  !verbose 3

  !insertmacro MUI_PAGE_INIT

  !ifndef MUI_${MUI_PAGE_UNINSTALLER}DIRECTORYPAGE
    !define MUI_${MUI_PAGE_UNINSTALLER}DIRECTORYPAGE
  !endif
  
  PageEx ${MUI_PAGE_UNINSTALLER_PREFIX}directory
  
    PageCallbacks ${MUI_PAGE_UNINSTALLER_PREFIX}mui.DirectoryPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.DirectoryShow_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.DirectoryLeave_${MUI_UNIQUEID}
    
    Caption " "
    
    !ifdef MUI_DIRECTORYPAGE_TEXT
      DirText ${MUI_DIRECTORYPAGE_TEXT}
      !undef MUI_DIRECTORYPAGE_TEXT
    !endif
    
    !ifdef MUI_DIRECTORYPAGE_VARIABLE
      DirVar "${MUI_DIRECTORYPAGE_VARIABLE}"
      !undef "MUI_DIRECTORYPAGE_VARIABLE"
    !endif
    
  PageExEnd
  
  !insertmacro MUI_FUNCTION_DIRECTORYPAGE ${MUI_PAGE_UNINSTALLER_PREFIX}mui.DirectoryPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.DirectoryShow_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.DirectoryLeave_${MUI_UNIQUEID}
  
  !verbose pop
  
!macroend

!macro MUI_PAGE_STARTMENU

  !verbose push
  !verbose 3

  !insertmacro MUI_PAGE_INIT

  !ifndef MUI_${MUI_PAGE_UNINSTALLER}STARTMENUPAGE
    !define MUI_${MUI_PAGE_UNINSTALLER}STARTMENUPAGE
  !endif
  
  PageEx ${MUI_PAGE_UNINSTALLER_PREFIX}custom
  
    PageCallbacks ${MUI_PAGE_UNINSTALLER_PREFIX}mui.StartmenuPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.StartmenuLeave_${MUI_UNIQUEID}
    
    Caption " "
    
  PageExEnd
  
  !insertmacro MUI_FUNCTION_STARTMENUPAGE ${MUI_PAGE_UNINSTALLER_PREFIX}mui.StartmenuPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.StartmenuLeave_${MUI_UNIQUEID}
  
  !verbose pop
  
!macroend

!macro MUI_PAGE_INSTFILES

  !verbose push
  !verbose 3
  
  !insertmacro MUI_PAGE_INIT
  
  PageEx ${MUI_PAGE_UNINSTALLER_PREFIX}instfiles
  
    PageCallbacks ${MUI_PAGE_UNINSTALLER_PREFIX}mui.InstFilesPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.InstFilesShow_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.InstFilesLeave_${MUI_UNIQUEID}
    
    Caption " "
    
  PageExEnd
  
  !insertmacro MUI_FUNCTION_INSTFILESPAGE ${MUI_PAGE_UNINSTALLER_PREFIX}mui.InstFilesPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.InstFilesShow_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.InstFilesLeave_${MUI_UNIQUEID}
   
  !verbose pop
   
!macroend

!macro MUI_PAGE_FINISH

  !verbose push
  !verbose 3
  
  !insertmacro MUI_PAGE_INIT
  
  !ifndef MUI_${MUI_PAGE_UNINSTALLER}FINISHPAGE
    !define MUI_${MUI_PAGE_UNINSTALLER}FINISHPAGE
  !endif
  
  PageEx ${MUI_PAGE_UNINSTALLER_PREFIX}custom
  
    PageCallbacks ${MUI_PAGE_UNINSTALLER_PREFIX}mui.FinishPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.FinishLeave_${MUI_UNIQUEID}
    
    Caption " "
    
  PageExEnd
  
  !insertmacro MUI_FUNCTION_FINISHPAGE ${MUI_PAGE_UNINSTALLER_PREFIX}mui.FinishPre_${MUI_UNIQUEID} ${MUI_PAGE_UNINSTALLER_PREFIX}mui.FinishLeave_${MUI_UNIQUEID}

  !verbose pop
  
!macroend

!macro MUI_UNPAGE_WELCOME

  !verbose push
  !verbose 3

  !insertmacro MUI_UNPAGE_INIT
  
    !insertmacro MUI_PAGE_WELCOME
  
  !insertmacro MUI_UNPAGE_END
  
  !verbose pop
  
!macroend

!macro MUI_UNPAGE_CONFIRM

  !verbose push
  !verbose 3

  !ifndef MUI_UNINSTALLER
    !define MUI_UNINSTALLER
  !endif

  !ifndef MUI_UNCONFIRMPAGE
    !define MUI_UNCONFIRMPAGE
  !endif
  
  !insertmacro MUI_PAGE_INIT
  
  PageEx un.uninstConfirm
  
    PageCallbacks un.mui.ConfirmPre_${MUI_UNIQUEID} un.mui.ConfirmShow_${MUI_UNIQUEID} un.mui.ConfirmLeave_${MUI_UNIQUEID}
    
    Caption " "
    
    !ifdef MUI_UNCONFIRMPAGE_TEXT
      UninstallText ${MUI_UNCONFIRMPAGE_TEXT}
      !undef MUI_UNCONFIRMPAGE_TEXT
    !endif
    
  PageExEnd
  
  !insertmacro MUI_UNFUNCTION_CONFIRMPAGE un.mui.ConfirmPre_${MUI_UNIQUEID} un.mui.ConfirmShow_${MUI_UNIQUEID} un.mui.ConfirmLeave_${MUI_UNIQUEID}
  
  !verbose pop
   
!macroend

!macro MUI_UNPAGE_LICENSE LICENSEDATA

  !verbose push
  !verbose 3

  !insertmacro MUI_UNPAGE_INIT
  
    !insertmacro MUI_PAGE_LICENSE "${LICENSEDATA}"
  
  !insertmacro MUI_UNPAGE_END
  
  !verbose pop
  
!macroend

!macro MUI_UNPAGE_COMPONENTS

  !verbose push
  !verbose 3

  !insertmacro MUI_UNPAGE_INIT
  
    !insertmacro MUI_PAGE_COMPONENTS
  
  !insertmacro MUI_UNPAGE_END
  
  !verbose pop
  
!macroend

!macro MUI_UNPAGE_DIRECTORY

  !verbose push
  !verbose 3

  !insertmacro MUI_UNPAGE_INIT
  
    !insertmacro MUI_PAGE_DIRECTORY
  
  !insertmacro MUI_UNPAGE_END
  
  !verbose pop
  
!macroend

!macro MUI_UNPAGE_INSTFILES

  !verbose push
  !verbose 3

  !insertmacro MUI_UNPAGE_INIT
  
    !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_END
  
  !verbose pop
  
!macroend

!macro MUI_UNPAGE_FINISH

  !verbose push
  !verbose 3

  !insertmacro MUI_UNPAGE_INIT
  
    !insertmacro MUI_PAGE_FINISH
  
  !insertmacro MUI_UNPAGE_END
  
  !verbose pop
  
!macroend

;--------------------------------
;PAGE FUNCTIONS

!macro MUI_PAGE_FUNCTION_CUSTOM TYPE

  !ifdef MUI_PAGE_CUSTOMFUNCTION_${TYPE}
    Call "${MUI_PAGE_CUSTOMFUNCTION_${TYPE}}"
    !undef MUI_PAGE_CUSTOMFUNCTION_${TYPE}
  !endif
  
!macroend

!macro MUI_FUNCTION_WELCOMEPAGE PRE LEAVE

  !ifndef MUI_VAR_HWND
    Var MUI_HWND
    !define MUI_VAR_HWND
  !endif

  Function "${PRE}"
  
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "NextButtonText" ""
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "CancelEnabled" ""
  
    !ifndef MUI_WELCOMEPAGE_TITLE
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 2" "Text" "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_WELCOME_INFO_TITLE)"
    !else
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 2" "Text" "${MUI_WELCOMEPAGE_TITLE}"
      !undef MUI_WELCOMEPAGE_TITLE
    !endif
    
    !ifndef MUI_WELCOMEPAGE_TEXT
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_WELCOME_INFO_TEXT)"
    !else
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "${MUI_WELCOMEPAGE_TEXT}"
      !undef MUI_WELCOMEPAGE_TEXT
    !endif

    !insertmacro MUI_PAGE_FUNCTION_CUSTOM PRE
    
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1035
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1037
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1038
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1045
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ioSpecial.ini"
    Pop $MUI_HWND
    SetCtlColors $MUI_HWND "" "${MUI_BGCOLOR}"
      
    GetDlgItem $MUI_TEMP1 $MUI_HWND 1201
    SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
    
    CreateFont $MUI_TEMP2 "$(MUI_FONT)" "12" "700"
    SendMessage $MUI_TEMP1 ${WM_SETFONT} $MUI_TEMP2 0
        
    GetDlgItem $MUI_TEMP1 $MUI_HWND 1202
    SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
    
    GetDlgItem $MUI_TEMP1 $MUI_HWND 1200
    SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"

    !insertmacro MUI_PAGE_FUNCTION_CUSTOM SHOW
  
    !insertmacro MUI_INSTALLOPTIONS_SHOW
     
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1035
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1037
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1038
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1045
    ShowWindow $MUI_TEMP1 ${SW_HIDE}
    
  FunctionEnd
  
  Function "${LEAVE}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM LEAVE
  
  FunctionEnd
  
!macroend

!macro MUI_FUNCTION_LICENSEPAGE PRE SHOW LEAVE

  Function "${PRE}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM PRE
    !insertmacro MUI_HEADER_TEXT_PAGE $(MUI_${MUI_PAGE_UNINSTALLER}TEXT_LICENSE_TITLE) $(MUI_${MUI_PAGE_UNINSTALLER}TEXT_LICENSE_SUBTITLE)
    
  FunctionEnd

  Function "${SHOW}"
  
    !ifndef MUI_LICENSEPAGE_TEXT_TOP
      !insertmacro MUI_INNERDIALOG_TEXT 1040 $(MUI_INNERTEXT_LICENSE_TOP)
    !else
      !insertmacro MUI_INNERDIALOG_TEXT 1040 "${MUI_LICENSEPAGE_TEXT_TOP}"
      !undef MUI_LICENSEPAGE_TEXT_TOP
    !endif
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM SHOW
    
  FunctionEnd
  
  Function "${LEAVE}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM LEAVE
    
  FunctionEnd

!macroend

!macro MUI_FUNCTION_COMPONENTSPAGE PRE SHOW LEAVE

  !ifndef MUI_VAR_TEXT
    Var MUI_TEXT
    !define MUI_VAR_TEXT
  !endif

  Function "${PRE}"
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM PRE
    !insertmacro MUI_HEADER_TEXT_PAGE $(MUI_${MUI_PAGE_UNINSTALLER}TEXT_COMPONENTS_TITLE) $(MUI_${MUI_PAGE_UNINSTALLER}TEXT_COMPONENTS_SUBTITLE)
  FunctionEnd

  Function "${SHOW}"
  
    !ifdef MUI_COMPONENTSPAGE_TEXT_DESCRIPTION_TITLE
      !insertmacro MUI_INNERDIALOG_TEXT 1042 "${MUI_COMPONENTSPAGE_TEXT_DESCRIPTION_TITLE}"
      !undef MUI_COMPONENTSPAGE_TEXT_DESCRIPTION_TITLE
    !else
      !insertmacro MUI_INNERDIALOG_TEXT 1042 "$(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE)"
    !endif
    
    FindWindow $MUI_TEMP1 "#32770" "" $HWNDPARENT
    GetDlgItem $MUI_TEMP1 $MUI_TEMP1 1043
    EnableWindow $MUI_TEMP1 0
    
    !ifdef MUI_COMPONENTSPAGE_TEXT_DESCRIPTION_INFO
      !insertmacro MUI_INNERDIALOG_TEXT 1043 "${MUI_COMPONENTSPAGE_TEXT_DESCRIPTION_INFO}"
      StrCpy $MUI_TEXT "${MUI_COMPONENTSPAGE_TEXT_DESCRIPTION_INFO}"
      !undef MUI_COMPONENTSPAGE_TEXT_DESCRIPTION_INFO
    !else
      !insertmacro MUI_INNERDIALOG_TEXT 1043 "$(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO)"
      StrCpy $MUI_TEXT "$(MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO)"
    !endif
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM SHOW
   
  FunctionEnd

  Function "${LEAVE}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM LEAVE
    
  FunctionEnd
    
!macroend

!macro MUI_FUNCTION_DIRECTORYPAGE PRE SHOW LEAVE

  Function "${PRE}"
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM PRE
    !insertmacro MUI_HEADER_TEXT_PAGE $(MUI_${MUI_PAGE_UNINSTALLER}TEXT_DIRECTORY_TITLE) $(MUI_${MUI_PAGE_UNINSTALLER}TEXT_DIRECTORY_SUBTITLE)
  FunctionEnd

  Function "${SHOW}"
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM SHOW
  FunctionEnd
  
  Function "${LEAVE}"
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM LEAVE
  FunctionEnd

!macroend

!macro MUI_FUNCTION_STARTMENUPAGE PRE LEAVE
  
  !ifndef MUI_STARTMENUPAGE_VARIABLE
    Var MUI_STARTMENU_FOLDER
    !define MUI_STARTMENUPAGE_VARIABLE "$MUI_STARTMENU_FOLDER"
  !endif
  
  !ifndef MUI_STARTMENUPAGE_DEFAULTFOLDER
    !define MUI_STARTMENUPAGE_DEFAULTFOLDER "$(^Name)"
  !endif
  
  Function "${PRE}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM PRE

     !ifdef MUI_STARTMENUPAGE_REGISTRY_ROOT & MUI_STARTMENUPAGE_REGISTRY_KEY & MUI_STARTMENUPAGE_REGISTRY_VALUENAME

      StrCmp "${MUI_STARTMENUPAGE_VARIABLE}" "" 0 +4

      ReadRegStr $MUI_TEMP1 "${MUI_STARTMENUPAGE_REGISTRY_ROOT}" "${MUI_STARTMENUPAGE_REGISTRY_KEY}" "${MUI_STARTMENUPAGE_REGISTRY_VALUENAME}"
        StrCmp $MUI_TEMP1 "" +2
          StrCpy "${MUI_STARTMENUPAGE_VARIABLE}" $MUI_TEMP1
    
    !endif
  
    !insertmacro MUI_HEADER_TEXT_PAGE $(MUI_TEXT_STARTMENU_TITLE) $(MUI_TEXT_STARTMENU_SUBTITLE)
    
    !ifdef MUI_TEMP_STARTMENUPAGE_TEXT
      !undef MUI_TEMP_STARTMENUPAGE_TEXT
    !endif
    
    !ifdef MUI_STARTMENUPAGE_TEXT
      !define MUI_TEMP_STARTMENUPAGE_TEXT "${MUI_STARTMENUPAGE_TEXT}"
      !undef MUI_STARTMENUPAGE_TEXT
    !else
      !define MUI_TEMP_STARTMENUPAGE_TEXT "$(MUI_INNERTEXT_STARTMENU_TOP)"
    !endif
    
    !ifdef MUI_TEMP_STARTMENUPAGE_TEXT_CHECKBOX
      !undef MUI_TEMP_STARTMENUPAGE_TEXT_CHECKBOX
    !endif
    
    !ifdef MUI_STARTMENUPAGE_TEXT_CHECKBOX
      !define MUI_TEMP_STARTMENUPAGE_TEXT_CHECKBOX "${MUI_STARTMENUPAGE_TEXT_CHECKBOX}"
      !undef MUI_STARTMENUPAGE_TEXT_CHECKBOX
    !else
      !define MUI_TEMP_STARTMENUPAGE_TEXT_CHECKBOX "$(MUI_INNERTEXT_STARTMENU_CHECKBOX)"
    !endif
    
    StrCmp $(^RTL) 0 mui.startmenu_nortl
      !ifndef MUI_STARTMENUPAGE_NODISABLE
        StartMenu::Select /rtl /noicon /autoadd /text "${MUI_TEMP_STARTMENUPAGE_TEXT}" /lastused "${MUI_STARTMENUPAGE_VARIABLE}" /checknoshortcuts "${MUI_TEMP_STARTMENUPAGE_TEXT_CHECKBOX}" "${MUI_STARTMENUPAGE_DEFAULTFOLDER}"
      !else
        StartMenu::Select /rtl /noicon /autoadd /text "${MUI_TEMP_STARTMENUPAGE_TEXT}" /lastused "${MUI_STARTMENUPAGE_VARIABLE}" "${MUI_STARTMENUPAGE_DEFAULTFOLDER}"
      !endif
      Goto mui.startmenu_calldone
    mui.startmenu_nortl:
      !ifndef MUI_STARTMENUPAGE_NODISABLE
        StartMenu::Select /noicon /autoadd /text "${MUI_TEMP_STARTMENUPAGE_TEXT}" /lastused "${MUI_STARTMENUPAGE_VARIABLE}" /checknoshortcuts "${MUI_TEMP_STARTMENUPAGE_TEXT_CHECKBOX}" "${MUI_STARTMENUPAGE_DEFAULTFOLDER}"
      !else
        StartMenu::Select /noicon /autoadd /text "${MUI_TEMP_STARTMENUPAGE_TEXT}" /lastused "${MUI_STARTMENUPAGE_VARIABLE}" "${MUI_STARTMENUPAGE_DEFAULTFOLDER}"
      !endif
    mui.startmenu_calldone:

    Pop $MUI_TEMP1
    StrCmp $MUI_TEMP1 "success" 0 +2
      Pop "${MUI_STARTMENUPAGE_VARIABLE}"
      
  FunctionEnd

  Function "${LEAVE}"

    !insertmacro MUI_PAGE_FUNCTION_CUSTOM LEAVE

  FunctionEnd

!macroend

!macro MUI_FUNCTION_INSTFILESPAGE PRE SHOW LEAVE

  Function "${PRE}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM PRE
    !insertmacro MUI_HEADER_TEXT_PAGE $(MUI_${MUI_PAGE_UNINSTALLER}TEXT_${MUI_PAGE_UNINSTALLER}INSTALLING_TITLE) $(MUI_${MUI_PAGE_UNINSTALLER}TEXT_${MUI_PAGE_UNINSTALLER}INSTALLING_SUBTITLE)
    
  FunctionEnd

  Function "${SHOW}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM SHOW
    
  FunctionEnd

  Function "${LEAVE}"
    
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM LEAVE
      
    !insertmacro MUI_FINISHHEADER
    !insertmacro MUI_LANGDLL_SAVELANGUAGE
    
  FunctionEnd
  
!macroend

!macro MUI_FUNCTION_FINISHPAGE PRE LEAVE

  !ifndef MUI_VAR_HWND
    Var MUI_HWND
    !define MUI_VAR_HWND
  !endif

  !ifndef MUI_FINISHPAGE_NOAUTOCLOSE
    AutoCloseWindow true
  !endif
  
  !ifdef MUI_FINISHPAGE_LINK
    !ifndef MUI_FINISHPAGE_LINK_COLOR
      !define MUI_FINISHPAGE_LINK_COLOR "0x800000"
    !endif
  !endif

  !ifdef MUI_FINISHPAGE_RUN | MUI_FINISHPAGE_SHOWREADME
    !ifndef MUI_FINISHPAGE_ABORTWARNINGCHECK
      !define MUI_FINISHPAGE_ABORTWARNINGCHECK
      Var MUI_NOABORTWARNING
    !endif
  !endif

  Function "${PRE}"
    
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1035
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1037
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1038
    ShowWindow $MUI_TEMP1 ${SW_HIDE}
      
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1045
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}
    
    !ifdef MUI_FINISHPAGE_BUTTON
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "NextButtonText" "${MUI_FINISHPAGE_BUTTON}"
      !undef MUI_FINISHPAGE_BUTTON
    !else
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "NextButtonText" "$(MUI_BUTTONTEXT_FINISH)"
    !endif
    
    !ifdef MUI_FINISHPAGE_ABORTWARNINGCHECK
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "CancelEnabled" "1"
    !endif
    
    !ifdef MUI_FINISHPAGE_TITLE
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 2" "Text" "${MUI_FINISHPAGE_TITLE}"
      !undef MUI_FINISHPAGE_TITLE
    !else
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 2" "Text" "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_FINISH_INFO_TITLE)"
    !endif
    
    !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Bottom" "85"
    
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
  
      IfRebootFlag 0 mui.finish_noreboot_init
    
        !ifdef MUI_FINISHPAGE_TEXT_REBOOT
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "${MUI_FINISHPAGE_TEXT_REBOOT}"
          !undef MUI_FINISHPAGE_TEXT_REBOOT
        !else
	  !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_TEXT_FINISH_INFO_REBOOT)"
	!endif
    
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Settings" "Numfields" "5"
        
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Type" "RadioButton"
        !ifdef MUI_FINISHPAGE_TEXT_REBOOTNOW
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Text" "${MUI_FINISHPAGE_TEXT_REBOOTNOW}"
          !undef MUI_FINISHPAGE_TEXT_REBOOTNOW
        !else
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_REBOOTNOW)"
        !endif
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Left" "120"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Right" "321"
        !ifndef MUI_${MUI_PAGE_UNINSTALLER}WELCOMEFINISHPAGE_3LINES
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Top" "90"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Bottom" "100"
        !else
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Top" "100"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Bottom" "110"        
        !endif
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "State" "1"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Type" "RadioButton"
        !ifdef MUI_FINISHPAGE_TEXT_REBOOTLATER
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Text" "${MUI_FINISHPAGE_TEXT_REBOOTLATER}"
          !undef MUI_FINISHPAGE_TEXT_REBOOTLATER
        !else
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Text" "$(MUI_TEXT_FINISH_REBOOTLATER)"
        !endif
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Left" "120"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Right" "321"
        !ifndef MUI_${MUI_PAGE_UNINSTALLER}WELCOMEFINISHPAGE_3LINES
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Top" "110"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Bottom" "120"
        !else
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Top" "110"
          !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 5" "Bottom" "120"
        !endif
    
        Goto mui.finish_load
     
      mui.finish_noreboot_init:
      
    !endif
    
    !ifdef MUI_FINISHPAGE_TEXT
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "${MUI_FINISHPAGE_TEXT}"
      !undef MUI_FINISHPAGE_TEXT
    !else
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 3" "Text" "$(MUI_${MUI_PAGE_UNINSTALLER}TEXT_FINISH_INFO_TEXT)"
    !endif
      
    !ifdef MUI_FINISHPAGE_RUN
      
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Type" "CheckBox"
      !ifdef MUI_FINISHPAGE_RUN_TEXT
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Text" "${MUI_FINISHPAGE_RUN_TEXT}"
        !undef MUI_FINISHPAGE_RUN_TEXT
      !else
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Text" "$(MUI_TEXT_FINISH_RUN)"
      !endif
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Left" "120"
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Right" "315"
      !ifndef MUI_${MUI_PAGE_UNINSTALLER}WELCOMEFINISHPAGE_3LINES
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Top" "90"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Bottom" "100"
      !else
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Top" "100"
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "Bottom" "110"      
      !endif
      !ifndef MUI_FINISHPAGE_RUN_NOTCHECKED
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field 4" "State" "1"
      !endif
    
    !endif
          
    !ifdef MUI_FINISHPAGE_SHOWREADME
    
      !ifdef MUI_FINISHPAGE_CURFIELD_NO
        !undef MUI_FINISHPAGE_CURFIELD_NO
      !endif
    
      !ifndef MUI_FINISHPAGE_RUN
        !define MUI_FINISHPAGE_CURFIELD_NO 4
        !ifndef MUI_${MUI_PAGE_UNINSTALLER}WELCOMEFINISHPAGE_3LINES
          !define MUI_FINISHPAGE_CURFIELD_TOP 90
          !define MUI_FINISHPAGE_CURFIELD_BOTTOM 100
        !else
          !define MUI_FINISHPAGE_CURFIELD_TOP 100
          !define MUI_FINISHPAGE_CURFIELD_BOTTOM 110
	!endif        
      !else
        !define MUI_FINISHPAGE_CURFIELD_NO 5
        !ifndef MUI_${MUI_PAGE_UNINSTALLER}WELCOMEFINISHPAGE_3LINES
          !define MUI_FINISHPAGE_CURFIELD_TOP 110
          !define MUI_FINISHPAGE_CURFIELD_BOTTOM 120
        !else
          !define MUI_FINISHPAGE_CURFIELD_TOP 120
          !define MUI_FINISHPAGE_CURFIELD_BOTTOM 130
        !endif
      !endif
      
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Type" "CheckBox"
      !ifdef MUI_FINISHPAGE_SHOWREADME_TEXT
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Text" "${MUI_FINISHPAGE_SHOWREADME_TEXT}"
        !undef MUI_FINISHPAGE_SHOWREADME_TEXT
      !else
        !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "Text" "$(MUI_TEXT_FINISH_SHOWREADME)"
      !endif
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
      !insertmacro MUI_INSTALLOPTIONS_WRITE "ioSpecial.ini" "Field ${MUI_FINISHPAGE_CURFIELD_NO}" "TxtColor" "${MUI_FINISHPAGE_LINK_COLOR}"
            
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
      
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM PRE
    
    !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ioSpecial.ini"
    Pop $MUI_HWND
    SetCtlColors $MUI_HWND "" "${MUI_BGCOLOR}"
    
    GetDlgItem $MUI_TEMP1 $MUI_HWND 1201
    SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
    
    CreateFont $MUI_TEMP2 "$(MUI_FONT)" "12" "700"
    SendMessage $MUI_TEMP1 ${WM_SETFONT} $MUI_TEMP2 0
    
    GetDlgItem $MUI_TEMP1 $MUI_HWND 1202
    SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
    
    GetDlgItem $MUI_TEMP1 $MUI_HWND 1200
    SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
    
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
        
      IfRebootFlag 0 mui.finish_noreboot_show
        
        GetDlgItem $MUI_TEMP1 $MUI_HWND 1203
        SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
        
        GetDlgItem $MUI_TEMP1 $MUI_HWND 1204
        SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
          
        Goto mui.finish_show
        
      mui.finish_noreboot_show:
        
    !endif
    
    !ifdef MUI_FINISHPAGE_RUN
      GetDlgItem $MUI_TEMP1 $MUI_HWND 1203
      SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
    !endif
           
    !ifdef MUI_FINISHPAGE_SHOWREADME
      !ifndef MUI_FINISHPAGE_RUN
        GetDlgItem $MUI_TEMP1 $MUI_HWND 1203
      !else
        GetDlgItem $MUI_TEMP1 $MUI_HWND 1204
      !endif
      SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"  
    !endif
    
    !ifdef MUI_FINISHPAGE_LINK
      !ifdef MUI_FINISHPAGE_RUN & MUI_FINISHPAGE_SHOWREADME
        GetDlgItem $MUI_TEMP1 $MUI_HWND 1205
      !else ifdef MUI_FINISHPAGE_RUN | MUI_FINISHPAGE_SHOWREADME
        GetDlgItem $MUI_TEMP1 $MUI_HWND 1204
      !else
        GetDlgItem $MUI_TEMP1 $MUI_HWND 1203
      !endif
      SetCtlColors $MUI_TEMP1 "" "${MUI_BGCOLOR}"
    !endif
     
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
      mui.finish_show:
    !endif

    !insertmacro MUI_PAGE_FUNCTION_CUSTOM SHOW
    
    !ifdef MUI_FINISHPAGE_ABORTWARNINGCHECK
      StrCpy $MUI_NOABORTWARNING "1"
    !endif
    
    !insertmacro MUI_INSTALLOPTIONS_SHOW
    
    !ifdef MUI_FINISHPAGE_ABORTWARNINGCHECK
      StrCpy $MUI_NOABORTWARNING ""
    !endif
    
    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1028
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1256
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1035
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1037
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1038
    ShowWindow $MUI_TEMP1 ${SW_NORMAL}

    GetDlgItem $MUI_TEMP1 $HWNDPARENT 1045
    ShowWindow $MUI_TEMP1 ${SW_HIDE}

  FunctionEnd
  
  Function "${LEAVE}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM LEAVE
    
    !ifndef MUI_FINISHPAGE_NOREBOOTSUPPORT
    
      IfRebootFlag "" mui.finish_noreboot_end
      
        !insertmacro MUI_INSTALLOPTIONS_READ $MUI_TEMP1 "ioSpecial.ini" "Field 4" "State"
       
          StrCmp $MUI_TEMP1 "1" 0 +2
            Reboot
            
          Return
      
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
  
  FunctionEnd
  
  !ifdef MUI_FINISHPAGE_RUN
    !undef MUI_FINISHPAGE_RUN
  !endif
    !ifdef MUI_FINISHPAGE_RUN_PARAMETERS
      !undef MUI_FINISHPAGE_RUN_PARAMETERS
    !endif
    !ifdef MUI_FINISHPAGE_RUN_NOTCHECKED
      !undef MUI_FINISHPAGE_RUN_NOTCHECKED
    !endif
    !ifdef MUI_FINISHPAGE_RUN_FUNCTION
      !undef MUI_FINISHPAGE_RUN_FUNCTION
    !endif
  !ifdef MUI_FINISHPAGE_SHOWREADME
    !undef MUI_FINISHPAGE_SHOWREADME
  !endif
    !ifdef MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
      !undef MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
    !endif
    !ifdef MUI_FINISHPAGE_SHOWREADME_FUNCTION
      !undef MUI_FINISHPAGE_SHOWREADME_FUNCTION
    !endif
  !ifdef MUI_FINISHPAGE_LINK
    !undef MUI_FINISHPAGE_LINK
  !endif
    !ifdef MUI_FINISHPAGE_LINK_LOCATION
      !undef MUI_FINISHPAGE_LINK_LOCATION
    !endif
    !ifdef MUI_FINISHPAGE_LINK_COLOR
      !undef MUI_FINISHPAGE_LINK_COLOR
    !endif
  !ifdef MUI_FINISHPAGE_NOREBOOTSUPPORT
    !undef MUI_FINISHPAGE_NOREBOOTSUPPORT
  !endif
  
!macroend

;--------------------------------
;INSTALL OPTIONS (CUSTOM PAGES)

!macro MUI_INSTALLOPTIONS_EXTRACT FILE

  !verbose push
  !verbose 3

  InitPluginsDir

  File "/oname=$PLUGINSDIR\${FILE}" "${FILE}"
  
  !insertmacro MUI_INSTALLOPTIONS_WRITE "${FILE}" "Settings" "RTL" "$(^RTL)"

  !verbose pop

!macroend

!macro MUI_INSTALLOPTIONS_EXTRACT_AS FILE FILENAME

  !verbose push
  !verbose 3

  InitPluginsDir

  File "/oname=$PLUGINSDIR\${FILENAME}" "${FILE}"
  
  !insertmacro MUI_INSTALLOPTIONS_WRITE "${FILENAME}" "Settings" "RTL" "$(^RTL)"
  
  !verbose pop

!macroend

!macro MUI_INSTALLOPTIONS_DISPLAY FILE

  !verbose push
  !verbose 3
  
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"
  Pop $MUI_TEMP1

  !verbose pop

!macroend

!macro MUI_INSTALLOPTIONS_DISPLAY_RETURN FILE

  !verbose push
  !verbose 3
  
  InstallOptions::dialog "$PLUGINSDIR\${FILE}"

  !verbose pop

!macroend

!macro MUI_INSTALLOPTIONS_INITDIALOG FILE

  !verbose push
  !verbose 3
  
  InstallOptions::initDialog /NOUNLOAD "$PLUGINSDIR\${FILE}"

  !verbose pop

!macroend

!macro MUI_INSTALLOPTIONS_SHOW

  !verbose push
  !verbose 3

  InstallOptions::show
  Pop $MUI_TEMP1

  !verbose pop

!macroend

!macro MUI_INSTALLOPTIONS_SHOW_RETURN

  !verbose push
  !verbose 3
  
  InstallOptions::show

  !verbose pop

!macroend

!macro MUI_INSTALLOPTIONS_READ VAR FILE SECTION KEY

  !verbose push
  !verbose 3

  ReadIniStr ${VAR} "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}"

  !verbose pop

!macroend

!macro MUI_INSTALLOPTIONS_WRITE FILE SECTION KEY VALUE

  !verbose push
  !verbose 3

  WriteIniStr "$PLUGINSDIR\${FILE}" "${SECTION}" "${KEY}" "${VALUE}"

  !verbose pop

!macroend

!macro MUI_UNFUNCTION_CONFIRMPAGE PRE SHOW LEAVE

  Function "${PRE}"
  
   !insertmacro MUI_PAGE_FUNCTION_CUSTOM PRE
   !insertmacro MUI_HEADER_TEXT_PAGE $(MUI_UNTEXT_CONFIRM_TITLE) $(MUI_UNTEXT_CONFIRM_SUBTITLE)
  
  FunctionEnd
  
  Function "${SHOW}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM SHOW
  
  FunctionEnd
  
  Function "${LEAVE}"
  
    !insertmacro MUI_PAGE_FUNCTION_CUSTOM LEAVE
    
  FunctionEnd
  
!macroend

;--------------------------------
;RESERVE FILES

!macro MUI_RESERVEFILE_INSTALLOPTIONS

  !verbose push
  !verbose 3
  
  ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
  
  !verbose pop
  
!macroend

!macro MUI_RESERVEFILE_LANGDLL

  !verbose push
  !verbose 3
  
  ReserveFile "${NSISDIR}\Plugins\LangDLL.dll"
  
  !verbose pop
  
!macroend

;--------------------------------
;LANGUAGES

!macro MUI_LANGUAGE LANGUAGE

  !verbose push
  !verbose 3
  
  !include "${NSISDIR}\Contrib\Modern UI\Language files\${LANGUAGE}.nsh"
  
  !verbose pop
  
!macroend

;--------------------------------
;LANGUAGE SELECTION DIALOG

!macro MUI_LANGDLL_DISPLAY

  !verbose push
  !verbose 3

  !ifdef NSIS_CONFIG_SILENT_SUPPORT
    IfSilent mui.langdll_done
  !endif

  !ifndef MUI_LANGDLL_WINDOWTITLE
    !define MUI_LANGDLL_WINDOWTITLE "Installer Language"
  !endif

  !ifndef MUI_LANGDLL_INFO
    !define MUI_LANGDLL_INFO "Please select a language."
  !endif
  
  !ifdef MUI_LANGDLL_REGISTRY_ROOT & MUI_LANGDLL_REGISTRY_KEY & MUI_LANGDLL_REGISTRY_VALUENAME
    
    ReadRegStr $MUI_TEMP1 "${MUI_LANGDLL_REGISTRY_ROOT}" "${MUI_LANGDLL_REGISTRY_KEY}" "${MUI_LANGDLL_REGISTRY_VALUENAME}"
    StrCmp $MUI_TEMP1 "" mui.langdll_show
      StrCpy $LANGUAGE $MUI_TEMP1
      !ifndef MUI_LANGDLL_ALWAYSSHOW
        Goto mui.langdll_done
      !endif
    mui.langdll_show:
  
  !endif
  
  LangDLL::LangDialog "${MUI_LANGDLL_WINDOWTITLE}" "${MUI_LANGDLL_INFO}" A ${MUI_LANGDLL_PUSHLIST} ""

  Pop $LANGUAGE
  StrCmp $LANGUAGE "cancel" 0 +2
    Abort
  
  !ifdef NSIS_CONFIG_SILENT_SUPPORT
    mui.langdll_done:
  !else ifdef MUI_LANGDLL_REGISTRY_ROOT & MUI_LANGDLL_REGISTRY_KEY & MUI_LANGDLL_REGISTRY_VALUENAME
    mui.langdll_done:
  !endif
    
  !verbose pop
    
!macroend

!macro MUI_LANGDLL_SAVELANGUAGE

  !ifdef MUI_LANGDLL_REGISTRY_ROOT & MUI_LANGDLL_REGISTRY_KEY & MUI_LANGDLL_REGISTRY_VALUENAME
    WriteRegStr "${MUI_LANGDLL_REGISTRY_ROOT}" "${MUI_LANGDLL_REGISTRY_KEY}" "${MUI_LANGDLL_REGISTRY_VALUENAME}" $LANGUAGE
  !endif
  
!macroend

!macro MUI_UNGETLANGUAGE

  !verbose pop

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
  
  !verbose pop

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

!macro MUI_LANGUAGEFILE_DEFAULT NAME VALUE

  !ifndef "${NAME}"
    !define "${NAME}" "${VALUE}"
    !warning "${LANGUAGE} Modern UI language file version doesn't match. Using default English texts for missing strings."
  !endif

!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING NAME

  LangString "${NAME}" 0 "${${NAME}}"
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_UNLANGSTRING NAME

  !ifdef MUI_UNINSTALLER
    LangString "${NAME}" 0 "${${NAME}}"
    !undef "${NAME}"
  !else
    !undef "${NAME}"
  !endif
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_PAGE PAGE NAME

  !ifdef MUI_${PAGE}PAGE
    LangString "${NAME}" 0 "${${NAME}}"
    !undef "${NAME}"
  !else
    !undef "${NAME}"
  !endif
  
!macroend

!macro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE PAGE NAME

  !ifdef MUI_UNINSTALLER
    !ifdef MUI_UN${PAGE}PAGE
      LangString "${NAME}" 0 "${${NAME}}"
      !undef "${NAME}"
    !else
      !undef "${NAME}"
    !endif
  !else
    !undef "${NAME}"
  !endif
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_NOUNDEF NAME

  LangString "${NAME}" 0 "${${NAME}}"
  
!macroend

!macro MUI_LANGUAGEFILE_DEFINE DEFINE NAME

  !ifndef "${DEFINE}"
    !define "${DEFINE}" "${${NAME}}"
  !endif
  !undef "${NAME}"
  
!macroend

!macro MUI_LANGUAGEFILE_LANGSTRING_FONT NAME DEFAULT

  !ifdef "${NAME}"
    Langstring "${NAME}" 0 "${${NAME}}"
    !undef "${NAME}"
  !else
    Langstring "${NAME}" 0 "${DEFAULT}"
  !endif
  
!macroend

!macro MUI_LANGUAGEFILE_END

  !include "${NSISDIR}\Contrib\Modern UI\Language files\Default.nsh"
  
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
 
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_FONT "MUI_FONT" "MS Shell Dlg"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_FONT "MUI_FONTSIZE" "8"
 
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_NOUNDEF "MUI_BGCOLOR"
  
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE WELCOME "MUI_TEXT_WELCOME_INFO_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE WELCOME "MUI_TEXT_WELCOME_INFO_TEXT"

  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE LICENSE "MUI_TEXT_LICENSE_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE LICENSE "MUI_TEXT_LICENSE_SUBTITLE"
  !ifdef MUI_LICENSEPAGE
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING_NOUNDEF "MUI_INNERTEXT_LICENSE_TOP"
  !endif
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE LICENSE "MUI_INNERTEXT_LICENSE_BOTTOM"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE LICENSE "MUI_INNERTEXT_LICENSE_BOTTOM_CHECKBOX"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE LICENSE "MUI_INNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS"

  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE COMPONENTS "MUI_TEXT_COMPONENTS_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE COMPONENTS "MUI_TEXT_COMPONENTS_SUBTITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE COMPONENTS "MUI_INNERTEXT_COMPONENTS_DESCRIPTION_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE COMPONENTS "MUI_INNERTEXT_COMPONENTS_DESCRIPTION_INFO"

  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE DIRECTORY "MUI_TEXT_DIRECTORY_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE DIRECTORY "MUI_TEXT_DIRECTORY_SUBTITLE"

  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE STARTMENU "MUI_TEXT_STARTMENU_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE STARTMENU "MUI_TEXT_STARTMENU_SUBTITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE STARTMENU "MUI_INNERTEXT_STARTMENU_TOP"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE STARTMENU "MUI_INNERTEXT_STARTMENU_CHECKBOX"
  
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_FINISH_SUBTITLE"

  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_ABORT_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_ABORT_SUBTITLE"
  
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_INSTALLING_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_INSTALLING_SUBTITLE"

  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE FINISH "MUI_BUTTONTEXT_FINISH"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE FINISH "MUI_TEXT_FINISH_INFO_TITLE"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE FINISH "MUI_TEXT_FINISH_INFO_TEXT"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE FINISH "MUI_TEXT_FINISH_INFO_REBOOT"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE FINISH "MUI_TEXT_FINISH_REBOOTNOW"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE FINISH "MUI_TEXT_FINISH_REBOOTLATER"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE FINISH "MUI_TEXT_FINISH_RUN"
  !insertmacro MUI_LANGUAGEFILE_LANGSTRING_PAGE FINISH "MUI_TEXT_FINISH_SHOWREADME"
  
  !ifdef MUI_ABORTWARNING
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_TEXT_ABORTWARNING"
  !else
    !undef MUI_TEXT_ABORTWARNING
  !endif
  
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE WELCOME "MUI_UNTEXT_WELCOME_INFO_TITLE"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE WELCOME "MUI_UNTEXT_WELCOME_INFO_TEXT"
  
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE CONFIRM "MUI_UNTEXT_CONFIRM_TITLE"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE CONFIRM "MUI_UNTEXT_CONFIRM_SUBTITLE"
  
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE LICENSE "MUI_UNTEXT_LICENSE_TITLE"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE LICENSE "MUI_UNTEXT_LICENSE_SUBTITLE"
  !ifndef MUI_LICENSEPAGE
    !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_INNERTEXT_LICENSE_TOP"
  !else
    !undef MUI_INNERTEXT_LICENSE_TOP
  !endif
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE LICENSE "MUI_UNINNERTEXT_LICENSE_BOTTOM"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE LICENSE "MUI_UNINNERTEXT_LICENSE_BOTTOM_CHECKBOX"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE LICENSE "MUI_UNINNERTEXT_LICENSE_BOTTOM_RADIOBUTTONS"

  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE COMPONENTS "MUI_UNTEXT_COMPONENTS_TITLE"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE COMPONENTS "MUI_UNTEXT_COMPONENTS_SUBTITLE"

  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE DIRECTORY "MUI_UNTEXT_DIRECTORY_TITLE"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE DIRECTORY  "MUI_UNTEXT_DIRECTORY_SUBTITLE"
   
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_UNINSTALLING_TITLE"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_UNINSTALLING_SUBTITLE"
   
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_FINISH_TITLE"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_FINISH_SUBTITLE"
  
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_ABORT_TITLE"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING "MUI_UNTEXT_ABORT_SUBTITLE"
  
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE FINISH "MUI_UNTEXT_FINISH_INFO_TITLE"
  !insertmacro MUI_LANGUAGEFILE_UNLANGSTRING_PAGE FINISH "MUI_UNTEXT_FINISH_INFO_TEXT"
  
  !ifdef MUI_UNABORTWARNING
    !insertmacro MUI_LANGUAGEFILE_LANGSTRING "MUI_UNTEXT_ABORTWARNING"
  !else
    !undef MUI_UNTEXT_ABORTWARNING
  !endif
    
!macroend

;--------------------------------
;END

!endif

!verbose 4