;NSIS Modern Style UI
;Example Script version 1.0
;Written by Joost Verburg

!define NAME "Test Software" ;Define your own software name here
!define VERSION "1.0" ;Define your own software version here

!verbose 3
!include "${NSISDIR}\Examples\WinMessages.nsh"
!verbose 4

!define CURRENTPAGE $9

!define TEMP1 $R0
!define TEMP2 $R1

;--------------------------------

  ;General
  Name "${NAME} ${VERSION}"
  OutFile "Example.exe"
  SetOverwrite on

  ;User interface
  Icon "${NSISDIR}\Contrib\Icons\adni18-installer-C-no48xp.ico"
  UninstallIcon "${NSISDIR}\Contrib\Icons\adni18-uninstall-C-no48xp.ico"
  XPStyle On
  ChangeUI all "${NSISDIR}\Contrib\UIs\modern.exe"
  SetFont Tahoma 8
  CheckBitmap "${NSISDIR}\Contrib\Icons\checks4-aa.bmp"

  ;License dialog
  LicenseText "Scroll down to see the rest of the agreement."
  LicenseData "License.txt"

  ;Component-select dialog
  ComponentText "Check the components you want to install and uncheck the components you don't want to install. Click Next to continue."

  ;Folder-select dialog
  InstallDir "$PROGRAMFILES\${NAME}"
  DirText "Setup will install ${NAME} in the following folder.$\r$\n$\r$\nTo install in this folder, click Install. To install in a different folder, click Browse and select another folder." " "

  ;Install dialog
  InstallColors /windows ;Default Windows colors for details list
  InstProgressFlags smooth

  ;Uninstaller
  UninstallText "This will uninstall ${NAME} from your system."

;--------------------------------
;Installer Sections

Section "Copy modern.exe" SecCopyUI

  ;Add your stuff here

  SetOutPath "$INSTDIR"
  File "${NSISDIR}\Contrib\UIs\modern.exe"

SectionEnd

Section "Create uninstaller" SecCreateUninst

  ;Add your stuff here

  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

Section ""

  ;Invisible section to display the Finish header

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1
  Call SetHeader
 
SectionEnd

;--------------------------------
;Installer Functions

Function .onInitDialog

  ;Set texts on inner dialogs

  Push ${TEMP1}

  FindWindow ${TEMP1} "#32770" "" $HWNDPARENT

  StrCmp ${CURRENTPAGE} 1 "" +4
    GetDlgItem ${TEMP1} ${TEMP1} 1040
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "If you accept all the terms of the agreement, choose I Agree to continue. If you choose Cancel, Setup will close. You must accept the agreement to install ${NAME}."
    Goto done

  StrCmp ${CURRENTPAGE} 2 "" +4
    GetDlgItem ${TEMP1} ${TEMP1} 1042
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Description"
    Goto done
   
  StrCmp ${CURRENTPAGE} 3 "" +3
    GetDlgItem ${TEMP1} ${TEMP1} 1041
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Destination Folder"

  done:

  Pop ${TEMP1}

FunctionEnd

Function .onNextPage

  Push ${TEMP1}
  Push ${TEMP2}  

  ;Set backgrounds & fonts for the outer dialog (only once)
  StrCmp ${CURRENTPAGE} "" "" no_set_outer
   
    GetDlgItem ${TEMP1} $HWNDPARENT 1037
    CreateFont ${TEMP2} "Tahoma" 16 1000
    SendMessage ${TEMP1} ${WM_SETFONT} ${TEMP2} 0
    SetStaticBkColor ${TEMP1} 0x00FFFFFF
 
    GetDlgItem ${TEMP1} $HWNDPARENT 1038
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    GetDlgItem ${TEMP1} $HWNDPARENT 1034
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    GetDlgItem ${TEMP1} $HWNDPARENT 1039
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    no_set_outer:

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1

  Call SetHeader

  Pop ${TEMP2}  
  Pop ${TEMP1}

FunctionEnd

Function .onPrevPage

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} - 1

  Call SetHeader

FunctionEnd

Function SetHeader

  ;Set the texts on the header (white rectangle)

  Push ${TEMP1}
  Push ${TEMP2}

  GetDlgItem ${TEMP1} $HWNDPARENT 1037
  GetDlgItem ${TEMP2} $HWNDPARENT 1038

  StrCmp ${CURRENTPAGE} 1 "" +4
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "License Agreement"
    SendMessage ${TEMP2} ${WM_SETTEXT} 0 "Please review the license terms before installing ${NAME}."
    Goto done

  StrCmp ${CURRENTPAGE} 2 "" +4
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Choose Components"
    SendMessage ${TEMP2} ${WM_SETTEXT} 0 "Choose the components you want to install."
    Goto done

  StrCmp ${CURRENTPAGE} 3 "" +4
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Choose Install Location"
    SendMessage ${TEMP2} ${WM_SETTEXT} 0 "Choose the folder in which to install ${NAME} in."
    Goto done

  StrCmp ${CURRENTPAGE} 4 "" +4
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Installing"
    SendMessage ${TEMP2} ${WM_SETTEXT} 0 "Please wait while ${NAME} is being installed."
    Goto done

  StrCmp ${CURRENTPAGE} 5 "" +3
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Finished"
    SendMessage ${TEMP2} ${WM_SETTEXT} 0 "Setup was completed successfully."

  done:

  Pop ${TEMP1}
  Pop ${TEMP2}

FunctionEnd

Function .onMouseOverSection

  ;Set text in Description area

  Push ${TEMP1}

  FindWindow ${TEMP1} "#32770" "" $HWNDPARENT
  GetDlgItem ${TEMP1} ${TEMP1} 1043

  StrCmp $0 ${SecCopyUI} "" +3
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Copy the modern.exe file to the application folder."
    Goto done
  
  StrCmp $0 ${SecCreateUninst} "" +2
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Create an uninstaller which can automatically remove ${NAME}."

  done:

  Pop ${TEMP1}
  
FunctionEnd

Function .onUserAbort

  ;Warning when 'Cancel' button is pressed

  MessageBox MB_YESNO|MB_ICONEXCLAMATION "Are you sure you want to quit ${NAME} Setup?" IDYES quit
    Abort
  quit:

FunctionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;Add your stuff here

  Delete "$INSTDIR\modern.exe"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  ;Display the Finish header
  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1
  Call un.SetHeader
 
SectionEnd

;--------------------------------
;Uninstaller Functions

Function un.onNextPage

  Push ${TEMP1}
  Push ${TEMP2}

  ;Set backgrounds & fonts for the outer dialog (only once)
  StrCmp ${CURRENTPAGE} "" "" no_set_outer

    GetDlgItem ${TEMP1} $HWNDPARENT 1037
    CreateFont ${TEMP2} "Tahoma" 16 1000
    SendMessage ${TEMP1} ${WM_SETFONT} ${TEMP2} 0
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    GetDlgItem ${TEMP1} $HWNDPARENT 1038
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    GetDlgItem ${TEMP1} $HWNDPARENT 1034
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    GetDlgItem ${TEMP1} $HWNDPARENT 1039
    SetStaticBkColor ${TEMP1} 0x00FFFFFF

    no_set_outer:

  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1

  Call un.SetHeader

  Pop ${TEMP2}
  Pop ${TEMP1}

FunctionEnd

Function un.SetHeader

  ;Set the texts on the header (white rectangle)

  Push ${TEMP1}
  Push ${TEMP2}

  GetDlgItem ${TEMP1} $HWNDPARENT 1037
  GetDlgItem ${TEMP2} $HWNDPARENT 1038

  StrCmp ${CURRENTPAGE} 1 "" +4
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Uninstall ${NAME}"
    SendMessage ${TEMP2} ${WM_SETTEXT} 0 "Remove ${NAME} from your system."
    Goto done

  StrCmp ${CURRENTPAGE} 2 "" +4
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Uninstalling"
    SendMessage ${TEMP2} ${WM_SETTEXT} 0 "Please wait while ${NAME} is being uninstalled."
    Goto done

  StrCmp ${CURRENTPAGE} 3 "" +3
    SendMessage ${TEMP1} ${WM_SETTEXT} 0 "Finished"
    SendMessage ${TEMP2} ${WM_SETTEXT} 0 "${NAME} has been removed from your system."

  done:

  Pop ${TEMP2}
  Pop ${TEMP1}

FunctionEnd

;eof