!verbose 3
!include "${NSISDIR}\Examples\WinMessages.nsh"
!verbose 4

Name "StartMenu.dll test"

OutFile "StartMenu Test.exe"

XPStyle on

Page directory
DirText "This installer will create some shortcuts to MakeNSIS in the start menu.$\nFor this it needs NSIS's folder path." \
  "Please specify the path in which you have installed NSIS:"
InstallDir "${NSISDIR}"
Function .onVerifyInstDir
	IfFileExists $INSTDIR\makensis.exe +2
		Abort
FunctionEnd

Page custom StartMenuGroupSelect
Function StartMenuGroupSelect
	SendMessage $HWNDPARENT ${WM_SETTEXT} 0 "STR:StartMenu.dll test Setup: Start Menu Folder"

	StartMenu::Select /autoadd "StartMenu.dll test"
	Pop $R1

	StrCpy $R2 $R1 5
	StrCmp $R2 "error" 0 +3
		; error
		MessageBox MB_OK $R1
		Return
	StrCmp $R1 "cancel" 0 +2
		Quit
	StrCmp $R1 "back" 0 +2
		Abort
	StrCpy $R0 $R1 ; got the dir
FunctionEnd

Page instfiles
Section
	CreateDirectory $SMPROGRAMS\$R0
	CreateShortCut $SMPROGRAMS\$R0\MakeNSIS.lnk $INSTDIR\makensis.exe

	SetShellVarContext All
	CreateDirectory $SMPROGRAMS\$R0
	CreateShortCut "$SMPROGRAMS\$R0\All users MakeNSIS.lnk" $INSTDIR\makensis.exe
SectionEnd