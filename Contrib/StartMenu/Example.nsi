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
	Pop $R0

	StrCpy $1 $R0 5
	StrCmp $1 "error" 0 +3
		; error
		MessageBox MB_OK $R0
		Return
	StrCmp $R0 "cancel" 0 +2
		Quit
	StrCmp $R0 "back" 0 +2
		Abort
FunctionEnd

Page instfiles
Section
	CreateDirectory $SMPROGRAMS\$R0
	CreateShortCut $SMPROGRAMS\$R0\MakeNSIS.lnk $INSTDIR\makensis.exe

	SetShellVarContext All
	CreateDirectory $SMPROGRAMS\$R0
	CreateShortCut "$SMPROGRAMS\$R0\All users MakeNSIS.lnk" $INSTDIR\makensis.exe
SectionEnd