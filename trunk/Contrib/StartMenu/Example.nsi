!verbose 3
!include "${NSISDIR}\Examples\WinMessages.nsh"
!verbose 4

Name "StartMenu.dll test"

OutFile "StartMenu Test.exe"

XPStyle on

Page directory
DirText "This installer will create some shortcuts to MakeNSIS in the start menu.$\nFor this it needs NSIS's path." \
  "Please specify the path in which you have installed NSIS:"
InstallDir "${NSISDIR}"
Function .onVerifyInstDir
	IfFileExists $INSTDIR\makensis.exe +2
		Abort
FunctionEnd

Page custom StartMenuGroupSelect ": Start Menu Folder"
Function StartMenuGroupSelect
	StartMenu::Select /autoadd /lastused $R0 "StartMenu.dll test"
	Pop $R1

	StrCpy $R2 $R1 5
	StrCmp $R2 "error" 0 +3
		; error
		MessageBox MB_OK $R1
		Return
	StrCpy $R0 $R1 ; got the dir, or cancel, but if it's cancel NSIS will exit and
				   ; then we shouldn't care about the value of $R0
FunctionEnd

Page instfiles
Section
	CreateDirectory $SMPROGRAMS\$R0
	CreateShortCut $SMPROGRAMS\$R0\MakeNSIS.lnk $INSTDIR\makensis.exe

	SetShellVarContext All
	CreateDirectory $SMPROGRAMS\$R0
	CreateShortCut "$SMPROGRAMS\$R0\All users MakeNSIS.lnk" $INSTDIR\makensis.exe
SectionEnd