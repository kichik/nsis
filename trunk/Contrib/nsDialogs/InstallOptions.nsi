!include LogicLib.nsh
!include WinMessages.nsh

Name "nsDialogs IO"
OutFile "nsDialogs IO.exe"

Page custom nsDialogsIO
Page instfiles

XPStyle on

ShowInstDetails show

!include nsDialogs.nsh

Function nsDialogsIO

	InitPluginsDir
	File /oname=$PLUGINSDIR\io.ini "${NSISDIR}\Examples\InstallOptions\test.ini"

	${If} ${Cmd} `MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Test the right-to-left version?" IDYES`
		WriteINIStr $PLUGINSDIR\io.ini Settings RTL 1
	${EndIf}

	StrCpy $0 $PLUGINSDIR\io.ini

	Call CreateDialogFromINI

FunctionEnd

Section
SectionEnd
