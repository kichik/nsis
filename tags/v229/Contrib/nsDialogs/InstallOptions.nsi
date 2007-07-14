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

	StrCpy $0 $PLUGINSDIR\io.ini

	Call CreateDialogFromINI

FunctionEnd

Section
SectionEnd
