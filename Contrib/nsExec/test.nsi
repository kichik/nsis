Name "nsExec Test"

OutFile "nsExec Test.exe"

ShowInstDetails show

Section "MakeNSIS commands help"
	nsExec::ExecToLog '"${NSISDIR}\makensis.exe" /CMDHELP'
	Pop $0
	DetailPrint ""
	DetailPrint "       Return value: $0"
SectionEnd

Section "Output to variable"
	ReadEnvStr $0 COMSPEC
	GetTempFileName $1
	StrCpy $2 "${NSISDIR}\makensis.exe"
	GetFullPathName /SHORT $2 $2
	StrCpy $0 '"$0" /C $2 /VERSION > "$1"'
	nsExec::Exec $0
	FileOpen $0 $1 r
	FileRead $0 $3
	FileClose $0
	SetDetailsPrint none
	Delete $1
	SetDetailsPrint both
	DetailPrint ""
	DetailPrint "$2 /VERSION returned: $3"
	DetailPrint ""
SectionEnd