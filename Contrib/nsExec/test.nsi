Name "nsExec Test"

OutFile "nsExec Test.exe"

ShowInstDetails show

Section
	nsExec::ExecToLog /TIMEOUT=1 '"${NSISDIR}\makensis.exe" /CMDHELP'
	Pop $0
	DetailPrint ""
	DetailPrint "       Return value: $0"
SectionEnd