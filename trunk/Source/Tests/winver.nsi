Name winver
OutFile winver.exe

!include WinVer.nsh

# basic winver fun & games to make sure it compiles

Section

	${If} ${IsWin2000}
		${If} ${AtLeastServicePack} 4
		${EndIf}
	${EndIf}

SectionEnd