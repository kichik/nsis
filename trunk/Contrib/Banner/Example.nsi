# Look at Readme.txt for usage alongside with the Modern UI

Name "Banner.dll test"

OutFile "Banner Test.exe"

ShowInstDetails show

Function .onInit
	Banner::show /NOUNLOAD "Calculating important stuff..."
	again:
		IntOp $0 $0 + 1
		Sleep 1
		StrCmp $0 2000 0 again
	Banner::destroy
FunctionEnd

Section
	DetailPrint "Using previous calculations to quickly calculate 1*2000..."
	Sleep 1000
	DetailPrint "Eureka! It's $0!!!"
	DetailPrint ""
SectionEnd