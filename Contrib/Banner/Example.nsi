Name "Banner.dll test"

OutFile "Banner Test.exe"

Function .onInit
	Banner::show /NOUNLOAD "Calculating important stuff..."
	again:
		IntOp $0 $0 + 1
		Sleep 1
		StrCmp $0 2000 0 again
	Banner::destroy
FunctionEnd

Section
SectionEnd