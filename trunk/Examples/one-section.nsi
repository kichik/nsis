Name example
OutFile setup.exe

ComponentText "please choose just one but the default"

Section !Required
	SectionIn RO
SectionEnd

Section "optional #1" sec1
SectionEnd

Section "optional #2" sec2
SectionEnd

Section "optional #3" sec3
SectionEnd

Section "optional #4" sec4
SectionEnd

!define SECTION_ON 0x80000000
!define SECTION_OFF 0x7FFFFFFF

Function .onInit
	Push $0

	StrCpy $1 ${sec1} ; Gotta remember which section we are at now...
	SectionGetFlags ${sec1} $0
	IntOp $0 $0 | ${SECTION_ON}
	SectionSetFlags ${sec1} $0

	SectionGetFlags ${sec2} $0
	IntOp $0 $0 & ${SECTION_OFF}
	SectionSetFlags ${sec2} $0

	SectionGetFlags ${sec3} $0
	IntOp $0 $0 & ${SECTION_OFF}
	SectionSetFlags ${sec3} $0

	SectionGetFlags ${sec4} $0
	IntOp $0 $0 & ${SECTION_OFF}
	SectionSetFlags ${sec4} $0

	Pop $0
FunctionEnd

Function .onSelChange
	Push $0

	; Turn off old selected section
	SectionGetFlags $1 $0
	IntOp $0 $0 & ${SECTION_OFF}
	SectionSetFlags $1 $0

	; Now remember the current selection
	Push $2
	StrCpy $2 $1

	SectionGetFlags ${sec1} $0
	IntOp $0 $0 & ${SECTION_ON}
	IntCmp $0 ${SECTION_ON} 0 +2 +2
		StrCpy $1 ${sec1}
	SectionGetFlags ${sec2} $0
	IntOp $0 $0 & ${SECTION_ON}
	IntCmp $0 ${SECTION_ON} 0 +2 +2
		StrCpy $1 ${sec2}
	SectionGetFlags ${sec3} $0
	IntOp $0 $0 & ${SECTION_ON}
	IntCmp $0 ${SECTION_ON} 0 +2 +2
		StrCpy $1 ${sec3}
	SectionGetFlags ${sec4} $0
	IntOp $0 $0 & ${SECTION_ON}
	IntCmp $0 ${SECTION_ON} 0 +2 +2
		StrCpy $1 ${sec4}

	StrCmp $2 $1 0 +2 ; selection hasn't changed
		SectionGetFlags $1 $0
		IntOp $0 $0 | ${SECTION_ON}
		SectionSetFlags $1 $0
	Pop $2
	Pop $0
FunctionEnd