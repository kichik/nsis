Name example
OutFile setup.exe

ComponentText "please choose just one but the the default"

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

!define SECTION_OFF 0x7FFFFFFF

Function .onInit
	StrCpy $1 ${sec1} ; Gotta remember which section we are at now...
	SectionGetFlags ${sec1} $0
	IntOp $0 $0 | 0x80000000
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
FunctionEnd

Function .onSelChange
	; Turn off old selected section
	StrCmp $1 ${sec1} 0 +4
		SectionGetFlags ${sec1} $0
		IntOp $0 $0 & ${SECTION_OFF}
		SectionSetFlags ${sec1} $0
	StrCmp $1 ${sec2} 0 +4
		SectionGetFlags ${sec2} $0
		IntOp $0 $0 & ${SECTION_OFF}
		SectionSetFlags ${sec2} $0
	StrCmp $1 ${sec3} 0 +4
		SectionGetFlags ${sec3} $0
		IntOp $0 $0 & ${SECTION_OFF}
		SectionSetFlags ${sec3} $0
	StrCmp $1 ${sec4} 0 +4
		SectionGetFlags ${sec4} $0
		IntOp $0 $0 & ${SECTION_OFF}
		SectionSetFlags ${sec4} $0

	; Now remember the current selection
	SectionGetFlags ${sec1} $0
	IntOp $0 $0 & 0x80000000
	IntCmp $0 0x80000000 0 +2 +2
		StrCpy $1 ${sec1}
	SectionGetFlags ${sec2} $0
	IntOp $0 $0 & 0x80000000
	IntCmp $0 0x80000000 0 +2 +2
		StrCpy $1 ${sec2}
	SectionGetFlags ${sec3} $0
	IntOp $0 $0 & 0x80000000
	IntCmp $0 0x80000000 0 +2 +2
		StrCpy $1 ${sec3}
	SectionGetFlags ${sec4} $0
	IntOp $0 $0 & 0x80000000
	IntCmp $0 0x80000000 0 +2 +2
		StrCpy $1 ${sec4}
FunctionEnd