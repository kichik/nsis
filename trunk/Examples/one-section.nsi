# This example demonstrates how to control section selection
# It allows only one of the four optional section to be selected at any given time
#
# Please note that the initial value will not be what you expect it to be if you are
# are using InstType because InstType is taken into consideration after the .onInit,
# where the initial state is set, executes.
# To use this code with InstType you will either have to specify you first InstType
# to match the initial value you set in .onInit (only StrCpy $1 ${sec1} is important
# in this case because the other set the section and that will be done by InstType),
# or set the initial section selection from .onSelChange when it is called for the
# first time.

Name example
OutFile one-section.exe

ComponentText "please choose just one but the default"

!define SF_SELECTED   1
!define SF_SUBSEC     2
!define SF_SUBSECEND  4
!define SF_BOLD       8
!define SF_RO         16
!define SF_EXPAND     32

!define SECTION_OFF   0xFFFFFFFE

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

Function .onInit
	Push $0

	StrCpy $1 ${sec1} ; Gotta remember which section we are at now...
	SectionGetFlags ${sec1} $0
	IntOp $0 $0 | ${SF_SELECTED}
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
	IntOp $0 $0 & ${SF_SELECTED}
	IntCmp $0 ${SF_SELECTED} 0 +2 +2
		StrCpy $1 ${sec1}
	SectionGetFlags ${sec2} $0
	IntOp $0 $0 & ${SF_SELECTED}
	IntCmp $0 ${SF_SELECTED} 0 +2 +2
		StrCpy $1 ${sec2}
	SectionGetFlags ${sec3} $0
	IntOp $0 $0 & ${SF_SELECTED}
	IntCmp $0 ${SF_SELECTED} 0 +2 +2
		StrCpy $1 ${sec3}
	SectionGetFlags ${sec4} $0
	IntOp $0 $0 & ${SF_SELECTED}
	IntCmp $0 ${SF_SELECTED} 0 +2 +2
		StrCpy $1 ${sec4}

	StrCmp $2 $1 0 +4 ; selection hasn't changed
		SectionGetFlags $1 $0
		IntOp $0 $0 | ${SF_SELECTED}
		SectionSetFlags $1 $0
	Pop $2
	Pop $0
FunctionEnd