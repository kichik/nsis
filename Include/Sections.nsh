; Some sections defines

; Include in your script using:
; !include "Sections.nsh"

!ifndef SECTIONS_NSH_INCLUDED

!define SECTIONS_NSH_INCLUDED

!define SF_SELECTED   1
!define SF_SUBSEC     2
!define SF_SUBSECEND  4
!define SF_BOLD       8
!define SF_RO         16
!define SF_EXPAND     32
!define SF_PSELECTED  64

!define SECTION_OFF   0xFFFFFFFE

!macro SelectSection SECTION
	Push $0
	SectionGetFlags "${SECTION}" $0
	IntOp $0 $0 | ${SF_SELECTED}
	SectionSetFlags "${SECTION}" $0
	Pop $0
!macroend

!macro UnselectSection SECTION
	Push $0
	SectionGetFlags "${SECTION}" $0
	IntOp $0 $0 & ${SECTION_OFF}
	SectionSetFlags "${SECTION}" $0
	Pop $0
!macroend

# if section selected, will unselect, if unselected, will select
!macro ReverseSection SECTION
	Push $0
	SectionGetFlags "${SECTION}" $0
	IntOp $0 $0 ^ ${SF_SELECTED}
	SectionSetFlags "${SECTION}" $0
	Pop $0
!macroend

# macros for mutually exclusive section selection
# written by Tim Gallagher

#### usage example (see one-section.nsi too):

# Var SomeVar
#
# Function .onSelChange
# !insertmacro StartRadioButtons $SomeVar
# !insertmacro RadioButton ${sec1}
# !insertmacro RadioButton ${sec2}
# !insertmacro RadioButton ${sec3}
# !insertmacro EndRadioButtons
# FunctionEnd
#
# Function .onInit
# StrCpy $SomeVar ${sec1} ; default section
# !insertmacro UnselectSection ${sec2}
# !insertmacro UnselectSection ${sec3}
# FunctionEnd

# Starts the Radio Button Block.
# You should pass a variable that keeps the selected section
# as the first parameter for this macro. This variable should
# be initialized to the default section's index. As this macro
# uses $R0 and $R1 you can't use those two as the varible
# which will keep the selected section.
!macro StartRadioButtons var
!define StartRadioButtons_Var "${var}"
	Push $R0
	SectionGetFlags "${StartRadioButtons_Var}" $R0
	IntOp $R0 $R0 & ${SECTION_OFF}
	SectionSetFlags "${StartRadioButtons_Var}" $R0

	Push $R1
	StrCpy $R1 "${StartRadioButtons_Var}"
!macroend

!macro RadioButton SECTION_NAME
	SectionGetFlags ${SECTION_NAME} $R0
	IntOp $R0 $R0 & ${SF_SELECTED}
	IntCmp $R0 ${SF_SELECTED} 0 +2 +2
		StrCpy "${StartRadioButtons_Var}" ${SECTION_NAME}
!macroend

# ends the radio button block
!macro EndRadioButtons
	StrCmp $R1 "${StartRadioButtons_Var}" 0 +4 ; selection hasn't changed
		SectionGetFlags "${StartRadioButtons_Var}" $R0
		IntOp $R0 $R0 | ${SF_SELECTED}
		SectionSetFlags "${StartRadioButtons_Var}" $R0

	Pop $R1
	Pop $R0
!undef StartRadioButtons_Var
!macroend

; For details about SetSectionInInstType and ClearSectionInInstType, see
; http://nsis.sourceforge.net/archive/nsisweb.php?page=287

!define INSTTYPE_1 1
!define INSTTYPE_2 2
!define INSTTYPE_3 4
!define INSTTYPE_4 8
!define INSTTYPE_5 16
!define INSTTYPE_6 32
!define INSTTYPE_7 64
!define INSTTYPE_8 128

!macro SetSectionInInstType SECTION_NAME WANTED_INSTTYPE
	Push $0
	SectionGetInstTypes "${SECTION_NAME}" $0
	IntOp $0 $0 | ${WANTED_INSTTYPE}
	SectionSetInstTypes "${SECTION_NAME}" $0
	Pop $0
!macroend

!macro ClearSectionInInstType SECTION_NAME WANTED_INSTTYPE
	Push $0
	Push $1
	SectionGetInstTypes "${SECTION_NAME}" $0
	StrCpy $1 ${WANTED_INSTTYPE}
	IntOp $1 $1 ~
	IntOp $0 $0 & $1
	SectionSetInstTypes "${SECTION_NAME}" $0
	Pop $1
	Pop $0
!macroend

# more macros by derekrprice

; Set one or more BITS in SECTION's flags.
!macro SetSectionFlag SECTION BITS
	Push $R0
	SectionGetFlags "${SECTION}" $R0
	IntOp $R0 $R0 | "${BITS}"
	SectionSetFlags "${SECTION}" $R0
	Pop $R0
!macroend

; Clear one or more BITS in SECTION's flags.
!macro ClearSectionFlag SECTION BITS
	Push $R0
	Push $R1
	SectionGetFlags "${SECTION}" $R0
	IntOp $R1 "${BITS}" ~
	IntOp $R0 $R0 & $R1
	SectionSetFlags "${SECTION}" $R0
	Pop $R1
	Pop $R0
!macroend

; Check if one or more BITS in SECTION's flags are set.
; If they are, jump to JUMPIFSET
; If not, jump to JUMPIFNOTSET
!macro SectionFlagIsSet SECTION BITS JUMPIFSET JUMPIFNOTSET
	Push $R0
	SectionGetFlags "${SECTION}" $R0
	IntOp $R0 $R0 & "${BITS}"
	IntCmp $R0 "${BITS}" +3
	Pop $R0
	StrCmp "" "${JUMPIFNOTSET}" +3 "${JUMPIFNOTSET}"
	Pop $R0
	Goto "${JUMPIFSET}"
!macroend


!endif