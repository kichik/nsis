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

!endif