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

!endif