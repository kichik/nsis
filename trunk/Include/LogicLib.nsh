; NSIS SELECT LIBRARY - selectlib.nsh
; Version 1.0 - 09/15/2003
; Questions/Comments - dselkirk@hotmail.com
;
; Description:
;   Provides the use of select statements
;
; Usage:
;   Section
;     !insertmacro SELECT "TESTCASE" "test1"
;       !insertmacro CASE "test1"
;         MessageBox MB_OK "case test1"
;       !insertmacro CASE "test 2;test3"
;         MessageBox MB_OK "case test2 or test3"
;       !insertmacro CASE_ELSE
;         MessageBox MB_OK "case else"
;     !insertmacro SELECTEND
;   SectionEnd
;
; History:
;		1.0 - 09/19/2003 - Initial release
;		1.1 - 09/20/2003 - Added simplified macros and removed NAME requirement

!ifndef SELECTLIB
  !define SELECTLIB

!define SELECT		"!insertmacro SELECT"
!define CASE			"!insertmacro CASE"
!define CASE_ELSE	"!insertmacro CASE_ELSE"
!define SELECTEND	"!insertmacro SELECTEND"

!macro SELECT VALUE
  !define SELECT_NAME		"${__LINE__}"
  !define SELECT_VALUE	"${VALUE}"
  !define SELECT_COUNT	"${__LINE__}"
  Goto "lbl_${SELECT_NAME}_${SELECT_COUNT}"
!macroend

!macro CASE VALUES
  Goto "lbl_${SELECT_NAME}"
  "lbl_${SELECT_NAME}_${SELECT_COUNT}:"

  !undef SELECT_COUNT
  !define SELECT_COUNT "${__LINE__}"

  Push $R1 ;counter
  Push $R2 ;value
  Push $R3 ;return
  StrCpy $R1 "${VALUES};"
  "lbl_${SELECT_NAME}_${SELECT_COUNT}_loop:"
    StrCmp $R1 "" "lbl_${SELECT_NAME}_${SELECT_COUNT}"
    Push "$R1"
    Push ";"
    Call StrTok
    Pop $R2
    Pop $R1
    StrCmp $R2 "${SELECT_VALUE}" "lbl_${SELECT_NAME}_${SELECT_COUNT}_done" "lbl_${SELECT_NAME}_${SELECT_COUNT}_loop"

  "lbl_${SELECT_NAME}_${SELECT_COUNT}_done:"
  Pop $R3
  Pop $R2
  Pop $R1

  ;StrCmp ${VALUES} "${SELECT_VALUE}" 0 "lbl_${SELECT_NAME}_${SELECT_COUNT}"
!macroend

!macro CASE_ELSE
  Goto "lbl_${SELECT_NAME}"
  "lbl_${SELECT_NAME}_${SELECT_COUNT}:"
!macroend

!macro SELECTEND
  lbl_${SELECT_NAME}:
  !undef SELECT_NAME
  !undef SELECT_VALUE
  !undef SELECT_COUNT
!macroend

Function StrTok
  Exch $R1
  Exch 1
  Exch $R0
  Push $R2
  Push $R3
  Push $R4
  Push $R5
  StrLen $R2 $R0
  IntOp $R2 $R2 + 1
  loop1:
    IntOp $R2 $R2 - 1
    IntCmp $R2 0 exit
    StrCpy $R4 $R0 1 -$R2
    StrLen $R3 $R1
    IntOp $R3 $R3 + 1
    loop2:
      IntOp $R3 $R3 - 1
      IntCmp $R3 0 loop1
      StrCpy $R5 $R1 1 -$R3
      StrCmp $R4 $R5 Found
    Goto loop2
  Goto loop1
  exit:
  StrCpy $R1 ""
  StrCpy $R0 ""
  Goto Cleanup
  Found:
  StrLen $R3 $R0
  IntOp $R3 $R3 - $R2
  StrCpy $R1 $R0 $R3
  IntOp $R2 $R2 - 1
  IntOp $R3 $R3 + 1
  StrCpy $R0 $R0 $R2 $R3
  Cleanup:
  Pop $R5
  Pop $R4
  Pop $R3
  Pop $R2
  Exch $R0
  Exch 1
  Exch $R1
FunctionEnd

!endif ;SELECTLIB