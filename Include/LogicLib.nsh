; NSIS LOGIC LIBRARY - logiclib.nsh
; Version 1.3 - 09/22/2003
; Questions/Comments - dselkirk@hotmail.com
;
; Description:
;   Provides the use of select and if statements within NSIS.
;
; Notes:
;   Only limitation is the statement depth. You can have no more than 10 statements
;   within each other.
;     ${IF}       - 1
;       ${IF}     - 2
;         ${IF}   - 3 ... etc.
;
; Usage:
;   Select Statements -
;     ${SELECT} "test1"
;       ${CASE} "test1"
;         MessageBox MB_OK "case: test1"
;       ${CASE} "test 2;test3"
;         MessageBox MB_OK "case: test2 or test3"
;       ${CASE_ELSE}
;         MessageBox MB_OK "case else"
;     ${ENDSELECT}
;
;   If Statements -
;     ${IF} 5 > 10
;       MessageBox MB_OK "if: 5>10"
;     ${ELSEIF} 5 = 10
;       MessageBox MB_OK "elseif: 5>10"
;     ${ELSE}
;       MessageBox MB_OK "else"
;     ${ENDIF}
;
; History:
;		1.0 - 09/19/2003 - Initial release.
;		1.1 - 09/20/2003 - Added simplified macros and removed NAME requirement.
;		1.2 - 09/21/2003 - Changed library name to Lib.
;                    - Allow for 5 statements deep without use of name variable.
;                    - Added If..ElseIf..Else..Endif statements.
;		1.3 - 09/22/2003 - Fixed maximum allow statements.
;                    - Now allows 10 statement depth.
;                    - Condensed code.

!ifndef LOGICLIB
  !define LOGICLIB

  ; check if defines are already in use
  !ifdef LOGIC_COUNTER \
    | LOGIC_DELIMITER \
    | LOGIC_NEXT \
    | LOGIC_STEP \
    | LOGIC_STEP_1 \
    | LOGIC_STEP_2 \
    | LOGIC_STEP_3 \
    | LOGIC_STEP_4 \
    | LOGIC_STEP_5 \
    | LOGIC_STEP_6 \
    | LOGIC_STEP_7 \
    | LOGIC_STEP_8 \
    | LOGIC_STEP_9 \
    | LOGIC_STEP_10 \
    | SELECT \
    | CASE \
    | CASE_ELSE \
    | SELECTEND \
    | IF \
    | ELSEIF \
    | ELSE \
    | ENDIF
    !error "Defines required for this library are already in use."
  !endif

  ; change if requried
  !define LOGIC_DELIMITER ";"

  ; create quick macros
  !define SELECT "!insertmacro SELECT"
  !define CASE "!insertmacro CASE"
  !define CASE_ELSE "!insertmacro CASE_ELSE"
  !define ENDSELECT "!insertmacro ENDSELECT"

  !define IF "!insertmacro IF"
  !define ELSEIF "!insertmacro ELSEIF"
  !define ELSE "!insertmacro ELSE"
  !define ENDIF "!insertmacro ENDIF"

  ; set local parameters based on counter and step
  !macro LOGIC_DEFINES
    !ifdef LOGIC_NEXT
      !undef LOGIC_NEXT
    !endif
    !ifdef LOGIC_CURRENT
      !undef LOGIC_CURRENT
    !endif
    !ifdef LOGIC_END
      !undef LOGIC_END
    !endif
    !define LOGIC_END "LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_END"

    !define LOGIC_CURRENT "${LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_LINE}"
    !define LOGIC_NEXT "LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_${LOGIC_CURRENT}"
  !macroend

  ; start new statment
  !macro LOGIC_START
    !ifdef LOGIC_STEP
      !undef LOGIC_STEP
      !ifdef LOGIC_STEP_2
        !ifdef LOGIC_STEP_3
          !ifdef LOGIC_STEP_4
            !ifdef LOGIC_STEP_5
              !ifdef LOGIC_STEP_6
                !ifdef LOGIC_STEP_7
                  !ifdef LOGIC_STEP_8
                    !ifdef LOGIC_STEP_9
                      !ifdef LOGIC_STEP_10
                        !error "Maximum statement depth reached."
                      !else
                        !define LOGIC_STEP 10
                        !define LOGIC_STEP_10
                      !endif
                    !else
                      !define LOGIC_STEP 9
                      !define LOGIC_STEP_9
                    !endif
                  !else
                    !define LOGIC_STEP 8
                    !define LOGIC_STEP_8
                  !endif
                !else
                  !define LOGIC_STEP 7
                  !define LOGIC_STEP_7
                !endif
              !else
                !define LOGIC_STEP 6
                !define LOGIC_STEP_6
              !endif
            !else
              !define LOGIC_STEP 5
              !define LOGIC_STEP_5
            !endif
          !else
            !define LOGIC_STEP 4
            !define LOGIC_STEP_4
          !endif
        !else
          !define LOGIC_STEP 3
          !define LOGIC_STEP_3
        !endif
      !else
        !define LOGIC_STEP 2
        !define LOGIC_STEP_2
      !endif
    !else
      !define LOGIC_STEP 1
      !define LOGIC_STEP_1

      !ifdef LOGIC_COUNTER
        !undef LOGIC_COUNTER
      !endif
      !define LOGIC_COUNTER ${__LINE__}
    !endif
    !define "LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_LINE" "${__LINE__}"
    !insertmacro LOGIC_DEFINES
    Goto "${LOGIC_NEXT}"
  !macroend

  ; complete last statement and increment line number
  !macro LOGIC_NEXT
    !insertmacro LOGIC_DEFINES
    Goto "${LOGIC_END}"
    "${LOGIC_NEXT}:"
    !undef "LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_LINE"
    !define "LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_LINE" "${__LINE__}"
    !insertmacro LOGIC_DEFINES
  !macroend

  ; complete statement and cleanup defines
  !macro LOGIC_END
    !insertmacro LOGIC_DEFINES
    Goto "${LOGIC_END}"
    Goto "${LOGIC_NEXT}"
    "${LOGIC_NEXT}:"
    "${LOGIC_END}:"

    !undef LOGIC_END
    !undef LOGIC_NEXT
    !undef "LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_LINE"
    !undef LOGIC_CURRENT

    !undef LOGIC_STEP
    !ifndef LOGIC_STEP_10
      !ifndef LOGIC_STEP_9
        !ifndef LOGIC_STEP_8
          !ifndef LOGIC_STEP_7
            !ifndef LOGIC_STEP_6
              !ifndef LOGIC_STEP_5
                !ifndef LOGIC_STEP_4
                  !ifndef LOGIC_STEP_3
                    !ifndef LOGIC_STEP_2
                      !ifndef LOGIC_STEP_1
                        !undef LOGIC_STEP_1
                      !endif
                    !else
                      !undef LOGIC_STEP_2
                      !define LOGIC_STEP 1
                      !endif
                  !else
                    !undef LOGIC_STEP_3
                    !define LOGIC_STEP 2
                  !endif
                !else
                  !undef LOGIC_STEP_4
                  !define LOGIC_STEP 3
                !endif
              !else
                !undef LOGIC_STEP_5
                !define LOGIC_STEP 4
              !endif
            !else
              !undef LOGIC_STEP_5
              !define LOGIC_STEP 4
            !endif
          !else
            !undef LOGIC_STEP_6
            !define LOGIC_STEP 5
          !endif
        !else
          !undef LOGIC_STEP_8
          !define LOGIC_STEP 7
        !endif
      !else
        !undef LOGIC_STEP_9
        !define LOGIC_STEP 8
      !endif
    !else
      !undef LOGIC_STEP_10
      !define LOGIC_STEP 9
    !endif
  !macroend

  !macro ELSEIF VALUE1 OP VALUE2
    !insertmacro LOGIC_NEXT

    StrCmp "${OP}" "<>" "${LOGIC_NEXT}_NotEqual"
    StrCmp "${OP}" "<" "${LOGIC_NEXT}_Less"
    StrCmp "${OP}" ">" "${LOGIC_NEXT}_Greater"
    StrCmp "${OP}" "<=" "${LOGIC_NEXT}_LessEqual"
    StrCmp "${OP}" ">=" "${LOGIC_NEXT}_GreaterEqual"
    StrCmp "${OP}" "=" "${LOGIC_NEXT}_Equal" 0 ;default

    "${LOGIC_NEXT}_Equal:"
      StrCmp "${VALUE1}" "${VALUE2}" 0 "${LOGIC_NEXT}"
      Goto "${LOGIC_NEXT}_done"

    "${LOGIC_NEXT}_NotEqual:"
      StrCmp "${VALUE1}" "${VALUE2}" "${LOGIC_NEXT}" 0
      Goto "${LOGIC_NEXT}_done"

    "${LOGIC_NEXT}_Less:"
      IntCmp "${VALUE1}" "${VALUE2}" "${LOGIC_NEXT}" 0 "${LOGIC_NEXT}"
      Goto "${LOGIC_NEXT}_done"

    "${LOGIC_NEXT}_Greater:"
      IntCmp "${VALUE1}" "${VALUE2}" "${LOGIC_NEXT}" "${LOGIC_NEXT}" 0
      Goto "${LOGIC_NEXT}_done"

    "${LOGIC_NEXT}_LessEqual:"
      IntCmp "${VALUE1}" "${VALUE2}" 0 0 "${LOGIC_NEXT}"
      Goto "${LOGIC_NEXT}_done"

    "${LOGIC_NEXT}_GreaterEqual:"
      IntCmp "${VALUE1}" "${VALUE2}" 0 "${LOGIC_NEXT}" 0
      Goto "${LOGIC_NEXT}_done"

    "${LOGIC_NEXT}_done:"
  !macroend

  !macro IF VALUE1 OP VALUE2
    !insertmacro LOGIC_START
    !insertmacro ELSEIF "${VALUE1}" "${OP}" "${VALUE2}"
  !macroend

  !macro ELSE
    !insertmacro LOGIC_NEXT
  !macroend

  !macro ENDIF
    !insertmacro LOGIC_END
  !macroend

  !macro SELECT VALUE
    !insertmacro LOGIC_START
    !define "LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_VALUE" "${VALUE}"
  !macroend

  !macro CASE VALUES
    !insertmacro LOGIC_NEXT
    Push $R1 ;counter
    Push $R2 ;value
    Push $R3 ;return
    StrCpy $R1 "${VALUES};"
    "${LOGIC_NEXT}_loop:"
    StrCmp $R1 "" "${LOGIC_NEXT}"
    Push "$R1"
    Push ";"
    Call StrTok
    Pop $R2
    Pop $R1
    StrCmp $R2 "${LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_VALUE}" "${LOGIC_NEXT}_done" "${LOGIC_NEXT}_loop"
    "${LOGIC_NEXT}_done:"
    Pop $R3
    Pop $R2
    Pop $R1
  !macroend

  !macro CASE_ELSE
    !insertmacro LOGIC_NEXT
  !macroend

  !macro ENDSELECT
!undef "LOGIC_${LOGIC_COUNTER}_${LOGIC_STEP}_VALUE"
    !insertmacro LOGIC_END
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
  Return
FunctionEnd

!endif ; LOGICLIB