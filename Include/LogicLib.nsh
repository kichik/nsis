; NSIS LOGIC LIBRARY - logiclib.nsh
; Version 2.4 - 12/13/2003
; By dselkirk@hotmail.com
; and eccles@users.sf.net
;
; Questions/Comments -
; See http://forums.winamp.com/showthread.php?s=&postid=1116241
;
; Description:
;   Provides the use of various logic statements within NSIS.
;
; Notes:
;   Version 2 is a complete rewrite of the original. Here are some of the major differences:
;   - Structure redesign based upon version by eccles.
;   - No statement limitations.
;   - Following statements are now available.
;       if/unless..elseif/unless..else..endif/unless
;         - Conditionally executes a group of statements, depending on the value of an expression.
;       ifthen..|..|
;         - Conditionally executes an inline statement, depending on the value of an expression.
;       ifcmd..||..|
;         - Conditionally executes an inline statement, depending on a True value of the provided NSIS function.
;       select..case..case2..case3..case4..case5..caseelse..endselect
;         - Executes one of several groups of statements, depending on the value of an expression.
;       switch..case..default..endswitch
;         - As above but with C-like features and behaviour.
;       for..exitfor..continue..break..next
;         - Repeats a group of statements a specified number of times.
;       foreach..exitfor..continue..break..next
;         - Repeats a group of statements a specified number of times stepping in order specified.
;       do..exitdo..continue..break..loop
;         - Repeats a block of statements until stopped.
;       dowhile..exitdo..continue..break..loop
;         - Repeats a block of statements while a condition is True.
;       dountil..exitdo..continue..break..loop
;         - Repeats a block of statements until a condition is True.
;       do..exitdo..continue..break..loopwhile
;         - Repeats a block of statements while a condition is True.
;       do..exitdo..continue..break..loopuntil
;         - Repeats a block of statements until a condition is True.
;       while..exitwhile..continue..break..endwhile
;         - Same as dowhile..loop.
;
; Usage:
;   See example.nsi
;
; History:
;   1.0 - 09/19/2003 - Initial release.
;   1.1 - 09/20/2003 - Added simplified macros and removed NAME requirement.
;   1.2 - 09/21/2003 - Changed library name to LogicLib.
;                         - Allow for 5 statements deep without use of name variable.
;                         - Added If..ElseIf..Else..Endif statements.
;   1.3 - 09/22/2003 - Fixed maximum allow statements.
;                         - Now allows 10 statement depth.
;                         - Condensed code.
;   2.0 - 10/03/2003 - Inital release 2, see notes.
;   2.1 - 10/05/2003 - Added continue and break labels to repeat type statements.
;   2.2 - 10/07/2003 - Updates by eccles
;                         - Simplified IfThen by utilising If and EndIf.
;                         - Simplified For by utilising ForEach.
;                         - Fixed ForEach missing the final iteration.
;                         - Fixed a couple of Break/Continue bugs.
;   2.3 - 12/10/2003 - Much reworking and refactoring of things to help reduce
;                      duplication, etc. E.g. all loop varieties now go through
;                      a common set of macros.
;                    - Added built-in support for the rest of NSIS's built-in
;                      conditional tests (Abort, Errors, FileExists, RebootFlag,
;                      Silent).
;                    - Added ability to use any NSIS conditional command in a
;                      normal If type statement (no longer restricted to the
;                      specialised IfCmd statement).
;                    - Optimised the code produced by If (fewer Goto's).
;                    - Added statement similar to If that works in reverse:
;                      "Unless" executes the code in the contained block if the
;                      condition is false. If, Unless, ElseIf, ElseUnless, EndIf
;                      and ElseUnless can be used freely in any combination.
;                    - Fixed bug where using Continue in a Do..LoopUntil loop
;                      went to the top of the loop and not the loop condition.
;                    - Added DoWhile..Loop and Do..LoopWhile loop varieties (the
;                      existing While..EndWhile loop is still available and is
;                      identical to DoWhile..Loop).
;                    - Optimised the code prodiced by Select (fewer Goto's).
;                    - Renamed Case_Else to CaseElse (nothing else has an
;                      underscore so why should that one). The old name is still
;                      available too though (if you must).
;                    - CaseElse can also be called Default (for the C-minded).
;   2.4 - 12/13/2003 - Added Switch..Case*/Default..EndSwitch: similar to Select
;                      but behaves just like the C version. I.e.:
;                         - Each Case is more like a label than a block so
;                           execution "falls through" unless you use Break.
;                         - CaseElse (or Default) does not have to be the final
;                           case.
;                         - Case*/Default can appear anywhere inside the Switch
;                           (e.g. inside an If inside the Switch).
;                      (With thanks to kichik for the idea and proof-of-concept
;                      model).
;                    - Added unsigned integer comparisons U<, U>=, U> and U<=.
;                    - Added 64-bit integer comparisons L=, L<>, L<, L>=, L> and
;                      L<= (these use System.dll).
;                    - Added case-sensitive string tests S== and S!= (these use
;                      System.dll).
;                    - Added string comparisons (not case sensitive) S<, S>=, S>
;                      and S<= (these use System.dll).
;                    - Added section flag tests (SectionIsSelected, etc.) (to
;                      use these your script must include sections.nsh).

!verbose push
!verbose 3
!ifndef LOGICLIB_VERBOSITY
  !define LOGICLIB_VERBOSITY 3
!endif
!define _LOGICLIB_VERBOSITY ${LOGICLIB_VERBOSITY}
!undef LOGICLIB_VERBOSITY
!verbose ${_LOGICLIB_VERBOSITY}

!ifndef LOGICLIB
  !define LOGICLIB
  !define | "'"
  !define || "' '"

  Var _LOGICLIB_TEMP  ; Temporary variable to aid the more elaborate logic tests

  !macro _PushLogic
    !insertmacro _PushScope Logic _${__LINE__}
  !macroend

  !macro _PopLogic
    !insertmacro _PopScope Logic
  !macroend

  !macro _PushScope Type label
    !ifdef _${Type}                                       ; If we already have a statement
      !define _Cur${Type} ${_${Type}}
      !undef _${Type}
      !define _${Type} ${label}
      !define ${_${Type}}Prev${Type} ${_Cur${Type}}       ; Save the current logic
      !undef _Cur${Type}
    !else
      !define _${Type} ${label}                           ; Initialise for first statement
    !endif
  !macroend

  !macro _PopScope Type
    !ifndef _${Type}
      !error "Cannot use _Pop${Type} without a preceding _Push${Type}"
    !endif
    !ifdef ${_${Type}}Prev${Type}                         ; If a previous statment was active then restore it
      !define _Cur${Type} ${_${Type}}
      !undef _${Type}
      !define _${Type} ${${_Cur${Type}}Prev${Type}}
      !undef ${_Cur${Type}}Prev${Type}
      !undef _Cur${Type}
    !else
      !undef _${Type}
    !endif
  !macroend

  ; String tests
  !macro _== _a _b _t _f
    StrCmp `${_a}` `${_b}` `${_t}` `${_f}`
  !macroend

  !macro _!= _a _b _t _f
    !insertmacro _== `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  ; Case-sensitive string tests
  !macro _StrCmp _a _b _e _l _m
    System::Call `kernel32::lstrcmpA(ts, ts) i.s` `${_a}` `${_b}`
    Pop $_LOGICLIB_TEMP
    IntCmp $_LOGICLIB_TEMP 0 `${_e}` `${_l}` `${_m}`
  !macroend

  !macro _S== _a _b _t _f
    !insertmacro _StrCmp `${_a}` `${_b}` `${_t}` `${_f}` `${_f}`
  !macroend

  !macro _S!= _a _b _t _f
    !insertmacro _S== `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  ; Extra string tests (cannot do these case-sensitively - I tried and lstrcmp still ignored the case)
  !macro _StrCmpI _a _b _e _l _m
    System::Call `kernel32::lstrcmpiA(ts, ts) i.s` `${_a}` `${_b}`
    Pop $_LOGICLIB_TEMP
    IntCmp $_LOGICLIB_TEMP 0 `${_e}` `${_l}` `${_m}`
  !macroend

  !macro _S< _a _b _t _f
    !insertmacro _StrCmpI `${_a}` `${_b}` `${_f}` `${_t}` `${_f}`
  !macroend

  !macro _S>= _a _b _t _f
    !insertmacro _S< `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  !macro _S> _a _b _t _f
    !insertmacro _StrCmpI `${_a}` `${_b}` `${_f}` `${_f}` `${_t}`
  !macroend

  !macro _S<= _a _b _t _f
    !insertmacro _S> `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  ; Integer tests
  !macro _= _a _b _t _f
    IntCmp `${_a}` `${_b}` `${_t}` `${_f}` `${_f}`
  !macroend

  !macro _<> _a _b _t _f
    !insertmacro _= `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  !macro _< _a _b _t _f
    IntCmp `${_a}` `${_b}` `${_f}` `${_t}` `${_f}`
  !macroend

  !macro _>= _a _b _t _f
    !insertmacro _< `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  !macro _> _a _b _t _f
    IntCmp `${_a}` `${_b}` `${_f}` `${_f}` `${_t}`
  !macroend

  !macro _<= _a _b _t _f
    !insertmacro _> `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  ; Unsigned integer tests (NB: no need for extra equality tests)
  !macro _U< _a _b _t _f
    IntCmpU `${_a}` `${_b}` `${_f}` `${_t}` `${_f}`
  !macroend

  !macro _U>= _a _b _t _f
    !insertmacro _U< `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  !macro _U> _a _b _t _f
    IntCmpU `${_a}` `${_b}` `${_f}` `${_f}` `${_t}`
  !macroend

  !macro _U<= _a _b _t _f
    !insertmacro _U> `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  ; Int64 tests
  !macro _Int64Cmp _a _o _b _t _f
    System::Int64Op `${_a}` `${_o}` `${_b}`
    Pop $_LOGICLIB_TEMP
    !insertmacro _= $_LOGICLIB_TEMP 0 `${_f}` `${_t}`
  !macroend

  !macro _L= _a _b _t _f
    !insertmacro _Int64Cmp `${_a}` = `${_b}` `${_t}` `${_f}`
  !macroend

  !macro _L<> _a _b _t _f
    !insertmacro _L= `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  !macro _L< _a _b _t _f
    !insertmacro _Int64Cmp `${_a}` < `${_b}` `${_t}` `${_f}`
  !macroend

  !macro _L>= _a _b _t _f
    !insertmacro _L< `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  !macro _L> _a _b _t _f
    !insertmacro _Int64Cmp `${_a}` > `${_b}` `${_t}` `${_f}`
  !macroend

  !macro _L<= _a _b _t _f
    !insertmacro _L> `${_a}` `${_b}` `${_f}` `${_t}`
  !macroend

  ; Flag tests
  !macro _Abort _a _b _t _f
    IfAbort `${_t}` `${_f}`
  !macroend
  !define Abort `"" Abort ""`

  !macro _Errors _a _b _t _f
    IfErrors `${_t}` `${_f}`
  !macroend
  !define Errors `"" Errors ""`

  !macro _FileExists _a _b _t _f
    IfFileExists `${_b}` `${_t}` `${_f}`
  !macroend
  !define FileExists `"" FileExists`

  !macro _RebootFlag _a _b _t _f
    IfRebootFlag `${_t}` `${_f}`
  !macroend
  !define RebootFlag `"" RebootFlag ""`

  !macro _Silent _a _b _t _f
    IfSilent `${_t}` `${_f}`
  !macroend
  !define Silent `"" Silent ""`

  ; "Any instruction" test
  !macro _Cmd _a _b _t _f
    !define _t=${_t}
    !ifdef _t=
      !define __t +2                                      ; If no jump then make sure we skip the Goto below
    !else
      !define __t ${_t}
    !endif
    !undef _t=${_t}
    ${_b} ${__t}
    !undef __t
    Goto ${_f}
  !macroend
  !define Cmd `"" Cmd`

  ; Section flag test
  !macro _SectionFlagIsSet _a _b _t _f
    SectionGetFlags `${_b}` $_LOGICLIB_TEMP
    IntOp $_LOGICLIB_TEMP $_LOGICLIB_TEMP & `${_a}`
    !insertmacro _= $_LOGICLIB_TEMP `${_a}` `${_t}` `${_f}`
  !macroend
  !define SectionIsSelected `${SF_SELECTED} SectionFlagIsSet`
  !define SectionIsSubSection `${SF_SUBSEC} SectionFlagIsSet`
  !define SectionIsSubSectionEnd `${SF_SUBSECEND} SectionFlagIsSet`
  !define SectionIsBold `${SF_BOLD} SectionFlagIsSet`
  !define SectionIsReadOnly `${SF_RO} SectionFlagIsSet`
  !define SectionIsExpanded `${SF_EXPAND} SectionFlagIsSet`
  !define SectionIsPartiallySelected `${SF_PSELECTED} SectionFlagIsSet`

  !define IfCmd `!insertmacro _IfThen "" Cmd ${|}`

  !macro _If _c _a _o _b
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !insertmacro _PushLogic
    !define ${_Logic}If
    !define ${_Logic}Else _${__LINE__}                    ; Get a label for the Else
    !define _c=${_c}
    !ifdef _c=true                                        ; If is true
      !insertmacro _${_o} `${_a}` `${_b}` "" ${${_Logic}Else}
    !else                                                 ; If condition is false
      !insertmacro _${_o} `${_a}` `${_b}` ${${_Logic}Else} ""
    !endif
    !undef _c=${_c}
    !verbose pop
  !macroend
  !define If     `!insertmacro _If true`
  !define Unless `!insertmacro _If false`

  !macro _Else
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !ifndef _Logic | ${_Logic}If
      !error "Cannot use Else without a preceding If or Unless"
    !endif
    !ifndef ${_Logic}Else
      !error "Cannot use Else following an Else"
    !endif
    !ifndef ${_Logic}EndIf                                ; First Else for this If?
      !define ${_Logic}EndIf _${__LINE__}                 ; Get a label for the EndIf
    !endif
    Goto ${${_Logic}EndIf}                                ; Go to the EndIf
    ${${_Logic}Else}:                                     ; Place the Else label
    !undef ${_Logic}Else                                  ; and remove it
    !verbose pop
  !macroend
  !define Else `!insertmacro _Else`

  !macro _ElseIf _c _a _o _b
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    ${Else}                                               ; Perform the Else
    !define ${_Logic}Else _${__LINE__}                    ; Get a label for the next Else and perform the new If
    !define _c=${_c}
    !ifdef _c=true                                        ; If is true
      !insertmacro _${_o} `${_a}` `${_b}` "" ${${_Logic}Else}
    !else                                                 ; If condition is false
      !insertmacro _${_o} `${_a}` `${_b}` ${${_Logic}Else} ""
    !endif
    !undef _c=${_c}
    !verbose pop
  !macroend
  !define ElseIf     `!insertmacro _ElseIf true`
  !define ElseUnless `!insertmacro _ElseIf false`

  !macro _EndIf _n
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !ifndef _Logic | ${_Logic}If
      !error "Cannot use End${_n} without a preceding If or Unless"
    !endif
    !ifdef ${_Logic}Else
      ${${_Logic}Else}:                                   ; Place the Else label
      !undef ${_Logic}Else                                ; and remove it
    !endif
    !ifdef ${_Logic}EndIf
      ${${_Logic}EndIf}:                                  ; Place the EndIf
      !undef ${_Logic}EndIf                               ; and remove it
    !endif
    !undef ${_Logic}If
    !insertmacro _PopLogic
    !verbose pop
  !macroend
  !define EndIf     `!insertmacro _EndIf If`
  !define EndUnless `!insertmacro _EndIf Unless`

  !macro _IfThen _a _o _b _t
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    ${If} `${_a}` `${_o}` `${_b}`
      ${_t}
    ${EndIf}
    !verbose pop
  !macroend
  !define IfThen `!insertmacro _IfThen`

  !macro _ForEach _v _f _t _o _s
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    StrCpy "${_v}" "${_f}"                                ; Assign the initial value
    Goto +2                                               ; Skip the loop expression for the first iteration
    !define _DoLoopExpression `IntOp "${_v}" "${_v}" "${_o}" "${_s}"` ; Define the loop expression
    !define _o=${_o}
    !ifdef _o=+                                           ; Check the loop expression operator
      !define __o >                                       ; to determine the correct loop condition
    !else ifdef _o=-
      !define __o <
    !else
      !error "Unsupported ForEach step operator (must be + or -)"
    !endif
    !undef _o=${_o}
    !insertmacro _Do For false `${_v}` `${__o}` `${_t}`   ; Let Do do the rest
    !undef __o
    !verbose pop
  !macroend
  !define ForEach `!insertmacro _ForEach`

  !macro _For _v _f _t
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    ${ForEach} `${_v}` `${_f}` `${_t}` + 1                ; Pass on to ForEach
    !verbose pop
  !macroend
  !define For `!insertmacro _For`

  !define ExitFor `!insertmacro _Goto ExitFor For`

  !define Next      `!insertmacro _Loop For Next "" "" "" ""`

  !define While     `!insertmacro _Do While true`

  !define ExitWhile `!insertmacro _Goto ExitWhile While`

  !define EndWhile  `!insertmacro _Loop While EndWhile "" "" "" ""`

  !macro _Do _n _c _a _o _b
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !insertmacro _PushLogic
    !define ${_Logic}${_n} _${__LINE__}                   ; Get a label for the start of the loop
    ${${_Logic}${_n}}:
    !insertmacro _PushScope Exit${_n} _${__LINE__}        ; Get a label for the end of the loop
    !insertmacro _PushScope Break ${_Exit${_n}}           ; Break goes to the end of the loop
    !ifdef _DoLoopExpression
      ${_DoLoopExpression}                                ; Special extra parameter for inserting code
      !undef _DoLoopExpression                            ; between the Continue label and the loop condition
    !endif
    !define _c=${_c}
    !ifdef _c=                                            ; No starting condition
      !insertmacro _PushScope Continue _${__LINE__}       ; Get a label for Continue at the end of the loop
    !else
      !insertmacro _PushScope Continue ${${_Logic}${_n}}  ; Continue goes to the start of the loop
      !ifdef _c=true                                      ; If is true
        !insertmacro _${_o} `${_a}` `${_b}` "" ${_Exit${_n}}
      !else                                               ; If condition is false
        !insertmacro _${_o} `${_a}` `${_b}` ${_Exit${_n}} ""
      !endif
    !endif
    !undef _c=${_c}
    !define ${_Logic}Condition ${_c}                      ; Remember the condition used
    !verbose pop
  !macroend
  !define Do      `!insertmacro _Do Do "" "" "" ""`
  !define DoWhile `!insertmacro _Do Do true`
  !define DoUntil `!insertmacro _Do Do false`

  !macro _Goto _n _s
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !ifndef _${_n}
      !error "Cannot use ${_n} without a preceding ${_s}"
    !endif
    Goto ${_${_n}}
    !verbose pop
  !macroend
  !define ExitDo   `!insertmacro _Goto ExitDo Do`

  !macro _Loop _n _e _c _a _o _b
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !ifndef _Logic | ${_Logic}${_n}
      !error "Cannot use ${_e} without a preceding ${_n}"
    !endif
    !define _c=${${_Logic}Condition}
    !ifdef _c=                                            ; If Do had no condition place the Continue label
      ${_Continue}:
    !endif
    !undef _c=${${_Logic}Condition}
    !define _c=${_c}
    !ifdef _c=                                            ; No ending condition
      Goto ${${_Logic}${_n}}
    !else ifdef _c=true                                   ; If condition is true
      !insertmacro _${_o} `${_a}` `${_b}` ${${_Logic}${_n}} ${_Exit${_n}}
    !else                                                 ; If condition is false
      !insertmacro _${_o} `${_a}` `${_b}` ${_Exit${_n}} ${${_Logic}${_n}}
    !endif
    !undef _c=${_c}
    Goto ${_Continue}                                     ; Just to ensure it is referenced at least once
    ${_Exit${_n}}:                                        ; Place the loop exit point
    !undef ${_Logic}Condition
    !insertmacro _PopScope Continue
    !insertmacro _PopScope Break
    !insertmacro _PopScope Exit${_n}
    !undef ${_Logic}${_n}
    !insertmacro _PopLogic
    !verbose pop
  !macroend
  !define Loop      `!insertmacro _Loop Do Loop "" "" "" ""`
  !define LoopWhile `!insertmacro _Loop Do LoopWhile true`
  !define LoopUntil `!insertmacro _Loop Do LoopUntil false`

  !define Continue `!insertmacro _Goto Continue "For or Do or While"`
  !define Break    `!insertmacro _Goto Break "For or Do or While"`

  !macro _Select _a
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !insertmacro _PushLogic
    !define ${_Logic}Select `${_a}`                       ; Remember the left hand side of the comparison
    !verbose pop
  !macroend
  !define Select `!insertmacro _Select`

  !macro _Select_CaseElse
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !ifndef _Logic | ${_Logic}Select
      !error "Cannot use Case without a preceding Select"
    !endif
    !ifdef ${_Logic}EndSelect                             ; This is set only after the first case
      !ifndef ${_Logic}Else
        !error "Cannot use Case following a CaseElse"
      !endif
      Goto ${${_Logic}EndSelect}                          ; Go to the EndSelect
      ${${_Logic}Else}:                                   ; Place the Else label
      !undef ${_Logic}Else                                ; and remove it
    !else
      !define ${_Logic}EndSelect _${__LINE__}             ; Get a label for the EndSelect
    !endif
    !verbose pop
  !macroend
  !define CaseElse `!insertmacro _CaseElse`
  !define Case_Else `!insertmacro _CaseElse`              ; Compatibility with 2.2 and earlier
  !define Default `!insertmacro _CaseElse`                ; For the C-minded

  !macro _Select_Case _a
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    ${CaseElse}                                           ; Perform the CaseElse
    !define ${_Logic}Else _${__LINE__}                    ; Get a label for the next Else and perform the new Case
    !insertmacro _== `${${_Logic}Select}` `${_a}` "" ${${_Logic}Else}
    !verbose pop
  !macroend
  !define Case `!insertmacro _Case`

  !macro _Case2 _a _b
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    ${CaseElse}                                           ; Perform the CaseElse
    !define ${_Logic}Else _${__LINE__}                    ; Get a label for the next Else and perform the new Case
    !insertmacro _== `${${_Logic}Select}` `${_a}` +2 ""
    !insertmacro _== `${${_Logic}Select}` `${_b}` "" ${${_Logic}Else}
    !verbose pop
  !macroend
  !define Case2 `!insertmacro _Case2`

  !macro _Case3 _a _b _c
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    ${CaseElse}                                           ; Perform the CaseElse
    !define ${_Logic}Else _${__LINE__}                    ; Get a label for the next Else and perform the new Case
    !insertmacro _== `${${_Logic}Select}` `${_a}` +3 ""
    !insertmacro _== `${${_Logic}Select}` `${_b}` +2 ""
    !insertmacro _== `${${_Logic}Select}` `${_c}` "" ${${_Logic}Else}
    !verbose pop
  !macroend
  !define Case3 `!insertmacro _Case3`

  !macro _Case4 _a _b _c _d
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    ${CaseElse}                                           ; Perform the CaseElse
    !define ${_Logic}Else _${__LINE__}                    ; Get a label for the next Else and perform the new Case
    !insertmacro _== `${${_Logic}Select}` `${_a}` +4 ""
    !insertmacro _== `${${_Logic}Select}` `${_b}` +3 ""
    !insertmacro _== `${${_Logic}Select}` `${_c}` +2 ""
    !insertmacro _== `${${_Logic}Select}` `${_d}` "" ${${_Logic}Else}
    !verbose pop
  !macroend
  !define Case4 `!insertmacro _Case4`

  !macro _Case5 _a _b _c _d _e
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    ${CaseElse}                                           ; Perform the CaseElse
    !define ${_Logic}Else _${__LINE__}                    ; Get a label for the next Else and perform the new Case
    !insertmacro _== `${${_Logic}Select}` `${_a}` +5 ""
    !insertmacro _== `${${_Logic}Select}` `${_b}` +4 ""
    !insertmacro _== `${${_Logic}Select}` `${_c}` +3 ""
    !insertmacro _== `${${_Logic}Select}` `${_d}` +2 ""
    !insertmacro _== `${${_Logic}Select}` `${_e}` "" ${${_Logic}Else}
    !verbose pop
  !macroend
  !define Case5 `!insertmacro _Case5`

  !macro _EndSelect
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !ifndef _Logic | ${_Logic}Select
      !error "Cannot use EndSelect without a preceding Select"
    !endif
    !ifdef ${_Logic}Else
      ${${_Logic}Else}:                                   ; Place the Else label
      !undef ${_Logic}Else                                ; and remove it
    !endif
    !ifdef ${_Logic}EndSelect                             ; This won't be set if there weren't any cases
      ${${_Logic}EndSelect}:                              ; Place the EndSelect
      !undef ${_Logic}EndSelect                           ; and remove it
    !endif
    !undef ${_Logic}Select
    !insertmacro _PopLogic
    !verbose pop
  !macroend
  !define EndSelect `!insertmacro _EndSelect`

  !macro _Switch _a
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !insertmacro _PushLogic
    !insertmacro _PushScope Switch ${_Logic}              ; Keep a separate stack for switch data
    !insertmacro _PushScope Break _${__LINE__}            ; Get a lable for beyond the end of the switch
    !define ${_Switch}Var `${_a}`                         ; Remember the left hand side of the comparison
    !define ${_Switch}Tmp "$%TMP%\${__LINE__}.tmp"        ; Get a name for a temporary file
    !system `@echo # logiclib temp file > "${${_Switch}Tmp}"` ; and create it
    !define ${_Logic}Switch _${__LINE__}                  ; Get a label for the end of the switch
    Goto ${${_Logic}Switch}                               ; and go there
    !verbose pop
  !macroend
  !define Switch `!insertmacro _Switch`

  !macro _Case _a
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !ifdef _Logic & ${_Logic}Select                       ; Check for an active Select
      !insertmacro _Select_Case `${_a}`
    !else ifndef _Switch                                  ; If not then check for an active Switch
      !error "Cannot use Case without a preceding Select or Switch"
    !else
      !define _label _${__LINE__}                         ; Get a label for this case,
      ${_label}:                                          ; place it and add it's check to the temp file
      !system `@echo !insertmacro _== $\`${${_Switch}Var}$\` $\`${_a}$\` ${_label} "" >> "${${_Switch}Tmp}"`
      !undef _label
    !endif
    !verbose pop
  !macroend

  !macro _CaseElse
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !ifdef _Logic & ${_Logic}Select                       ; Check for an active Select
      !insertmacro _Select_CaseElse
    !else ifndef _Switch                                  ; If not then check for an active Switch
      !error "Cannot use Case without a preceding Select or Switch"
    !else ifdef ${_Switch}Else                            ; Already had a default case?
      !error "Cannot use CaseElse following a CaseElse"
    !else
      !define ${_Switch}Else _${__LINE__}                 ; Get a label for the default case,
      ${${_Switch}Else}:                                  ; and place it
    !endif
    !verbose pop
  !macroend

  !macro _EndSwitch
    !verbose push
    !verbose ${LOGICLIB_VERBOSITY}
    !ifndef _Logic | ${_Logic}Switch
      !error "Cannot use EndSwitch without a preceding Switch"
    !endif
    Goto ${_Break}                                        ; Skip the jump table
    ${${_Logic}Switch}:                                   ; Place the end of the switch
    !undef ${_Logic}Switch
    !include "${${_Switch}Tmp}"                           ; Include the jump table
    !system `@del /q "${${_Switch}Tmp}"`                  ; and clear it up
    !ifdef ${_Switch}Else                                 ; Was there a default case?
      Goto ${${_Switch}Else}                              ; then go there if all else fails
      !undef ${_Switch}Else
    !endif
    !undef ${_Switch}Tmp
    !undef ${_Switch}Var
    ${_Break}:                                            ; Place the break label
    !insertmacro _PopScope Break
    !insertmacro _PopScope Switch
    !insertmacro _PopLogic
    !verbose pop
  !macroend
  !define EndSwitch `!insertmacro _EndSwitch`

!endif ; LOGICLIB
!verbose 3
!define LOGICLIB_VERBOSITY ${_LOGICLIB_VERBOSITY}
!undef _LOGICLIB_VERBOSITY
!verbose pop
