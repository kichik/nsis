; NSIS LOGIC LIBRARY - logiclib.nsh
; Version 2.1 - 10/05/2003
; Questions/Comments - dselkirk@hotmail.com
; Special thanks to eccles for Push/Pop Logic!
;
; Description:
;   Provides the use of various logic statements within NSIS.
;
; Notes:
;   Version 2 is a complete rewrite of the original. Here are some of the major differences:
;   - Structure redesign based upon version by eccles.
;   - No statement limitations.
;   - Following statements are now available.
;       if..elseif..else..endif
;         - Conditionally executes a group of statements, depending on the value of an expression.
;       ifthen..|..|
;         - Conditionally executes an inline statement, depending on the value of an expression.
;       ifcmd..||..|
;         - Conditionally executes an inline statement, depending on a True value of the provided NSIS function.
;       select..case..case2..case3..case4..case5..case_else..endselect
;         - Executes one of several groups of statements, depending on the value of an expression. 
;       for..exitfor..next
;         - Repeats a group of statements a specified number of times.
;       foreach..exitfor..continue..break..next
;         - Repeats a group of statements a specified number of times stepping in order specified.
;       do..exitdo..continue..break..loop
;         - Repeats a block of statements until stopped.
;       dountil..exitdo..continue..break..loop
;         - Repeats a block of statements until a condition is True.
;       do..exitdo..continue..break..loopuntil
;         - Repeats a block of statements until a condition is True.
;       while..exitwhile..continue..break..endwhile
;         - Executes a series of statements as long as a given condition is True.
;
; Usage:
;   See example.nsi
;
; History:
;		1.0 - 09/19/2003 - Initial release.
;		1.1 - 09/20/2003 - Added simplified macros and removed NAME requirement.
;		1.2 - 09/21/2003 - Changed library name to LogicLib.
;                    			- Allow for 5 statements deep without use of name variable.
;                   			- Added If..ElseIf..Else..Endif statements.
;		1.3 - 09/22/2003 - Fixed maximum allow statements.
;                    			- Now allows 10 statement depth.
;                    			- Condensed code.
;   2.0 - 10/03/2003 - Inital release 2, see notes.
;   2.1 - 10/05/2003 - Added continue and break labels to repeat type statements.

!verbose 3

!ifndef LOGICLIB
  !define LOGICLIB
  !define | "'"
  !define || "' '"

  !macro _PushLogic
    !ifdef _Logic                             ; If we already have a statement
      !define _CurLogic ${_Logic}
      !undef _Logic
      !define _Logic _${__LINE__}
      !define ${_Logic}Prev ${_CurLogic}      ; Save the current logic
      !undef _CurLogic
    !else
      !define _Logic _${__LINE__}             ; Initialise for first statement
    !endif
  !macroend

  !macro _PopLogic
    !ifdef ${_Logic}Prev                      ; If a previous statment was active then restore it
      !define _CurLogic ${_Logic}
      !undef _Logic
      !define _Logic ${${_CurLogic}Prev}
      !undef ${_CurLogic}Prev
      !undef _CurLogic
    !else
      !undef _Logic
    !endif
  !macroend

  !macro _PushCustom Type label
    !ifdef _${Type}                           ; If we already have a statement
      !define _Cur${Type} ${_${Type}}
      !undef _${Type}
      !define _${Type} ${label}
      !define ${_${Type}}Prev${Type} ${_Cur${Type}}      ; Save the current logic
      !undef _Cur${Type}
    !else
      !define _${Type} ${label}             ; Initialise for first statement
    !endif
  !macroend

  !macro _PopCustom Type
    !ifndef _${Type}
      !error "Cannot use _Pop${Type} without a preceding _Push${Type}"
    !endif
    !ifdef ${_${Type}}Prev${Type}                      ; If a previous statment was active then restore it
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
    StrCmp "${_a}" "${_b}" "${_t}" "${_f}"
  !macroend

  !macro _!= _a _b _t _f
    !insertmacro _== "${_a}" "${_b}" "${_f}" "${_t}"
  !macroend

  ; Integer tests
  !macro _= _a _b _t _f
    IntCmp "${_a}" "${_b}" "${_t}" "${_f}" "${_f}"
  !macroend

  !macro _<> _a _b _t _f
    !insertmacro _= "${_a}" "${_b}" "${_f}" "${_t}"
  !macroend

  !macro _< _a _b _t _f
    IntCmp "${_a}" "${_b}" "${_f}" "${_t}" "${_f}"
  !macroend

  !macro _>= _a _b _t _f
    !insertmacro _< "${_a}" "${_b}" "${_f}" "${_t}"
  !macroend

  !macro _> _a _b _t _f
    IntCmp "${_a}" "${_b}" "${_f}" "${_f}" "${_t}"
  !macroend

  !macro _<= _a _b _t _f
    !insertmacro _> "${_a}" "${_b}" "${_f}" "${_t}"
  !macroend

  !macro IfThen _a _o _b _t
    !verbose 3
    !insertmacro _PushLogic
    !define ${_Logic}IfThen _${__LINE__}       ; Get a label for the (provisional) EndIf (it might turn out to be Else)
    !insertmacro _${_o} "${_a}" "${_b}" "" ${${_Logic}IfThen}
    ${_t}
    Goto ${${_Logic}IfThen}
    ${${_Logic}IfThen}:                        ; Place the EndIf
    !undef ${_Logic}IfThen
    !insertmacro _PopLogic
    !verbose 4
  !macroend
  !define IfThen "!insertmacro IfThen"

  !macro IfCmd _a _t
    !verbose 3
    !insertmacro _PushLogic
    !define ${_Logic}IfTrue _${__LINE__}
    !define ${_Logic}IfCmd _${__LINE__}
    ${_a} ${${_Logic}IfTrue}
    Goto ${${_Logic}IfCmd}
    ${${_Logic}IfTrue}:
    ${_t}
    Goto ${${_Logic}IfCmd}
    ${${_Logic}IfCmd}:                        ; Place the EndIf
    !undef ${_Logic}IfTrue
    !undef ${_Logic}IfCmd
    !insertmacro _PopLogic
    !verbose 4
  !macroend
  !define IfCmd "!insertmacro IfCmd '"

  !macro If _a _o _b
    !verbose 3
    !insertmacro _PushLogic
    !define ${_Logic}Elseif _${__LINE__}       ; Get a label for the (provisional) EndIf (it might turn out to be Else)
    !define ${_Logic}EndIf _${__LINE__}       ; Get a label for the (provisional) EndIf (it might turn out to be Else)
    !insertmacro _${_o} "${_a}" "${_b}" "" ${${_Logic}Elseif}
    !verbose 4
  !macroend
  !define If "!insertmacro If"

  !macro Else
    !verbose 3
    !ifndef _Logic | ${_Logic}Elseif
      !error "Cannot use Else|Elseif without a preceding If"
    !endif
    !ifdef ${_Logic}Else
      !error "Cannot use Else after Else"
    !endif
    Goto ${${_Logic}EndIf}
    !define ${_Logic}Else ${${_Logic}Elseif}   ; Save current Else label
    !undef ${_Logic}Elseif
    !define ${_Logic}Elseif _${__LINE__}       ; Get a label for the (new) EndIf and go there
    ${${_Logic}Else}:                         ; Place the saved Else label
    !verbose 4
  !macroend
  !define Else "!insertmacro Else"

  !macro ElseIf _a _o _b
    !verbose 3
    !insertmacro Else                         ; Place in Else code as normal
    !undef ${_Logic}Else                      ; Forget the Else and perform the new If
    !insertmacro _${_o} "${_a}" "${_b}" "" ${${_Logic}Elseif}
    !verbose 4
  !macroend
  !define ElseIf "!insertmacro ElseIf"

  !macro EndIf
    !verbose 3
    !ifndef _Logic | ${_Logic}Elseif
      !error "Cannot use EndIf without a preceding If"
    !endif
    Goto ${${_Logic}EndIf}
    Goto ${${_Logic}ElseIf}
    ${${_Logic}ElseIf}:
    ${${_Logic}EndIf}:                        ; Place the EndIf
    !undef ${_Logic}Elseif
    !undef ${_Logic}EndIf
    !ifdef ${_Logic}Else
      !undef ${_Logic}Else                    ; Clear the Else flag
    !endif
    !insertmacro _PopLogic
    !verbose 4
  !macroend
  !define EndIf "!insertmacro EndIf"

  !macro For _v _f _t
    !verbose 3
    StrCpy ${_v} ${_f}
    !insertmacro _PushLogic
    !define ${_Logic}For _${__LINE__}       ; Get a label for the start of the loop
    !define ${_Logic}For2 _${__LINE__}       ; Get a label for the start of the loop
    !define ${_Logic}Next _${__LINE__}    ; Get a label for the end of the loop
    !insertmacro _PushCustom "ExitFor" ${${_Logic}Next}
    Goto ${${_Logic}For2}
    ${${_Logic}For}:                        ; Insert the loop condition
      IntOp ${_v} ${_v} + 1
    ${${_Logic}For2}:                        ; Insert the loop condition
    !insertmacro _> ${_v} ${_t} ${${_Logic}Next} ""
    !undef ${_Logic}For2
    !verbose 4
  !macroend
  !define For "!insertmacro For"

  !macro ForEach _v _f _t _o _s
    !verbose 3
    StrCpy ${_v} ${_f}
    !insertmacro _PushLogic
    !define ${_Logic}For _${__LINE__}       ; Get a label for the start of the loop
    !define ${_Logic}For2 _${__LINE__}       ; Get a label for the start of the loop
    !define ${_Logic}Next _${__LINE__}    ; Get a label for the end of the loop
    !insertmacro _PushCustom "ExitFor" ${${_Logic}Next}
    !insertmacro _PushCustom "Break" ${${_Logic}Next}
    !insertmacro _PushCustom "Continue" ${${_Logic}For}
    Goto ${${_Logic}For2}
    ${${_Logic}For}:                        ; Insert the loop condition
      IntOp ${_v} ${_v} ${_o} ${_s}
    ${${_Logic}For2}:                        ; Insert the loop condition
    IntCmp ${_v} ${_t} ${${_Logic}Next}
    !undef ${_Logic}For2
    !verbose 4
  !macroend
  !define ForEach "!insertmacro ForEach"

  !define ExitFor "Goto ${_ExitFor}"

  !macro Next
    !verbose 3
    !ifndef _Logic | ${_Logic}For
      !error "Cannot use Next without a preceding For"
    !endif
    Goto ${${_Logic}For} ; Loop back to the For condition
    ${${_Logic}Next}: ; Place the Next
    !undef ${_Logic}For
    !undef ${_Logic}Next
    !insertmacro _PopLogic
    !insertmacro _PopCustom "ExitFor"
    !insertmacro _PushCustom "Break" ${${_Logic}Next}
    !insertmacro _PushCustom "Continue" ${${_Logic}For}
    !verbose 4
  !macroend
  !define Next "!insertmacro Next"

  !macro While _a _o _b
    !verbose 3
    !insertmacro _PushLogic
    !define ${_Logic}While _${__LINE__}       ; Get a label for the start of the loop
    !define ${_Logic}EndWhile _${__LINE__}    ; Get a label for the end of the loop
    !insertmacro _PushCustom "ExitWhile" ${${_Logic}EndWhile}
    !insertmacro _PushCustom "Break" ${${_Logic}EndWhile}
    !insertmacro _PushCustom "Continue" ${${_Logic}While}
    ${${_Logic}While}:                        ; Insert the loop condition
    !insertmacro _${_o} "${_a}" "${_b}" "" ${${_Logic}EndWhile}
    !verbose 4
  !macroend
  !define While "!insertmacro While"

  !define ExitWhile "Goto ${_ExitWhile}"

  !macro EndWhile
    !verbose 3
    !ifndef _Logic | ${_Logic}While
      !error "Cannot use EndWhile without a preceding While"
    !endif
    Goto ${${_Logic}While}                    ; Loop back to the While condition
    ${${_Logic}EndWhile}:                     ; Place the EndWhile
    !undef ${_Logic}While
    !undef ${_Logic}EndWhile
    !insertmacro _PopLogic
    !insertmacro _PopCustom "ExitWhile"
    !insertmacro _PushCustom "Break" ${${_Logic}ExitWhile}
    !insertmacro _PushCustom "Continue" ${${_Logic}While}
    !verbose 4
  !macroend
  !define EndWhile "!insertmacro EndWhile"

  !macro Do
    !verbose 3
    !insertmacro _PushLogic
    !define ${_Logic}Do _${__LINE__}       ; Get a label for the start of the loop
    !define ${_Logic}Loop _${__LINE__}    ; Get a label for the end of the loop
    !insertmacro _PushCustom "ExitDo" ${${_Logic}Loop}
    !insertmacro _PushCustom "Break" ${${_Logic}Loop}
    !insertmacro _PushCustom "Continue" ${${_Logic}Do}
    ${${_Logic}Do}:                        ; Insert the loop condition
    !verbose 4
  !macroend
  !define Do "!insertmacro Do"

  !macro DoUntil _a _o _b
    !verbose 3
    !insertmacro Do
    !insertmacro _${_o} "${_a}" "${_b}" ${${_Logic}Loop} ""
    !verbose 4
  !macroend
  !define DoUntil "!insertmacro DoUntil"

  !define ExitDo "Goto ${_ExitDo}"

  !macro Loop
    !verbose 3
    !ifndef _Logic | ${_Logic}Do
      !error "Cannot use EndWhile without a preceding Do"
    !endif
    Goto ${${_Logic}Do}                    ; Loop back to the Do condition
    ${${_Logic}Loop}:                     ; Place the Loop
    !undef ${_Logic}Do
    !undef ${_Logic}Loop
    !insertmacro _PopLogic
    !insertmacro _PopCustom "ExitDo"
    !insertmacro _PopCustom "Break"
    !insertmacro _PopCustom "Continue"
    !verbose 4
  !macroend
  !define Loop "!insertmacro Loop"

  !macro LoopUntil _a _o _b
    !verbose 3
    !ifndef _Logic | ${_Logic}Do
      !error "Cannot use EndWhile without a preceding Do"
    !endif
    !insertmacro _${_o} "${_a}" "${_b}" ${${_Logic}Loop} ${${_Logic}Do}
    ${${_Logic}Loop}:                     ; Place the Loop
    !undef ${_Logic}Do
    !undef ${_Logic}Loop
    !insertmacro _PopLogic
    !insertmacro _PopCustom "ExitDo"
    !insertmacro _PopCustom "Break"
    !insertmacro _PopCustom "Continue"
    !verbose 4
  !macroend
  !define LoopUntil "!insertmacro LoopUntil"

  !macro Select _a
    !verbose 3
    !insertmacro _PushLogic
    !define ${_Logic}Case _${__LINE__}       ; Get a label for the (provisional) EndSelect (it might turn out to be case_else)
    !define ${_Logic}EndSelect _${__LINE__}       ; Get a label for the (provisional) EndIf (it might turn out to be Else)
    !define ${_Logic}Value "${_a}"
    Goto ${${_Logic}Case}
    !verbose 4
  !macroend
  !define Select "!insertmacro Select"

  !macro Case_Else
    !verbose 3
    !ifndef _Logic | ${_Logic}Case
      !error "Cannot use Case|case_else without a preceding Select"
    !endif
    !ifdef ${_Logic}case_else
      !error "Cannot use case_else after case_else"
    !endif
    Goto ${${_Logic}EndSelect}
    !define ${_Logic}case_else ${${_Logic}Case}   ; Save current case_else label
    !undef ${_Logic}Case
    !define ${_Logic}Case _${__LINE__}       ; Get a label for the (new) EndSelect and go there
    ${${_Logic}case_else}:                         ; Place the saved case_else label
    !verbose 4
  !macroend
  !define case_else "!insertmacro case_else"

  !macro Case _a
    !verbose 3
    !insertmacro Case_Else                         ; Place in case_else code as normal
    !undef ${_Logic}case_else                      ; Forget the case_else and perform the new Case
    !insertmacro _== "${_a}" "${${_Logic}Value}" "" ${${_Logic}Case}
    !verbose 4
  !macroend
  !define Case "!insertmacro Case"

  !macro Case2 _a _b
    !verbose 3
    !insertmacro case_else                         ; Place in case_else code as normal
    !undef ${_Logic}case_else                      ; Forget the case_else and perform the new Case
    !insertmacro _== "${_a}" "${${_Logic}Value}" +2 0
    !insertmacro _== "${_b}" "${${_Logic}Value}" 0 ${${_Logic}Case}
    !verbose 4
  !macroend
  !define Case2 "!insertmacro Case2"

  !macro Case3 _a _b _c
    !verbose 3
    !insertmacro case_else                         ; Place in case_else code as normal
    !undef ${_Logic}case_else                      ; Forget the case_else and perform the new Case
    !insertmacro _== "${_a}" "${${_Logic}Value}" +3 0
    !insertmacro _== "${_b}" "${${_Logic}Value}" +2 0
    !insertmacro _== "${_c}" "${${_Logic}Value}" 0 ${${_Logic}Case}
    !verbose 4
  !macroend
  !define Case3 "!insertmacro Case3"

  !macro Case4 _a _b _c _d
    !verbose 3
    !insertmacro case_else                         ; Place in case_else code as normal
    !undef ${_Logic}case_else                      ; Forget the case_else and perform the new Case
    !insertmacro _== "${_a}" "${${_Logic}Value}" +4 0
    !insertmacro _== "${_b}" "${${_Logic}Value}" +3 0
    !insertmacro _== "${_c}" "${${_Logic}Value}" +2 0
    !insertmacro _== "${_d}" "${${_Logic}Value}" 0 ${${_Logic}Case}
    !verbose 4
  !macroend
  !define Case4 "!insertmacro Case4"

  !macro Case5 _a _b _c _d _e
    !verbose 3
    !insertmacro case_else                         ; Place in case_else code as normal
    !undef ${_Logic}case_else                      ; Forget the case_else and perform the new Case
    !insertmacro _== "${_a}" "${${_Logic}Value}" +5 0
    !insertmacro _== "${_b}" "${${_Logic}Value}" +4 0
    !insertmacro _== "${_c}" "${${_Logic}Value}" +3 0
    !insertmacro _== "${_d}" "${${_Logic}Value}" +2 0
    !insertmacro _== "${_e}" "${${_Logic}Value}" 0 ${${_Logic}Case}
    !verbose 4
  !macroend
  !define Case5 "!insertmacro Case5"

  !macro EndSelect
    !verbose 3
    !ifndef _Logic | ${_Logic}Case
      !error "Cannot use EndSelect without a preceding Select"
    !endif
    Goto ${${_Logic}Case}
    Goto ${${_Logic}EndSelect}
    ${${_Logic}Case}:
    ${${_Logic}EndSelect}:                        ; Place the EndSelect
    !undef ${_Logic}Case
    !undef ${_Logic}EndSelect
    !undef ${_Logic}Value
    !ifdef ${_Logic}case_else
      !undef ${_Logic}case_else                    ; Clear the case_else flag
    !endif
    !insertmacro _PopLogic
    !verbose 4
  !macroend
  !define EndSelect "!insertmacro EndSelect"

!endif ; LOGICLIB
!verbose 4