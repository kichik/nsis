Name "NSIS LogicLib Example"
OutFile "example.exe"
ShowInstDetails show

;!define LOGICLIB_VERBOSITY 4
!include "logiclib.nsh"

Section

  ; if..endif
  StrCpy $R1 1
  StrCpy $R2 ""
  ${If} $R1 = 1
    StrCpy $R2 $R2A
  ${EndIf}
  ${If} $R1 = 2
    StrCpy $R2 $R2B
  ${EndIf}
  ${If} $R1 < 2
    StrCpy $R2 $R2C
  ${EndIf}
  ${If} $R1 < -2
    StrCpy $R2 $R2D
  ${EndIf}
  ${If} $R1 > 2
    StrCpy $R2 $R2E
  ${EndIf}
  ${If} $R1 > -2
    StrCpy $R2 $R2F
  ${EndIf}
  ${If} $R1 <> 1
    StrCpy $R2 $R2G
  ${EndIf}
  ${If} $R1 <> 2
    StrCpy $R2 $R2H
  ${EndIf}
  ${If} $R1 >= 2
    StrCpy $R2 $R2I
  ${EndIf}
  ${If} $R1 >= -2
    StrCpy $R2 $R2J
  ${EndIf}
  ${If} $R1 <= 2
    StrCpy $R2 $R2K
  ${EndIf}
  ${If} $R1 <= -2
    StrCpy $R2 $R2L
  ${EndIf}
  ${If} $R2 == "ACFHJK"
    DetailPrint "PASSED If..EndIf test"
  ${Else}
    DetailPrint "FAILED If..EndIf test"
  ${EndIf}

  ; if..elseif..else..endif
  StrCpy $R1 A
  StrCpy $R2 ""
  ${If} $R1 == A
    StrCpy $R2 $R2A
  ${ElseIf} $R1 == B
    StrCpy $R2 $R2B
  ${ElseUnless} $R1 != C
    StrCpy $R2 $R2C
  ${Else}
    StrCpy $R2 $R2D
  ${EndIf}
  ${If} $R1 == D
    StrCpy $R2 $R2D
  ${ElseIf} $R1 == A
    StrCpy $R2 $R2A
  ${ElseUnless} $R1 != B
    StrCpy $R2 $R2B
  ${Else}
    StrCpy $R2 $R2C
  ${EndIf}
  ${If} $R1 == C
    StrCpy $R2 $R2C
  ${ElseIf} $R1 == D
    StrCpy $R2 $R2D
  ${ElseUnless} $R1 != A
    StrCpy $R2 $R2A
  ${Else}
    StrCpy $R2 $R2B
  ${EndIf}
  ${If} $R1 == B
    StrCpy $R2 $R2B
  ${ElseIf} $R1 == C
    StrCpy $R2 $R2C
  ${ElseUnless} $R1 != D
    StrCpy $R2 $R2D
  ${Else}
    StrCpy $R2 $R2A
  ${EndIf}
  ${If} $R2 == "$R1$R1$R1$R1"
    DetailPrint "PASSED If..ElseIf..Else..EndIf test"
  ${Else}
    DetailPrint "FAILED If..ElseIf..Else..EndIf test"
  ${EndIf}

  ; ifthen..|..|
  StrCpy $R1 1
  StrCpy $R2 ""
  ${ifthen} $R1 = 1 ${|} StrCpy $R2 $R2A ${|}
  ${ifthen} $R1 = 2 ${|} StrCpy $R2 $R2B ${|}
  ${If} $R2 == "A"
    DetailPrint "PASSED IfThen test"
  ${Else}
    DetailPrint "FAILED IfThen test"
  ${EndIf}

  ; ifcmd..||..|
  StrCpy $R2 ""
  ${ifcmd} MessageBox MB_YESNO "Please press Yes" IDYES ${||} StrCpy $R2 $R2A ${|}
  ${ifcmd} MessageBox MB_YESNO|MB_DEFBUTTON2 "Please press No" IDYES ${||} StrCpy $R2 $R2B ${|}
  ${If} $R2 == "A"
    DetailPrint "PASSED IfCmd test"
  ${Else}
    DetailPrint "FAILED IfCmd test"
  ${EndIf}

  ; select..case..case2..case3..case4..case5..caseelse..endselect
  StrCpy $R1 1
  StrCpy $R2 ""
  ${Select} $R1
    ${Case} "1"
      StrCpy $R2 $R2A
    ${Case} "2"
      StrCpy $R2 $R2B
    ${Case2} "3" "4"
      StrCpy $R2 $R2C
    ${CaseElse}
      StrCpy $R2 $R2D
  ${EndSelect}
  ${Select} $R1
    ${Case} "2"
      StrCpy $R2 $R2A
    ${Case} "3"
      StrCpy $R2 $R2B
    ${Case2} "4" "5"
      StrCpy $R2 $R2C
    ${CaseElse}
      StrCpy $R2 $R2D
  ${EndSelect}
  ${Select} $R1
    ${Case} "3"
      StrCpy $R2 $R2A
    ${Case} "4"
      StrCpy $R2 $R2B
    ${Case2} "5" "1"
      StrCpy $R2 $R2C
    ${CaseElse}
      StrCpy $R2 $R2D
  ${EndSelect}
  ${Select} $R1
    ${Case} "4"
      StrCpy $R2 $R2A
    ${Case} "5"
      StrCpy $R2 $R2B
    ${Case2} "1" "2"
      StrCpy $R2 $R2C
    ${CaseElse}
      StrCpy $R2 $R2D
  ${EndSelect}
  ${If} $R2 == "ADCC"
    DetailPrint "PASSED Select..Case*..EndSelect test"
  ${Else}
    DetailPrint "FAILED Select..Case*..EndSelect test"
  ${EndIf}

  ; for[each]..exitfor..next
  StrCpy $R2 ""
  ${For} $R1 1 5
    StrCpy $R2 $R2$R1
  ${Next}
  ${ForEach} $R1 10 1 - 1
    StrCpy $R2 $R2$R1
  ${Next}
  ${For} $R1 1 0
    StrCpy $R2 $R2$R1
  ${Next}
  ${If} $R2 == "1234510987654321"
    DetailPrint "PASSED For[Each]..Next test"
  ${Else}
    DetailPrint "FAILED For[Each]..Next test"
  ${EndIf}

  ; do..exitdo..loop
  StrCpy $R1 0
  StrCpy $R2 ""
  ${Do}
    StrCpy $R2 $R2$R1
    IntOp $R1 $R1 + 1
    ${If} $R1 > 10
      ${ExitDo}
    ${EndIf}
  ${Loop}
  ${If} $R2 == "012345678910"
    DetailPrint "PASSED Do..ExitDo..Loop test"
  ${Else}
    DetailPrint "FAILED Do..ExitDo..Loop test"
  ${EndIf}

  ; do..exitdo..loopuntil
  StrCpy $R1 0
  StrCpy $R2 ""
  ${Do}
    StrCpy $R2 $R2$R1
    IntOp $R1 $R1 + 1
  ${LoopUntil} $R1 >= 5
  ${If} $R2 == "01234"
    DetailPrint "PASSED Do..ExitDo..LoopUntil test"
  ${Else}
    DetailPrint "FAILED Do..ExitDo..LoopUntil test"
  ${EndIf}

  ; dountil..exitdo..loop
  StrCpy $R1 0
  StrCpy $R2 ""
  ${DoUntil} $R1 >= 5
    StrCpy $R2 $R2$R1
    IntOp $R1 $R1 + 1
  ${Loop}
  ${If} $R2 == "01234"
    DetailPrint "PASSED DoUntil..ExitDo..Loop test"
  ${Else}
    DetailPrint "FAILED DoUntil..ExitDo..Loop test"
  ${EndIf}

  ; nested do test
  StrCpy $R1 0
  StrCpy $R2 0
  StrCpy $R3 ""
  ${Do}
    StrCpy $R3 $R3$R1$R2
    IntOp $R1 $R1 + 1
    ${If} $R1 > 5
      ${ExitDo}
    ${EndIf}
    StrCpy $R2 0
    ${Do}
      StrCpy $R3 $R3$R1$R2
      IntOp $R2 $R2 + 1
      ${If} $R2 >= 5
        ${ExitDo}
      ${EndIf}
    ${Loop}
  ${Loop}
  ${If} $R3 == "00101112131415202122232425303132333435404142434445505152535455"
    DetailPrint "PASSED nested Do test"
  ${Else}
    DetailPrint "FAILED nested Do test"
  ${EndIf}

  ; while..exitwhile..endwhile (exact replica of dowhile..enddo}
  StrCpy $R1 0
  StrCpy $R2 ""
  ${While} $R1 < 5
    StrCpy $R2 $R2$R1
    IntOp $R1 $R1 + 1
  ${EndWhile}
  ${If} $R2 == "01234"
    DetailPrint "PASSED While..ExitWhile..EndWhile test"
  ${Else}
    DetailPrint "FAILED While..ExitWhile..EndWhile test"
  ${EndIf}

  ; kinds of if other than "value1 comparison value2"
  ClearErrors
  FindFirst $R1 $R2 "$PROGRAMFILES\*"
  ${Unless} ${Errors}
    ${Do}
      ${Select} $R2
        ${Case2} "." ".."
          ; Do nothing
        ${CaseElse}
          DetailPrint "Found $PROGRAMFILES\$R2"
      ${EndSelect}
      FindNext $R1 $R2
    ${LoopUntil} ${Errors}
    FindClose $R1
  ${EndUnless}

  StrCpy $R1 "example.xxx"
  ${If} ${FileExists} "${__FILE__}"
    DetailPrint 'Source file "${__FILE__}" still exists'
  ${Else}
    DetailPrint 'Source file "${__FILE__}" has gone'
  ${EndIf}

SectionEnd
