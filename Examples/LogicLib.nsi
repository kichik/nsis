Name "NSIS LogicLib Example"
OutFile "example.exe"
SilentInstall silent

!include "logiclib.nsh"
!define MsgBox "MessageBox MB_OK"

Section

  ; if..elseif..else..endif
  StrCpy $R1 1 ;change to test the following if statement
  ${if} $R1 = 1
    MessageBox MB_OK "if: R1=1"
  ${elseif} $R1 = 2
    MessageBox MB_OK "ifelse: R1=2"
  ${else}
    MessageBox MB_OK "else: R1=$R1"
  ${endif}

  ; ifthen..|..|
  StrCpy $R1 1 ; change to test ifthen statement.
  ${ifthen} $R1 = 1 ${|} MessageBox MB_OK "R1=1" ${|}

  ; ifcmd..||..|
  StrCpy $R1 "example.nsi" ; change to test ifcmd statement
  ${ifcmd} IfFileExists "example.nsi" ${||} MessageBox MB_OK "IfFileExists: R1=$R1" ${|}

  ; select..case..case2..case3..case4..case5..case_else..endselect
  StrCpy $R1 1 ;change to test the following if statement
  ${select} $R1
    ${case} "1"
      MessageBox MB_OK "case: R1=1"
    ${case} "2"
      MessageBox MB_OK "case: R1=2"
    ${case2} "3" "4"
      MessageBox MB_OK "case2: R1=3 or 4, R1=$R1"
    ${case_else}
      MessageBox MB_OK "caseelse: R1=$R1"
  ${endselect}

  ; for..exitfor..next
  ${for} $R1 1 5
    MessageBox MB_OK "for: R1=$R1"
  ${next}

  ; foreach..exitfor..next
  ${foreach} $R1 10 1 - 1 
    MessageBox MB_OK "foreach: R1=$R1"
  ${next}

  ; do..exitdo..loop
  StrCpy $R1 0
  ${do}
    IntOp $R1 $R1 + 1
    MessageBox MB_YESNO "Do..Loop statement test, iteration $R1.$\nDo you want to stop?" IDYES 0 IDNO +2
    ${exitdo}
  ${loop}

  ; do..exitdo..loopuntil 
  StrCpy $R1 0
  ${do}
    IntOp $R1 $R1 + 1
  ${loopuntil} $R1 >= 5 ; Change to test loop until
  MessageBox MB_OK "do..loopuntil: R1=$R1"

  ; dountil..exitdo..loop
  StrCpy $R1 0
  ${dountil} $R1 >= 5 ; Change to test loop until
    IntOp $R1 $R1 + 1
  ${loop}
  MessageBox MB_OK "dountil..loop: R1=$R1"

  ; exitdo statement test
  StrCpy $R1 0
  StrCpy $R2 0
  ${do}
    IntOp $R1 $R1 + 1
    IntCmp $R1 5 +2 +2 0
    ${exitdo}
    StrCpy $R2 0
    ${do}
      IntOp $R2 $R2 + 1
      MessageBox MB_OK "loop1: $R1$\nloop2: $R2"
      IntCmp $R2 5 0 +2 0
      ${exitdo}
    ${loop}
  ${loop}
  MessageBox MB_OK "loopR1: $R1$\nloop2: $R2"

  ; while..exitwhile..endwhile
  StrCpy $R1 0
  ${while} $R1 < 5 ;change to test while statement.
    IntOp $R1 $R1 + 1
  ${endwhile}
  MessageBox MB_OK "while: R1=$R1"

SectionEnd