; rtest.nsi
;
; This script tests some advanced NSIS functions.

;--------------------------------

Name "rtest"
OutFile "rtest.exe"

ComponentText "Select tests!"
ShowInstDetails show

;--------------------------------

Section "Test 1"

  StrCpy $0 "a"
  Call test1
  
  StrCmp $0 "a182345678" success
  
  DetailPrint "Test 1 failed (output: $0)"
  Goto end
  
  success:
  DetailPrint "Test 1 succeded (output: $0)"
  
  end:
  
SectionEnd

Function test1

  GetLabelAddress $9 skip
  
  IntOp $9 $9 - 1
  StrCpy $0 $01
  
  Call $9
  
  StrCpy $0 $02
  StrCpy $0 $03
  StrCpy $0 $04
  StrCpy $0 $05
  StrCpy $0 $06
  StrCpy $0 $07
  StrCpy $0 $08
  
  skip:
  
FunctionEnd

;--------------------------------

Section "Test 2"

  StrCpy $0 "0"
  StrCpy $1 "11"
  
  Call test2
  
  StrCmp $1 "11,10,9,8,7,6,5,4,3,2,1" success
  
  DetailPrint "Test 2 failed (output: $1)"
  Goto end
  
  success:
  DetailPrint "Test 2 succeded (output: $1)"
  
  end:

SectionEnd

Function test2

  IntOp $0 $0 + 1
  IntCmp $0 10 done
  
  Push $0
  
  Call test2
  
  Pop $0
  
  done:
  StrCpy $1 "$1,$0"
  
FunctionEnd