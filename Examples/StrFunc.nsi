Name "NSIS StrFunc Example"
OutFile "StrFunc.exe"
ShowInstDetails show

!include "StrFunc.nsh"

# declare used functions
${StrClbGet}
${StrClbSet}
${StrIOToNSIS}
${StrLoc}
${StrLowerCase}
${StrNSISToIO}
${StrRep}
${StrStr}
${StrStrAdv}
${StrTok}
${StrTrimNewLines}
${StrUpperCase}

Section

  # test clipboard functions
  ${StrClbSet} "StrFunc clipboard test"
  ${StrClbGet} $0
  StrCmp $0 "StrFunc clipboard test" +3
    DetailPrint "FAILED StrClbGet/StrClbSet test"
    Goto +2
    DetailPrint "PASSED StrClbGet/StrClbSet test"

  # test IO functions
  !macro testio str
  ${StrNSISToIO} $0 "${str}"
  ${StrIOToNSIS} $0 $0
  StrCmp $0 "${str}" 0 ioerror
  !macroend
  !insertmacro testio "$\rtest$\n"
  !insertmacro testio "test$\n"
  !insertmacro testio "$\rtest"
  !insertmacro testio "test"
  !insertmacro testio "$\r\$\t$\n"
  !insertmacro testio "$\r \ $\t $\n $$"
  !insertmacro testio ""
  !insertmacro testio " "
  DetailPrint "PASSED StrNSISToIO/StrIOToNSIS test"
  Goto +2
ioerror:
  DetailPrint "FAILED StrNSISToIO/StrIOToNSIS test"

  # test string search functions
  ${StrLoc} $0 "This is just an example" "just" "<"
  StrCmp $0 "11" 0 strlocerror
  ${StrLoc} $0 a abc <
  StrCmp $0 "" 0 strlocerror
  ${StrLoc} $0 a abc >
  StrCmp $0 "" 0 strlocerror
  ${StrLoc} $0 abc a >
  StrCmp $0 "0" 0 strlocerror
  ${StrLoc} $0 abc b >
  StrCmp $0 "1" 0 strlocerror
  ${StrLoc} $0 abc c >
  StrCmp $0 "2" 0 strlocerror
  ${StrLoc} $0 abc a <
  StrCmp $0 "2" 0 strlocerror
  ${StrLoc} $0 abc b <
  StrCmp $0 "1" 0 strlocerror
  ${StrLoc} $0 abc c <
  StrCmp $0 "0" 0 strlocerror
  ${StrLoc} $0 abc d <
  StrCmp $0 "" 0 strlocerror
  DetailPrint "PASSED StrLoc test"
  Goto +2
strlocerror:
  DetailPrint "FAILED StrLoc test"

  ${StrStr} $0 "abcefghijklmnopqrstuvwxyz" "g"
  StrCmp $0 "ghijklmnopqrstuvwxyz" 0 strstrerror
  ${StrStr} $0 "abcefghijklmnopqrstuvwxyz" "ga"
  StrCmp $0 "" 0 strstrerror
  ${StrStr} $0 "abcefghijklmnopqrstuvwxyz" ""
  StrCmp $0 "abcefghijklmnopqrstuvwxyz" 0 strstrerror
  ${StrStr} $0 "a" "abcefghijklmnopqrstuvwxyz"
  StrCmp $0 "" 0 strstrerror
  DetailPrint "PASSED StrStr test"
  Goto +2
strstrerror:
  DetailPrint "FAILED StrStr test"

  ${StrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "0"
  StrCmp $0 "abcabcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "1"
  StrCmp $0 "abcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "2"
  StrCmp $0 "abc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "3"
  StrCmp $0 "" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" ">" "<" "1" "1"
  StrCmp $0 "abcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" ">" "<" "0" "1"
  StrCmp $0 "abcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" "<" "<" "1" "0"
  StrCmp $0 "abcabcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" "<" "<" "0" "0"
  StrCmp $0 "abcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" "<" ">" "0" "0"
  StrCmp $0 "" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" "<" ">" "0" "1"
  StrCmp $0 "abc" 0 strstradverror
  DetailPrint "PASSED StrStrAdv test"
  Goto +2
strstradverror:
  DetailPrint "FAILED StrStrAdv test"

  # test string replacement
  ${StrRep} $0 "This is just an example" "an" "one"
  StrCmp $0 "This is just one example" 0 strreperror
  ${StrRep} $0 "test... test... 1 2 3..." "test" "testing"
  StrCmp $0 "testing... testing... 1 2 3..." 0 strreperror
  ${StrRep} $0 "" "test" "testing"
  StrCmp $0 "" 0 strreperror
  ${StrRep} $0 "test" "test" "testing"
  StrCmp $0 "testing" 0 strreperror
  ${StrRep} $0 "test" "test" ""
  StrCmp $0 "" 0 strreperror
  ${StrRep} $0 "test" "" "abc"
  StrCmp $0 "test" 0 strreperror
  ${StrRep} $0 "test" "" ""
  StrCmp $0 "test" 0 strreperror
  DetailPrint "PASSED StrRep test"
  Goto +2
strreperror:
  DetailPrint "FAILED StrRep test"

  # test lower/upper case
  ${StrLowerCase} $0 "abcefghijklmnopqrstuvwxyz"
  ${StrUpperCase} $0 $0
  StrCmp $0 "abcefghijklmnopqrstuvwxyz" +3
    DetailPrint "FAILED StrLowerCase/StrUpperCase test"
    Goto +2
    DetailPrint "PASSED StrLowerCase/StrUpperCase test"

  # test tokenizer
  ${StrTok} $0 "This is, or is not, just an example" " ," "5" "1"
  StrCmp $0 "not" 0 strtokerror
  ${StrTok} $0 "This is, or is not, just an example" " ," "5" "0"
  StrCmp $0 "is" 0 strtokerror
  ${StrTok} $0 "This is, or is not, just an example" " ," "152" "0"
  StrCmp $0 "" 0 strtokerror
  ${StrTok} $0 "This is, or is not, just an example" " ," "0" "0"
  StrCmp $0 "example" 0 strtokerror
  ${StrTok} $0 "This is, or is not, just an example" " ," "-1" "0"
  StrCmp $0 "example" 0 strtokerror
  ${StrTok} $0 "This is, or is not, just an example" " ," "1" "0"
  StrCmp $0 "This" 0 strtokerror
  DetailPrint "PASSED StrTok test"
  Goto +2
strtokerror:
  DetailPrint "FAILED StrTok test"

  # test trim new lines
  ${StrTrimNewLines} $0 "$\r$\ntest$\r$\ntest$\r$\n"
  StrCmp $0 "$\r$\ntest$\r$\ntest" +3
    DetailPrint "FAILED StrTrimNewLines test"
    Goto +2
    DetailPrint "PASSED StrTrimNewLines test"

SectionEnd