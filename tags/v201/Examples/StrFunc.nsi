Name "NSIS StrFunc Example"
OutFile "StrFunc.exe"
ShowInstDetails show
ShowUninstDetails show
XPStyle on

!include "StrFunc.nsh"

# Declare used functions
${StrCase}
${StrClb}
${StrIOToNSIS}
${StrLoc}
${StrNSISToIO}
${StrRep}
${StrStr}
${StrStrAdv}
${StrTok}
${StrTrimNewLines}
${StrSort}

${UnStrCase}
${UnStrClb}
${UnStrIOToNSIS}
${UnStrLoc}
${UnStrNSISToIO}
${UnStrRep}
${UnStrStr}
${UnStrStrAdv}
${UnStrTok}
${UnStrTrimNewLines}
${UnStrSort}

Section

  # Test case conversion
  ${StrCase} $0 "This is just an example. A very simple one." ""
  StrCmp $0 "This is just an example. A very simple one." 0 strcaseerror

  ${StrCase} $0 "THIS IS JUST AN EXAMPLE. A VERY SIMPLE ONE." "S"
  StrCmp $0 "This is just an example. A very simple one." 0 strcaseerror
  ${StrCase} $0 "This is just an example. A very simple one." "L"
  StrCmp $0 "this is just an example. a very simple one." 0 strcaseerror
  ${StrCase} $0 "This is just an example. A very simple one." "U"
  StrCmp $0 "THIS IS JUST AN EXAMPLE. A VERY SIMPLE ONE." 0 strcaseerror
  ${StrCase} $0 "This is just an example. A very simple one." "T"
  StrCmp $0 "This Is Just An Example. A Very Simple One." 0 strcaseerror
  ${StrCase} $0 "This is just an example. A very simple one." "<>"
  StrCmp $0 "tHIS IS JUST AN EXAMPLE. a VERY SIMPLE ONE." 0 strcaseerror

  ${StrCase} $0 "123456789!@#%^&*()-_=+[]{};:,./<>?" "S"
  StrCmp $0 "123456789!@#%^&*()-_=+[]{};:,./<>?" 0 strcaseerror

  ${StrCase} $0 "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#%^&*()abcdefghijklmnopqrstuvwxyz-_=+[]{};:,./<>?" "<>"
  StrCmp $0 "123456789abcdefghijklmnopqrstuvwxyz!@#%^&*()ABCDEFGHIJKLMNOPQRSTUVWXYZ-_=+[]{};:,./<>?" 0 strcaseerror

  ${StrCase} $0 "what about taking a shower tomorrow? it's late to do so now! try to sleep now. Good Night!" "S"
  StrCmp $0 "What about taking a shower tomorrow? It's late to do so now! Try to sleep now. Good night!" 0 strcaseerror

  DetailPrint "PASSED StrCase test"
  Goto +2
strcaseerror:
  DetailPrint "FAILED StrCase test"

  # Test clipboard function
  ${StrClb} $0 "StrFunc clipboard test" ">"
  ${StrClb} $0 "" "<"
  StrCmp $0 "StrFunc clipboard test" 0 strclberror
  
  DetailPrint "PASSED StrClb test"
  Goto +2
strclberror:
  DetailPrint "FAILED StrClb test"

  # Test IO functions
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

  # Test string search functions
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

  ${StrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "0" "0"
  StrCmp $0 "abcabcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "1" "0"
  StrCmp $0 "abcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "2" "0"
  StrCmp $0 "abc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "3" "0"
  StrCmp $0 "" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" ">" "<" "1" "1" "0"
  StrCmp $0 "abcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" ">" "<" "0" "1" "0"
  StrCmp $0 "abc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" "<" "<" "1" "0" "0"
  StrCmp $0 "abcabcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" "<" "<" "0" "0" "0"
  StrCmp $0 "abcabc" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" "<" ">" "0" "0" "0"
  StrCmp $0 "" 0 strstradverror
  ${StrStrAdv} $0 "abcabcabc" "abc" "<" ">" "0" "1" "0"
  StrCmp $0 "abc" 0 strstradverror
  
  ${StrStrAdv} $0 "ABCabcabc" "a" ">" ">" "1" "0" "1"
  StrCmp $0 "abcabc" 0 strstradverror
  ${StrStrAdv} $0 "ABCabcabc" "a" ">" ">" "1" "1" "1"
  StrCmp $0 "abc" 0 strstradverror
  ${StrStrAdv} $0 "ABCabcabc" "a" ">" ">" "1" "2" "1"
  StrCmp $0 "" 0 strstradverror
  ${StrStrAdv} $0 "ABCabcabc" "a" ">" ">" "1" "3" "1"
  StrCmp $0 "" 0 strstradverror
  ${StrStrAdv} $0 "ABCabcabc" "abc" ">" "<" "1" "1" "1"
  StrCmp $0 "ABCabcabc" 0 strstradverror
  ${StrStrAdv} $0 "ABCabcabc" "abc" ">" "<" "0" "1" "1"
  StrCmp $0 "ABCabc" 0 strstradverror
  ${StrStrAdv} $0 "ABCabcabc" "abc" "<" "<" "1" "0" "1"
  StrCmp $0 "ABCabcabc" 0 strstradverror
  ${StrStrAdv} $0 "ABCabcabc" "abc" "<" "<" "0" "0" "1"
  StrCmp $0 "ABCabc" 0 strstradverror
  ${StrStrAdv} $0 "ABCabcabc" "abc" "<" ">" "0" "0" "1"
  StrCmp $0 "" 0 strstradverror
  ${StrStrAdv} $0 "ABCabcabc" "abc" "<" ">" "0" "1" "1"
  StrCmp $0 "abc" 0 strstradverror
  DetailPrint "PASSED StrStrAdv test"
  Goto +2
strstradverror:
  DetailPrint "FAILED StrStrAdv test"

  # Test string replacement
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

  # Test sorting
  ${StrSort} $0 "This is just an example" " just" "" "ple" "0" "0"
  StrCmp $0 "This is an exam" 0 strsorterror
  ${StrSort} $0 "This is just an example" "j" " " " " "0" "1"
  StrCmp $0 "just" 0 strsorterror
  ${StrSort} $0 "This is just an example" "j" "" "" "0" "1"
  StrCmp $0 "This is just an example" 0 strsorterror
  ${StrSort} $0 "This is just an example" "us" " " "" "0" "1"
  StrCmp $0 "just an example" 0 strsorterror
  ${StrSort} $0 "This is just an example" "u" "" " " "0" "1"
  StrCmp $0 "This is just" 0 strsorterror
  ${StrSort} $0 "This is just an example" "just" " " " " "0" "1"
  StrCmp $0 "just" 0 strsorterror
  ${StrSort} $0 "This is just an example" "t" " " " " "0" "1"
  StrCmp $0 "This" 0 strsorterror
  ${StrSort} $0 "This is just an example" "le" " " " " "0" "1"
  StrCmp $0 "example" 0 strsorterror
  ${StrSort} $0 "This is just an example" "le" " " " " "1" "0"
  StrCmp $0 " examp" 0 strsorterror
  ${StrSort} $0 "an error has occured" "e" " " " " "0" "1"
  StrCmp $0 "error" 0 strsorterror
  ${StrSort} $0 "" "something" " " " " "0" "1"
  StrCmp $0 "" 0 strsorterror
  ${StrSort} $0 "This is just an example" "j" " " " " "1" "1"
  StrCmp $0 " just " 0 strsorterror
  ${StrSort} $0 "This is just an example" "j" " " " " "1" "0"
  StrCmp $0 " ust " 0 strsorterror
  ${StrSort} $0 "This is just an example" "j" "" "" "1" "0"
  StrCmp $0 "This is ust an example" 0 strsorterror
  ${StrSort} $0 "This is just an example" "us" " " "" "1" "0"
  StrCmp $0 " jt an example" 0 strsorterror
  ${StrSort} $0 "This is just an example" "u" "" " " "1" "0"
  StrCmp $0 "This is jst " 0 strsorterror
  ${StrSort} $0 "This is just an example" "just" " " " " "1" "0"
  StrCmp $0 "  " 0 strsorterror
  ${StrSort} $0 "an error has occured" "e" " " " " "1" "0"
  StrCmp $0 " rror " 0 strsorterror
  ${StrSort} $0 "" "something" " " " " "1" "0"
  StrCmp $0 "" 0 strsorterror
  DetailPrint "PASSED StrSort test"
  Goto +2
strsorterror:
  DetailPrint "FAILED StrSort test"

  # Test tokenizer
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

  # Test trim new lines
  ${StrTrimNewLines} $0 "$\r$\ntest$\r$\ntest$\r$\n"
  StrCmp $0 "$\r$\ntest$\r$\ntest" +3
    DetailPrint "FAILED StrTrimNewLines test"
    Goto +2
    DetailPrint "PASSED StrTrimNewLines test"

  WriteUninstaller $EXEDIR\UnStrFunc.exe
  
  Exec $EXEDIR\UnStrFunc.exe

SectionEnd

Section Uninstall

  # Test case conversion
  ${UnStrCase} $0 "This is just an example. A very simple one." ""
  StrCmp $0 "This is just an example. A very simple one." 0 strcaseerror

  ${UnStrCase} $0 "THIS IS JUST AN EXAMPLE. A VERY SIMPLE ONE." "S"
  StrCmp $0 "This is just an example. A very simple one." 0 strcaseerror
  ${UnStrCase} $0 "This is just an example. A very simple one." "L"
  StrCmp $0 "this is just an example. a very simple one." 0 strcaseerror
  ${UnStrCase} $0 "This is just an example. A very simple one." "U"
  StrCmp $0 "THIS IS JUST AN EXAMPLE. A VERY SIMPLE ONE." 0 strcaseerror
  ${UnStrCase} $0 "This is just an example. A very simple one." "T"
  StrCmp $0 "This Is Just An Example. A Very Simple One." 0 strcaseerror
  ${UnStrCase} $0 "This is just an example. A very simple one." "<>"
  StrCmp $0 "tHIS IS JUST AN EXAMPLE. a VERY SIMPLE ONE." 0 strcaseerror

  ${UnStrCase} $0 "123456789!@#%^&*()-_=+[]{};:,./<>?" "S"
  StrCmp $0 "123456789!@#%^&*()-_=+[]{};:,./<>?" 0 strcaseerror

  ${UnStrCase} $0 "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#%^&*()abcdefghijklmnopqrstuvwxyz-_=+[]{};:,./<>?" "<>"
  StrCmp $0 "123456789abcdefghijklmnopqrstuvwxyz!@#%^&*()ABCDEFGHIJKLMNOPQRSTUVWXYZ-_=+[]{};:,./<>?" 0 strcaseerror

  ${UnStrCase} $0 "what about taking a shower tomorrow? it's late to do so now! try to sleep now. Good Night!" "S"
  StrCmp $0 "What about taking a shower tomorrow? It's late to do so now! Try to sleep now. Good night!" 0 strcaseerror

  DetailPrint "PASSED StrCase test"
  Goto +2
strcaseerror:
  DetailPrint "FAILED StrCase test"

  # Test clipboard function
  ${UnStrClb} $0 "StrFunc clipboard test" ">"
  ${UnStrClb} $0 "" "<"
  StrCmp $0 "StrFunc clipboard test" 0 strclberror

  DetailPrint "PASSED StrClb test"
  Goto +2
strclberror:
  DetailPrint "FAILED StrClb test"

  # Test IO functions
  !macro untestio str
  ${UnStrNSISToIO} $0 "${str}"
  ${UnStrIOToNSIS} $0 $0
  StrCmp $0 "${str}" 0 ioerror
  !macroend
  !insertmacro untestio "$\rtest$\n"
  !insertmacro untestio "test$\n"
  !insertmacro untestio "$\rtest"
  !insertmacro untestio "test"
  !insertmacro untestio "$\r\$\t$\n"
  !insertmacro untestio "$\r \ $\t $\n $$"
  !insertmacro untestio ""
  !insertmacro untestio " "
  DetailPrint "PASSED StrNSISToIO/StrIOToNSIS test"
  Goto +2
ioerror:
  DetailPrint "FAILED StrNSISToIO/StrIOToNSIS test"

  # Test string search functions
  ${UnStrLoc} $0 "This is just an example" "just" "<"
  StrCmp $0 "11" 0 strlocerror
  ${UnStrLoc} $0 a abc <
  StrCmp $0 "" 0 strlocerror
  ${UnStrLoc} $0 a abc >
  StrCmp $0 "" 0 strlocerror
  ${UnStrLoc} $0 abc a >
  StrCmp $0 "0" 0 strlocerror
  ${UnStrLoc} $0 abc b >
  StrCmp $0 "1" 0 strlocerror
  ${UnStrLoc} $0 abc c >
  StrCmp $0 "2" 0 strlocerror
  ${UnStrLoc} $0 abc a <
  StrCmp $0 "2" 0 strlocerror
  ${UnStrLoc} $0 abc b <
  StrCmp $0 "1" 0 strlocerror
  ${UnStrLoc} $0 abc c <
  StrCmp $0 "0" 0 strlocerror
  ${UnStrLoc} $0 abc d <
  StrCmp $0 "" 0 strlocerror
  DetailPrint "PASSED StrLoc test"
  Goto +2
strlocerror:
  DetailPrint "FAILED StrLoc test"

  ${UnStrStr} $0 "abcefghijklmnopqrstuvwxyz" "g"
  StrCmp $0 "ghijklmnopqrstuvwxyz" 0 strstrerror
  ${UnStrStr} $0 "abcefghijklmnopqrstuvwxyz" "ga"
  StrCmp $0 "" 0 strstrerror
  ${UnStrStr} $0 "abcefghijklmnopqrstuvwxyz" ""
  StrCmp $0 "abcefghijklmnopqrstuvwxyz" 0 strstrerror
  ${UnStrStr} $0 "a" "abcefghijklmnopqrstuvwxyz"
  StrCmp $0 "" 0 strstrerror
  DetailPrint "PASSED StrStr test"
  Goto +2
strstrerror:
  DetailPrint "FAILED StrStr test"

  ${UnStrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "0" "0"
  StrCmp $0 "abcabcabc" 0 strstradverror
  ${UnStrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "1" "0"
  StrCmp $0 "abcabc" 0 strstradverror
  ${UnStrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "2" "0"
  StrCmp $0 "abc" 0 strstradverror
  ${UnStrStrAdv} $0 "abcabcabc" "a" ">" ">" "1" "3" "0"
  StrCmp $0 "" 0 strstradverror
  ${UnStrStrAdv} $0 "abcabcabc" "abc" ">" "<" "1" "1" "0"
  StrCmp $0 "abcabc" 0 strstradverror
  ${UnStrStrAdv} $0 "abcabcabc" "abc" ">" "<" "0" "1" "0"
  StrCmp $0 "abc" 0 strstradverror
  ${UnStrStrAdv} $0 "abcabcabc" "abc" "<" "<" "1" "0" "0"
  StrCmp $0 "abcabcabc" 0 strstradverror
  ${UnStrStrAdv} $0 "abcabcabc" "abc" "<" "<" "0" "0" "0"
  StrCmp $0 "abcabc" 0 strstradverror
  ${UnStrStrAdv} $0 "abcabcabc" "abc" "<" ">" "0" "0" "0"
  StrCmp $0 "" 0 strstradverror
  ${UnStrStrAdv} $0 "abcabcabc" "abc" "<" ">" "0" "1" "0"
  StrCmp $0 "abc" 0 strstradverror
  
  ${UnStrStrAdv} $0 "ABCabcabc" "a" ">" ">" "1" "0" "1"
  StrCmp $0 "abcabc" 0 strstradverror
  ${UnStrStrAdv} $0 "ABCabcabc" "a" ">" ">" "1" "1" "1"
  StrCmp $0 "abc" 0 strstradverror
  ${UnStrStrAdv} $0 "ABCabcabc" "a" ">" ">" "1" "2" "1"
  StrCmp $0 "" 0 strstradverror
  ${UnStrStrAdv} $0 "ABCabcabc" "a" ">" ">" "1" "3" "1"
  StrCmp $0 "" 0 strstradverror
  ${UnStrStrAdv} $0 "ABCabcabc" "abc" ">" "<" "1" "1" "1"
  StrCmp $0 "ABCabcabc" 0 strstradverror
  ${UnStrStrAdv} $0 "ABCabcabc" "abc" ">" "<" "0" "1" "1"
  StrCmp $0 "ABCabc" 0 strstradverror
  ${UnStrStrAdv} $0 "ABCabcabc" "abc" "<" "<" "1" "0" "1"
  StrCmp $0 "ABCabcabc" 0 strstradverror
  ${UnStrStrAdv} $0 "ABCabcabc" "abc" "<" "<" "0" "0" "1"
  StrCmp $0 "ABCabc" 0 strstradverror
  ${UnStrStrAdv} $0 "ABCabcabc" "abc" "<" ">" "0" "0" "1"
  StrCmp $0 "" 0 strstradverror
  ${UnStrStrAdv} $0 "ABCabcabc" "abc" "<" ">" "0" "1" "1"
  StrCmp $0 "abc" 0 strstradverror
  DetailPrint "PASSED StrStrAdv test"
  Goto +2
strstradverror:
  DetailPrint "FAILED StrStrAdv test"

  # Test string replacement
  ${UnStrRep} $0 "This is just an example" "an" "one"
  StrCmp $0 "This is just one example" 0 strreperror
  ${UnStrRep} $0 "test... test... 1 2 3..." "test" "testing"
  StrCmp $0 "testing... testing... 1 2 3..." 0 strreperror
  ${UnStrRep} $0 "" "test" "testing"
  StrCmp $0 "" 0 strreperror
  ${UnStrRep} $0 "test" "test" "testing"
  StrCmp $0 "testing" 0 strreperror
  ${UnStrRep} $0 "test" "test" ""
  StrCmp $0 "" 0 strreperror
  ${UnStrRep} $0 "test" "" "abc"
  StrCmp $0 "test" 0 strreperror
  ${UnStrRep} $0 "test" "" ""
  StrCmp $0 "test" 0 strreperror
  DetailPrint "PASSED StrRep test"
  Goto +2
strreperror:
  DetailPrint "FAILED StrRep test"

  # Test sorting
  ${UnStrSort} $0 "This is just an example" " just" "" "ple" "0" "0"
  StrCmp $0 "This is an exam" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "j" " " " " "0" "1"
  StrCmp $0 "just" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "j" "" "" "0" "1"
  StrCmp $0 "This is just an example" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "us" " " "" "0" "1"
  StrCmp $0 "just an example" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "u" "" " " "0" "1"
  StrCmp $0 "This is just" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "just" " " " " "0" "1"
  StrCmp $0 "just" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "t" " " " " "0" "1"
  StrCmp $0 "This" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "le" " " " " "0" "1"
  StrCmp $0 "example" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "le" " " " " "1" "0"
  StrCmp $0 " examp" 0 strsorterror
  ${UnStrSort} $0 "an error has occured" "e" " " " " "0" "1"
  StrCmp $0 "error" 0 strsorterror
  ${UnStrSort} $0 "" "something" " " " " "0" "1"
  StrCmp $0 "" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "j" " " " " "1" "1"
  StrCmp $0 " just " 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "j" " " " " "1" "0"
  StrCmp $0 " ust " 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "j" "" "" "1" "0"
  StrCmp $0 "This is ust an example" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "us" " " "" "1" "0"
  StrCmp $0 " jt an example" 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "u" "" " " "1" "0"
  StrCmp $0 "This is jst " 0 strsorterror
  ${UnStrSort} $0 "This is just an example" "just" " " " " "1" "0"
  StrCmp $0 "  " 0 strsorterror
  ${UnStrSort} $0 "an error has occured" "e" " " " " "1" "0"
  StrCmp $0 " rror " 0 strsorterror
  ${UnStrSort} $0 "" "something" " " " " "1" "0"
  StrCmp $0 "" 0 strsorterror
  DetailPrint "PASSED StrSort test"
  Goto +2
strsorterror:
  DetailPrint "FAILED StrSort test"

  # Test tokenizer
  ${UnStrTok} $0 "This is, or is not, just an example" " ," "5" "1"
  StrCmp $0 "not" 0 strtokerror
  ${UnStrTok} $0 "This is, or is not, just an example" " ," "5" "0"
  StrCmp $0 "is" 0 strtokerror
  ${UnStrTok} $0 "This is, or is not, just an example" " ," "152" "0"
  StrCmp $0 "" 0 strtokerror
  ${UnStrTok} $0 "This is, or is not, just an example" " ," "0" "0"
  StrCmp $0 "example" 0 strtokerror
  ${UnStrTok} $0 "This is, or is not, just an example" " ," "-1" "0"
  StrCmp $0 "example" 0 strtokerror
  ${UnStrTok} $0 "This is, or is not, just an example" " ," "1" "0"
  StrCmp $0 "This" 0 strtokerror
  DetailPrint "PASSED StrTok test"
  Goto +2
strtokerror:
  DetailPrint "FAILED StrTok test"

  # Test trim new lines
  ${UnStrTrimNewLines} $0 "$\r$\ntest$\r$\ntest$\r$\n"
  StrCmp $0 "$\r$\ntest$\r$\ntest" +3
    DetailPrint "FAILED StrTrimNewLines test"
    Goto +2
    DetailPrint "PASSED StrTrimNewLines test"

SectionEnd
