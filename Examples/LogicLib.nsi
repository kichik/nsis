name "Test"
outfile "test.exe"
silentinstall silent

!include "selectlib.nsh"

Section
  !insertmacro SELECT "TESTCASE" "test1"
    !insertmacro CASE "test1"
      MessageBox MB_OK "case test1"
    !insertmacro CASE "test 2;test3"
      MessageBox MB_OK "case test2 or test3"
    !insertmacro CASE_ELSE
      MessageBox MB_OK "case else"
  !insertmacro SELECTEND
SectionEnd