name "Test"
outfile "test.exe"
silentinstall silent

!include "selectlib.nsh"

Section
  ${SELECT} "test1"
    ${CASE} "test1"
      MessageBox MB_OK "case test1"
    ${CASE} "test 2;test3"
      MessageBox MB_OK "case test2 or test3"
    ${CASE_ELSE}
      MessageBox MB_OK "case else"
  ${SELECTEND}
SectionEnd