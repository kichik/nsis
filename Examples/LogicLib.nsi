Name "test"
OutFile "test.exe"
SilentInstall silent

!include "logiclib.nsh"

Section
  ${IF} "test" = "test2"
    MessageBox MB_OK "test"
  ${ELSE}
    ${IF} "test" = "test"
      ${SELECT} 5
        ${CASE} 4
          MessageBox MB_OK "case 4"
        ${CASE} 5;6
          MessageBox MB_OK "case 5 or 6"
        ${CASE_ELSE}
          MessageBox MB_OK "case else"
      ${ENDSELECT}
    ${ENDIF}
  ${ENDIF}
SectionEnd