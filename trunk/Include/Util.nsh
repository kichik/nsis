; ---------------------
;       Util.nsh
; ---------------------
;
; Voodoo macros to make end-user usage easier. This may be documented someday.

!verbose push
!verbose 3

!ifndef ___UTIL__NSH___
!define ___UTIL__NSH___

# see WinVer.nsh and *Func.nsh for usage examples
!macro CallArtificialFunction NAME
  !ifndef __UNINSTALL__
    !define CallArtificialFunction_TYPE inst
  !else
    !define CallArtificialFunction_TYPE uninst
  !endif
  Call :.${NAME}${CallArtificialFunction_TYPE}
  !ifndef ${NAME}${CallArtificialFunction_TYPE}_DEFINED
    Goto ${NAME}${CallArtificialFunction_TYPE}_DONE
    !define ${NAME}${CallArtificialFunction_TYPE}_DEFINED
    .${NAME}${CallArtificialFunction_TYPE}:
      !insertmacro ${NAME}
    Return
    ${NAME}${CallArtificialFunction_TYPE}_DONE:
  !endif
  !undef CallArtificialFunction_TYPE
!macroend
!define CallArtificialFunction `!insertmacro CallArtificialFunction`

!endif # !___UTIL__NSH___

!verbose pop
