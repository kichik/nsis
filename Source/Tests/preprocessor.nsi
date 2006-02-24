!ifndef file_is_included
!define file_is_included

Name preprocessor
OutFile preprocessor.exe

!ifdef some_define_that_doesnt_exist
this should not be executed, so no error should be raised
/*
code inside comments should not be executed
!ifdef
*/
!endif

!ifdef d1
!error "d1 is not defined!"
!else ifdef d2
!error "d2 is not defined!"
!else
# this should be compiled
!endif

!define d1

!ifdef d1
# this should be compiled
!else ifdef d2
!error "d2 is not defined!"
!else
!error "d1 is defined!"
!endif

!undef d1
!define d2

!ifdef d1
!error "d1 is not defined!"
!else ifdef d2
# this should be compiled
!else
!error "d2 is defined!"
!endif

!ifdef some_define_that_doesnt_exist
the next !endif should be part of this line\
!endif
!\
e\
n\
d\
i\
f

# tests for !if statement
!if 'test' == 'test'
 !if 1 <= 2
  !if ! 100 < 99.99
   !if 2.2 > 1.12
    !if ! 23 >= 37
     !if 1 && 1
      !if ! 0 || 0

        # this should be compiled

      !else
       !error "!if ! 0 || 0 is true!"
      !endif
     !else
      !error "!if 1 && 1 is true!"
     !endif
    !else
     !error "!if ! 23 >= 37 is true!"
    !endif
   !else
    !error "!if 2.2 > 1.12 is true!"
   !endif
  !else
   !error "!if ! 100 < 99.99 is true!"
  !endif
 !else
  !error "!if 1 <= 2 is true!"
 !endif
!else
 !error "!if 'test' == 'test' is true!"
!endif

; testing of two math functions and a macro hack :)
!define increase "!insertmacro increase"
!macro increase DEFINE
  !define /math ${DEFINE}_MACROTEMP ${${DEFINE}} + 1
  !undef ${DEFINE}
  !define ${DEFINE} ${${DEFINE}_MACROTEMP}
  !undef ${DEFINE}_MACROTEMP
!macroend

!define number1 1 #  1
!define /math number2 2 + 3
!define /math number3 ${number2} - ${number1}
${increase} number3
!define /math number4 2 * ${number3}
!define /math number5 ${number4} % 3

!if ${number1} != 1
  !error "number1 != 1"
!endif

!if ${number2} != 5
  !error "number1 != 5"
!endif

!if ${number3} != 5
  !error "number1 != 5"
!endif

!if ${number4} != 10
  !error "number1 != 10"
!endif

!if ${number5} != 1
  !error "number1 != 1"
!endif

!undef number1
!undef number2
!undef number3
; end math functions

# this should just give a warning, not an error
!include /NONFATAL file_that_doesnt_exist.nsh

# this should include this file just one time.
!include preprocessor.nsi

Section
SectionEnd

!else
# this should just give a warning, not an error
!include /NONFATAL another_file_that_doesnt_exist.nsh
!endif
