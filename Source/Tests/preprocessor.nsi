### Preprocessor Tests ###

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
# invalid preprocessor should be ignored
!hello
!error valid_preprocessor_syntax_must_be_ignored
!define /foo /bar /baz and the same with invalid parameters
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

!if 0
/*
this shouldn't be compiled
!endif
*/
!endif

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


!define ASSERT `!insertmacro ASSERT "${U+24}{__FILE__}" ${U+24}{__LINE__} `
!macro ASSERT __file __line __xpr
!if ${__xpr}
!else
!error `ASSERT: ${__xpr} (${__file}:${__line})`
!endif
!macroend


; test macros
!macro TM_0
!macroend
!macro TM_1
!error "Wrong TM_1"
!macroend
!macro TM_2
!error "Wrong TM_2"
!macroend
!macroundef TM_2 ; Undefine the last macro
!macro TM_2
!if 0
!endif
!macroend
!ifmacrodef TM_1
!macroundef TM_1 ; Undefine "in the middle" macro
!endif
!macro TM_1
!macroend
!insertmacro TM_1
!insertmacro TM_2

!macro TM_Recursion def
!if '${${def}}' < 42
  !define /redef /math ${def} '${${def}}' + 1
  !insertmacro ${__MACRO__} ${def}
!endif
!macroend
!define /redef OUT1 0
!insertmacro TM_Recursion OUT1
${ASSERT} '${OUT1} = 42'


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
  !error "number2 != 5"
!endif

!if ${number3} != 5
  !error "number3 != 5"
!endif

!if ${number4} != 10
  !error "number4 != 10"
!endif

!if ${number5} != 1
  !error "number5 != 1"
!endif

!define /redef /math OUT1 0xffffffff >> 31
${ASSERT} '${OUT1} = -1'
!define /redef /math OUT1 0xffffffff >>> 31
${ASSERT} '${OUT1} = 1'
!define /redef /math OUT1 1 << 31
${ASSERT} '${OUT1} = 0x80000000'

!define /redef /math OUT1 0x80000000 ^ 0x40000000
${ASSERT} '${OUT1} = 0xC0000000'

!define /redef /intfmt OUT1 "0x%.3X" 42
${ASSERT} '${OUT1} = 0x02A'

!pragma internal x OUT "0x10 | 0x40" ; NSD requires this when using LoadAndSetImage
!if "${OUT}" <> 80
  !error "math expression failed"
!endif

; end math functions


# this should just give a warning, not an error
!include /NONFATAL file_that_doesnt_exist.nsh

# this should include this file just one time.
!include preprocessor.nsi


# test scopes

Section
Return
WriteUninstaller uninst.exe # avoid warning
SectionEnd

!macro TEST_SCOPE scope def should_exist

  !if ${should_exist} == y
    !ifndef ${def}
      !error "${def} not defined in ${scope} scope"
    !endif
  !else
    !ifdef ${def}
      !error "${def} defined in ${scope} scope"
    !endif
  !endif

!macroend

!macro TEST_SCOPES scope global section function pageex uninstall

  !insertmacro TEST_SCOPE "${scope}" __GLOBAL__    ${global}
  !insertmacro TEST_SCOPE "${scope}" __SECTION__   ${section}
  !insertmacro TEST_SCOPE "${scope}" __FUNCTION__  ${function}
  !insertmacro TEST_SCOPE "${scope}" __PAGEEX__    ${pageex}
  !insertmacro TEST_SCOPE "${scope}" __UNINSTALL__ ${uninstall}

!macroend

!insertmacro TEST_SCOPE "macro" __MACRO__ y
!insertmacro TEST_SCOPES "global" y n n n n

Section test
!insertmacro TEST_SCOPES "section" n y n n n
!if ${__SECTION__} != test
  !error "invalid __SECTION__ value"
!endif
SectionEnd

Section un.test
!insertmacro TEST_SCOPES "uninstall section" n y n n y
!if ${__SECTION__} != test
  !error "invalid __SECTION__ value"
!endif
SectionEnd

Function test
Call test # avoid warning
!insertmacro TEST_SCOPES "function" n n y n n
!if ${__FUNCTION__} != test
  !error "invalid __FUNCTION__ value"
!endif
FunctionEnd

Function un.test
Call un.test # avoid warning
!insertmacro TEST_SCOPES "uninstall function" n n y n y
!if ${__FUNCTION__} != test
  !error "invalid __FUNCTION__ value"
!endif
FunctionEnd

PageEx instfiles
!insertmacro TEST_SCOPES "pageex" n n n y n
!if ${__PAGEEX__} != instfiles
  !error "invalid __PAGEEX__ value"
!endif
PageExEnd

PageEx un.instfiles
!insertmacro TEST_SCOPES "uninstall pageex" n n n y y
!if ${__PAGEEX__} != instfiles
  !error "invalid __PAGEEX__ value"
!endif
PageExEnd

!insertmacro TEST_SCOPES "global" y n n n n


# test !pragma
!pragma warning push
  !pragma warning disable 7000
  !include /NONFATAL doesnt_exist_nor_can_you_see_me.nsh
!pragma warning pop

!pragma warning push
  !pragma warning disable all
  !include /NONFATAL doesnt_exist_nor_can_you_see_me.nsh
  !pragma warning push
    !pragma warning error all
  !pragma warning pop
  !warning "You can't see me" ; "disable all" is still in effect
!pragma warning pop

!if ! 1 <> 0
  !error "1 is not 0"
!endif
!pragma warning push
!pragma warning disable 7070 ; Invalid number
!if Hello <> 0
  !error "Hello is not a number"
!endif
!pragma warning pop

# test !searchparse
!searchparse "AbcDef" "Abc" OUT1
${ASSERT} '${OUT1} S== "Def"'

!define /redef OUT1 FAILED
!searchparse /noerrors "AbcDef" "FailThis" OUT1
${ASSERT} '${OUT1} S== "FAILED"'

!searchparse /ignorecase "AbcDef" "ABC" OUT1
${ASSERT} '${OUT1} S== "Def"'

!searchparse "AbcDef" "Ab" OUT1 "D" OUT2
${ASSERT} '"${OUT1}${OUT2}" S== "cef"'

!searchparse /ignorecase /file "${__FILE__}" "### " OUT1 " Tests"
${ASSERT} '${OUT1} == "Preprocessor"'

!searchparse "AbcDef" "" OUT1 "Def" ; Empty first search string and chopping off the end without defining OUTPUTSYMBOL2
${ASSERT} '${OUT1} S== "Abc"'


# test !searchreplace
!searchreplace OUT1 "FooBar" "Bar" "Baz"
${ASSERT} '${OUT1} S== "FooBaz"'

!searchreplace OUT1 "FooBarBar" "Bar" "Baz" ; "replacing all instances"
${ASSERT} '${OUT1} S== "FooBazBaz"'

!searchreplace OUT1 "FooBar" "BAR" "Baz"
${ASSERT} '${OUT1} S== "FooBar"'

!searchreplace /ignorecase OUT1 "FooBar" "BAR" "Baz"
${ASSERT} '${OUT1} S== "FooBaz"'

!searchreplace OUT1 "FooBar" "FailThis" "Baz" ; "allows you to redefine symbol_out without warning or error"
${ASSERT} '${OUT1} S== "FooBar"'


!verbose 4
!echo "Completed tests"
!verbose 2
!pragma whip 0 # EOF
!else

# this should just give a warning, not an error
!include /NONFATAL another_file_that_doesnt_exist.nsh

!endif
