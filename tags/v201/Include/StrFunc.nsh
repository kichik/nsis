/*
o-----------------------------------------------------------------------------o
|String Functions Header File 1.07                                            |
(-----------------------------------------------------------------------------)
| By deguix                                     / A Header file for NSIS 2.01 |
| <cevo_deguix@yahoo.com.br>                   -------------------------------|
|                                                                             |
|    This header file contains NSIS functions for string manipulation.        |
o-----------------------------------------------------------------------------o
*/

!verbose push
!verbose 3
!ifndef STRFUNC_VERBOSITY
  !define STRFUNC_VERBOSITY 3
!endif
!define _STRFUNC_VERBOSITY ${STRFUNC_VERBOSITY}
!undef STRFUNC_VERBOSITY
!verbose ${_STRFUNC_VERBOSITY}

!include LogicLib.nsh

!ifndef STRFUNC

  ;Header File Identification

  !define STRFUNC `String Functions Header File`
  !define STRFUNC_SHORT `StrFunc`
  !define STRFUNC_CREDITS `2004 Diego Pedroso`

  ;Header File Version

  !define STRFUNC_VERMAJ 1
  !define STRFUNC_VERMED 07
 ;!define STRFUNC_VERMIN 0
 ;!define STRFUNC_VERBLD 0

  !define STRFUNC_VER `${STRFUNC_VERMAJ}.${STRFUNC_VERMED}`

  ;Header File Init Message Prefix and Postfix

  !define STRFUNC_INITMSGPRE `----------------------------------------------------------------------$\r$\n`
  !define STRFUNC_INITMSGPOST `$\r$\n----------------------------------------------------------------------$\r$\n`

  ;Header File Init Message

  !verbose push
  !verbose 4
  !echo `${STRFUNC_INITMSGPRE}NSIS ${STRFUNC} ${STRFUNC_VER} - © ${STRFUNC_CREDITS}${STRFUNC_INITMSGPOST}`
  !verbose pop

  ;Header File Function Init Message Prefix and Postfix

  !define STRFUNC_FUNCMSGPRE ``
  !define STRFUNC_FUNCMSGPOST ``
  
  ;Header File Function Macros

  !macro STRFUNC_DEFFUNC Name
    !define `${Name}` `!insertmacro FUNCTION_STRING_${Name}`
    !define `Un${Name}` `!insertmacro FUNCTION_STRING_Un${Name}`
  !macroend
  
  !macro STRFUNC_FUNC ShortName Credits
    !verbose push
    !verbose 4

    !ifndef `Un${ShortName}`
      !echo `${STRFUNC_FUNCMSGPRE}$ {Un${ShortName}} - © ${Credits}${STRFUNC_FUNCMSGPOST}`
      !verbose pop
      !define `Un${ShortName}` `!insertmacro FUNCTION_STRING_Un${ShortName}_Call`
      Function `un.${ShortName}`
    !else
      !echo `${STRFUNC_FUNCMSGPRE}$ {${ShortName}} - © ${Credits}${STRFUNC_FUNCMSGPOST}`
      !verbose pop
      !undef `${ShortName}`
      !define `${ShortName}` `!insertmacro FUNCTION_STRING_${ShortName}_Call`
      Function `${ShortName}`
    !endif
  !macroend

  ;Function Names Startup Definition

  !insertmacro STRFUNC_DEFFUNC StrCase
  !macro `FUNCTION_STRING_UnStrCase`
    !undef UnStrCase
    !insertmacro FUNCTION_STRING_StrCase
  !macroend
  
  !insertmacro STRFUNC_DEFFUNC StrClb
  !macro `FUNCTION_STRING_UnStrClb`
    !undef UnStrClb
    !insertmacro FUNCTION_STRING_StrClb
  !macroend

  !insertmacro STRFUNC_DEFFUNC StrIOToNSIS
  !macro `FUNCTION_STRING_UnStrIOToNSIS`
    !undef UnStrIOToNSIS
    !insertmacro FUNCTION_STRING_StrIOToNSIS
  !macroend

  !insertmacro STRFUNC_DEFFUNC StrLoc
  !macro `FUNCTION_STRING_UnStrLoc`
    !undef UnStrLoc
    !insertmacro FUNCTION_STRING_StrLoc
  !macroend

  !insertmacro STRFUNC_DEFFUNC StrNSISToIO
  !macro `FUNCTION_STRING_UnStrNSISToIO`
    !undef UnStrNSISToIO
    !insertmacro FUNCTION_STRING_StrNSISToIO
  !macroend

  !insertmacro STRFUNC_DEFFUNC StrRep
  !macro `FUNCTION_STRING_UnStrRep`
    !undef UnStrRep
    !insertmacro FUNCTION_STRING_StrRep
  !macroend

  !insertmacro STRFUNC_DEFFUNC StrSort
  !macro `FUNCTION_STRING_UnStrSort`
    !undef UnStrSort
    !insertmacro FUNCTION_STRING_StrSort
  !macroend

  !insertmacro STRFUNC_DEFFUNC StrStr
  !macro `FUNCTION_STRING_UnStrStr`
    !undef UnStrStr
    !insertmacro FUNCTION_STRING_StrStr
  !macroend

  !insertmacro STRFUNC_DEFFUNC StrStrAdv
  !macro `FUNCTION_STRING_UnStrStrAdv`
    !undef UnStrStrAdv
    !insertmacro FUNCTION_STRING_StrStrAdv
  !macroend

  !insertmacro STRFUNC_DEFFUNC StrTok
  !macro `FUNCTION_STRING_UnStrTok`
    !undef UnStrTok
    !insertmacro FUNCTION_STRING_StrTok
  !macroend

  !insertmacro STRFUNC_DEFFUNC StrTrimNewLines
  !macro `FUNCTION_STRING_UnStrTrimNewLines`
    !undef UnStrTrimNewLines
    !insertmacro FUNCTION_STRING_StrTrimNewLines
  !macroend

  ;Function Codes for Install and Uninstall

  # Function StrCase
  ################

  !macro FUNCTION_STRING_StrCase
    !insertmacro STRFUNC_FUNC `StrCase` `2004 Diego Pedroso - Based on StrUpper and StrLower by Dave Laundon`

      Exch $1
      Exch
      Exch $0
      Push $2
      Push $3
      Push $4
      Push $5
      Push $6
      Push $7
      Push $8

      ; Clean pushed variables

      StrCpy $2 ""
      StrCpy $3 ""
      StrCpy $4 ""
      StrCpy $5 ""
      StrCpy $6 ""
      StrCpy $7 ""
      StrCpy $8 ""

      ${If} $1 == "U"

        ;Upper Case System:
        ;------------------
        ; Convert all characters to upper case.

        System::Call "User32::CharUpper(t r0 r5)i"
        Goto StrCase_End
      ${ElseIf} $1 == "L"

        ;Lower Case System:
        ;------------------
        ; Convert all characters to lower case.

        System::Call "User32::CharLower(t r0 r5)i"
        Goto StrCase_End
      ${EndIf}

      StrLen $2 $0

      ; For the rest of cases, make a loop
      ${For} $3 0 $2

        IntOp $4 $3 + 1

        ; Step 1: Detect one character at a time

        ${If} $3 <> 0
          StrCpy $6 $0 `` $3
        ${EndIf}

        ${If} $4 <> $2
          ${If} $3 = 0
            StrCpy $6 $0 1
          ${Else}
            StrCpy $6 $6 1
          ${EndIf}
        ${EndIf}

        ; Step 2: Convert to the advanced case user chose:

        ${If} $1 == "T"

          ;Title Case System:
          ;------------------
          ; Convert all characters after a non-alphabetic character to upper case.
          ; Else convert to lower case.

          System::Call "*(&t1 r7) i .r8"
          System::Call "*$8(&i1 .r7)"
		  System::Free $8
          System::Call "user32::IsCharAlpha(i r7) i .r8"
          ${If} $8 = 0
            System::Call "User32::CharUpper(t r6 r6)i"
          ${Else}
            System::Call "User32::CharLower(t r6 r6)i"
          ${EndIf}
        ${ElseIf} $1 == "S"

          ;Sentence Case System:
          ;------------------
          ; Convert all characters after a ".", "!" or "?" character to upper case.
          ; Else convert to lower case. Spaces or tabs after these marks are ignored.

          ${If} $6 == " "
          ${OrIf} $6 == "$\t"
            Goto IgnoreLetter
          ${EndIf}

          ${If} $7 == "."
          ${OrIf} $7 == "!"
          ${OrIf} $7 == "?"
          ${OrIf} $7 == ""
            System::Call "User32::CharUpper(t r6 r6)i"
          ${Else}
            System::Call "User32::CharLower(t r6 r6)i"
          ${EndIf}
        ${ElseIf} $1 == "<>"

          ;Switch Case System:
          ;------------------
          ; Switch all characters cases to their inverse case.

          System::Call "*(&t1 r6) i .r8"
          System::Call "*$8(&i1 .r7)"
		  System::Free $8
          System::Call "user32::IsCharUpper(i r7) i .r8"
          ${If} $8 = 0
            System::Call "User32::CharUpper(t r6 r6)i"
          ${Else}
            System::Call "User32::CharLower(t r6 r6)i"
          ${EndIf}
        ${EndIf}

        ; Write the character to TempString

        StrCpy $7 $6

        IgnoreLetter:
        StrCpy $5 `$5$6`

      ${Next}

      StrCase_End:

      ; TempString is the final string

      StrCpy $0 $5

      Pop $8
      Pop $7
      Pop $6
      Pop $5
      Pop $4
      Pop $3
      Pop $2
      Pop $1
      Exch $0

    FunctionEnd

  !macroend

  !macro FUNCTION_STRING_StrClb
    !insertmacro STRFUNC_FUNC `StrClb` `2004 Diego Pedroso - Based on CopyToClipboard and CopyFromClipboard by Nik Medved`

      ;Get input from user

      Exch $1
      Exch
      Exch $0
      Exch
      Push $2
      Push $3
      Push $4

      ;Open the clipboard to do the operations the user chose
      System::Call 'user32::OpenClipboard(i $HWNDPARENT)'

      ${If} $1 == ">" ;Set

        ;Step 1: Clear the clipboard
        System::Call 'user32::EmptyClipboard()'

        ;Step 2: Allocate global heap
        StrLen $2 $0
        IntOp $2 $2 + 1
        System::Call 'kernel32::GlobalAlloc(i 2, i r2) i.r2'

        ;Step 3: Lock the handle
        System::Call 'kernel32::GlobalLock(i r2) i.r3'

        ;Step 4: Copy the text to locked clipboard buffer
        System::Call 'kernel32::lstrcpyA(i r3, t r0)'

        ;Step 5: Unlock the handle again
        System::Call 'kernel32::GlobalUnlock(i r2)'

        ;Step 6: Set the information to the clipboard
        System::Call 'user32::SetClipboardData(i 1, i r2)'

        StrCpy $0 ""

      ${ElseIf} $1 == "<" ;Get

        ;Step 1: Get clipboard data
        System::Call 'user32::GetClipboardData(i 1) i .r2'

		;Step 2: Lock and copy data
		System::Call 'kernel32::GlobalLock(i r2) t .r0'

		;Step 3: Unlcok
		System::Call 'kernel32::GlobalUnlock(i r2)'

      ${ElseIf} $1 == "<>" ;Swap

        ;Step 1: Get clipboard data
        System::Call 'user32::GetClipboardData(i 1) t .r2'

		;Step 2: Lock and copy data
		System::Call 'kernel32::GlobalLock(i r2) t .r4'

		;Step 3: Unlcok
		System::Call 'kernel32::GlobalUnlock(i r2)'

        ;Step 4: Clear the clipboard
        System::Call 'user32::EmptyClipboard()'

        ;Step 5: Allocate global heap
        StrLen $2 $0
        IntOp $2 $2 + 1
        System::Call 'kernel32::GlobalAlloc(i 2, i r2) i.r2'

        ;Step 6: Lock the handle
        System::Call 'kernel32::GlobalLock(i r2) i.r3'

        ;Step 7: Copy the text to locked clipboard buffer
        System::Call 'kernel32::lstrcpyA(i r3, t r0)'

        ;Step 8: Unlock the handle again
        System::Call 'kernel32::GlobalUnlock(i r2)'

        ;Step 9: Set the information to the clipboard
        System::Call 'user32::SetClipboardData(i 1, i r2)'

        StrCpy $0 $4
      ${Else} ;Clear

        ;Step 1: Clear the clipboard
        System::Call 'user32::EmptyClipboard()'

        StrCpy $0 ""
      ${EndIf}

      ;Close the clipboard
      System::Call 'user32::CloseClipboard()'

      ;Return result to user

      Pop $4
      Pop $3
      Pop $2
      Pop $1
      Exch $0
      
    FunctionEnd

  !macroend

  # Function StrIOToNSIS
  ####################

  !macro FUNCTION_STRING_StrIOToNSIS
    !insertmacro STRFUNC_FUNC `StrIOToNSIS` `2003-2004 Amir Szekely, Joost Verburg and Dave Laundon`

      Exch $0 ; The source
      Push $1 ; The output
      Push $2 ; Temporary char
      StrCpy $1 `` ; Initialise the output
    loop:
      StrCpy $2 $0 1 ; Get the next source char
      StrCmp $2 `` done ; Abort when none left
        StrCpy $0 $0 `` 1 ; Remove it from the source
        StrCmp $2 `\` +3 ; Escape character?
          StrCpy $1 `$1$2` ; If not just output
          Goto loop
        StrCpy $2 $0 1 ; Get the next source char
        StrCpy $0 $0 `` 1 ; Remove it from the source
        StrCmp $2 `\` `` +3 ; Back-slash?
          StrCpy $1 `$1\`
          Goto loop
        StrCmp $2 `r` `` +3 ; Carriage return?
          StrCpy $1 `$1$\r`
          Goto loop
        StrCmp $2 `n` `` +3 ; Line feed?
          StrCpy $1 `$1$\n`
          Goto loop
        StrCmp $2 `t` `` +3 ; Tab?
          StrCpy $1 `$1$\t`
          Goto loop
        StrCpy $1 `$1$2` ; Anything else (should never get here)
        Goto loop
    done:
      StrCpy $0 $1
      Pop $2
      Pop $1
      Exch $0
    FunctionEnd

  !macroend

  # Function StrLoc
  ###############

  !macro FUNCTION_STRING_StrLoc
    !insertmacro STRFUNC_FUNC `StrLoc` `2004 Diego Pedroso`

      Exch $R0
      Exch
      Exch $R1 ; st=haystack,old$R1, $R1=needle
      Exch 2    ; st=old$R1,haystack
      Exch $R2 ; st=old$R1,old$R2, $R2=haystack
      Push $R3
      Push $R4
      Push $R5
      StrLen $R3 $R1
      StrCpy $R4 0
      loop:
        StrCpy $R5 $R2 $R3 $R4
        StrCmp $R5 $R1 done
        StrCmp $R5 `` error
        IntOp $R4 $R4 + 1
        Goto loop
      done:

      StrCmp $R0 `<` 0 +5
        StrLen $R0 $R2
        IntOp $R5 $R3 + $R4
        IntOp $R0 $R0 - $R5
        Goto +2

      StrCpy $R0 $R4
      Goto +2

      error:
      StrCpy $R0 ``

      Pop $R5
      Pop $R4
      Pop $R3
      Pop $R2
      Exch
      Pop $R1
      Exch $R0
    FunctionEnd

  !macroend

  # Function StrNSISToIO
  ####################

  !macro FUNCTION_STRING_StrNSISToIO
    !insertmacro STRFUNC_FUNC `StrNSISToIO` `2003-2004 Amir Szekely, Joost Verburg and Dave Laundon`

      Exch $0 ; The source
      Push $1 ; The output
      Push $2 ; Temporary char
      StrCpy $1 `` ; Initialise the output
    loop:
      StrCpy $2 $0 1 ; Get the next source char
      StrCmp $2 `` done ; Abort when none left
        StrCpy $0 $0 `` 1 ; Remove it from the source
        StrCmp $2 `\` `` +3 ; Back-slash?
          StrCpy $1 `$1\\`
          Goto loop
        StrCmp $2 `$\r` `` +3 ; Carriage return?
          StrCpy $1 `$1\r`
          Goto loop
        StrCmp $2 `$\n` `` +3 ; Line feed?
          StrCpy $1 `$1\n`
          Goto loop
        StrCmp $2 `$\t` `` +3 ; Tab?
          StrCpy $1 `$1\t`
          Goto loop
        StrCpy $1 `$1$2` ; Anything else
        Goto loop
    done:
      StrCpy $0 $1
      Pop $2
      Pop $1
      Exch $0
    FunctionEnd

  !macroend

  # Function StrRep
  ###############

  !macro FUNCTION_STRING_StrRep
    !insertmacro STRFUNC_FUNC `StrRep` `2002-2004 Hendri Adriaens`

      Exch $0 ;this will replace wrong characters
      Exch
      Exch $1 ;needs to be replaced
      Exch
      Exch 2
      Exch $2 ;the orginal string
      Push $3 ;counter
      Push $4 ;temp character
      Push $5 ;temp string
      Push $6 ;length of string that need to be replaced
      Push $7 ;length of string that will replace
      Push $R0 ;tempstring
      Push $R1 ;tempstring
      Push $R2 ;tempstring
      StrCpy $3 `-1`
      StrCpy $5 ``
      StrLen $6 $1
      StrLen $7 $0
      Loop:
      IntOp $3 $3 + 1
      StrCpy $4 $2 $6 $3
      StrCmp $4 `` ExitLoop
      StrCmp $4 $1 Replace
      Goto Loop
      Replace:
      StrCpy $R0 $2 $3
      IntOp $R2 $3 + $6
      StrCpy $R1 $2 `` $R2
      StrCpy $2 $R0$0$R1
      IntOp $3 $3 + $7
      Goto Loop
      ExitLoop:
      StrCpy $0 $2
      Pop $R2
      Pop $R1
      Pop $R0
      Pop $7
      Pop $6
      Pop $5
      Pop $4
      Pop $3
      Pop $2
      Pop $1
      Exch $0
    FunctionEnd

  !macroend

  # Function StrSort
  ################

  !macro FUNCTION_STRING_StrSort
    !insertmacro STRFUNC_FUNC `StrSort` `2004 Diego Pedroso - based on SortString by "Afrow UK"`

      # Prepare Variables

      Exch $R7 ;Include Center string
      Exch
      Exch $R6 ;Include Left and Right strings
      Exch 2
      Exch $0 ;Right String
      Exch 3
      Exch $1 ;Left String
      Exch 4
      Exch $2 ;Center String
      Exch 5
      Exch $R0 ;String
      Push $3
      Push $4
      Push $5
      Push $R1
      Push $R2
      Push $R3
      Push $R4
      Push $R5

      StrLen $3 $0
      StrLen $4 $1
      StrLen $5 $2
      StrCpy $R1 0

      # Center String Search

      loop:
        StrCpy $R3 $R0 $5 $R1
        StrCmp $R3 `` error
          StrCmp $R3 $2 done
            IntOp $R1 $R1 + 1
            Goto loop
      done:

      StrCpy $R5 $R1

      IntOp $R1 $R1 - $4

      # Left String Search

      loop2:
        StrCpy $R3 $R0 $4 $R1
        StrCmp $R3 `` error2
          StrCmp $R3 $1 done2
            IntOp $R1 $R1 - 1
            Goto loop2

        error2:
        StrCpy $R1 0
        StrCpy $R3 0
        Goto +2

      done2:
      StrCpy $R3 1

      StrCpy $R4 $R0 $R5

      StrCmp $R1 0 +2
        StrCpy $R4 $R4 `` $R1

      StrCmp $R3 1 0 +3
        StrCmp $R6 0 0 +2
          StrCpy $R4 $R4 `` $4

      # Center String Addition

      StrCmp $R7 0 +2
        StrCpy $R4 $R4$2

      StrCpy $R1 $R5
      IntOp $R1 $R1 + $5

      # Right String Search

      loop3:

        StrCpy $R3 $R0 $3 $R1
        StrCmp $R3 `` error3
          StrCmp $R3 $0 done3
            IntOp $R1 $R1 + 1
            Goto loop3

        error3:
        StrCpy $R1 0

      done3:

      IntOp $R5 $R5 + $5
      StrCpy $R3 $R0 `` $R5

      StrCmp $R1 0 +5
        IntOp $R1 $R1 - $R5
        StrCmp $R6 0 +2
          IntOp $R1 $R1 + $3
        StrCpy $R3 $R3 $R1

      StrCpy $R4 $R4$R3

      StrCpy $2 $R4
      Goto +2

        Error:
        StrCpy $2 ``

      # Return to User

      Pop $R5
      Pop $R4
      Pop $R3
      Pop $R2
      Pop $R1
      Pop $5
      Pop $4
      Pop $3
      Pop $R0
      Pop $R7
      Pop $R6
      Pop $0
      Pop $1
      Exch $2

    FunctionEnd

  !macroend
  
  # Function StrStr
  ###############

  !macro FUNCTION_STRING_StrStr
    !insertmacro STRFUNC_FUNC `StrStr` `2002-2004 Ximon Eighteen`

      Exch $R1 ; st=haystack,old$R1, $R1=needle
      Exch    ; st=old$R1,haystack
      Exch $R2 ; st=old$R1,old$R2, $R2=haystack
      Push $R3
      Push $R4
      Push $R5
      StrLen $R3 $R1
      StrCpy $R4 0
      ; $R1=needle
      ; $R2=haystack
      ; $R3=len(needle)
      ; $R4=cnt
      ; $R5=tmp
      loop:
        StrCpy $R5 $R2 $R3 $R4
        StrCmp $R5 $R1 done
        StrCmp $R5 `` done
        IntOp $R4 $R4 + 1
        Goto loop
      done:
      StrCpy $R1 $R2 `` $R4
      Pop $R5
      Pop $R4
      Pop $R3
      Pop $R2
      Exch $R1
    FunctionEnd

  !macroend

  # Function StrStrAdv
  ##################

  !macro FUNCTION_STRING_StrStrAdv
    !insertmacro STRFUNC_FUNC `StrStrAdv` `2003-2004 Diego Pedroso`

      ;Get input from user

      Exch $6 ;CaseSensitive (input)
      Exch
      Exch $5 ;NumLoops (input)
      Exch
      Exch 2
      Exch $4 ;ShowStrToSearch (input)
      Exch 2
      Exch 3
      Exch $3 ;DirectionOfReturn (input)
      Exch 3
      Exch 4
      Exch $2 ;DirectionOfSearch (input)
      Exch 4
      Exch 5
      Exch $1 ;StrToSearch (input)
      Exch 5
      Exch 6
      Exch $0 ;String (input)
      Exch 6
      Push $7 ;StringLength (temp)
      Push $8 ;StrToSearchLength (temp)
      Push $9 ;Loop (temp)
      Push $R3 ;Temp (temp)
      Push $R2 ;OutVar (output)
      Push $R1 ;StartCharPos (output)
      Push $R0 ;EndCharPos (output)

      ; Verify if we have the correct values on the variables
      ${If} $0 == ``
        SetErrors ;AdvStrStr_StrToSearch not found
        Goto AdvStrStr_End
      ${EndIf}

      ${If} $1 == ``
        SetErrors ;No text to search
        Goto AdvStrStr_End
      ${EndIf}

      ${If} $2 != <
        StrCpy $2 >
      ${EndIf}

      ${If} $3 != <
        StrCpy $3 >
      ${EndIf}

      ${If} $4 <> 0
        StrCpy $4 1
      ${EndIf}

      ${If} $5 <= 0
        StrCpy $5 0
      ${EndIf}

      ${If} $6 <> 1
        StrCpy $6 0
      ${EndIf}

      ; Find "AdvStrStr_String" length
      StrLen $7 $0

      ; Then find "AdvStrStr_StrToSearch" length
      StrLen $8 $1

      ; Now set up basic variables

      ${If} $2 == <
        IntOp $R1 $7 - $8
        StrCpy $R2 $7
      ${Else}
        StrCpy $R1 0
        StrCpy $R2 $8
      ${EndIf}

      StrCpy $9 0 ; First loop

      ;Let's begin the search

      ${Do}
        ; Step 1: If the starting or ending numbers are negative
        ;         or more than AdvStrStr_StringLen, we return
        ;         error

        ${If} $R1 < 0
          StrCpy $R1 ``
          StrCpy $R2 ``
          StrCpy $R3 ``
          SetErrors ;AdvStrStr_StrToSearch not found
          Goto AdvStrStr_End
        ${ElseIf} $R2 > $7
          StrCpy $R1 ``
          StrCpy $R2 ``
          StrCpy $R3 ``
          SetErrors ;AdvStrStr_StrToSearch not found
          Goto AdvStrStr_End
        ${EndIf}

        ; Step 2: Start the search depending on
        ;         AdvStrStr_DirectionOfSearch. Chop down not needed
        ;         characters.

        ${If} $R1 <> 0
          StrCpy $R3 $0 `` $R1
        ${EndIf}

        ${If} $R2 <> $7
          ${If} $R1 = 0
            StrCpy $R3 $0 $8
          ${Else}
            StrCpy $R3 $R3 $8
          ${EndIf}
        ${EndIf}

        ; Step 3: Make sure that's the string we want

        ; Case-Sensitive Support <- Use "AdvStrStr_Temp"
        ; variable because it won't be used anymore

        ${If} $6 == 1
          System::Call `kernel32::lstrcmpA(ts, ts) i.s` `$R3` `$1`
          Pop $R3
          ${If} $R3 = 0
            StrCpy $R3 1 ; Continue
          ${Else}
            StrCpy $R3 0 ; Break
          ${EndIf}
        ${Else}
          ${If} $R3 == $1
            StrCpy $R3 1 ; Continue
          ${Else}
            StrCpy $R3 0 ; Break
          ${EndIf}
        ${EndIf}

        ; After the comparasion, confirm that it is the
        ; value we want.

        ${If} $R3 = 1

          ;We found it, return except if the user has set up to
          ;search for another one:
          ${If} $9 >= $5

            ;Now, let's see if the user wants
            ;AdvStrStr_StrToSearch to appear:
            ${If} $4 == 0
              ;Return depends on AdvStrStr_DirectionOfReturn
              ${If} $3 == <
      	        ; RTL
      	        StrCpy $R0 $0 $R1
      	      ${Else}
      	        ; LTR
      	        StrCpy $R0 $0 `` $R2
              ${EndIf}
              ${Break}
            ${Else}
              ;Return depends on AdvStrStr_DirectionOfReturn
              ${If} $3 == <
                ; RTL
                StrCpy $R0 $0 $R2
              ${Else}
                ; LTR
                StrCpy $R0 $0 `` $R1
              ${EndIf}
              ${Break}
            ${EndIf}
          ${Else}
            ;If the user wants to have more loops, let's do it so!
            IntOp $9 $9 + 1

            ${If} $2 == <
              IntOp $R1 $R1 - 1
              IntOp $R2 $R2 - 1
            ${Else}
              IntOp $R1 $R1 + 1
              IntOp $R2 $R2 + 1
            ${EndIf}
          ${EndIf}
        ${Else}
          ; Step 4: We didn't find it, so do steps 1 thru 3 again

          ${If} $2 == <
            IntOp $R1 $R1 - 1
            IntOp $R2 $R2 - 1
          ${Else}
            IntOp $R1 $R1 + 1
            IntOp $R2 $R2 + 1
          ${EndIf}
        ${EndIf}
      ${Loop}

      AdvStrStr_End:

      ;Add 1 to AdvStrStr_EndCharPos to be supportable
      ;by "StrCpy"

      IntOp $R2 $R2 - 1

      ;Return output to user

      Exch $R0
      Exch
      Pop $R1
      Exch
      Pop $R2
      Exch
      Pop $R3
      Exch
      Pop $9
      Exch
      Pop $8
      Exch
      Pop $7
      Exch
      Pop $6
      Exch
      Pop $5
      Exch
      Pop $4
      Exch
      Pop $3
      Exch
      Pop $2
      Exch
      Pop $1
      Exch
      Pop $0

    FunctionEnd

  !macroend

  # Function StrTok
  ###############

  !macro FUNCTION_STRING_StrTok
    !insertmacro STRFUNC_FUNC `StrTok` `2004 Diego Pedroso - based on StrTok by "bigmac666"`
      Exch $9
      Exch
      Exch $R0
      Exch 2
      Exch $R1
      Exch 3
      Exch $R2
      Push $R3
      Push $R4
      Push $R5
      Push $R6
      Push $R7
      Push $R8
      Push $R9
      Push $0
      Push $1
      Push $2

      StrCpy $R8 0
      StrCpy $R9 $R1

      IntCmp $R0 0 0 0 +2
        StrCpy $R0 L


      StrCmp $R0 L 0 +5
        StrCpy $2 1
        StrCpy $R0 0
        StrCpy $1 ``
        StrCpy $9 1

      PartLoop:

      StrCpy $R4 0
      IntOp $R8 $R8 + 1
      StrCpy $0 0

      loop:

        StrCpy $R5 $R2 1 $R4
        StrCmp $R5 `` done

        StrCpy $R6 -1
        StrCpy $R7 0
        loop2:

          IntOp $R6 $R6 + 1
          IntOp $R7 $R6 + 1
          StrCpy $R3 $R1 $R7 $R6
          StrCmp $R3 `` 0 +3
            IntOp $0 $0 + 1
            Goto ContLoop2
          StrCmp $R5 $R3 0 Loop2
            StrCmp $9 1 0 done
              StrCmp $0 0 0 done
                StrCpy $R2 $R2 `` 1
                Goto Loop

        ContLoop2:

        IntOp $R4 $R4 + 1
        Goto loop

      done:
      IntOp $R4 $R4 + $0
      StrCpy $R1 $R2 $0
      IntOp $0 $0 + 1
      StrCpy $R2 $R2 `` $0

      StrCmp $2 1 0 +4
        StrCmp $R1 `` 0 +3
          StrCpy $R1 $1
          Goto End

      StrCmp $R0 $R8 End
        StrCmp $2 1 0 +2
          StrCpy $1 $R1
        StrCpy $R1 $R9
        Goto PartLoop

      End:

      StrCpy $9 $R1

      Pop $2
      Pop $1
      Pop $0
      Pop $R9
      Pop $R8
      Pop $R7
      Pop $R6
      Pop $R5
      Pop $R4
      Pop $R3
      Pop $R2
      Pop $9
      Pop $R0
      Exch $R1
    FunctionEnd

  !macroend

  # Function StrTrimNewLines
  ########################

  !macro FUNCTION_STRING_StrTrimNewLines
    !insertmacro STRFUNC_FUNC `StrTrimNewLines` `2003-2004 Ximon Eighteen`

      Exch $R0
      Push $R1
      Push $R2
      StrCpy $R1 0

    loop:
      IntOp $R1 $R1 - 1
      StrCpy $R2 $R0 1 $R1
      StrCmp $R2 `$\r` loop
      StrCmp $R2 `$\n` loop

      IntOp $R1 $R1 + 1
      IntCmp $R1 0 no_trim_needed
      StrCpy $R0 $R0 $R1

    no_trim_needed:
      Pop $R2
      Pop $R1
      Exch $R0
    FunctionEnd

  !macroend

  ;Function Calls for Install and Uninstall

  !macro FUNCTION_STRING_StrCase_Call ResultVar String Type
    !verbose push
    !verbose 4
    !echo `$ {StrCase} "${ResultVar}" "${String}" "${Type}"`
    !verbose pop

    Push `${String}`
    Push `${Type}`
    Call StrCase
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrCase_Call ResultVar String Type
    !verbose push
    !verbose 4
    !echo `$ {UnStrCase} "${ResultVar}" "${String}" "${Type}"`
    !verbose pop

    Push `${String}`
    Push `${Type}`
    Call un.StrCase
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrClb_Call ResultVar String Action
    !verbose push
    !verbose 4
    !echo `$ {StrClb} "${ResultVar}" "${String}" "${Action}"`
    !verbose pop

    Push `${String}`
    Push `${Action}`
    Call StrClb
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrClb_Call ResultVar String Action
    !verbose push
    !verbose 4
    !echo `$ {UnStrClb} "${ResultVar}" "${String}" "${Action}"`
    !verbose pop

    Push `${String}`
    Push `${Action}`
    Call un.StrClb
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrIOToNSIS_Call ResultVar String
    !verbose push
    !verbose 4
    !echo `$ {StrIOToNSIS} "${ResultVar}" "${String}"`
    !verbose pop

    Push `${String}`
    Call StrIOToNSIS
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrIOToNSIS_Call ResultVar String
    !verbose push
    !verbose 4
    !echo `$ {UnStrIOToNSIS} "${ResultVar}" "${String}"`
    !verbose pop

    Push `${String}`
    Call un.StrIOToNSIS
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrLoc_Call ResultVar String StrToSearchFor OffsetDirection
    !verbose push
    !verbose 4
    !echo `$ {StrLoc} "${ResultVar}" "${String}" "${StrToSearchFor}" "${OffsetDirection}"`
    !verbose pop

    Push `${String}`
    Push `${StrToSearchFor}`
    Push `${OffsetDirection}`
    Call StrLoc
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrLoc_Call ResultVar String StrToSearchFor OffsetDirection
    !verbose push
    !verbose 4
    !echo `$ {UnStrLoc} "${ResultVar}" "${String}" "${StrToSearchFor}" "${OffsetDirection}"`
    !verbose pop

    Push `${String}`
    Push `${StrToSearchFor}`
    Push `${OffsetDirection}`
    Call un.StrLoc
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrNSISToIO_Call ResultVar String
    !verbose push
    !verbose 4
    !echo `$ {StrNSISToIO} "${ResultVar}" "${String}"`
    !verbose pop

    Push `${String}`
    Call StrNSISToIO
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrNSISToIO_Call ResultVar String
    !verbose push
    !verbose 4
    !echo `$ {UnStrNSISToIO} "${ResultVar}" "${String}"`
    !verbose pop

    Push `${String}`
    Call un.StrNSISToIO
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrRep_Call ResultVar String StringToReplace ReplacementString
    !verbose push
    !verbose 4
    !echo `$ {StrRep} "${ResultVar}" "${String}" "${StringToReplace}" "${ReplacementString}"`
    !verbose pop

    Push `${String}`
    Push `${StringToReplace}`
    Push `${ReplacementString}`
    Call StrRep
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrRep_Call ResultVar String StringToReplace ReplacementString
    !verbose push
    !verbose 4
    !echo `$ {UnStrRep} "${ResultVar}" "${String}" "${StringToReplace}" "${ReplacementString}"`
    !verbose pop

    Push `${String}`
    Push `${StringToReplace}`
    Push `${ReplacementString}`
    Call un.StrRep
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrSort_Call ResultVar String CenterStr LeftStr RightStr IncludeLeftRightStr IncludeCenterStr
    !verbose push
    !verbose 4
    !echo `$ {StrSort} "${ResultVar}" "${String}" "${CenterStr}" "${LeftStr}" "${RightStr}" "${IncludeLeftRightStr}" "${IncludeCenterStr}"`
    !verbose pop

    Push `${String}`
    Push `${CenterStr}`
    Push `${LeftStr}`
    Push `${RightStr}`
    Push `${IncludeLeftRightStr}`
    Push `${IncludeCenterStr}`
    Call StrSort
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrSort_Call ResultVar String CenterStr LeftStr RightStr IncludeLeftRightStr IncludeCenterStr
    !verbose push
    !verbose 4
    !echo `$ {UnStrSort} "${ResultVar}" "${String}" "${CenterStr}" "${LeftStr}" "${RightStr}" "${IncludeLeftRightStr}" "${IncludeCenterStr}"`
    !verbose pop

    Push `${String}`
    Push `${CenterStr}`
    Push `${LeftStr}`
    Push `${RightStr}`
    Push `${IncludeLeftRightStr}`
    Push `${IncludeCenterStr}`
    Call un.StrSort
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrStr_Call ResultVar String StrToSearchFor
    !verbose push
    !verbose 4
    !echo `$ {StrStr} "${ResultVar}" "${String}" "${StrToSearchFor}"`
    !verbose pop

    Push `${String}`
    Push `${StrToSearchFor}`
    Call StrStr
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrStr_Call ResultVar String StrToSearchFor
    !verbose push
    !verbose 4
    !echo `$ {UnStrStr} "${ResultVar}" "${String}" "${StrToSearchFor}"`
    !verbose pop

    Push `${String}`
    Push `${StrToSearchFor}`
    Call un.StrStr
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrStrAdv_Call ResultVar String StrToSearchFor SearchDirection ResultStrDirection DisplayStrToSearch Loops CaseSensitive
    !verbose push
    !verbose 4
    !echo `$ {StrStrAdv} "${ResultVar}" "${String}" "${StrToSearchFor}" "${SearchDirection}" "${ResultStrDirection}" "${DisplayStrToSearch}" "${Loops}" "${CaseSensitive}"`
    !verbose pop

    Push `${String}`
    Push `${StrToSearchFor}`
    Push `${SearchDirection}`
    Push `${ResultStrDirection}`
    Push `${DisplayStrToSearch}`
    Push `${Loops}`
    Push `${CaseSensitive}`
    Call StrStrAdv
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrStrAdv_Call ResultVar String StrToSearchFor SearchDirection ResultStrDirection DisplayStrToSearch Loops CaseSensitive
    !verbose push
    !verbose 4
    !echo `$ {UnStrStrAdv} "${ResultVar}" "${String}" "${StrToSearchFor}" "${SearchDirection}" "${ResultStrDirection}" "${DisplayStrToSearch}" "${Loops}" "${CaseSensitive}"`
    !verbose pop

    Push `${String}`
    Push `${StrToSearchFor}`
    Push `${SearchDirection}`
    Push `${ResultStrDirection}`
    Push `${DisplayStrToSearch}`
    Push `${Loops}`
    Push `${CaseSensitive}`
    Call un.StrStrAdv
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrTok_Call ResultVar String Separators ResultPart SkipEmptyParts
    !verbose push
    !verbose 4
    !echo `$ {StrTok} "${ResultVar}" "${String}" "${Separators}" "${ResultPart}" "${SkipEmptyParts}"`
    !verbose pop

    Push `${String}`
    Push `${Separators}`
    Push `${ResultPart}`
    Push `${SkipEmptyParts}`
    Call StrTok
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrTok_Call ResultVar String Separators ResultPart SkipEmptyParts
    !verbose push
    !verbose 4
    !echo `$ {UnStrTok} "${ResultVar}" "${String}" "${Separators}" "${ResultPart}" "${SkipEmptyParts}"`
    !verbose pop

    Push `${String}`
    Push `${Separators}`
    Push `${ResultPart}`
    Push `${SkipEmptyParts}`
    Call un.StrTok
    Pop `${ResultVar}`
  !macroend

  !macro FUNCTION_STRING_StrTrimNewLines_Call ResultVar String
    !verbose push
    !verbose 4
    !echo `$ {StrTrimNewLines} "${ResultVar}" "${String}"`
    !verbose pop

    Push `${String}`
    Call StrTrimNewLines
    Pop `${ResultVar}`
  !macroend
  !macro FUNCTION_STRING_UnStrTrimNewLines_Call ResultVar String
    !verbose push
    !verbose 4
    !echo `$ {UnStrTrimNewLines} "${ResultVar}" "${String}"`
    !verbose pop

    Push `${String}`
    Call un.StrTrimNewLines
    Pop `${ResultVar}`
  !macroend

  !ifndef MUI_VERBOSE
    !define MUI_VERBOSE 4
  !endif
!endif
