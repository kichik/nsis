/*

Functions Header File for NSIS

StrFunc.nsh
This file contains functions for string manipulation for NSIS

by Diego Pedroso (aka deguix)

*/

!ifndef MUI_VERBOSE
  !define MUI_VERBOSE 3
!endif

!echo "$\r$\n----------------------------------------------------------------------$\r$\nNSIS String Functions Header File 1.02 - © 2004 Diego Pedroso$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

!define StrClbGet "!insertmacro FUNCTION_STRING_StrClbGet"
!define StrClbSet "!insertmacro FUNCTION_STRING_StrClbSet"
!define StrIOToNSIS "!insertmacro FUNCTION_STRING_StrIOToNSIS"
!define StrLoc "!insertmacro FUNCTION_STRING_StrLoc"
!define StrLowerCase "!insertmacro FUNCTION_STRING_StrLowerCase"
!define StrNSISToIO "!insertmacro FUNCTION_STRING_StrNSISToIO"
!define StrRep "!insertmacro FUNCTION_STRING_StrRep"
!define StrSort "!insertmacro FUNCTION_STRING_StrSort"
!define StrStr "!insertmacro FUNCTION_STRING_StrStr"
!define StrStrAdv "!insertmacro FUNCTION_STRING_StrStrAdv"
!define StrTok "!insertmacro FUNCTION_STRING_StrTok"
!define StrTrimNewLines "!insertmacro FUNCTION_STRING_StrTrimNewLines"
!define StrUpperCase "!insertmacro FUNCTION_STRING_StrUpperCase"

!macro FUNCTION_STRING_StrStr

  !ifndef FUNCTION_STRING_StrStr

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nSearch in String Function - 2002-2004 Ximon Eighteen\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrStr
    !undef StrStr
    !define StrStr "!insertmacro FUNCTION_STRING_StrStr_Call"

    Function StrStr
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
        StrCmp $R5 "" done
        IntOp $R4 $R4 + 1
        Goto loop
      done:
      StrCpy $R1 $R2 "" $R4
      Pop $R5
      Pop $R4
      Pop $R3
      Pop $R2
      Exch $R1
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrStr_Call ResultVar String StrToSearchFor

  !echo `$ {StrStr} "${ResultVar}" "${String}" "${StrToSearchFor}"$\r$\n`

  Push `${String}`
  Push `${StrToSearchFor}`

  Call StrStr

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrLoc

  !ifndef FUNCTION_STRING_StrLoc

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nLocalize in String Function - © 2004 Diego Pedroso\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrLoc
    !undef StrLoc
    !define StrLoc "!insertmacro FUNCTION_STRING_StrLoc_Call"

    Function StrLoc
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
        StrCmp $R5 "" error
        IntOp $R4 $R4 + 1
        Goto loop
      done:

      StrCmp $R0 "<" 0 +5
        StrLen $R0 $R2
        IntOp $R5 $R3 + $R4
        IntOp $R0 $R0 - $R5
        Goto +2

      StrCpy $R0 $R4
      Goto +2

      error:
      StrCpy $R0 ""

      Pop $R5
      Pop $R4
      Pop $R3
      Pop $R2
      Pop $R1
      Exch $R0
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrLoc_Call ResultVar String StrToSearchFor OffsetDirection

  !echo `$ {StrLoc} "${ResultVar}" "${String}" "${StrToSearchFor}" "${OffsetDirection}"$\r$\n`

  Push `${String}`
  Push `${StrToSearchFor}`
  Push `${OffsetDirection}`

  Call StrLoc

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrStrAdv

  !ifndef FUNCTION_STRING_StrStrAdv

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nAdvanced Search in String Function - © 2003-2004 Diego Pedroso$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrStrAdv
    !undef StrStrAdv

    !define StrStrAdv "!insertmacro FUNCTION_STRING_StrStrAdv_Call"

    Function AdvancedStrStr

     # Preparing Variables

     Exch $R9
     Exch
     Exch $R8
     Exch
     Exch 2
     Exch $R7
     Exch 2
     Exch 3
     Exch $R6
     Exch 3
     Exch 4
     Exch $R5
     Exch 4
     Exch 5
     Exch $R4
     Exch 5
     Push $R3
     Push $R2
     Push $R1
     Push $R0
     Push $9
     Push $8
     Push $7
     Push $6
     StrCpy $R2 $R4
     StrCpy $R1 $R5
     StrCpy $R4 ""
     StrCpy $R5 ""
     StrCpy $7 $R2

     # Detect Empty Input

     StrCmp $R1 "" 0 +3
       SetErrors
       Goto granddone

     StrCmp $R2 "" 0 +3
       SetErrors
       Goto granddone

     StrCmp $R6 "" 0 +2
       StrCpy $R6 >

     StrCmp $R7 "" 0 +2
       StrCpy $R7 >

     # Preparing StrStr

     StrCpy $R0 0

     IntCmp $R9 1 +2 0 +2
       StrCpy $R9 0

     IntOp $R9 $R9 + 1

     # Loops and more loops if you want...

       grandloop:

       # Detect if the loops number given by user = code runs...

       StrCpy $R4 0
       StrLen $R3 $R1
       StrCpy $6 $R3
       StrCmp $9 1 0 +4
         StrCmp $R6 "<" 0 +2
           IntOp $R3 $R3 + 1
           IntOp $R4 $R4 + 1

       StrCmp $R6 "<" 0 +5
         IntOp $R3 $R3 * -1
         StrCpy $6 $R3
         IntCmp $R0 0 +2 0 0
           IntOp $6 $6 + 1

       # Searching the string

         loop:

         # RTL...

         StrCmp $R6 "<" 0 EndBack

           IntOp $9 $R4 * -1

           StrCmp $9 0 0 +3
             StrCpy $R5 $R2 "" $R3
             Goto +2
           StrCpy $R5 $R2 $9 $R3
           Goto +2

         EndBack:

         # LTR...

         StrCpy $R5 $R2 $R3 $R4

         # Detect if the value returned is the searched...

         StrCmp $R5 $R1 done

         StrCmp $R5 "" granddone

             # If not, make a loop...

             IntOp $R4 $R4 + 1
             StrCmp $R6 "<" 0 +2
               IntOp $R3 $R3 - 1

         Goto loop

       done:

       StrCmp $R6 "<" 0 +3
         IntOp $8 $9 + $8
           Goto +2
       IntOp $8 $R4 + $8

       # Looping Calculation...

        IntOp $R0 $R0 + 1

       IntCmp $R0 $R9 0 continueloop 0

       # Customizing the string to fit user conditions (supported by loops)...

       # RTL...

         StrCmp $R6 "<" 0 EndBackward
           StrCmp $R7 ">" 0 +7
             StrCmp $8 0 0 +3
               StrCpy $R2 ""
               Goto +2
             StrCpy $R2 $7 "" $8
             StrCpy $R2 $R1$R2
             Goto +3

           StrCmp $9 0 +2
             StrCpy $R2 $R2 $9

           StrCmp $R8 1 EndForward 0
             StrCmp $R7 ">" 0 End>
               Push $6
               IntOp $6 $6 * -1
               StrCpy $R2 $R2 "" $6
               Pop $6
                 Goto +2
             End>:
             StrCpy $R2 $R2 $6
               Goto EndForward
         EndBackward:

         # LTR...

         StrCmp $R7 "<" 0 +4
           StrCpy $R2 $7 $8
           StrCpy $R2 $R2$R1
           Goto +2
         StrCpy $R2 $R2 "" $R4
         StrCmp $R8 1 EndForward 0
           StrCmp $R7 "<" 0 End<
             Push $6
             IntOp $6 $6 * 2
             StrCpy $R2 $R2 $6
             Pop $6
               Goto +2
           End<:
           StrCpy $R2 $R2 "" $R3
         EndForward:

         Goto stoploop

       continueloop:

       # Customizing the string to fits user conditions (not supported by loops)...

       # RTL...

       StrCmp $R6 "<" 0 +4
         StrCmp $9 0 +4
         StrCpy $R2 $R2 $9
           Goto +2

       # LTR...

       StrCpy $R2 $R2 "" $R4

       stoploop:

       # Return to grandloop init...

       StrCpy $9 1

       IntCmp $R0 $R9 0 grandloop 0

     StrCpy $R4 $R2

     Goto +2

     granddone:

     # Return the result to user

     StrCpy $R4 ""

     Pop $6
     Pop $7
     Pop $8
     Pop $9
     Pop $R0
     Pop $R1
     Pop $R2
     Pop $R3
     Pop $R9
     Pop $R8
     Pop $R7
     Pop $R6
     Pop $R5
     Exch $R4

    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrStrAdv_Call ResultVar String StrToSearchFor SearchDirection ResultStrDirection DisplayStrToSearch Loops

  !echo `$ {StrStrAdv} "${ResultVar}" "${String}" "${StrToSearchFor}" "${SearchDirection}" "${ResultStrDirection}" "${DisplayStrToSearch}" "${Loops}"$\r$\n`

  Push `${String}`
  Push `${StrToSearchFor}`
  Push `${SearchDirection}`
  Push `${ResultStrDirection}`
  Push `${DisplayStrToSearch}`
  Push `${Loops}`

  Call AdvancedStrStr

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrTok

  !ifndef FUNCTION_STRING_StrTok

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nAdvanced Token String Function - © 2004 Diego Pedroso$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrTok
    !undef StrTok
    !define StrTok "!insertmacro FUNCTION_STRING_StrTok_Call"

    Function AdvancedStrTok
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
        StrCpy $1 ""
        StrCpy $9 1
      
      PartLoop:

      StrCpy $R4 0
      IntOp $R8 $R8 + 1
      StrCpy $0 0
      
      loop:

        StrCpy $R5 $R2 1 $R4
        StrCmp $R5 "" done

        StrCpy $R6 -1
        StrCpy $R7 0
        loop2:

          IntOp $R6 $R6 + 1
          IntOp $R7 $R6 + 1
          StrCpy $R3 $R1 $R7 $R6
          StrCmp $R3 "" 0 +3
            IntOp $0 $0 + 1
            Goto ContLoop2
          StrCmp $R5 $R3 0 Loop2
            StrCmp $9 1 0 done
              StrCmp $0 0 0 done
                StrCpy $R2 $R2 "" 1
                Goto Loop
            
        ContLoop2:

        IntOp $R4 $R4 + 1
        Goto loop

      done:
      IntOp $R4 $R4 + $0
      StrCpy $R1 $R2 $0
      IntOp $0 $0 + 1
      StrCpy $R2 $R2 "" $0
      
      StrCmp $2 1 0 +4
        StrCmp $R1 "" 0 +3
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
      Pop $R1
      Pop $R0
      Exch $9
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrTok_Call ResultVar StrToTokenize Separators ResultPart SkipEmptyParts

  !echo `$ {StrTok} "${ResultVar}" "${StrToTokenize}" "${Separators}" "${ResultPart}" "${SkipEmptyParts}"$\r$\n`

  Push `${StrToTokenize}`
  Push `${Separators}`
  Push `${ResultPart}`
  Push `${SkipEmptyParts}`

  Call AdvancedStrTok

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrClbSet

  !ifndef FUNCTION_STRING_StrClbSet

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nCopy To Clipboard - 2003-2004 Nik Medved$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrClbSet
    !undef StrClbSet
    
    !define StrClbSet "!insertmacro FUNCTION_STRING_StrClb_Set"

    Function CopyToClipboard
      Exch $0 ;input string
      Push $1
      Push $2
      System::Call 'user32::OpenClipboard(i 0)'
      System::Call 'user32::EmptyClipboard()'
      StrLen $1 $0
      IntOp $1 $1 + 1
      System::Call 'kernel32::GlobalAlloc(i 2, i r1) i.r1'
      System::Call 'kernel32::GlobalLock(i r1) i.r2'
      System::Call 'kernel32::lstrcpyA(i r2, t r0)'
      System::Call 'kernel32::GlobalUnlock(i r1)'
      System::Call 'user32::SetClipboardData(i 1, i r1)'
      System::Call 'user32::CloseClipboard()'
      Pop $2
      Pop $1
      Pop $0
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrClbGet 

  !ifndef FUNCTION_STRING_StrClbGet

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nCopy From Clipboard Function - 2003-2004 Nik Medved - changed by Diego Pedroso$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrClbGet
    !undef StrClbGet

    !define StrClbGet "!insertmacro FUNCTION_STRING_StrClb_Get"

    Function CopyFromClipboard
      Push $0
      System::Call 'user32::OpenClipboard(i 0)'
      System::Call 'user32::GetClipboardData(i 1) t .r0'
      System::Call 'user32::CloseClipboard()'
      Exch $0
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrClb_Set String

  !echo `$ {StrClbSet} "${String}"$\r$\n`

  Push `${String}`

  Call CopyToClipboard
  
!macroend

!macro FUNCTION_STRING_StrClb_Get ResultVar

  !echo `$ {StrClbGet} "${ResultVar}"$\r$\n`

  Call CopyFromClipboard
  
  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrUpperCase

  !ifndef FUNCTION_STRING_StrUpperCase

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nUppercase String Function - 2002-2004 Dave Laundon $\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrUpperCase
    !undef StrUpperCase
    !define StrUpperCase "!insertmacro FUNCTION_STRING_StrUpperCase_Call"

    Function StrUpper
      Exch $0 ; Original string
      Push $1 ; Final string
      Push $2 ; Current character
      Push $3
      Push $4
      StrCpy $1 ""
    Loop:
      StrCpy $2 $0 1 ; Get next character
      StrCmp $2 "" Done
      StrCpy $0 $0 "" 1
      StrCpy $3 65 ; 65 = ASCII code for A
    Loop2:
      IntFmt $4 %c $3 ; Get character from current ASCII code
      StrCmp $2 $4 Match
      IntOp $3 $3 + 1
      StrCmp $3 91 NoMatch Loop2 ; 91 = ASCII code one beyond Z
    Match:
      StrCpy $2 $4 ; It 'matches' (either case) so grab the uppercase version
    NoMatch:
      StrCpy $1 $1$2 ; Append to the final string
      Goto Loop
    Done:
      StrCpy $0 $1 ; Return the final string
      Pop $4
      Pop $3
      Pop $2
      Pop $1
      Exch $0
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrUpperCase_Call ResultVar String

  !echo `$ {StrUpperCase} "${ResultVar}" "${String}"$\r$\n`

  Push `${String}`

  Call StrUpper

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrLowerCase

  !ifndef FUNCTION_STRING_StrLowerCase

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nLowercase String Function - changed from Uppercase String Function 2002-2004 Dave Laundon$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrLowerCase
    !undef StrLowerCase
    !define StrLowerCase "!insertmacro FUNCTION_STRING_StrLowerCase_Call"

    Function StrLower
      Exch $0 ; Original string
      Push $1 ; Final string
      Push $2 ; Current character
      Push $3
      Push $4
      StrCpy $1 ""
    Loop:
      StrCpy $2 $0 1 ; Get next character
      StrCmp $2 "" Done
      StrCpy $0 $0 "" 1
      StrCpy $3 122 ; 122 = ASCII code for z
    Loop2:
      IntFmt $4 %c $3 ; Get character from current ASCII code
      StrCmp $2 $4 Match
      IntOp $3 $3 - 1
      StrCmp $3 91 NoMatch Loop2 ; 90 = ASCII code one beyond Z
    Match:
      StrCpy $2 $4 ; It 'matches' (either case) so grab the lowercase version
    NoMatch:
      StrCpy $1 $1$2 ; Append to the final string
      Goto Loop
    Done:
      StrCpy $0 $1 ; Return the final string
      Pop $4
      Pop $3
      Pop $2
      Pop $1
      Exch $0
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrLowerCase_Call ResultVar String

  !echo `$ {StrLowerCase} "${ResultVar}" "${String}"$\r$\n`

  Push `${String}`

  Call StrLower

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrRep

  !ifndef FUNCTION_STRING_StrRep

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nReplace String Function - 2002-2004 Hendri Adriaens$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrRep
    !undef StrRep
    !define StrRep "!insertmacro FUNCTION_STRING_StrRep_Call"

    Function StrReplace
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
      StrCpy $3 "-1"
      StrCpy $5 ""
      StrLen $6 $1
      StrLen $7 $0
      Loop:
      IntOp $3 $3 + 1
      StrCpy $4 $2 $6 $3
      StrCmp $4 "" ExitLoop
      StrCmp $4 $1 Replace
      Goto Loop
      Replace:
      StrCpy $R0 $2 $3
      IntOp $R2 $3 + $6
      StrCpy $R1 $2 "" $R2
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

  !endif

!macroend

!macro FUNCTION_STRING_StrRep_Call ResultVar String StringToReplace ReplacementString

  !echo `$ {StrRep} "${ResultVar}" "${String}" "${StringToReplace}" "${ReplacementString}"$\r$\n`

  Push `${String}`
  Push `${StringToReplace}`
  Push `${ReplacementString}`

  Call StrReplace

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrSort

  !ifndef FUNCTION_STRING_StrSort

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nAdvanced String Sort Function - © 2004 Diego Pedroso$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrSort
    !undef StrSort
    !define StrSort "!insertmacro FUNCTION_STRING_StrSort_Call"

    Function AdvStrSort

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
        StrCmp $R3 "" error
          StrCmp $R3 $2 done
            IntOp $R1 $R1 + 1
            Goto loop
      done:

      StrCpy $R5 $R1

      IntOp $R1 $R1 - $4

      # Left String Search

      loop2:
        StrCpy $R3 $R0 $4 $R1
        StrCmp $R3 "" error2
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
        StrCpy $R4 $R4 "" $R1

      StrCmp $R3 1 0 +3
        StrCmp $R6 0 0 +2
          StrCpy $R4 $R4 "" $4

      # Center String Addition

      StrCmp $R7 0 +2
        StrCpy $R4 $R4$2

      StrCpy $R1 $R5
      IntOp $R1 $R1 + $5

      # Right String Search

      loop3:

        StrCpy $R3 $R0 $3 $R1
        StrCmp $R3 "" error3
          StrCmp $R3 $0 done3
            IntOp $R1 $R1 + 1
            Goto loop3

        error3:
        StrCpy $R1 0

      done3:

      IntOp $R5 $R5 + $5
      StrCpy $R3 $R0 "" $R5

      StrCmp $R1 0 +5
        IntOp $R1 $R1 - $R5
        StrCmp $R6 0 +2
          IntOp $R1 $R1 + $3
        StrCpy $R3 $R3 $R1

      StrCpy $R4 $R4$R3

      StrCpy $2 $R4
      Goto +2

        Error:
        StrCpy $2 ""

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

  !endif

!macroend

!macro FUNCTION_STRING_StrSort_Call ResultVar String CenterStr LeftStr RightStr IncludeLeftRightStr IncludeCenterStr

  !echo `$ {StrSort} "${ResultVar}" "${String}" "${CenterStr}" "${LeftStr}" "${RightStr}" "${IncludeLeftRightStr}" "${IncludeCenterStr}"$\r$\n`

  Push `${String}`
  Push `${CenterStr}`
  Push `${LeftStr}`
  Push `${RightStr}`
  Push `${IncludeLeftRightStr}`
  Push `${IncludeCenterStr}`

  Call AdvStrSort

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrTrimNewLines

  !ifndef FUNCTION_STRING_StrTrimNewLines

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nTrim New Lines Function - 2003-2004 Ximon Eighteen$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrTrimNewLines
    !undef StrTrimNewLines
    !define StrTrimNewLines "!insertmacro FUNCTION_STRING_StrTrimNewLines_Call"

    Function TrimNewlines
      Exch $R0
      Push $R1
      Push $R2
      StrCpy $R1 0

    loop:
      IntOp $R1 $R1 - 1
      StrCpy $R2 $R0 1 $R1
      StrCmp $R2 "$\r" loop
      StrCmp $R2 "$\n" loop

      IntOp $R1 $R1 + 1
      IntCmp $R1 0 no_trim_needed
      StrCpy $R0 $R0 $R1

    no_trim_needed:
      Pop $R2
      Pop $R1
      Exch $R0
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrTrimNewLines_Call ResultVar String

  !echo `$ {StrTrimNewLines} "${ResultVar}" "${String}"$\r$\n`

  Push `${String}`

  Call TrimNewLines

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrNSISToIO

  !ifndef FUNCTION_STRING_StrNSISToIO

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nNSIS -> Install Options String Convertion Function - 2003-2004 Amir Szekely, Joost Verburg and Dave Laundon$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrNSISToIO
    !undef StrNSISToIO
    !define StrNSISToIO "!insertmacro FUNCTION_STRING_StrNSISToIO_Call"

    Function Nsis2Io
      Exch $0 ; The source
      Push $1 ; The output
      Push $2 ; Temporary char
      StrCpy $1 "" ; Initialise the output
    loop:
      StrCpy $2 $0 1 ; Get the next source char
      StrCmp $2 "" done ; Abort when none left
        StrCpy $0 $0 "" 1 ; Remove it from the source
        StrCmp $2 "\" "" +3 ; Back-slash?
          StrCpy $1 "$1\\"
          Goto loop
        StrCmp $2 "$\r" "" +3 ; Carriage return?
          StrCpy $1 "$1\r"
          Goto loop
        StrCmp $2 "$\n" "" +3 ; Line feed?
          StrCpy $1 "$1\n"
          Goto loop
        StrCmp $2 "$\t" "" +3 ; Tab?
          StrCpy $1 "$1\t"
          Goto loop
        StrCpy $1 "$1$2" ; Anything else
        Goto loop
    done:
      StrCpy $0 $1
      Pop $2
      Pop $1
      Exch $0
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrNSISToIO_Call ResultVar String

  !echo `$ {StrNSISToIO} "${ResultVar}" "${String}"$\r$\n`

  Push `${String}`

  Call NSIS2IO

  Pop `${ResultVar}`

!macroend

!macro FUNCTION_STRING_StrIOToNSIS

  !ifndef FUNCTION_STRING_StrIOToNSIS

    !echo "$\r$\n----------------------------------------------------------------------$\r$\nInstall Options -> NSIS String Convertion Function - 2003-2004 Amir Szekely, Joost Verburg and Dave Laundon$\r$\n----------------------------------------------------------------------$\r$\n$\r$\n"

    !define FUNCTION_STRING_StrIOToNSIS
    !undef StrIOToNSIS
    !define StrIOToNSIS "!insertmacro FUNCTION_STRING_StrIOToNSIS_Call"

    Function Io2Nsis
      Exch $0 ; The source
      Push $1 ; The output
      Push $2 ; Temporary char
      StrCpy $1 "" ; Initialise the output
    loop:
      StrCpy $2 $0 1 ; Get the next source char
      StrCmp $2 "" done ; Abort when none left
        StrCpy $0 $0 "" 1 ; Remove it from the source
        StrCmp $2 "\" +3 ; Escape character?
          StrCpy $1 "$1$2" ; If not just output
          Goto loop
        StrCpy $2 $0 1 ; Get the next source char
        StrCpy $0 $0 "" 1 ; Remove it from the source
        StrCmp $2 "\" "" +3 ; Back-slash?
          StrCpy $1 "$1\"
          Goto loop
        StrCmp $2 "r" "" +3 ; Carriage return?
          StrCpy $1 "$1$\r"
          Goto loop
        StrCmp $2 "n" "" +3 ; Line feed?
          StrCpy $1 "$1$\n"
          Goto loop
        StrCmp $2 "t" "" +3 ; Tab?
          StrCpy $1 "$1$\t"
          Goto loop
        StrCpy $1 "$1$2" ; Anything else (should never get here)
        Goto loop
    done:
      StrCpy $0 $1
      Pop $2
      Pop $1
      Exch $0
    FunctionEnd

  !endif

!macroend

!macro FUNCTION_STRING_StrIOToNSIS_Call ResultVar String

  !echo `$ {StrIOToNSIS} "${ResultVar}" "${String}"$\r$\n`

  Push `${String}`

  Call IO2NSIS

  Pop `${ResultVar}`

!macroend

!ifndef MUI_VERBOSE
  !define MUI_VERBOSE 4
!endif
