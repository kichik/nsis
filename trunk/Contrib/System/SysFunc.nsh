; Some useful functions based on System plugin
; 
; (c) brainsucker, 2002
; (r) BSForce

!include "${NSISDIR}\Contrib\System\System.nsh"
!include "${NSISDIR}\Examples\WinMessages.nsh"

!define ssName $9
!define ssDelay $8
!define ssInstance $7
!define ssImage $6
!define ssWnd $5
!define ssResult $4
!define ssCallback $3

Function _systemSplashWndCB
   ; Callback receives 4 values (
   Pop $R2 ; hwnd
   Pop $R5 ; umsg
   Pop $R0 ; wparam
   Pop $R1 ; lparam

   ; Save globals and use them
   Push $R6
   Push $R7
   Push $R8
   Push $R9
   Push $2
   Push $5
   Push $7
   Push $9
   StrCpy $2 $R2
   StrCpy $5 $R5
   StrCpy $7 $R0
   StrCpy $9 $R1

;   MessageBox MB_OK "Got: $2 $5 $7 $9"

   ; Message branching
   IntCmp $5 ${WM_CLOSE} m_Close
   IntCmp $5 ${WM_TIMER} m_Timer
   IntCmp $5 ${WM_LBUTTONDOWN} m_Lbtn
   IntCmp $5 ${WM_CREATE} m_Create
   IntCmp $5 ${WM_PAINT} m_Paint
   goto default

m_Create:

   ; Create structures
   System::Call "*${sysRECT} (_) .R8"
   System::Call "*${sysBITMAP} (_) .R9"

   ; Get bitmap info
   System::Call "${sysGetObject} (r6, ${sysBITMAP_SIZE}, R9)" 
   
   ; Get desktop info
   System::Call "${sysSystemParametersInfo} (${SPI_GETWORKAREA}, 0, R8, 0)" 

   ; Style (callbacked)
   System::Call "${sysSetWindowLong} (r2, ${GWL_STYLE}, 0) .s" 
   !insertmacro SINGLE_CALLBACK 5 $R7 1 _systemSplashWndCB

   ; Calculate and set window pos

   ; Get bmWidth(R2) and bmHeight(R3)
   System::Call "*$R9${sysBITMAP} (,.R2,.R3)"
   ; Get left(R4), top(R5), right(R6), bottom(R7)
   System::Call "*$R8${sysRECT} (.R4,.R5,.R6,.R7)"

   ; Left pos
   IntOp $R0 $R6 - $R4
   IntOp $R0 $R0 - $R2
   IntOp $R0 $R0 / 2
   IntOp $R0 $R0 + $R4

   ; Top pos
   IntOp $R1 $R7 - $R5
   IntOp $R1 $R1 - $R3
   IntOp $R1 $R1 / 2
   IntOp $R1 $R1 + $R5

   System::Call "${sysSetWindowPos} (r2, 0, R0, R1, R2, R3, ${SWP_NOZORDER}) .s" 
   !insertmacro SINGLE_CALLBACK 6 $R7 1 _systemSplashWndCB

   ; Show window
   System::Call "${sysShowWindow} (r2, ${SW_SHOW}) .s" 
   !insertmacro SINGLE_CALLBACK 7 $R7 1 _systemSplashWndCB

   ; Set Timer
   System::Call "${sysSetTimer} (r2, 1, r8,)"

   ; Free used memory
   System::Free $R8
   System::Free $R9

   StrCpy $R0 0
   goto exit

m_Paint:

   ; Create structures
   System::Call "*${sysRECT} (_) .R8"
   System::Call "*${sysPAINTSTRUCT} (_) .R9"

   ; Begin Paint
   System::Call "${sysBeginPaint} (r2, R9) .R7"

   ; CreateCompatibleDC
   System::Call "${sysCreateCompatibleDC} (R7) .R6"

   ; GetClientRect
   System::Call "${sysGetClientRect} (r2, R8)"
  
   ; Select new bitmap
   System::Call "${sysSelectObject} (R6, r6) .R5"

   ; Get left(R0), top(R1), right(R2), bottom(R3)
   System::Call "*$R8${sysRECT} (.R0,.R1,.R2,.R3)"
     
   ; width=right-left  
   IntOp $R2 $R2 - $R0
   ; height=bottom-top
   IntOp $R3 $R3 - $R1

   System::Call "${sysBitBlt} (R7, R0, R1, R2, R3, R6, 0, 0, ${SRCCOPY})" 

   ; Select old bitmap
   System::Call "${sysSelectObject} (R6, R5)"
   
   ; Delete compatible DC
   System::Call "${sysDeleteDC} (R6)"

   ; End Paint
   System::Call "${sysEndPaint} (r2, R9)"

   ; Free used memory
   System::Free $R8
   System::Free $R9

   StrCpy $R0 0
   goto exit

m_Timer:
m_Lbtn:
   StrCpy ${ssResult} 0
   IntCmp $5 ${WM_TIMER} destroy
        StrCpy ${ssResult} 1

destroy:
   System::Call "${sysDestroyWindow} (r2) .s"
   !insertmacro SINGLE_CALLBACK 12 $R4 1 _systemSplashWndCB

default:
   ; Default
   System::Call "${sysDefWindowProc} (r2, r5, r7, r9) .s"
   !insertmacro SINGLE_CALLBACK 14 $R0 1 _systemSplashWndCB
   goto exit

m_Close:
   StrCpy $R0 0
   goto exit

exit:
   ; Restore globals
   Pop $9
   Pop $7
   Pop $5
   Pop $2
   Pop $R9
   Pop $R8
   Pop $R7
   Pop $R6

   ; Return from callback
   System::Call "${ssCallback}" $R0
FunctionEnd

Function systemSplash
   Pop $R8
   Pop $R9

   ; Save global register
   Push $9
   Push $8
   Push $7
   Push $6
   Push $5
   Push $4
   Push $3

   StrCpy ${ssName} $R9
   StrCpy ${ssDelay} $R8

   ; Get module instance
   System::Call "${sysGetModuleHandle} (i) .r7"
        ;   Pop ${ssInstance}

   ; Get arrow cursor
   System::Call "${sysLoadCursor} (0, i ${IDC_ARROW}) .R9" 
        ;   Pop $R9

   ; Get callback
   System::Get "${sysWNDPROC}"
   Pop ${ssCallback}

   ; Create window class
   System::Call "*${sysWNDCLASS} (,r3,,,r7,,R9,,,s) .R9" "_sp"
        ;   Pop $R9

   ; Register window class
   System::Call "${sysRegisterClass} (R9) .R9"
   IntCmp $R9 0 errorexit ; Class registered ok?

   ; Load Image (LR_CREATEDIBSECTION|LR_LOADFROMFILE = 0x2010)
   System::Call '${sysLoadImage} (, s, ${IMAGE_BITMAP}, 0, 0, ${LR_CREATEDIBSECTION}|${LR_LOADFROMFILE}) .r6' "${ssName}.bmp"
        ;   Pop ${ssImage}
   IntCmp ${ssImage} 0 errorexit        ; Image loaded ok?

   ; Start the sound (SND_ASYNC|SND_FILENAME|SND_NODEFAULT = 0x20003)
   System::Call "${sysPlaySound} (s,,${SND_ASYNC}|${SND_FILENAME}|${SND_NODEFAULT})" "${ssName}.wav" 

   ; Create window
   System::Call "${sysCreateWindowEx} (${WS_EX_TOOLWINDOW}, s, s,,,,,, $HWNDPARENT,,r7,) .s" "_sp" "_sp" 
        ;    Pop ${ssWnd}    -> SINGLE_CALLBACK will do that
   !insertmacro SINGLE_CALLBACK 1 ${ssWnd} 1 _systemSplashWndCB

   ; Create MSG struct
   System::Call "*${sysMSG} (_) i.R9"
        ;    Pop $R9

   ; -------------------------
repeat:
        ; Check for window
        System::Call "${sysIsWindow} (r5) .s"
        !insertmacro SINGLE_CALLBACK 2 $R8 1 _systemSplashWndCB
        IntCmp $R8 0 finish

        ; Get message
        System::Call "${sysGetMessage} (R9, r5,_) .s"
        !insertmacro SINGLE_CALLBACK 3 $R8 1 _systemSplashWndCB
        IntCmp $R8 0 finish

        ; Dispatch message
        System::Call "${sysDispatchMessage} (R9) .s"
        !insertmacro SINGLE_CALLBACK 4 $R8 1 _systemSplashWndCB

        ; Repeat dispatch cycle
        goto repeat
   ; -------------------------

finish:
   ; Stop the sound
   System::Call "${sysPlaySound}"
   
   ; Delete bitmap object
   System::Call "${sysDeleteObject} (r6)"

   ; Delete the callback queue
   System::Free /CALLBACK ${ssCallback}

   ; Dialog return
   StrCpy $R0 ${ssResult}
   goto exit

; Exit in case of error
errorexit:
   StrCpy $R0 -1
   goto exit

exit:
   ; Restore globals
   Pop $3
   Pop $4
   Pop $5
   Pop $6
   Pop $7
   Pop $8
   Pop $9
   
   ; Return
   Push $R0
FunctionEnd