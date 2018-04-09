; ---------------------
;       x64.nsh
; ---------------------
;
; A few simple macros to handle installations on x64 machines.
;
; RunningX64 checks if the installer is running on a 64-bit OS.
; IsWow64 checks if the installer is a 32-bit application running on a 64-bit OS.
;
;   ${If} ${RunningX64}
;     MessageBox MB_OK "Running on 64-bit Windows"
;   ${EndIf}
;
; IsNative* checks the OS native CPU architecture.
;
;   ${If} ${IsNativeAMD64}
;     ; Install AMD64 64-bit driver/library
;   ${ElseIf} ${IsNativeARM64}
;     ; Install ARM64 64-bit driver/library
;   ${ElseIf} ${IsNativeIA32}
;     ; Install i386 32-bit driver/library
;   ${Else}
;     Abort "Unsupported CPU architecture!"
;   ${EndIf}
;
; DisableX64FSRedirection disables file system redirection.
; EnableX64FSRedirection enables file system redirection.
;
;   SetOutPath $SYSDIR
;   ${DisableX64FSRedirection}
;   File something.bin # extracts to C:\Windows\System32
;   ${EnableX64FSRedirection}
;   File something.bin # extracts to C:\Windows\SysWOW64
;

!ifndef ___X64__NSH___
!define ___X64__NSH___

!include LogicLib.nsh


!define IsWow64 `"" IsWow64 ""`
!macro _IsWow64 _a _b _t _f
  !insertmacro _LOGICLIB_TEMP
  System::Call kernel32::GetCurrentProcess()p.s
  System::Call kernel32::IsWow64Process(ps,*i0s)
  Pop $_LOGICLIB_TEMP
  !insertmacro _!= $_LOGICLIB_TEMP 0 `${_t}` `${_f}`
!macroend


!define RunningX64 `"" RunningX64 ""`
!macro _RunningX64 _a _b _t _f 
  !if ${NSIS_PTR_SIZE} > 4
    !insertmacro LogicLib_JumpToBranch `${_t}` `${_f}`
  !else
    !insertmacro _IsWow64 `${_a}` `${_b}` `${_t}` `${_f}`
  !endif
!macroend


!define GetNativeProcessorArchitecture "!insertmacro GetNativeProcessorArchitecture "
!macro GetNativeProcessorArchitecture outvar
  !if ${outvar} != $1
  Push $1
  !endif
  !if "${NSIS_PTR_SIZE}" <= 4
  System::Call 'KERNEL32::GetSystemInfo(@r1)' ; < XP
  !endif
  System::Call 'KERNEL32::GetNativeSystemInfo(@r1)'
  System::Call '*$1(&i2.s)' ; Extract wProcessorArchitecture (PROCESSOR_ARCHITECTURE_*)
  Pop ${outvar}
  !if ${outvar} != $1
  Pop $1
  !endif
!macroend

!define IsNativeProcessorArchitecture `"" IsNativeProcessorArchitecture `
!macro _IsNativeProcessorArchitecture _ignore _arc _t _f
  !insertmacro _LOGICLIB_TEMP
  ${GetNativeProcessorArchitecture} $_LOGICLIB_TEMP
  !insertmacro _= $_LOGICLIB_TEMP ${_arc} `${_t}` `${_f}`
!macroend
!define IsNativeIA32 '${IsNativeProcessorArchitecture} 0' ; Intel x86
!define IsNativeAMD64 '${IsNativeProcessorArchitecture} 9' ; x86-64/x64
!define IsNativeARM64 '${IsNativeProcessorArchitecture} 12'


!define DisableX64FSRedirection "!insertmacro DisableX64FSRedirection"
!macro DisableX64FSRedirection
  System::Call kernel32::Wow64EnableWow64FsRedirection(i0)
!macroend

!define EnableX64FSRedirection "!insertmacro EnableX64FSRedirection"
!macro EnableX64FSRedirection
  System::Call kernel32::Wow64EnableWow64FsRedirection(i1)
!macroend


!endif # !___X64__NSH___
