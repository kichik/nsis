; ---------------------
;       x64.nsh
; ---------------------
;
; A few simple macros to handle installations on x64 machines.
;
; IsRunningX64 checks if the installers is running on x64.
;
;   ${IsRunningX64}
;   Pop $0
;   ${If} $0 != 0
;     MessageBox MB_OK "running on x64"
;   ${EndIf}
;
; DisableX64FSRedirection disables file system redirection.
; EnableX64FSRedirection enables file system redirection.
;
;   ${DisableX64FSRedirection}
;   DetailPrint $SYSDIR # prints C:\Windows\System32
;   ${EnableX64FSRedirection}
;   DetailPrint $SYSDIR # prints C:\Windows\SysWOW64
;

!ifndef ___X64__NSH___
!define ___X64__NSH___

!macro IsRunningX64

  System::Call kernel32::GetCurrentProcess()i.s
  System::Call kernel32::IsWow64Process(is,*i.s)

!macroend

!define IsRunningX64 "!insertmacro IsRunningX64"

!macro DisableX64FSRedirection

  System::Call kernel32::Wow64EnableWow64FsRedirection(i0)

!macroend

!define DisableX64FSRedirection "!insertmacro DisableX64FSRedirection"

!macro EnableX64FSRedirection

  System::Call kernel32::Wow64EnableWow64FsRedirection(i1)

!macroend

!define EnableX64FSRedirection "!insertmacro EnableX64FSRedirection"

!endif # !___X64__NSH___
