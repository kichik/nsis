; This is just an example of System Plugin
; 
; (c) brainsucker, 2002
; (r) BSForce

Name "System Plugin Example"
OutFile "System.exe"
SetPluginUnload  alwaysoff

!include "${NSISDIR}\Contrib\System\Additionals\sysfunc.nsh"

Section "ThisNameIsIgnoredSoWhyBother?"
     SetOutPath $TEMP

     ; ----- Sample 1 ----- Message box with custom icon -----

     !insertmacro smMessageBox "i 0" "Message box with custom icon!" "System Example 1a" ${MB_OK} "i 103"
     ; i 0 - installer exe as module
     ; i 103 - icon ID

     ; The same example but using icon from resource.dll.
     ; You could use this dll for storing your resources, just replace FAR icon
     ; with something you really need.   
     File "${NSISDIR}\Contrib\System\Additionals\Resource.dll"
     !insertmacro smMessageBox "`$TEMP\resource.dll`" "Message box with custom icon from resource.dll!" "System Example 1b" ${MB_OK} "i 103"
     Delete $TEMP\resource.dll

     ; ----- Sample 2 ----- Fixed disks size/space -----

     StrCpy $7 '               Disk,                Size,                Free,                Free for user:$\n$\n'

     ; Memory for paths   
     System::Alloc 1024
     Pop $1
     ; Get drives   
     System::Call '${sysGetLogicalDriveStrings}(1024, r1)'
enumok:
     ; One more drive?   
     System::Call '${syslstrlen}(i r1) .r2'
     IntCmp $2 0 enumex

     ; Is it DRIVE_FIXED?
     System::Call '${sysGetDriveType} (i r1) .r3'
     IntCmp $3 ${DRIVE_FIXED} 0 enumnext

     ; Drive space   
     System::Call '${sysGetDiskFreeSpaceEx}(i r1, .r3, .r4, .r5)'

     ; Pretty KBs will be saved on stack
     System::Int64Op $3 / 1048576
     System::Int64Op $5 / 1048576
     System::Int64Op $4 / 1048576

     ; Get pretty drive path string   
     System::Call '*$1(&t1024 .r6)'
     System::Call '${syswsprintf} (.r7, "%s%20s    %20s mb    %20s mb    %20s mb$\n", tr7, tr6, ts, ts, ts)'

enumnext:
     ; Next drive path       
     IntOp $1 $1 + $2
     IntOp $1 $1 + 1
     goto enumok   
enumex: ; End of drives or user cancel
     ; Free memory for paths   
     System::Free $1   

     ; Message box      
     System::Call '${sysMessageBox}($HWNDPARENT, s, "System Example 2", 33)' "$7"

     ; ----- Sample 3 ----- Direct proc defenition -----

     ; Direct specification demo
     System::Call 'user32::MessageBoxA(i $HWNDPARENT, t "Just direct MessageBoxA specification demo ;)", t "System Example 3", i 33) i.s'
     Pop $0

     ; ----- Sample 4 ----- Int64, mixed definition demo -----

     ; Long int demo
     StrCpy $2 "12312312"
     StrCpy $3 "12345678903"
     System::Int64Op $2 "*" $3
     Pop $4

     ; Cdecl demo (uses 3 defenitions (simple example))
     System::Call "${syswsprintf}(.R1, s,,, t, ir0) .R0 (,,tr2,tr3,$4_)" "Int64 ops and strange defenition demo, %s x %s == %s, and previous msgbox result = %d"
     MessageBox MB_OK "Cool: '$R1'"

     ; ----- Sample 5 ----- Small structure example -----

     ; Create & Fill structure
     System::Call "*(i 123123123, &t10 'Hello', &i1 0x123dd, &i2 0xffeeddccaa) i.s"
     Pop $1
     ; Read data from structure   
     System::Call "*$1(i .r2, &t10 .r3, &i1 .r4, &i2 .r5, &l0 .r6)"
     ; Show data and delete structure
     MessageBox MB_OK "Structure example: $\nint == $2 $\nstring == $3 $\nbyte == $4 $\nshort == $5 $\nsize == $6"
     System::Free $1

     ; ----- Sample 6 ----- systemGetFileSysTime demo -----

     !insertmacro smGetFileSysTime $CMDLINE
     System::Call '*$R0${stSYSTEMTIME}(.r1, .r2, .r3, .r4, .r5, .r6, .r7, .r8)'
 
     MessageBox MB_OK "GetFileSysTime example: file '$CMDLINE', year $1, month $2, dow $3, day $4, hour $5, min $6, sec $7, ms $8"     

     ; free memory from SYSTEMTIME
     System::Free $R0   

     ; ----- Sample 7 ----- systemSplash -> Callbacks demonstration -----

     ; Logo
     File /oname=spltmp.bmp "${NSISDIR}\Contrib\Makensisw\logo.bmp"

     ; I. systemSplash variant
     !insertmacro smSystemSplash 2000 "$TEMP\spltmp"

     ; II. Splash Plugin variant
;    splash::show 2000 $TEMP\spltmp
;    Pop $R0 ; $R0 has '1' if the user closed the splash screen early,

     ; remove logo
     Delete $TEMP\spltmp.bmp

     ; Display splash result
     MessageBox MB_OK "Splash (callbacks) demo result $R0"
SectionEnd 

; eof
