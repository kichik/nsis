; This is just an example of System Plugin
; 
; Read LastComments.txt and System.txt (unfinished) 
;
; (c) BSForce, 2002

Name "System Plugin Example"
OutFile "SystemEx.exe"

Function .onInit

  ; First example: Message box, uses PartAddr, ShortAddr, Call

  System::PartAddr "user32.dll?MessageBoxA?iissi" 
  Pop $0
  System::ShortAddr "ssss?1nssn" $0
  Pop $0
  System::Call  33 "Example 1" "Just something" 0 $0
  Pop $3
  Pop $2
  MessageBox MB_OK "Result: $1, strings '$2' and '$3'"
  System::Free $0
  Pop $0

  ; Second example: uses FullAddr, Call, incorpotates int64, returns to $INSTDIR

  System::FullAddr "snnn?s1al2" "kernel32.dll?GetDiskFreeSpaceExA?isplplpl" 
  Pop $0
  System::Call "c:\" $0
  Pop $3
  MessageBox MB_OK "Path '$1', Free '$R0', Total '$INSTDIR', FreeUser '$2', Result $3"
  System::Free $0
  Pop $0

  ; Third example: uses FullCall

  System::FullCall "?9" "kernel32.dll?GetVersion?i" 
  ; Version aquired, but we wonna built number for example
  IntOp $8 $9 / 65536
  MessageBox MB_OK "Aquired windows version: $9 (Build $8)"

  Quit
FunctionEnd

Section "ThisNameIsIgnoredSoWhyBother?"
SectionEnd 

; eof
