; viewhtml.nsi
;
; This script creates a silent installer which extracts one (or more) HTML
; files to a temporary directory, opens Internet Explorer to view the file(s),
; and when Internet Explorer has quit, deletes the file(s).

;--------------------------------

; The name of the installer (not really used in a silent install)
Name "ViewHTML"

; Set to silent mode
SilentInstall silent

; The file to write
OutFile "viewhtml.exe"

;--------------------------------

; The stuff to install
Section ""

  ; Get a temporary filename (in the Windows Temp directory)
  GetTempFileName $R0
  
  ; Extract file
  File /oname=$R0 "..\Docs\Chapter1.html"
  
  ; View file
  ; ExecShell "open" '"$R0"'
  
  ; Delete the files (on reboot if file is in use)
  Delete /REBOOTOK $R0

SectionEnd