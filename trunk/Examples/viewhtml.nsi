; viewhtml.nsi
;
; This script creates a silent installer which extracts one (or more) HTML
; files to a temporary directory, opens Internet Explorer to view the file(s),
; and when Internet Explorer has quit, deletes the file(s).
;

; The name of the installer (not really used in a silent install)
Name "ViewHTML"

; Set to silent mode
SilentInstall silent

; The file to write
OutFile "viewhtml.exe"

; The installation directory (the user never gets to change this)
InstallDir "$TEMP\ViewHTML"

; The stuff to install
Section ""
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  ; Extract file
  File "..\makensis.htm"
  ; View file
  ExecWait '"$PROGRAMFILES\Internet Explorer\iexplore.exe" "$INSTDIR\makensis.htm"'
  ; Delete the files
  Delete $INSTDIR\Makensis.htm
  RMDir $INSTDIR
SectionEnd

; Note: another way of doing this would be to use ExecShell, but then you 
; really couldn't get away with deleting the files. Here is the ExecShell
; line that you would want to use:
;
; ExecShell "open" '"$INSTDIR\makensis.htm"'
;
; The advantage of this way is that it would use the default browser to
; open the HTML. 
;

; eof
