; InstallOptions script demonstrating custom buttons
;----------------------------------------------------

!include WinMessages.nsh

Var hwnd ; Window handle of the custom page

; The name of the installer
Name "InstallOptions Test"

; The file to write
OutFile "TestNotify.exe"

; Show install details
ShowInstDetails show

; Called before anything else as installer initialises
Function .onInit

  ; Extract InstallOptions files
  ; $PLUGINSDIR will automatically be removed when the installer closes
  InitPluginsDir
  File /oname=$PLUGINSDIR\test.ini "testnotify.ini"

FunctionEnd

; Our custom page
Page custom ShowCustom LeaveCustom ": Testing InstallOptions"

Function ShowCustom

  ; Initialise the dialog but don't show it yet
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Test the right-to-left version?" IDNO +2
    WriteINIStr "$PLUGINSDIR\test.ini" "Settings" "RTL" "1"
  InstallOptions::initDialog /NOUNLOAD "$PLUGINSDIR\test.ini"
  ; In this mode InstallOptions returns the window handle so we can use it
  Pop $hwnd
  ; Now show the dialog and wait for it to finish
  InstallOptions::show
  ; Finally fetch the InstallOptions status value (we don't care what it is though)
  Pop $0

FunctionEnd

Function LeaveCustom

  ; At this point the user has either pressed Next or one of our custom buttons
  ; We find out which by reading from the INI file
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Settings" "State"
  StrCmp $0 0 validate  ; Next button?
  StrCmp $0 2 supportx  ; "Install support for X"?
  StrCmp $0 9 clearbtn  ; "Clear" button?
  StrCmp $0 11 droplist ; "Show|Hide" drop-list?
  Abort ; Return to the page

supportx:
  ; Make the FileRequest field depend on the first checkbox
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 2" "State"
  GetDlgItem $1 $hwnd 1204 ; PathRequest control (1200 + field 5 - 1)
  EnableWindow $1 $0
  GetDlgItem $1 $hwnd 1205 ; ... button (the following control)
  EnableWindow $1 $0
  Abort ; Return to the page

clearbtn:
  ; Clear all text fields
  GetDlgItem $1 $hwnd 1204 ; PathRequest control (1200 + field 5 - 1)
  SendMessage $1 ${WM_SETTEXT} 0 "STR:"
  GetDlgItem $1 $hwnd 1206 ; DirRequest control (1200 + field 6 - 1 + 1 browse button)
  SendMessage $1 ${WM_SETTEXT} 0 "STR:"
  GetDlgItem $1 $hwnd 1209 ; Multiline control (1200 + field 8 - 1 + 2 browse buttons)
  SendMessage $1 ${WM_SETTEXT} 0 "STR:"
  Abort ; Return to the page

droplist:
  ; Make the DirRequest field depend on the droplist
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 11" "State"
  StrCmp $0 "Show" +3
    StrCpy $0 0
  Goto +2
    StrCpy $0 1
  GetDlgItem $1 $hwnd 1206 ; DirRequest control (1200 + field 6 - 1 + 1 browse button)
  EnableWindow $1 $0
  GetDlgItem $1 $hwnd 1207 ; ... button (the following control)
  EnableWindow $1 $0
  Abort ; Return to the page

validate:
  ; At this point we know the Next button was pressed, so perform any validation
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 2" "State"
  StrCmp $0 1 done
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 3" "State"
  StrCmp $0 1 done
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 4" "State"
  StrCmp $0 1 done
    MessageBox MB_ICONEXCLAMATION|MB_OK "You must select at least one install option!"
    Abort
done:

FunctionEnd

; Installation page
Page instfiles

Section

  ;Get Install Options dialog user input
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 2" "State"
  DetailPrint "Install X=$0"
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 3" "State"
  DetailPrint "Install Y=$0"
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 4" "State"
  DetailPrint "Install Z=$0"
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 5" "State"
  DetailPrint "File=$0"
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 6" "State"
  DetailPrint "Dir=$0"
  ReadINIStr $0 "$PLUGINSDIR\test.ini" "Field 8" "State"
  DetailPrint "Info=$0"

SectionEnd
