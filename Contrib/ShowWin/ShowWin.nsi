;ShowWin Example

Name "ShowWin Example"
OutFile "ShowWin.exe"

LicenseText "Hide Richedit Control and disable the cancel button."
LicenseData "ShowWin.txt"

Section ""
SectionEnd

Function .onInitDialog
  ;hide richedit control
  FindWindow $R1 "#32770" "" $HWNDPARENT
  GetDlgItem $R1 $R1 1000
  ShowWin::Hide $R1

  ;disable the 'I Agree' button
  GetDlgItem $R1 $HWNDPARENT 1
  ShowWin::Disable $R1
FunctionEnd