!define VER_MAJOR 2
!define VER_MINOR 0a6

!ifdef NO_COMPRESSION
SetCompress off
SetDatablockOptimize off
!endif

!ifdef NO_CRC
CRCCheck off
!endif

Name "NSIS"
Caption "Nullsoft Install System - Setup"
OutFile ..\nsis${VER_MAJOR}${VER_MINOR}.exe

SetCompressor bzip2

!ifdef uglyinstaller
BGGradient 000000 308030 FFFFFF
InstallColors FF8080 000000
InstProgressFlags smooth colored
XPStyle on
!else
WindowIcon off
!endif

!ifdef NSIS_CONFIG_LICENSEPAGE
LicenseText "You must read the following license before installing:"
LicenseData ..\license.txt
!endif
!ifdef NSIS_CONFIG_COMPONENTPAGE
ComponentText "This will install the Nullsoft Install System v${VER_MAJOR}.${VER_MINOR} on your computer:"
InstType "Full (w/ Source and Contrib)"
InstType "Normal (w/ Contrib, w/o Source)"
InstType "Lite (w/o Source or Contrib)"
!endif

AutoCloseWindow false
ShowInstDetails show
ShowUninstDetails show
DirText "Please select a location to install NSIS (or use the default):"
SetOverwrite on
SetDateSave on
!ifdef HAVE_UPX
  !packhdr tmp.dat "upx\upx --best --compress-icons=1 tmp.dat"
!endif

InstallDir $PROGRAMFILES\NSIS
InstallDirRegKey HKLM SOFTWARE\NSIS ""

Section "NSIS development system (required)"
  SectionIn 1 2 3 RO
  SetOutPath $INSTDIR
  SetOverwrite try
  Delete $INSTDIR\makensis-bz2.exe
  File ..\makensis.exe
  File ..\makensisw.exe
  File ..\makensis.htm
  File ..\license.txt
  SetOverwrite off
  File ..\nsisconf.nsi
SectionEnd

Section "NSIS Examples (recommended)"
  SectionIn 1 2 3
  SetOutPath $INSTDIR\Examples
  SetOverwrite try
  Delete $INSTDIR\*.nsh
  Delete $INSTDIR\viewhtml.nsi
  Delete $INSTDIR\waplugin.nsi
  Delete $INSTDIR\example*.nsi
  Delete $INSTDIR\*test.nsi
  Delete $INSTDIR\primes.nsi
  Delete $INSTDIR\functions.htm
  File ..\Examples\makensis.nsi
  File ..\Examples\example1.nsi
  File ..\Examples\example2.nsi
  File ..\Examples\viewhtml.nsi
  File ..\Examples\waplugin.nsi
  File ..\Examples\bigtest.nsi
  File ..\Examples\primes.nsi
  File ..\Examples\rtest.nsi
  File ..\Examples\gfx.nsi
  File ..\Examples\one-section.nsi
  File ..\Examples\WinMessages.nsh
  File ..\Examples\branding.nsh
  File ..\Examples\functions.htm
SectionEnd

Section "NSI Development Shell Extensions"
  SectionIn 1 2 3
  ; back up old value of .nsi
  ReadRegStr $1 HKCR ".nsi" ""
  StrCmp $1 "" Label1
    StrCmp $1 "NSISFile" Label1
    WriteRegStr HKCR ".nsi" "backup_val" $1
  Label1:
  WriteRegStr HKCR ".nsi" "" "NSISFile"
  WriteRegStr HKCR "NSISFile" "" "NSI Script File"
  WriteRegStr HKCR "NSISFile\shell" "" "open"
  WriteRegStr HKCR "NSISFile\DefaultIcon" "" $INSTDIR\makensis.exe,0
  WriteRegStr HKCR "NSISFile\shell\open\command" "" 'notepad.exe "%1"'
  WriteRegStr HKCR "NSISFile\shell\compile" "" "Compile NSI"
  WriteRegStr HKCR "NSISFile\shell\compile\command" "" '"$INSTDIR\makensisw.exe" "$INSTDIR\makensis.exe" /CD "%1"'
  WriteRegStr HKCR "NSISFile\shell\compile-bz2" "" "Compile NSI (with bz2)"
  WriteRegStr HKCR "NSISFile\shell\compile-bz2\command" "" '"$INSTDIR\makensisw.exe" "$INSTDIR\makensis.exe" /CD /X"SetCompressor bzip2" "%1"'
SectionEnd

Section "Start Menu + Desktop Icons"
  SectionIn 1 2 3
  SetOutPath $SMPROGRAMS\NSIS
  Delete "$SMPROGRAMS\NSIS\NSIS Home Page.lnk"
  WriteINIStr "$SMPROGRAMS\NSIS\NSIS Home Page.url" "InternetShortcut" "URL" "http://www.nullsoft.com/free/nsis/"
  CreateShortCut "$SMPROGRAMS\NSIS\Uninstall NSIS.lnk" "$INSTDIR\uninst-nsis.exe"
  CreateShortCut "$SMPROGRAMS\NSIS\NSIS Documentation.lnk" "$INSTDIR\makensis.htm"
  CreateShortCut "$SMPROGRAMS\NSIS\NSIS Program Directory.lnk" "$INSTDIR"
  Delete "$SMPROGRAMS\NSIS\NSI Online Template Generator.lnk"
  WriteINIStr "$SMPROGRAMS\NSIS\NSI Online Template Generator.url" "InternetShortcut" "URL" "http://www.firehose.net/free/nsis/makensitemplate.phtml"
  SetOutPath $INSTDIR
  CreateShortCut "$DESKTOP\MakeNSIS.lnk" "$INSTDIR\Makensisw.exe" '"$INSTDIR\makensis.exe" /CD'
SectionEnd

!ifndef NO_CONTRIB

SubSection "Contrib"

Section "Extra Icons"
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\Icons
  SetOverwrite try
  Delete $INSTDIR\Contrib\*.ico
  Delete $INSTDIR\Contrib\*.bmp
  File ..\Contrib\Icons\*.ico
  File ..\Contrib\Icons\*.bmp
  SetOutPath $INSTDIR
SectionEnd

Section "Extra UIs"
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\UIs
  SetOverwrite try
  File ..\Contrib\UIs\*.exe
  SetOutPath $INSTDIR
SectionEnd

Section "Language files"
  SectionIn 1 2
  SetOutPath "$INSTDIR\Contrib\Language files"
  SetOverwrite try
  File "..\Contrib\Language files\*.nlf"
  SetOutPath $INSTDIR
SectionEnd

Section "Splash"
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\Splash
  SetOverwrite try
  File ..\Contrib\Splash\splash.c
  File ..\Contrib\Splash\splash.dsp
  File ..\Contrib\Splash\splash.dsw
  File ..\Contrib\splash\splash.txt
  SetOutPath $INSTDIR\Bin
  File ..\Bin\splash.exe
  IfFileExists $SMPROGRAMS\NSIS 0 NoShortCuts
    CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\Splash Screen Help.lnk" "$INSTDIR\contrib\splash\splash.txt"
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\Splash project workspace.lnk" "$INSTDIR\source\splash\splash.dsw"
  NoShortCuts:
SectionEnd

Section "Splash w/transparency"
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\MagicLime
  SetOverwrite try
  File ..\Contrib\MagicLime\splash.c
  SetOutPath $INSTDIR\Bin
  File ..\Bin\magiclime.exe
SectionEnd

Section "Zip2Exe"
  SectionIn 1 2
  DetailPrint "Extracting zip2exe source"
  SetDetailsPrint textonly
  RMDir /r $INSTDIR\Source\Zip2Exe
  SetOutPath $INSTDIR\Contrib\zip2exe
  SetOverwrite try
  File ..\Contrib\zip2exe\*.cpp
  File ..\Contrib\zip2exe\*.ico
  File ..\Contrib\zip2exe\*.h
  File ..\Contrib\zip2exe\*.rc
  File ..\Contrib\zip2exe\*.dsw
  File ..\Contrib\zip2exe\*.dsp
  SetOutPath $INSTDIR\Contrib\zip2exe\zlib
  File ..\Contrib\zip2exe\zlib\*.*
  SetOutPath $INSTDIR\Bin
  File ..\Bin\zip2exe.exe
  SetDetailsPrint both
  IfFileExists $SMPROGRAMS\NSIS 0 NoShortCuts
    CreateDirectory $SMPROGRAMS\NSIS\Contrib
    Delete "$SMPROGRAMS\Bin\NSIS\ZIP2EXE converter.lnk"
    Delete "$SMPROGRAMS\NSIS\ZIP2EXE project workspace.lnk"
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\ZIP2EXE converter.lnk" "$INSTDIR\Bin\zip2exe.exe"
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\ZIP2EXE project workspace.lnk" "$INSTDIR\source\zip2exe\zip2exe.dsw"
  NoShortCuts:
SectionEnd

Section "InstallOptions"
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\InstallOptions
  SetOverwrite try
  File ..\contrib\installoptions\io.dsp
  File ..\contrib\installoptions\io.dsw
  File ..\contrib\installoptions\test.ini
  File ..\contrib\installoptions\test.nsi
  File ..\contrib\installoptions\InstallerOptions.cpp
  File ..\contrib\installoptions\*.rc
  File ..\contrib\installoptions\*.h
  File "..\contrib\installoptions\Install Options.html"
  SetOutPath $INSTDIR\Bin
  File ..\Bin\InstallOptions.dll
  IfFileExists $SMPROGRAMS\NSIS 0 NoShortCuts
    CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\InstallOptions Readme.lnk" "$INSTDIR\contrib\InstallOptions\install options.html"
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\InstallOptions project workspace.lnk" "$INSTDIR\contrib\InstallOptions\io.dsw"
  NoShortCuts:
SectionEnd

Section "NSIS-DL"
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\NSISdl
  SetOverwrite try
  File ..\contrib\NSISdl\nsisdl.dsw
  File ..\contrib\NSISdl\nsisdl.dsp
  File ..\contrib\NSISdl\*.cpp
  File ..\contrib\NSISdl\*.h
  File ..\contrib\NSISdl\*.rc
  File ..\contrib\NSISdl\ReadMe.txt
  SetOutPath $INSTDIR\Bin
  File ..\Bin\nsisdl.dll
  IfFileExists $SMPROGRAMS\NSIS 0 NoShortCuts
    CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\NSIS-DL Readme.lnk" "$INSTDIR\contrib\NSISDL\ReadMe.txt"
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\NSIS-DL project workspace.lnk" "$INSTDIR\contrib\NSISDL\nsisdl.dsw"
  NoShortCuts:
SectionEnd

SubSectionEnd

!endif


!ifndef NO_SOURCE

SubSection "Source code"

Section "NSIS Source Code"
  SectionIn 1
  DetailPrint "Extracting source code...."
  SetDetailsPrint textonly
  SetOutPath $INSTDIR\Source
  SetOverwrite try
  File ..\Source\*.cpp
  File ..\Source\*.c
  File ..\Source\*.h
  File ..\Source\script1.rc
  File ..\Source\Makefile
  File ..\Source\makenssi.dsp
  File ..\Source\makenssi.dsw
  File ..\Source\icon.ico
  SetOutPath $INSTDIR\Source\zlib
  File ..\Source\zlib\*.*
  SetOutPath $INSTDIR\Source\bzip2
  File ..\Source\bzip2\*.*
  SetOutPath $INSTDIR\Source\exehead
  File ..\Source\exehead\*.c
  File ..\Source\exehead\*.h
  File ..\Source\exehead\exehead.xml
  File ..\Source\exehead\resource.rc
  File ..\Source\exehead\*.dsp
  File ..\Source\exehead\Makefile
  File ..\Source\exehead\nsis.ico
  File ..\Source\exehead\uninst.ico
  File ..\Source\exehead\bitmap1.bmp
  File ..\Source\exehead\bin2h.exe
  IfFileExists $SMPROGRAMS\NSIS 0 NoSourceShortCuts
    CreateShortCut "$SMPROGRAMS\NSIS\MakeNSIS project workspace.lnk" "$INSTDIR\source\makenssi.dsw"
  NoSourceShortCuts:
  SetDetailsPrint both
SectionEnd

SubSection "Contrib"

Section "ExDLL Source"
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\ExDLL
  SetOverwrite try
  File ..\contrib\exdll\exdll.c
  File ..\contrib\exdll\exdll.dpr
  File ..\contrib\exdll\exdll.dsp
  File ..\contrib\exdll\exdll.dsw
  SetOutPath $INSTDIR
  IfFileExists $SMPROGRAMS\NSIS 0 NoShortCuts
    CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\ExDLL project workspace.lnk" "$INSTDIR\contrib\ExDLL\exdll.dsw"
  NoShortCuts:
SectionEnd

Section "MakeNSISW Source"
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\Makensisw
  SetOverwrite try
  File ..\contrib\makensisw\*.cpp
  File ..\contrib\makensisw\*.xml
  File ..\contrib\makensisw\*.h
  File ..\contrib\makensisw\*.ds?
  File ..\contrib\makensisw\*.rc
  File ..\contrib\makensisw\*.txt
  #File ..\contrib\makensisw\Makefile
  IfFileExists $SMPROGRAMS\NSIS 0 NoShortCuts
    CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\MakeNSISW project workspace.lnk" "$INSTDIR\contrib\MakeNsisw\makensisw.dsw"
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\MakeNSISW readme.lnk" "$INSTDIR\contrib\MakeNsisw\readme.txt"
  NoShortCuts:
SectionEnd

SubSectionEnd

SubSectionEnd

!endif

Section -post
  WriteRegStr HKLM SOFTWARE\NSIS "" $INSTDIR
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "DisplayName" "NSIS Development Kit (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "UninstallString" '"$INSTDIR\uninst-nsis.exe"'
  SetOutPath $INSTDIR
  IfFileExists $SMPROGRAMS\NSIS "" nofunshit
    ExecShell open '$SMPROGRAMS\NSIS'
    Sleep 500
    BringToFront
  nofunshit:
  ; since the installer is now created last (in 1.2+), this makes sure 
  ; that any old installer that is readonly is overwritten.
  Delete $INSTDIR\uninst-nsis.exe 
  WriteUninstaller $INSTDIR\uninst-nsis.exe
SectionEnd

Function .onInstSuccess
  MessageBox MB_YESNO|MB_ICONQUESTION "Setup has completed. View readme file now?" IDNO NoReadme
    ExecShell open '$INSTDIR\makensis.htm'
  NoReadme:
FunctionEnd

!ifndef NO_UNINST

UninstallText "This will uninstall NSIS from your system:"

Section Uninstall
  IfFileExists $INSTDIR\makensis.exe skip_confirmation
    MessageBox MB_YESNO "It does not appear that NSIS is installed in the directory '$INSTDIR'.$\r$\nContinue anyway (not recommended)" IDYES skip_confirmation
    Abort "Uninstall aborted by user"
  skip_confirmation:
  ReadRegStr $1 HKCR ".nsi" ""
  StrCmp $1 "NSISFile" 0 NoOwn ; only do this if we own it
    ReadRegStr $1 HKCR ".nsi" "backup_val"
    StrCmp $1 "" 0 RestoreBackup ; if backup == "" then delete the whole key
      DeleteRegKey HKCR ".nsi"
    Goto NoOwn
    RestoreBackup:
      WriteRegStr HKCR ".nsi" "" $1
      DeleteRegValue HKCR ".nsi" "backup_val"
  NoOwn:

  DeleteRegKey HKCR "NSISFile"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS"
  DeleteRegKey HKLM SOFTWARE\NSIS

  Delete $SMPROGRAMS\NSIS\Contrib\*.lnk
  Delete $SMPROGRAMS\NSIS\Contrib\*.url
  RMDir $SMPROGRAMS\NSIS\Contrib
  RMDir /r $INSTDIR\Contrib

  Delete $SMPROGRAMS\NSIS\*.lnk
  Delete $SMPROGRAMS\NSIS\*.url
  RMDir $SMPROGRAMS\NSIS
  Delete $DESKTOP\MakeNSIS.lnk
  Delete $INSTDIR\makensis*.exe
  Delete $INSTDIR\Bin\zip2exe.exe
  Delete $INSTDIR\Bin\installoptions.exe
  Delete $INSTDIR\Bin\installoptions.dll
  Delete $INSTDIR\Bin\splash.txt
  Delete $INSTDIR\Bin\splash.exe
  Delete $INSTDIR\Bin\nsisdl.dll
  Delete $INSTDIR\makensis.htm
  Delete $INSTDIR\Examples\functions.htm
  Delete $INSTDIR\makensis.rtf
  Delete $INSTDIR\uninst-nsis.exe
  Delete $INSTDIR\nsisconf.nsi
  Delete $INSTDIR\Examples\makensis.nsi
  Delete $INSTDIR\Examples\example1.nsi
  Delete $INSTDIR\Examples\example2.nsi
  Delete $INSTDIR\Examples\waplugin.nsi
  Delete $INSTDIR\Examples\viewhtml.nsi
  Delete $INSTDIR\Examples\bigtest.nsi
  Delete $INSTDIR\Examples\primes.nsi
  Delete $INSTDIR\Examples\rtest.nsi
  Delete $INSTDIR\Examples\uglytest.nsi
  Delete $INSTDIR\Examples\spin.nsi
  Delete $INSTDIR\Examples\wafull.nsi
  Delete $INSTDIR\Examples\upgradedll.nsh
  Delete $INSTDIR\Examples\WinMessages.nsh
  Delete $INSTDIR\main.ico
  Delete $INSTDIR\makensis-license.txt
  Delete $INSTDIR\license.txt
  Delete $INSTDIR\uninst.ico
  Delete $INSTDIR\bitmap1.bmp
  Delete $INSTDIR\bitmap2.bmp
  RMDir /r $INSTDIR\Source
  RMDir /r $INSTDIR\Bin
  RMDir /r $INSTDIR\Examples
  RMDir $INSTDIR

  ; if $INSTDIR was removed, skip these next ones
  IfFileExists $INSTDIR 0 Removed 
    MessageBox MB_YESNO|MB_ICONQUESTION \
      "Remove all files in your NSIS directory? (If you have anything you created that you want to keep, click No)" IDNO Removed
    Delete $INSTDIR\*.* ; this would be skipped if the user hits no
    RMDir /r $INSTDIR
    IfFileExists $INSTDIR 0 Removed 
      MessageBox MB_OK|MB_ICONEXCLAMATION "Note: $INSTDIR could not be removed."
  Removed:
SectionEnd

!endif
