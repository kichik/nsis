!define VER_MAJOR 2
!define VER_MINOR 0a7
!define NAME "NSIS"

!verbose 3
!include "${NSISDIR}\Examples\WinMessages.nsh"
!include "${NSISDIR}\Examples\Modern UI\ModernUI.nsh"
!verbose 4

!define CURRENTPAGE $9

!define TEMP1 $R0
!define TEMP2 $R1

Name "NSIS"
Caption "Nullsoft Install System - Setup"
OutFile ..\nsis${VER_MAJOR}${VER_MINOR}.exe
SetCompressor bzip2

!insertmacro MUI_INTERFACE "modern2.exe" "adni18-installer-C-no48xp.ico" "adni18-uninstall-C-no48xp.ico" "modern.bmp" "smooth"

LicenseText "Scroll down to see the rest of the agreement."
LicenseData ..\license.txt

ComponentText "This will install the Nullsoft Install System v${VER_MAJOR}.${VER_MINOR} on your computer:"
InstType "Full"
InstType "Normal"
InstType "Lite"

AutoCloseWindow false
ShowInstDetails show
ShowUninstDetails show
DirText "Please select a location to install NSIS (or use the default):" " "
SetOverwrite on
SetDateSave on

InstallDir $PROGRAMFILES\NSIS
InstallDirRegKey HKLM SOFTWARE\NSIS ""

Section "NSIS Development System (required)" SecCore
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

Section "NSIS Examples (recommended)" SecExample
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
  File ..\Examples\languages.nsi
  File ..\Examples\WinMessages.nsh
  File ..\Examples\branding.nsh
  File ..\Examples\functions.htm
  SetOutPath "$INSTDIR\Examples\Modern UI"
  File "..\Examples\Modern UI\Screenshot.png"
  File "..\Examples\Modern UI\Readme.png"
  File "..\Examples\Modern UI\Readme.html"
  File "..\Examples\Modern UI\License.txt"
  File "..\Examples\Modern UI\Example.nsi"
  File "..\Examples\Modern UI\MultiLanguage.nsi"
  File "..\Examples\Modern UI\ModernUI.nsh"
SectionEnd

Section "NSI Development Shell Extensions" SecExtention
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

Section "Start Menu + Desktop Icons" SecIcons
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

SubSection "Contrib" SecContrib
Section "Extra Icons" SecContribIcons
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\Icons
  SetOverwrite try
  Delete $INSTDIR\Contrib\*.ico
  Delete $INSTDIR\Contrib\*.bmp
  File ..\Contrib\Icons\*.ico
  File ..\Contrib\Icons\*.bmp
  SetOutPath $INSTDIR
SectionEnd

Section "Extra UIs" SecContribUIs
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\UIs
  SetOverwrite try
  File ..\Contrib\UIs\*.exe
  SetOutPath $INSTDIR
SectionEnd

Section "Language files" SecContribLang
  SectionIn 1 2
  SetOutPath "$INSTDIR\Contrib\Language files"
  SetOverwrite try
  File "..\Contrib\Language files\*.nlf"
  SetOutPath $INSTDIR\Bin
  File ..\Bin\MakeLangID.exe
  SetOutPath $INSTDIR
SectionEnd

Section "Language DLL" SecContribLangDLL
  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  SetOverwrite try
  File ..\Plugins\LangDLL.dll
  SetOutPath $INSTDIR\Contrib\LangDLL
  File ..\Contrib\LangDLL\LangDLL.c
  File ..\Contrib\LangDLL\resource.h
  File ..\Contrib\LangDLL\resource.rc
  File ..\Contrib\LangDLL\LangDLL.dsw
  File ..\Contrib\LangDLL\LangDLL.dsp
SectionEnd

Section "Splash" SecContribSplash
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

Section "Splash w/transparency" SecContribSplashT
  SectionIn 1 2
  SetOutPath $INSTDIR\Bin
  SetOverwrite try
  File ..\Bin\magiclime.txt
  File ..\Bin\magiclime.exe
SectionEnd

Section "Zip2Exe" SecContribZ2E
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

Section "InstallOptions" SecContribIO
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
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\InstallOptions.dll
  IfFileExists $SMPROGRAMS\NSIS 0 NoShortCuts
    CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\InstallOptions Readme.lnk" "$INSTDIR\contrib\InstallOptions\install options.html"
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\InstallOptions project workspace.lnk" "$INSTDIR\contrib\InstallOptions\io.dsw"
  NoShortCuts:
SectionEnd

Section "NSIS-DL" SecContribNSISDL
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\NSISdl
  SetOverwrite try
  File ..\contrib\NSISdl\nsisdl.dsw
  File ..\contrib\NSISdl\nsisdl.dsp
  File ..\contrib\NSISdl\*.cpp
  File ..\contrib\NSISdl\*.h
  File ..\contrib\NSISdl\*.rc
  File ..\contrib\NSISdl\ReadMe.txt
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\nsisdl.dll
  IfFileExists $SMPROGRAMS\NSIS 0 NoShortCuts
    CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\NSIS-DL Readme.lnk" "$INSTDIR\contrib\NSISDL\ReadMe.txt"
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\NSIS-DL project workspace.lnk" "$INSTDIR\contrib\NSISDL\nsisdl.dsw"
  NoShortCuts:
SectionEnd
SubSectionEnd


SubSection "Source code" SecSrc
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

SubSection "Contrib" SecSrcContrib
Section "ExDLL Source" SecSrcEx
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

Section "MakeNSISW Source" SecSrcMNW
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
  Delete $INSTDIR\uninst-nsis.exe 
  WriteUninstaller $INSTDIR\uninst-nsis.exe
  !insertmacro MUI_FINISHHEADER
SectionEnd

Function .onInstSuccess
  MessageBox MB_YESNO|MB_ICONQUESTION "Setup has completed. View readme file now?" IDNO NoReadme
    ExecShell open '$INSTDIR\makensis.htm'
  NoReadme:
FunctionEnd

Function .onInitDialog

  !insertmacro MUI_INNERDIALOG_INIT

    !insertmacro MUI_INNERDIALOG_START 1
      !insertmacro MUI_INNERDIALOG_TEXT 1033 1040 "If you accept all the terms of the agreement, choose I Agree to continue. If you choose Cancel, Setup will close. You must accept the agreement to install ${NAME}."
    !insertmacro MUI_INNERDIALOG_STOP 1

    !insertmacro MUI_INNERDIALOG_START 2
      !insertmacro MUI_INNERDIALOG_TEXT 1033 1042 "Description"
      !insertmacro MUI_INNERDIALOG_TEXT 1033 1043 "Hover your mouse over a component to see it's description."
    !insertmacro MUI_INNERDIALOG_STOP 2

    !insertmacro MUI_INNERDIALOG_START 3
      !insertmacro MUI_INNERDIALOG_TEXT 1033 1041 "Destination Folder"
      !insertmacro MUI_INNERDIALOG_STOP 3

  !insertmacro MUI_INNERDIALOG_END

FunctionEnd

Function .onNextPage

  !insertmacro MUI_NEXTPAGE_OUTER
  !insertmacro MUI_NEXTPAGE SetHeader
  
FunctionEnd

Function .onPrevPage

  !insertmacro MUI_PREVPAGE

FunctionEnd

Function SetHeader

  !insertmacro MUI_HEADER_INIT

    !insertmacro MUI_HEADER_START 1
       !insertmacro MUI_HEADER_TEXT 1033 "License Agreement" "Please review the license terms before installing ${NAME}."
    !insertmacro MUI_HEADER_STOP 1

    !insertmacro MUI_HEADER_START 2
      !insertmacro MUI_HEADER_TEXT 1033 "Choose Components" "Choose the components you want to install."
    !insertmacro MUI_HEADER_STOP 2

    !insertmacro MUI_HEADER_START 3
      !insertmacro MUI_HEADER_TEXT 1033 "Choose Install Location" "Choose the folder in which to install ${NAME} in."
    !insertmacro MUI_HEADER_STOP 3

    !insertmacro MUI_HEADER_START 4
      !insertmacro MUI_HEADER_TEXT 1033 "Installing" "Please wait while ${NAME} is being installed."
    !insertmacro MUI_HEADER_STOP 4

    !insertmacro MUI_HEADER_START 5
      !insertmacro MUI_HEADER_TEXT 1033 "Finished" "Setup was completed successfully."
    !insertmacro MUI_HEADER_STOP 5

  !insertmacro MUI_HEADER_END

FunctionEnd

Function .onUserAbort

  !insertmacro MUI_ABORTWARNING 1033 "Are you sure you want to quit ${NAME} Setup?"
  !insertmacro MUI_ABORTWARNING_END
  
FunctionEnd

Function .onMouseOverSection

  !insertmacro MUI_DESCRIPTION_INIT

    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecCore} "The Core files required to use NSIS"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecExample} "Example installation scripts that show you how to use NSIS"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecExtention} "Adds right mouse click integration to nsi files so you can compile scripts easily"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecIcons} "Adds icons to your start menu and your desktop for easy access"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContrib} "Tools, files, and other utilities contributed by other NSIS developers"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribIcons} "Icon files contributed by other NSIS developers"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribUIs} "User interface designs that can be used to change the installer look and feel"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribLang} "Language files used to support multiple languages in an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribLangDLL} "Plugin that lets you add a language select dialog to your installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribSplash} "Splash screen add-on that lets you add a splash screen to an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribSplashT} "Splash screen add-on with transparency support that lets you add a splash screen to an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribZ2E} "Zip2Exe utility that converts zip files into an NSIS installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribIO} "Plugin that lets you add user interface components to an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribNSISDL} "Plugin that lets you create a web based installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecSrc} "Source code to NSIS and all related files"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecSrcContrib} "Source code to user contributed utilities"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecSrcEx} "Example DLL source in C"
    !insertmacro MUI_DESCRIPTION_TEXT 1033  ${SecSrcMNW} "MakeNSIS Wrapper source code"

 !insertmacro MUI_DESCRIPTION_END

FunctionEnd

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
  Delete $INSTDIR\Plugins\installoptions.dll
  Delete $INSTDIR\Bin\splash.txt
  Delete $INSTDIR\Bin\splash.exe
  Delete $INSTDIR\Bin\magiclime.exe
  Delete $INSTDIR\Bin\magiclime.txt
  Delete $INSTDIR\Plugins\nsisdl.dll
  Delete $INSTDIR\Bin\MakeLangID.exe
  Delete $INSTDIR\Plugins\LangDLL.dll
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
  Delete $INSTDIR\Examples\languages.nsi
  Delete $INSTDIR\Examples\upgradedll.nsh
  Delete $INSTDIR\Examples\WinMessages.nsh
  Delete "$INSTDIR\Examples\Modern UI\Screenshot.png"
  Delete "$INSTDIR\Examples\Modern UI\Readme.png"
  Delete "$INSTDIR\Examples\Modern UI\Readme.html"
  Delete "$INSTDIR\Examples\Modern UI\License.txt"
  Delete "$INSTDIR\Examples\Modern UI\Example.nsi"
  Delete "$INSTDIR\Examples\Modern UI\MultiLanguage.nsi"
  Delete "$INSTDIR\Examples\Modern UI\ModernUI.nsh"
  Delete $INSTDIR\main.ico
  Delete $INSTDIR\makensis-license.txt
  Delete $INSTDIR\license.txt
  Delete $INSTDIR\uninst.ico
  Delete $INSTDIR\bitmap1.bmp
  Delete $INSTDIR\bitmap2.bmp
  RMDir /r $INSTDIR\Source
  RMDir /r $INSTDIR\Bin
  RMDir /r $INSTDIR\Plugins
  RMDir /r "$INSTDIR\Examples\Modern UI"
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
  IntOp ${CURRENTPAGE} ${CURRENTPAGE} + 1
  Call un.SetHeader
SectionEnd

Function un.onNextPage

  !insertmacro MUI_NEXTPAGE_OUTER
  !insertmacro MUI_NEXTPAGE un.SetHeader
  
FunctionEnd

Function un.SetHeader

  !insertmacro MUI_HEADER_INIT

    !insertmacro MUI_HEADER_START 1
      !insertmacro MUI_HEADER_TEXT 1033 "Uninstall ${NAME}" "Remove ${NAME} from your system."
    !insertmacro MUI_HEADER_STOP 1

    !insertmacro MUI_HEADER_START 2
      !insertmacro MUI_HEADER_TEXT 1033 "Uninstalling" "Please wait while ${NAME} is being uninstalled."
    !insertmacro MUI_HEADER_STOP 2

    !insertmacro MUI_HEADER_START 3
      !insertmacro MUI_HEADER_TEXT 1033 "Finished" "${NAME} has been removed from your system."
    !insertmacro MUI_HEADER_STOP 3

  !insertmacro MUI_HEADER_END

FunctionEnd
