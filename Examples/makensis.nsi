!define VER_MAJOR 2
!define VER_MINOR 0b0
!define NAME "NSIS"

!verbose 3
!include "${NSISDIR}\Examples\WinMessages.nsh"
!ifndef CLASSIC_UI
!include "${NSISDIR}\Examples\Modern UI\ModernUI.nsh"
!endif
!verbose 4

Name "NSIS"
Caption "Nullsoft Install System - Setup"
OutFile ..\nsis${VER_MAJOR}${VER_MINOR}.exe
SetCompressor bzip2

!ifndef CLASSIC_UI
!insertmacro MUI_INTERFACE "modern2.exe" "adni18-installer-C-no48xp.ico" "adni18-uninstall-C-no48xp.ico" "modern.bmp" "smooth" "$9"
!endif

LicenseText "Scroll down to see the rest of the agreement."
LicenseData ..\license.txt

ComponentText "This will install the Nullsoft Install System v${VER_MAJOR}.${VER_MINOR} on your computer:"
InstType "Full (w/ Source and Contrib)"
InstType "Normal (w/ Contrib, w/o Source)"
InstType "Lite (w/o Source or Contrib)"

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
  RMDir /r $SMPROGRAMS\NSIS

  SetOverwrite try
  Delete $INSTDIR\makensis-bz2.exe
  File ..\makensis.exe
  File ..\makensisw.exe
  File ..\makensis.htm
  File ..\license.txt
  SetOverwrite off
  File ..\nsisconf.nsi
  SetOverwrite try

  SetOutPath $INSTDIR\Docs
  File ..\Docs\*.html
  File ..\Docs\*.css

  SetOutPath $INSTDIR\Contrib\Makensisw
  File ..\contrib\makensisw\*.txt
SectionEnd

Section "NSIS Examples (recommended)" SecExample
  SectionIn 1 2 3
  SetOutPath $INSTDIR\Examples
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
  File "..\Examples\Modern UI\Readme.jpg"
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
  WriteRegStr HKCR ".nsh" "" "NSHFile"
  WriteRegStr HKCR "NSHFile" "" "NSI Script File"
  WriteRegStr HKCR "NSHFile\shell" "" "open"
  WriteRegStr HKCR "NSHFile\DefaultIcon" "" $INSTDIR\makensisw.exe,1
  WriteRegStr HKCR "NSHFile\shell\open\command" "" 'notepad.exe "%1"'
  WriteRegStr HKCR ".nsi" "" "NSISFile"
  WriteRegStr HKCR "NSISFile" "" "NSI Script File"
  WriteRegStr HKCR "NSISFile\shell" "" "open"
  WriteRegStr HKCR "NSISFile\DefaultIcon" "" $INSTDIR\makensisw.exe,1
  WriteRegStr HKCR "NSISFile\shell\open\command" "" 'notepad.exe "%1"'
  WriteRegStr HKCR "NSISFile\shell\compile" "" "Compile NSI"
  WriteRegStr HKCR "NSISFile\shell\compile\command" "" '"$INSTDIR\makensisw.exe" /CD "%1"'
  WriteRegStr HKCR "NSISFile\shell\compile-bz2" "" "Compile NSI (with bz2)"
  WriteRegStr HKCR "NSISFile\shell\compile-bz2\command" "" '"$INSTDIR\makensisw.exe" /CD /X"SetCompressor bzip2" "%1"'
SectionEnd

Section "Start Menu + Desktop Shortcuts" SecIcons
  SectionIn 1 2 3
  SetOutPath $INSTDIR
  CreateDirectory $SMPROGRAMS\NSIS

  CreateShortCut "$SMPROGRAMS\NSIS\MakeNSIS GUI.lnk" "$INSTDIR\Makensisw.exe" ""
  WriteINIStr "$SMPROGRAMS\NSIS\NSIS Home Page.url" "InternetShortcut" "URL" "http://www.nullsoft.com/free/nsis/"
  CreateShortCut "$SMPROGRAMS\NSIS\Uninstall NSIS.lnk" "$INSTDIR\uninst-nsis.exe"
  CreateShortCut "$SMPROGRAMS\NSIS\NSIS Documentation.lnk" "$INSTDIR\makensis.htm"
  
  CreateShortCut "$DESKTOP\MakeNSIS.lnk" "$INSTDIR\Makensisw.exe" ""
SectionEnd

SubSection "Contrib" SecContrib
Section "Extra Icons" SecContribIcons
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\Icons
  Delete $INSTDIR\Contrib\*.ico
  Delete $INSTDIR\Contrib\*.bmp
  File ..\Contrib\Icons\*.ico
  File ..\Contrib\Icons\*.bmp
SectionEnd

Section "Extra UIs" SecContribUIs
  SectionIn 1 2
  SetOutPath $INSTDIR\Contrib\UIs
  File ..\Contrib\UIs\*.exe
SectionEnd

Section "Language files" SecContribLang
  SectionIn 1 2
  SetOutPath "$INSTDIR\Contrib\Language files"
  File "..\Contrib\Language files\*.nlf"
  SetOutPath $INSTDIR\Bin
  File ..\Bin\MakeLangID.exe
SectionEnd

SubSection "Plugins" SecContribPlugins

Section "Language DLL" SecContribLangDLL
  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\LangDLL.dll
SectionEnd

Section "nsExec" SecContribnsExec
  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\nsExec.dll
  SetOutPath $INSTDIR\Contrib\nsExec
  File ..\Contrib\nsExec\*.txt
SectionEnd

Section "Splash" SecContribSplash
  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\splash.dll
  SetOutPath $INSTDIR\Contrib\Splash
  File ..\Contrib\splash\splash.txt
SectionEnd

Section "UberSplash w/transparency" SecContribSplashT
  SectionIn 1 2
  SetOutPath $INSTDIR\Bin
  File ..\Bin\UberSplash.exe
  SetOutPath $INSTDIR\Contrib\UberSplash
  File ..\Contrib\UberSplash\*.txt
SectionEnd

Section "InstallOptions" SecContribIO
  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\InstallOptions.dll
  SetOutPath $INSTDIR\Contrib\InstallOptions
  File "..\contrib\installoptions\Install Options.html"
  File ..\contrib\installoptions\test.ini
  File ..\contrib\installoptions\test.nsi
SectionEnd

Section "NSIS-DL" SecContribNSISDL
  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\nsisdl.dll
  SetOutPath $INSTDIR\Contrib\NSISdl
  File ..\contrib\NSISdl\ReadMe.txt
SectionEnd

Section "System" SecContribSystem
  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\System.dll
SectionEnd

Section "ShowWin" SecContribShowWin
  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\ShowWin.dll
  SetOutPath $INSTDIR\contrib\ShowWin
  File ..\contrib\ShowWin\ShowWin.txt
  File ..\contrib\ShowWin\ShowWin.nsi
SectionEnd

SubSectionEnd

Section "Zip2Exe" SecContribZ2E
  SectionIn 1 2
  SetOutPath $INSTDIR\Bin
  File ..\Bin\zip2exe.exe
SectionEnd


SubSectionEnd


SubSection "Source code" SecSrc
Section "NSIS Source Code" SecSrcNSIS
  SectionIn 1
  DetailPrint "Extracting source code...."
  SetDetailsPrint textonly
  SetOutPath $INSTDIR\Source
  File ..\Source\*.cpp
  File ..\Source\*.c
  File ..\Source\*.h
  File ..\Source\script1.rc
  File ..\Source\Makefile
  File ..\Source\makenssi.dsp
  File ..\Source\makenssi.dsw
  SetOutPath $INSTDIR\Source\zlib
  File ..\Source\zlib\*.*
  SetOutPath $INSTDIR\Source\bzip2
  File ..\Source\bzip2\*.*
  SetOutPath $INSTDIR\Source\exehead
  File ..\Source\exehead\*.c
  File ..\Source\exehead\*.h
  File ..\Source\exehead\resource.rc
  File ..\Source\exehead\*.dsp
  File ..\Source\exehead\Makefile
  File ..\Source\exehead\nsis.ico
  File ..\Source\exehead\uninst.ico
  File ..\Source\exehead\bitmap1.bmp
  File ..\Source\exehead\bin2h.exe
  SetDetailsPrint both
SectionEnd

SubSection "Contrib" SecSrcContrib
Section "ExDLL Source" SecSrcEx
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\ExDLL
  File ..\contrib\exdll\exdll.c
  File ..\contrib\exdll\exdll.dpr
  File ..\contrib\exdll\exdll.dsp
  File ..\contrib\exdll\exdll.dsw
SectionEnd

Section "MakeNSISW Source" SecSrcMNW
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\Makensisw
  File ..\contrib\makensisw\*.cpp
  File ..\contrib\makensisw\*.xml
  File ..\contrib\makensisw\*.h
  File ..\contrib\makensisw\*.ds?
  File ..\contrib\makensisw\*.rc
  File ..\contrib\makensisw\*.txt
  #File ..\contrib\makensisw\Makefile

SectionEnd

SubSection "Plugins" SecContribPluginsS

Section "Language DLL Source" SecContribLangDLLS
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\LangDLL
  File ..\Contrib\LangDLL\LangDLL.c
  File ..\Contrib\LangDLL\resource.h
  File ..\Contrib\LangDLL\resource.rc
  File ..\Contrib\LangDLL\LangDLL.dsw
  File ..\Contrib\LangDLL\LangDLL.dsp
SectionEnd

Section "nsExec Source" SecContribnsExecS
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\nsExec
  File ..\Contrib\nsExec\*.c
  File ..\Contrib\nsExec\*.txt
  File ..\Contrib\nsExec\*.dsw
  File ..\Contrib\nsExec\*.dsp
SectionEnd

Section "Splash Source" SecContribSplashS
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\Splash
  File ..\Contrib\Splash\splash.c
  File ..\Contrib\Splash\splash.dsp
  File ..\Contrib\Splash\splash.dsw
  File ..\Contrib\splash\splash.txt
SectionEnd

Section "UberSplash Source" SecContribSplashTS
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\UberSplash
  File ..\Contrib\UberSplash\splash.*
  File ..\Contrib\UberSplash\*.txt
SectionEnd

Section "InstallOptions Source" SecContribIOS
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\InstallOptions
  File ..\contrib\installoptions\io.dsp
  File ..\contrib\installoptions\io.dsw
  File ..\contrib\installoptions\test.ini
  File ..\contrib\installoptions\test.nsi
  File ..\contrib\installoptions\InstallerOptions.cpp
  File ..\contrib\installoptions\*.rc
  File ..\contrib\installoptions\*.h
  File "..\contrib\installoptions\Install Options.html"
SectionEnd

Section "NSIS-DL Source" SecContribNSISDLS
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\NSISdl
  File ..\contrib\NSISdl\nsisdl.dsw
  File ..\contrib\NSISdl\nsisdl.dsp
  File ..\contrib\NSISdl\*.cpp
  File ..\contrib\NSISdl\*.h
  File ..\contrib\NSISdl\*.rc
  File ..\contrib\NSISdl\ReadMe.txt
SectionEnd

Section "System Source" SecContribSystemS
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\System
  File ..\contrib\System\*.c
  File ..\contrib\System\*.h
  File ..\contrib\System\*.ncb
  File ..\contrib\System\*.sln
  File ..\contrib\System\*.txt
  File ..\contrib\System\*.vcproj
SectionEnd

Section "ShowWin Source" SecContribShowWinS
  SectionIn 1
  SetOutPath $INSTDIR\Contrib\System
  File ..\contrib\System\*.c
  File ..\contrib\System\*.dsp
  File ..\contrib\System\*.dsw
  File ..\contrib\System\ShowWin.txt
  File ..\contrib\System\ShowWin.nsi
SectionEnd


SubSectionEnd ; plugins

Section "Zip2Exe Source" SecContribZ2ES
  SectionIn 1
  DetailPrint "Extracting zip2exe source"
  SetDetailsPrint textonly
  RMDir /r $INSTDIR\Source\Zip2Exe
  SetOutPath $INSTDIR\Contrib\zip2exe
  File ..\Contrib\zip2exe\*.cpp
  File ..\Contrib\zip2exe\*.ico
  File ..\Contrib\zip2exe\*.h
  File ..\Contrib\zip2exe\*.rc
  File ..\Contrib\zip2exe\*.dsw
  File ..\Contrib\zip2exe\*.dsp
  SetOutPath $INSTDIR\Contrib\zip2exe\zlib
  File ..\Contrib\zip2exe\zlib\*.*
  SetDetailsPrint both
SectionEnd

SubSectionEnd
SubSectionEnd

Section -post
  SetOutPath $INSTDIR
  WriteRegStr HKLM SOFTWARE\NSIS "" $INSTDIR
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "DisplayName" "NSIS Development Kit (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "UninstallString" '"$INSTDIR\uninst-nsis.exe"'
  IfFileExists $SMPROGRAMS\NSIS "" nofunshit

    IfFileExists $INSTDIR\Examples 0 NoExShortCuts
      CreateShortCut "$SMPROGRAMS\NSIS\NSIS Examples Directory.lnk" "$INSTDIR\Examples"
    NoExShortCuts:

    CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\MakeNSISW readme.lnk" "$INSTDIR\contrib\MakeNsisw\readme.txt"

    IfFileExists $INSTDIR\Contrib\Makensisw\*.dsw 0 NoMNWShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Source\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Source\Contrib\MakeNSISW project workspace.lnk" "$INSTDIR\contrib\MakeNsisw\makensisw.dsw"
    NoMNWShortCuts:
    IfFileExists "$INSTDIR\Contrib\ExDll" 0 NoExDLLShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Source\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Source\Contrib\ExDLL project workspace.lnk" "$INSTDIR\contrib\ExDLL\exdll.dsw"
    NoExDLLShortCuts:
    IfFileExists "$INSTDIR\Source" 0 NoSourceShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Source
      CreateShortCut "$SMPROGRAMS\NSIS\Source\MakeNSIS project workspace.lnk" "$INSTDIR\source\makenssi.dsw"
    NoSourceShortCuts:

    IfFileExists "$INSTDIR\Plugins\installoptions.dll" 0 NoIOShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Contrib\InstallOptions Readme.lnk" "$INSTDIR\contrib\InstallOptions\install options.html"
    NoIOShortCuts:
    
    IfFileExists "$INSTDIR\Contrib\InstallOptions\io.dsw" 0 NoIOShortCutsS
      CreateDirectory $SMPROGRAMS\NSIS\Source\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Source\Contrib\InstallOptions project workspace.lnk" "$INSTDIR\contrib\InstallOptions\io.dsw"
    NoIOShortCutsS:

    IfFileExists "$INSTDIR\Bin\zip2exe.exe" 0 Noz2eShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Contrib\ZIP 2 EXE converter.lnk" "$INSTDIR\Bin\zip2exe.exe"
    Noz2eShortCuts:

    IfFileExists "$INSTDIR\Contrib\ZIP2EXE\*.dsw" 0 Noz2eShortCutsS
      CreateDirectory $SMPROGRAMS\NSIS\Source\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Source\Contrib\ZIP2EXE project workspace.lnk" "$INSTDIR\source\zip2exe\zip2exe.dsw"
    Noz2eShortCutsS:

    IfFileExists "$INSTDIR\Examples\Modern UI\Readme.html" 0 NoMUIShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Contrib\Modern UI Readme.lnk" "$INSTDIR\Examples\Modern UI\Readme.html"
    NoMUIShortCuts:

    IfFileExists "$INSTDIR\Plugins\splash.dll" 0 NoSPLShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Contrib\Splash Screen Help.lnk" "$INSTDIR\contrib\splash\splash.txt"
    NoSPLShortCuts:

    IfFileExists "$INSTDIR\Contrib\Splash\*.dsw" 0 NoSPLShortCutsS
      CreateDirectory $SMPROGRAMS\NSIS\Source\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Source\Contrib\Splash project workspace.lnk" "$INSTDIR\Contrib\splash\splash.dsw"
    NoSPLShortCutsS:

    IfFileExists "$INSTDIR\Bin\ubersplash.exe" 0 NoUSPLShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Contrib\UberSplash Screen Help.lnk" "$INSTDIR\contrib\ubersplash\ubersplash.txt"
    NoUSPLShortCuts:

    IfFileExists "$INSTDIR\Contrib\UberSplash\*.dpr" 0 NoUSPLShortCutsS
      CreateDirectory $SMPROGRAMS\NSIS\Source\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Source\Contrib\UberSplash project directory.lnk" "$INSTDIR\Contrib\ubersplash"
    NoUSPLShortCutsS:


    IfFileExists "$INSTDIR\Plugins\nsisdl.dll" 0 NoDLShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Contrib\NSIS-DL Readme.lnk" "$INSTDIR\contrib\NSISDL\ReadMe.txt"
    NoDLShortCuts:

    IfFileExists "$INSTDIR\Contrib\NSISDL\*.dsw" 0 NoDLSShortCuts
      CreateDirectory $SMPROGRAMS\NSIS\Source\Contrib
      CreateShortCut "$SMPROGRAMS\NSIS\Source\Contrib\NSIS-DL project workspace.lnk" "$INSTDIR\contrib\NSISDL\nsisdl.dsw"
    NoDLSShortCuts:

    ExecShell open '$SMPROGRAMS\NSIS'
    Sleep 500
    BringToFront
  nofunshit:
  Delete $INSTDIR\uninst-nsis.exe
  WriteUninstaller $INSTDIR\uninst-nsis.exe
!ifndef CLASSIC_UI
  !insertmacro MUI_FINISHHEADER SetPage
!endif
SectionEnd

Function .onInstSuccess
  MessageBox MB_YESNO|MB_ICONQUESTION "Setup has completed. View readme file now?" IDNO NoReadme
    ExecShell open '$INSTDIR\makensis.htm'
  NoReadme:
FunctionEnd

!ifndef CLASSIC_UI
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

  !insertmacro MUI_NEXTPAGE SetPage

FunctionEnd

Function .onPrevPage

  !insertmacro MUI_PREVPAGE SetPage

FunctionEnd

Function SetPage

  !insertmacro MUI_PAGE_INIT

    !insertmacro MUI_PAGE_START 1
       !insertmacro MUI_HEADER_TEXT 1033 "License Agreement" "Please review the license terms before installing ${NAME}."
    !insertmacro MUI_PAGE_STOP 1

    !insertmacro MUI_PAGE_START 2
      !insertmacro MUI_HEADER_TEXT 1033 "Choose Components" "Choose the components you want to install."
    !insertmacro MUI_PAGE_STOP 2

    !insertmacro MUI_PAGE_START 3
      !insertmacro MUI_HEADER_TEXT 1033 "Choose Install Location" "Choose the folder in which to install ${NAME}."
    !insertmacro MUI_PAGE_STOP 3

    !insertmacro MUI_PAGE_START 4
      !insertmacro MUI_HEADER_TEXT 1033 "Installing" "Please wait while ${NAME} is being installed."
    !insertmacro MUI_PAGE_STOP 4

    !insertmacro MUI_PAGE_START 5
      !insertmacro MUI_HEADER_TEXT 1033 "Finished" "Setup was completed successfully."
    !insertmacro MUI_PAGE_STOP 5

  !insertmacro MUI_PAGE_END

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
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribPlugins} "Useful plugins that extend NSIS's functionality"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribPluginsS} "Source code for plugins"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribLangDLL} "Plugin that lets you add a language select dialog to your installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribLangDLLS} "Source code to plugin that lets you add a language select dialog to your installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribnsExec} "Plugin that executes DOS based programs and hides the output"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribnsExecS} "Source code to plugin that executes DOS based programs and hides the output"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribSplash} "Splash screen add-on that lets you add a splash screen to an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribSplashS} "Source code to splash screen add-on that lets you add a splash screen to an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribSplashT} "Splash screen add-on with transparency support that lets you add a splash screen to an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribSplashTS} "Source code to splash screen add-on with transparency support that lets you add a splash screen to an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribSystem} "Plugin that lets you call Win32 API from NSIS scripts"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribSystemS} "Source code to plugin that lets you call Win32 API from NSIS scripts"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribShowWin} "Plugin that lets you hide/show/enable/disable controls on NSIS dialogs"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribShowWinS} "Source code to plugin that lets you hide/show/enable/disable controls on NSIS dialogs"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribSystemS} "Source code to plugin that lets you call Win32 API from NSIS scripts"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribZ2E} "Zip2Exe utility that converts zip files into an NSIS installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribZ2ES} "Source code to Zip2Exe utility that converts zip files into an NSIS installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribIO} "Plugin that lets you add user interface components to an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribIOS} "Source code to plugin that lets you add user interface components to an installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribNSISDL} "Plugin that lets you create a web based installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecContribNSISDLS} "Source code to plugin that lets you create a web based installer"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecSrc} "Source code to NSIS and all related files"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecSrcNSIS} "Source code to NSIS"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecSrcContrib} "Source code to user contributed utilities"
    !insertmacro MUI_DESCRIPTION_TEXT 1033 ${SecSrcEx} "Example DLL source in C"
    !insertmacro MUI_DESCRIPTION_TEXT 1033  ${SecSrcMNW} "MakeNSIS Wrapper source code"

 !insertmacro MUI_DESCRIPTION_END

FunctionEnd
!endif

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

  ReadRegStr $1 HKCR ".nsh" ""
  StrCmp $1 "NSHFile" 0 NoOwn2 ; only do this if we own it
    ReadRegStr $1 HKCR ".nsh" "backup_val"
    StrCmp $1 "" 0 RestoreBackup2 ; if backup == "" then delete the whole key
      DeleteRegKey HKCR ".nsh"
    Goto NoOwn
    RestoreBackup2:
      WriteRegStr HKCR ".nsh" "" $1
      DeleteRegValue HKCR ".nsh" "backup_val"
  NoOwn2:

  DeleteRegKey HKCR "NSISFile"
  DeleteRegKey HKCR "NSHFile"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS"
  DeleteRegKey HKLM SOFTWARE\NSIS

  RMDir /r $SMPROGRAMS\NSIS
  RMDir /r $INSTDIR\Contrib
  Delete $DESKTOP\MakeNSIS.lnk
  Delete $INSTDIR\makensis*.exe
  Delete $INSTDIR\Docs\*.html
  Delete $INSTDIR\Docs\*.css
  Delete $INSTDIR\Bin\zip2exe.exe
  Delete $INSTDIR\Bin\installoptions.exe
  Delete $INSTDIR\Plugins\installoptions.dll
  Delete $INSTDIR\Bin\splash.txt
  Delete $INSTDIR\Bin\splash.exe
  Delete $INSTDIR\Plugins\splash.dll
  Delete $INSTDIR\Bin\UberSplash.exe
  Delete $INSTDIR\Plugins\nsisdl.dll
  Delete $INSTDIR\Bin\MakeLangID.exe
  Delete $INSTDIR\Plugins\LangDLL.dll
  Delete $INSTDIR\makensis.htm
  Delete $INSTDIR\Examples\functions.htm
  Delete $INSTDIR\license.txt
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
  Delete "$INSTDIR\Examples\Modern UI\Readme.jpg"
  Delete "$INSTDIR\Examples\Modern UI\Readme.html"
  Delete "$INSTDIR\Examples\Modern UI\License.txt"
  Delete "$INSTDIR\Examples\Modern UI\Example.nsi"
  Delete "$INSTDIR\Examples\Modern UI\MultiLanguage.nsi"
  Delete "$INSTDIR\Examples\Modern UI\ModernUI.nsh"
  RMDir /r $INSTDIR\Source
  RMDir /r $INSTDIR\Bin
  RMDir /r $INSTDIR\Plugins
  RMDir /r "$INSTDIR\Examples\Modern UI"
  RMDir /r $INSTDIR\Examples
  RMDir /r $INSTDIR\Docs
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
!ifndef CLASSIC_UI
  !insertmacro MUI_FINISHHEADER un.SetPage
!endif
SectionEnd


!ifndef CLASSIC_UI

Function un.onNextPage

  !insertmacro MUI_NEXTPAGE un.SetPage

FunctionEnd

Function un.SetPage

  !insertmacro MUI_PAGE_INIT

    !insertmacro MUI_PAGE_START 1
      !insertmacro MUI_HEADER_TEXT 1033 "Uninstall ${NAME}" "Remove ${NAME} from your system."
    !insertmacro MUI_PAGE_STOP 1

    !insertmacro MUI_PAGE_START 2
      !insertmacro MUI_HEADER_TEXT 1033 "Uninstalling" "Please wait while ${NAME} is being uninstalled."
    !insertmacro MUI_PAGE_STOP 2

    !insertmacro MUI_PAGE_START 3
      !insertmacro MUI_HEADER_TEXT 1033 "Finished" "${NAME} has been removed from your system."
    !insertmacro MUI_PAGE_STOP 3

  !insertmacro MUI_PAGE_END

FunctionEnd
!endif