;NSIS Setup Script
;--------------------------------

!define VER_MAJOR 2
!define VER_MINOR 0
!define VER_REVISION 5
!define VER_BUILD 0

!define VER_FILE "205"
!define VER_DISPLAY "2.05"

;--------------------------------
;Compile CVS Data Setup

!system '"${NSISDIR}\Bin\InstallCVSData.exe" nooverwrite'
!echo "Compiling CVS Data Setup..."
!system '"${NSISDIR}\makensis" /V1 cvsdata.nsi' = 0

;--------------------------------
;Configuration

OutFile ..\nsis${VER_FILE}.exe
SetCompressor lzma

InstType "Full"
InstType "Standard"
InstType "Lite"

InstallDir $PROGRAMFILES\NSIS
InstallDirRegKey HKLM Software\NSIS ""

;--------------------------------
;Header Files

!include "MUI.nsh"
!include "Sections.nsh"

;--------------------------------
;Definitions

!define SHCNE_ASSOCCHANGED 0x8000000
!define SHCNF_IDLIST 0

;--------------------------------
;Configuration

;Names
Name "NSIS"
Caption "NSIS ${VER_DISPLAY} Setup"

;Interface Settings
!define MUI_ABORTWARNING

!define MUI_HEADERIMAGE
!define MUI_WELCOMEFINISHPAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Wizard\nsis.bmp"

!define MUI_COMPONENTSPAGE_SMALLDESC

;Pages
!define MUI_WELCOMEPAGE_TITLE "Welcome to the NSIS ${VER_DISPLAY} Setup Wizard"
!define MUI_WELCOMEPAGE_TEXT "This wizard will guide you through the installation of NSIS (Nullsoft Scriptable Install System) ${VER_DISPLAY}, the next generation of the Windows installer and uninstaller system that doesn't suck and isn't huge.\r\n\r\nNSIS 2 includes a new Modern User Interface, LZMA compression, support for multiple languages and an easy plug-in system.\r\n\r\n$_CLICK"

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "..\license.txt"
Page custom PageReinstall PageLeaveReinstall
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!define MUI_FINISHPAGE_LINK "Visit the NSIS site for the latest news, FAQs and support"
!define MUI_FINISHPAGE_LINK_LOCATION "http://nsis.sf.net/"

!define MUI_FINISHPAGE_RUN "$INSTDIR\NSIS.exe"
!define MUI_FINISHPAGE_NOREBOOTSUPPORT

!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages

!insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Reserve Files

  ;These files should be inserted before other files in the data block

  ReserveFile "makensis.ini"
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

;--------------------------------
;Installer Sections

Section "NSIS Core Files (required)" SecCore

  SetDetailsPrint textonly
  DetailPrint "Installing NSIS Core Files..."
  SetDetailsPrint listonly

  SectionIn 1 2 3 RO
  SetOutPath $INSTDIR
  RMDir /r $SMPROGRAMS\NSIS

  SetOverwrite on
  File ..\makensis.exe
  File ..\makensisw.exe
  File ..\license.txt
  File ..\NSIS.chm

  File ..\NSIS.exe
  IfFileExists $INSTDIR\nsisconf.nsi "" +2
  Rename $INSTDIR\nsisconf.nsi $INSTDIR\nsisconf.nsh
  SetOverwrite off
  File ..\nsisconf.nsh
  SetOverwrite on

  SetOutPath $INSTDIR\Include
  File ..\Include\WinMessages.nsh
  File ..\Include\Sections.nsh
  File ..\Include\Library.nsh
  File ..\Include\UpgradeDLL.nsh
  File ..\Include\LogicLib.nsh
  File ..\Include\StrFunc.nsh
  File ..\Include\StrFunc.txt
  File ..\Include\Colors.nsh

  SetOutPath $INSTDIR\Contrib\Makensisw
  File ..\Contrib\Makensisw\*.txt

  SetOutPath $INSTDIR\Menu
  File ..\Menu\*.html
  SetOutPath $INSTDIR\Menu\images
  File ..\Menu\images\*.gif

  Delete $INSTDIR\makensis.htm
  Delete $INSTDIR\Docs\*.html
  Delete $INSTDIR\Docs\style.css
  RMDir $INSTDIR\Docs

  SetOutPath $INSTDIR\Contrib\Library\LibraryLocal
  File ..\Contrib\Library\LibraryLocal\*.exe
  File ..\Contrib\Library\LibraryLocal\*.cpp
  File ..\Contrib\Library\LibraryLocal\*.dsw
  File ..\Contrib\Library\LibraryLocal\*.dsp

  SetOutPath $INSTDIR\Contrib\Library\RegTool
  File ..\Contrib\Library\RegTool\*.nsi
  File ..\Contrib\Library\RegTool\*.bin

  SetOutPath $INSTDIR\Contrib\Library\TypeLib
  File ..\Contrib\Library\TypeLib\*.cpp
  File ..\Contrib\Library\TypeLib\*.dsw
  File ..\Contrib\Library\TypeLib\*.dsp

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\TypeLib.dll

  ReadRegStr $R0 HKCR ".nsi" ""
  StrCmp $R0 "NSISFile" 0 +2
    DeleteRegKey HKCR "NSISFile"

  WriteRegStr HKCR ".nsi" "" "NSIS.Script"
  WriteRegStr HKCR "NSIS.Script" "" "NSIS Script File"
  WriteRegStr HKCR "NSIS.Script\DefaultIcon" "" "$INSTDIR\makensisw.exe,1"
  ReadRegStr $R0 HKCR "NSIS.Script\shell\open\command" ""
  StrCmp $R0 "" 0 no_nsiopen
    WriteRegStr HKCR "NSIS.Script\shell" "" "open"
    WriteRegStr HKCR "NSIS.Script\shell\open\command" "" 'notepad.exe "%1"'
  no_nsiopen:
  WriteRegStr HKCR "NSIS.Script\shell\compile" "" "Compile NSIS Script"
  WriteRegStr HKCR "NSIS.Script\shell\compile\command" "" '"$INSTDIR\makensisw.exe" "%1"'
  WriteRegStr HKCR "NSIS.Script\shell\compile-compressor" "" "Compile NSIS Script (Choose Compressor)"
  WriteRegStr HKCR "NSIS.Script\shell\compile-compressor\command" "" '"$INSTDIR\makensisw.exe" /ChooseCompressor "%1"'

  ReadRegStr $R0 HKCR ".nsh" ""
  StrCmp $R0 "NSHFile" 0 +2
    DeleteRegKey HKCR "NSHFile"

  WriteRegStr HKCR ".nsh" "" "NSIS.Header"
  WriteRegStr HKCR "NSIS.Header" "" "NSIS Header File"
  WriteRegStr HKCR "NSIS.Header\DefaultIcon" "" "$INSTDIR\makensisw.exe,1"
  ReadRegStr $R0 HKCR "NSIS.Header\shell\open\command" ""
  StrCmp $R0 "" 0 no_nshopen
    WriteRegStr HKCR "NSIS.Header\shell" "" "open"
    WriteRegStr HKCR "NSIS.Header\shell\open\command" "" 'notepad.exe "%1"'
  no_nshopen:

  System::Call 'Shell32::SHChangeNotify(i ${SHCNE_ASSOCCHANGED}, i ${SHCNF_IDLIST}, i 0, i 0)'

SectionEnd

Section "Script Examples" SecExample

  SetDetailsPrint textonly
  DetailPrint "Installing Script Examples..."
  SetDetailsPrint listonly

  SectionIn 1 2 3
  SetOutPath $INSTDIR\Examples
  File ..\Examples\cvsdata.nsi
  File ..\Examples\makensis.nsi
  File ..\Examples\makensis.ini
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
  File ..\Examples\VersionInfo.nsi
  File ..\Examples\UserVars.nsi
  File ..\Examples\LogicLib.nsi
  File ..\Examples\silent.nsi
  File ..\Examples\StrFunc.nsi
SectionEnd

!ifndef NO_STARTMENUSHORTCUTS
Section "Start Menu and Desktop Shortcuts" SecShortcuts

  SetDetailsPrint textonly
  DetailPrint "Installing Start Menu and Desktop Shortcuts..."
  SetDetailsPrint listonly

!else
Section "Desktop Shortcut" SecShortcuts

  SetDetailsPrint textonly
  DetailPrint "Installing Desktop Shortcut..."
  SetDetailsPrint listonly

!endif
  SectionIn 1 2 3
  SetOutPath $INSTDIR
!ifndef NO_STARTMENUSHORTCUTS
  CreateDirectory $SMPROGRAMS\NSIS

  CreateShortCut "$SMPROGRAMS\NSIS\NSIS Menu.lnk" "$INSTDIR\NSIS.exe" ""

  CreateShortCut "$SMPROGRAMS\NSIS\MakeNSISW (Compiler GUI).lnk" "$INSTDIR\makensisw.exe"

  IfFileExists "$INSTDIR\Bin\NSISUpdate.exe" "" +2
    CreateShortCut "$SMPROGRAMS\NSIS\NSIS Update.lnk" "$INSTDIR\Bin\NSISUpdate.exe"

  CreateShortCut "$SMPROGRAMS\NSIS\NSIS Documentation.lnk" "$INSTDIR\NSIS.chm"
  WriteINIStr "$SMPROGRAMS\NSIS\NSIS Site.url" "InternetShortcut" "URL" "http://nsis.sourceforge.net/"
  CreateShortCut "$SMPROGRAMS\NSIS\Uninstall NSIS.lnk" "$INSTDIR\uninst-nsis.exe"

!endif

  CreateShortCut "$DESKTOP\Nullsoft Install System.lnk" "$INSTDIR\NSIS.exe"

SectionEnd

SectionGroup "User Interfaces" SecInterfaces

Section "Modern User Interface" SecInterfacesModernUI

  SetDetailsPrint textonly
  DetailPrint "Installing User Interfaces | Modern User Interface..."
  SetDetailsPrint listonly

  SectionIn 1 2 3

  SetOutPath "$INSTDIR\Examples\Modern UI"
  File "..\Examples\Modern UI\Basic.nsi"
  File "..\Examples\Modern UI\HeaderBitmap.nsi"
  File "..\Examples\Modern UI\MultiLanguage.nsi"
  File "..\Examples\Modern UI\InstallOptions.nsi"
  File "..\Examples\Modern UI\ioA.ini"
  File "..\Examples\Modern UI\ioB.ini"
  File "..\Examples\Modern UI\ioC.ini"
  File "..\Examples\Modern UI\StartMenu.nsi"
  File "..\Examples\Modern UI\WelcomeFinish.nsi"

  SetOutPath "$INSTDIR\Contrib\Modern UI"
  File "..\Contrib\Modern UI\System.nsh"
  File "..\Contrib\Modern UI\Readme.html"
  File "..\Contrib\Modern UI\Changelog.txt"
  File "..\Contrib\Modern UI\License.txt"
  File "..\Contrib\Modern UI\ioSpecial.ini"

  SetOutPath "$INSTDIR\Contrib\Modern UI\images"
  File "..\Contrib\Modern UI\images\header.gif"
  File "..\Contrib\Modern UI\images\screen1.png"
  File "..\Contrib\Modern UI\images\screen2.png"
  File "..\Contrib\Modern UI\images\open.gif"
  File "..\Contrib\Modern UI\images\closed.gif"

  SetOutPath $INSTDIR\Contrib\UIs
  File "..\Contrib\UIs\modern.exe"
  File "..\Contrib\UIs\modern_headerbmp.exe"
  File "..\Contrib\UIs\modern_headerbmpr.exe"
  File "..\Contrib\UIs\modern_nodesc.exe"
  File "..\Contrib\UIs\modern_smalldesc.exe"

  SetOutPath $INSTDIR\Include
  File "..\Include\MUI.nsh"

SectionEnd

Section "Default User Interface" SecInterfacesDefaultUI

  SetDetailsPrint textonly
  DetailPrint "Installing User Interfaces | Default User Interface..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath "$INSTDIR\Contrib\UIs"
  File "..\Contrib\UIs\default.exe"

SectionEnd

Section "Tiny User Interface" SecInterfacesTinyUI

  SetDetailsPrint textonly
  DetailPrint "Installing User Interfaces | Tiny User Interface..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath "$INSTDIR\Contrib\UIs"
  File "..\Contrib\UIs\sdbarker_tiny.exe"

SectionEnd

SectionGroupEnd

Section "Graphics" SecGraphics

  SetDetailsPrint textonly
  DetailPrint "Installing Graphics..."
  SetDetailsPrint listonly

  SectionIn 1 2

  Delete $INSTDIR\Contrib\Icons\*.ico
  Delete $INSTDIR\Contrib\Icons\*.bmp
  RMDir $INSTDIR\Contrib\Icons
  SetOutPath $INSTDIR\Contrib\Graphics
  File /r "..\Contrib\Graphics\*.ico"
  File /r "..\Contrib\Graphics\*.bmp"
SectionEnd

Section "Language Files" SecLangFiles

  SetDetailsPrint textonly
  DetailPrint "Installing Language Files..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath "$INSTDIR\Contrib\Language files"
  File "..\Contrib\Language files\*.nlf"

  SetOutPath $INSTDIR\Bin
  File ..\Bin\MakeLangID.exe

  !insertmacro SectionFlagIsSet ${SecInterfacesModernUI} ${SF_SELECTED} mui nomui
  mui:
    SetOutPath "$INSTDIR\Contrib\Modern UI\Language files"
    File "..\Contrib\Modern UI\Language files\*.nsh"
  nomui:

SectionEnd

SectionGroup "Tools" SecTools

Section "NSIS Update" SecToolsUpdate

  SetDetailsPrint textonly
  DetailPrint "Installing Tools | NSIS Update..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Bin
  File ..\Bin\NSISUpdate.exe
  File ..\Bin\InstallCVSData.exe

  IfFileExists "$INSTDIR\CVS\Root" 0 +2
    Exec '"$INSTDIR\Bin\InstallCVSData.exe"'

SectionEnd

Section "Zip2Exe" SecToolsZ2E

  SetDetailsPrint textonly
  DetailPrint "Installing Tools | Zip2Exe..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Bin
  File ..\Bin\zip2exe.exe
  SetOutPath $INSTDIR\Contrib\zip2exe
  File ..\Contrib\zip2exe\Base.nsh
  File ..\Contrib\zip2exe\Modern.nsh
  File ..\Contrib\zip2exe\Classic.nsh

SectionEnd

SectionGroupEnd

SectionGroup "Plug-ins" SecPluginsPlugins

Section "Banner" SecPluginsBanner

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | Banner..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\Banner.dll
  SetOutPath $INSTDIR\Contrib\Banner
  File ..\Contrib\Banner\Readme.txt
  File ..\Contrib\Banner\Example.nsi
SectionEnd

Section "Language DLL" SecPluginsLangDLL

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | Language DLL..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\LangDLL.dll
SectionEnd

Section "nsExec" SecPluginsnsExec

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | nsExec..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\nsExec.dll
  SetOutPath $INSTDIR\Contrib\nsExec
  File ..\Contrib\nsExec\*.txt
  File ..\Contrib\nsExec\*.nsi
SectionEnd

Section "Splash" SecPluginsSplash

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | Splash..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\splash.dll
  SetOutPath $INSTDIR\Contrib\Splash
  File ..\Contrib\Splash\splash.txt
  File ..\Contrib\Splash\Example.nsi
SectionEnd

Section "AdvSplash" SecPluginsSplashT

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | AdvSplash..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\advsplash.dll
  SetOutPath $INSTDIR\Contrib\AdvSplash
  File ..\Contrib\AdvSplash\advsplash.txt
  File ..\Contrib\AdvSplash\Example.nsi
SectionEnd

Section "BgImage" SecPluginsBgImage

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | BgImage..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\BgImage.dll
  SetOutPath $INSTDIR\Contrib\BgImage
  File ..\Contrib\BgImage\BgImage.txt
  File ..\Contrib\BgImage\Example.nsi
SectionEnd

Section "InstallOptions" SecPluginsIO

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | InstallOptions..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\InstallOptions.dll
  SetOutPath $INSTDIR\Contrib\InstallOptions
  File ..\Contrib\InstallOptions\Readme.html
  File ..\Contrib\InstallOptions\Changelog.txt
  File ..\Contrib\InstallOptions\test.ini
  File ..\Contrib\InstallOptions\test.nsi
  File ..\Contrib\InstallOptions\testlink.ini
  File ..\Contrib\InstallOptions\testlink.nsi
  File ..\Contrib\InstallOptions\testnotify.ini
  File ..\Contrib\InstallOptions\testnotify.nsi
SectionEnd

Section "Math" SecPluginsMath

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | Math..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\Math.dll
  SetOutPath $INSTDIR\Contrib\Math
  File ..\Contrib\Math\Math.txt
  File ..\Contrib\Math\math.nsi
  File ..\Contrib\Math\mathtest.txt
  File ..\Contrib\Math\mathtest.nsi
  File ..\Contrib\Math\mathtest.ini

SectionEnd

Section "NSISdl" SecPluginsNSISDL

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | NSISdl..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\nsisdl.dll
  SetOutPath $INSTDIR\Contrib\NSISdl
  File ..\Contrib\NSISdl\ReadMe.txt
  File ..\Contrib\NSISdl\License.txt
SectionEnd

Section "System" SecPluginsSystem

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | System..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\System.dll
  SetOutPath $INSTDIR\Contrib\System
  File ..\Contrib\System\*.dll
  File ..\Contrib\System\*.nsh
  File ..\Contrib\System\*.nsi
  File ..\Contrib\System\*.txt
  File ..\Contrib\System\*.html
SectionEnd

Section "StartMenu" SecPluginsStartMenu

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | StartMenu..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\StartMenu.dll
  SetOutPath $INSTDIR\Contrib\StartMenu
  File ..\Contrib\StartMenu\Example.nsi
  File ..\Contrib\StartMenu\Readme.txt
SectionEnd

Section "UserInfo" SecPluginsUserInfo

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | UserInfo..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\UserInfo.dll
  SetOutPath $INSTDIR\Contrib\UserInfo
  File ..\Contrib\UserInfo\UserInfo.nsi
SectionEnd

Section "Dialer" SecPluginsDialer

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | Dialer..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\Dialer.dll
  SetOutPath $INSTDIR\Contrib\Dialer
  File ..\Contrib\Dialer\Dialer.txt
SectionEnd

Section "VPatch" SecPluginsVPatch

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | VPatch..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\VPatch.dll
  SetOutPath $INSTDIR\Contrib\VPatch
  File ..\Contrib\VPatch\GenPat.exe
  File ..\Contrib\VPatch\Readme.html
  File ..\Contrib\VPatch\example.nsi
  File ..\Contrib\VPatch\oldfile.txt
  File ..\Contrib\VPatch\newfile.txt
  File ..\Contrib\VPatch\patch.pat
SectionEnd

SectionGroupEnd

SectionGroup "Source code" SecSrc

Section "NSIS Source Code" SecSrcNSIS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | NSIS Source Code..."
  SetDetailsPrint listonly

  SectionIn 1

  # makensis
  SetOutPath $INSTDIR\Source
  File ..\Source\*.cpp
  File ..\Source\*.c
  File ..\Source\*.h
  File ..\Source\Makefile
  File ..\Source\makenssi.dsp
  File ..\Source\makenssi.dsw
  # boost
  SetOutPath $INSTDIR\Source\boost
  File ..\Source\boost\*.hpp
  SetOutPath $INSTDIR\Source\boost\detail
  File ..\Source\boost\detail\*.hpp
  # zlib
  SetOutPath $INSTDIR\Source\zlib
  File ..\Source\zlib\*.*
  # bzip2
  SetOutPath $INSTDIR\Source\bzip2
  File ..\Source\bzip2\*.*
  # lzma
  SetOutPath $INSTDIR\Source\7zip
  File ..\Source\7zip\*.*
  SetOutPath $INSTDIR\Source\7zip\Common
  File ..\Source\7zip\Common\*.*
  SetOutPath $INSTDIR\Source\7zip\7zip
  File ..\Source\7zip\7zip\*.*
  SetOutPath $INSTDIR\Source\7zip\7zip\Compress\LZ
  File ..\Source\7zip\7zip\Compress\LZ\*.*
  SetOutPath $INSTDIR\Source\7zip\7zip\Compress\LZ\BinTree
  File ..\Source\7zip\7zip\Compress\LZ\BinTree\*.*
  SetOutPath $INSTDIR\Source\7zip\7zip\Compress\LZMA
  File ..\Source\7zip\7zip\Compress\LZMA\*.*
  SetOutPath $INSTDIR\Source\7zip\7zip\Compress\RangeCoder
  File ..\Source\7zip\7zip\Compress\RangeCoder\*.*
  SetOutPath $INSTDIR\Source\7zip\7zip\Common
  File ..\Source\7zip\7zip\Common\*.*
  # exehead
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
  # tests
  SetOutPath $INSTDIR\Source\Tests
  File ..\Source\Tests\*.cpp
  File ..\Source\Tests\Tests.dsp
SectionEnd

Section "ExDLL Source (required)" SecSrcEx

  ;required for other plugins sources
  ;text changes in .onSelChange

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | ExDLL Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\ExDLL
  File ..\Contrib\ExDLL\exdll.c
  File ..\Contrib\ExDLL\exdll.h
  File ..\Contrib\ExDLL\exdll.dsp
  File ..\Contrib\ExDLL\exdll.dsw
  File ..\Contrib\ExDLL\exdll.dpr
  File ..\Contrib\ExDLL\exdll_with_unit.dpr
  File ..\Contrib\ExDLL\nsis.pas
  File ..\Contrib\ExDLL\extdll.inc

SectionEnd

Section "Zip2Exe Source" SecToolsZ2ES

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Zip2exe Source..."
  SetDetailsPrint listonly

  SectionIn 1

  RMDir /r $INSTDIR\Source\Zip2Exe
  SetOutPath $INSTDIR\Contrib\zip2exe
  File ..\Contrib\zip2exe\*.cpp
  File ..\Contrib\zip2exe\*.ico
  File ..\Contrib\zip2exe\*.h
  File ..\Contrib\zip2exe\*.rc
  File ..\Contrib\zip2exe\*.dsw
  File ..\Contrib\zip2exe\*.dsp
  File ..\Contrib\zip2exe\*.xml
  SetOutPath $INSTDIR\Contrib\zip2exe\zlib
  File ..\Contrib\zip2exe\zlib\*.*
SectionEnd

SectionGroup "Tools" SecToolsS

Section "MakeNSISW Source" SecSrcMNW

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | MakeNSISW Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\Makensisw
  File ..\Contrib\Makensisw\*.cpp
  File ..\Contrib\Makensisw\*.xml
  File ..\Contrib\Makensisw\*.h
  File ..\Contrib\Makensisw\*.dsw
  File ..\Contrib\Makensisw\*.dsp
  File ..\Contrib\Makensisw\*.rc
  File ..\Contrib\Makensisw\*.bmp
  File ..\Contrib\Makensisw\*.ico
  File ..\Contrib\Makensisw\*.psp
  #File ..\Contrib\Makensisw\Makefile
SectionEnd

Section "UI Holder Source" SecSrcUIHolder

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | UI Holder..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath "$INSTDIR\Contrib\UIs\UI Holder"
  File "..\Contrib\UIs\UI Holder\*.h"
  File "..\Contrib\UIs\UI Holder\*.cpp"
  File "..\Contrib\UIs\UI Holder\*.rc"
  File "..\Contrib\UIs\UI Holder\*.dsw"
  File "..\Contrib\UIs\UI Holder\*.dsp"
SectionEnd

SectionGroupEnd

SectionGroup "Plug-ins" SecPluginsPluginsS

Section "Banner Source" SecPluginsBannerS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | Banner Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\Banner
  File ..\Contrib\Banner\Banner.dsw
  File ..\Contrib\Banner\Banner.dsp
  File ..\Contrib\Banner\Banner.c
SectionEnd

Section "Language DLL Source" SecPluginsLangDLLS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | Language DLL Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\LangDLL
  File ..\Contrib\LangDLL\LangDLL.c
  File ..\Contrib\LangDLL\resource.h
  File ..\Contrib\LangDLL\resource.rc
  File ..\Contrib\LangDLL\LangDLL.dsw
  File ..\Contrib\LangDLL\LangDLL.dsp
SectionEnd

Section "nsExec Source" SecPluginsnsExecS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | nsExec Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\nsExec
  File ..\Contrib\nsExec\*.c
  File ..\Contrib\nsExec\*.dsw
  File ..\Contrib\nsExec\*.dsp
SectionEnd

Section "Splash Source" SecPluginsSplashS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | Splash Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\Splash
  File ..\Contrib\Splash\splash.c
  File ..\Contrib\Splash\splash.dsp
  File ..\Contrib\Splash\splash.dsw
SectionEnd

Section "AdvSplash Source" SecPluginsSplashTS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | AdvSplash Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\AdvSplash
  File ..\Contrib\AdvSplash\*.c
  File ..\Contrib\AdvSplash\*.dsw
  File ..\Contrib\AdvSplash\*.dsp
SectionEnd

Section "BgImage Source" SecPluginsBgImageS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | BgImage Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\BgImage
  File ..\Contrib\BgImage\BgImage.cpp
  File ..\Contrib\BgImage\BgImage.dsw
  File ..\Contrib\BgImage\BgImage.dsp
SectionEnd

Section "InstallOptions Source" SecPluginsIOS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | InstallOptions Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\InstallOptions
  File ..\Contrib\InstallOptions\io.dsp
  File ..\Contrib\InstallOptions\io.dsw
  File ..\Contrib\InstallOptions\InstallerOptions.cpp
  File ..\Contrib\InstallOptions\*.rc
  File ..\Contrib\InstallOptions\*.h
SectionEnd

Section "Math Source" SecPluginsMathS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | Math Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\Math\Source
  File ..\Contrib\Math\Source\*.c
  File ..\Contrib\Math\Source\*.h
  File ..\Contrib\Math\Source\*.sln
  File ..\Contrib\Math\Source\*.lib
  File ..\Contrib\Math\Source\*.vcproj
SectionEnd

Section "NSISdl Source" SecPluginsNSISDLS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | NSISdl Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\NSISdl
  File ..\Contrib\NSISdl\nsisdl.dsw
  File ..\Contrib\NSISdl\nsisdl.dsp
  File ..\Contrib\NSISdl\*.cpp
  File ..\Contrib\NSISdl\*.h
SectionEnd

Section "System Source" SecPluginsSystemS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | System Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\System\Source
  File ..\Contrib\System\Source\*.c
  File ..\Contrib\System\Source\*.h
  File ..\Contrib\System\Source\*.sln
  File ..\Contrib\System\Source\*.obj
  File ..\Contrib\System\Source\*.vcproj
SectionEnd

Section "StartMenu Source" SecPluginsStartMenuS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | StartMenu Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\StartMenu
  File ..\Contrib\StartMenu\StartMenu.c
  File ..\Contrib\StartMenu\StartMenu.dsp
  File ..\Contrib\StartMenu\StartMenu.dsw
  File ..\Contrib\StartMenu\StartMenu.rc
  File ..\Contrib\StartMenu\resource.h
SectionEnd

Section "UserInfo Source" SecPluginsUserInfoS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | UserInfo Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\UserInfo
  File ..\Contrib\UserInfo\UserInfo.c
  File ..\Contrib\UserInfo\UserInfo.dsp
  File ..\Contrib\UserInfo\UserInfo.dsw
SectionEnd

Section "Dialer Source" SecPluginsDialerS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | Dialer Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\Dialer
  File ..\Contrib\Dialer\dialer.c
  File ..\Contrib\Dialer\dialer.dsp
  File ..\Contrib\Dialer\dialer.dsw
SectionEnd

Section "VPatch Source" SecPluginsVPatchS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Plug-ins | VPatch Source..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Contrib\VPatch\Source
  File ..\Contrib\VPatch\Source\*.bpg

  SetOutPath $INSTDIR\Contrib\VPatch\Source\GenPat
  File ..\Contrib\VPatch\Source\GenPat\*.pas
  File ..\Contrib\VPatch\Source\GenPat\*.dpr

  SetOutPath $INSTDIR\Contrib\VPatch\Source\Plugin
  File ..\Contrib\VPatch\Source\Plugin\*.c
  File ..\Contrib\VPatch\Source\Plugin\*.dsw
  File ..\Contrib\VPatch\Source\Plugin\*.dsp

  SetOutPath $INSTDIR\Contrib\VPatch\Source\GUI
  File ..\Contrib\VPatch\Source\GUI\*.pas
  File ..\Contrib\VPatch\Source\GUI\*.dpr
  File ..\Contrib\VPatch\Source\GUI\*.dfm
  File ..\Contrib\VPatch\Source\GUI\*.dof
  File ..\Contrib\VPatch\Source\GUI\*.res

SectionEnd

SectionGroupEnd ; plugins

SectionGroupEnd

Section -post

  ; When Modern UI is installed:
  ; * Always install the English language file
  ; * Always install default icons / bitmaps

  !insertmacro SectionFlagIsSet ${SecInterfacesModernUI} ${SF_SELECTED} mui nomui

    mui:

    SetDetailsPrint textonly
    DetailPrint "Configurating Modern UI..."
    SetDetailsPrint listonly

    !insertmacro SectionFlagIsSet ${SecLangFiles} ${SF_SELECTED} langfiles nolangfiles

      nolangfiles:

      SetOutPath "$INSTDIR\Contrib\Language files"
      File "..\Contrib\Language files\English.nlf"
      SetOutPath "$INSTDIR\Contrib\Modern UI\Language files"
      File "..\Contrib\Modern UI\Language files\Default.nsh"
      File "..\Contrib\Modern UI\Language files\English.nsh"

    langfiles:

    !insertmacro SectionFlagIsSet ${SecGraphics} ${SF_SELECTED} graphics nographics

      nographics:

      SetOutPath $INSTDIR\Contrib\Graphics
      SetOutPath $INSTDIR\Contrib\Graphics\Checks
      File "..\Contrib\Graphics\Checks\modern.bmp"
      SetOutPath $INSTDIR\Contrib\Graphics\Icons
      File "..\Contrib\Graphics\Icons\modern-install.ico"
      File "..\Contrib\Graphics\Icons\modern-uninstall.ico"
      SetOutPath $INSTDIR\Contrib\Graphics\Header
      File "..\Contrib\Graphics\Header\nsis.bmp"
      SetOutPath $INSTDIR\Contrib\Graphics\Wizard
      File "..\Contrib\Graphics\Wizard\win.bmp"

    graphics:

  nomui:

  SetDetailsPrint textonly
  DetailPrint "Creating Registry Keys..."
  SetDetailsPrint listonly

  SetOutPath $INSTDIR

  WriteRegStr HKLM "Software\NSIS" "" $INSTDIR
  WriteRegDword HKLM "Software\NSIS" "VersionMajor" "${VER_MAJOR}"
  WriteRegDword HKLM "Software\NSIS" "VersionMinor" "${VER_MINOR}"
  WriteRegDword HKLM "Software\NSIS" "VersionRevision" "${VER_REVISION}"
  WriteRegDword HKLM "Software\NSIS" "VersionBuild" "${VER_BUILD}"

  WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "UninstallString" '"$INSTDIR\uninst-nsis.exe"'
  WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "InstallLocation" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "DisplayName" "Nullsoft Install System"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "DisplayIcon" "$INSTDIR\NSIS.exe,0"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "DisplayVersion" "${VER_DISPLAY}"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "VersionMajor" "${VER_MAJOR}"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "VersionMinor" "${VER_MINOR}.${VER_REVISION}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "URLInfoAbout" "http://nsis.sourceforge.net/"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "NoModify" "1"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "NoRepair" "1"

!ifndef NO_STARTMENUSHORTCUTS
  IfFileExists $SMPROGRAMS\NSIS "" no_startshortcuts

  SetDetailsPrint textonly
  DetailPrint "Creating Shortcuts..."
  SetDetailsPrint listonly

  IfFileExists $INSTDIR\Examples 0 +2
    CreateShortCut "$SMPROGRAMS\NSIS\NSIS Examples Directory.lnk" "$INSTDIR\Examples"

  IfFileExists "$INSTDIR\Source" 0 +2
    CreateShortCut "$SMPROGRAMS\NSIS\MakeNSIS project workspace.lnk" "$INSTDIR\source\makenssi.dsw"

  CreateDirectory $SMPROGRAMS\NSIS\Contrib\Source

  ; MakeNSISW
  CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\MakeNSISW readme.lnk" "$INSTDIR\contrib\MakeNsisw\readme.txt"

  Push MakeNSISW
  Call AddWorkspaceToStartMenu

  ; ExDLL
  Push ExDLL
  Call AddWorkspaceToStartMenu

  ; InstallOptions
  Push InstallOptions
  Call AddReadmeToStartMenu

  Push "InstallOptions\io.dsw"
  Push "Source\InstallOptions project workspace"
  Call AddContribToStartMenu

  ; ZIP2EXE
  IfFileExists "$INSTDIR\Bin\zip2exe.exe" 0 +2
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\Zip2Exe (create SFX).lnk" "$INSTDIR\Bin\zip2exe.exe"

  Push ZIP2EXE
  Call AddWorkspaceToStartMenu

  ; Modern UI
  Push "Modern UI"
  Call AddReadmeToStartMenu

  ; Splash
  Push Splash
  Call AddReadmeToStartMenu

  Push Splash
  Call AddWorkspaceToStartMenu

  ; Advanced splash
  Push AdvSplash
  Call AddReadmeToStartMenu

  Push AdvSplash
  Call AddWorkspaceToStartMenu

  ; Math
  Push Math
  Call AddReadmeToStartMenu

  Push "Math\Source\Math.sln"
  Push "Source\Math project workspace"
  Call AddContribToStartMenu

  ; NSISdl
  Push NSISdl
  Call AddReadmeToStartMenu

  Push NSISdl
  Call AddWorkspaceToStartMenu

  ; UserInfo
  Push UserInfo
  Call AddWorkspaceToStartMenu

  ; nsExec
  Push nsExec
  Call AddReadmeToStartMenu

  Push nsExec
  Call AddWorkspaceToStartMenu

  ; LangDLL
  Push LangDLL
  Call AddWorkspaceToStartMenu

  ; StartMenu
  Push StartMenu
  Call AddReadmeToStartMenu

  Push StartMenu
  Call AddWorkspaceToStartMenu

  ; BgImage
  Push BgImage
  Call AddReadmeToStartMenu

  Push BgImage
  Call AddWorkspaceToStartMenu

  ; Banner
  Push Banner
  Call AddReadmeToStartMenu

  Push Banner
  Call AddWorkspaceToStartMenu

  ; System
  Push System
  Call AddReadmeToStartMenu

  Push System\Source\System.sln
  Push "Source\System project workspace"
  Call AddContribToStartMenu

  ; VPatch
  Push VPatch
  Call AddReadmeToStartMenu

  no_startshortcuts:
!endif

  ; will only be removed if empty
  SetDetailsPrint none
  RMDir $INSTDIR\Contrib\Source
  SetDetailsPrint lastused

  WriteUninstaller $INSTDIR\uninst-nsis.exe

  SetDetailsPrint both

SectionEnd

;--------------------------------
;Descriptions

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCore} "The core files required to use NSIS (compiler etc.)"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecExample} "Example installation scripts that show you how to use NSIS"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecShortcuts} "Adds icons to your start menu and your desktop for easy access"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecInterfaces} "User interface designs that can be used to change the installer look and feel"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecInterfacesModernUI} "A modern user interface like the wizards of recent Windows versions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecInterfacesDefaultUI} "The default NSIS user interface which you can customize to make your own UI"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecInterfacesTinyUI} "A tiny version of the default user interface"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecTools} "Tools that help you with NSIS development"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecToolsS} "Source code to tools that help you with NSIS development"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecToolsUpdate} "A tool that lets you check for new NSIS releases and download the latest development files"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecToolsZ2E} "A utility that converts a ZIP file to a NSIS installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecToolsZ2ES} "Source code to a utility that converts a ZIP file to a NSIS installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecGraphics} "Icons, checkbox images and other graphics"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecLangFiles} "Language files used to support multiple languages in an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsPlugins} "Useful plugins that extend NSIS's functionality"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsPluginsS} "Source code for plugins"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsBanner} "Plugin that lets you show a banner before installation starts"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsBannerS} "Source code to plugin that lets you show a banner before installation starts"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsLangDLL} "Plugin that lets you add a language select dialog to your installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsLangDLLS} "Source code to plugin that lets you add a language select dialog to your installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsnsExec} "Plugin that executes console programs and prints its output in the NSIS log window or hides it"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsnsExecS} "Source code to plugin that executes console programs and prints its output in the NSIS log window or hides it"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsSplash} "Splash screen add-on that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsSplashS} "Source code to splash screen add-on that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsSplashT} "Splash screen add-on with transparency support that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsSplashTS} "Source code to splash screen add-on with transparency support that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsSystem} "Plugin that lets you call Win32 API or external DLLs"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsSystemS} "Source code to plugin that lets you call Win32 API or external DLLs"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsMath} "Plugin that lets you evaluate complicated mathematical expressions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsMathS} "Source code to plugin that lets you evaluate complicated mathematical expressions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsDialer} "Plugin that provides internet connection functions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsDialerS} "Source code to plugin that provides internet connection functions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsIO} "Plugin that lets you add custom pages to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsIOS} "Source code to plugin that lets you add custom pages to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsStartMenu} "Plugin that lets the user select the start menu folder"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsStartMenuS} "Source code to plugin that lets the user select the start menu folder"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsBgImage} "Plugin that lets you show a persistent background image plugin and play sounds"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsBgImageS} "Source code to plugin that lets you show a persistent background image plugin and play sounds"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsUserInfo} "Plugin that that gives you the user name and the user account type"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsUserInfoS} "Source code to plugin that that gives you the user name and the user account type"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsNSISDL} "Plugin that lets you create a web based installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsNSISDLS} "Source code to plugin that lets you create a web based installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsVPatch} "Plugin that lets you create patches to upgrade older files"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsVPatchS} "Source code to plugin that lets you create patches to upgrade older files"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrc} "Source code to NSIS and all related files"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrcNSIS} "Source code to NSIS"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrcEx} "Example DLL plugin source in C and plugin function header"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrcUIHolder} "Source code to the UI Holder where you can put your recources in to preview your user interface"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrcMNW} "Source code to MakeNSISW (compiler interface)"
!insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Installer Functions

Function .onInit

  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "makensis.ini"

FunctionEnd

Function PageReinstall

  ReadRegStr $R0 HKLM "Software\NSIS" ""

  StrCmp $R0 "" 0 +2
    Abort

  ;Detect version
    ReadRegDWORD $R0 HKLM "Software\NSIS" "VersionMajor"
    IntCmp $R0 ${VER_MAJOR} minor_check new_version older_version
  minor_check:
    ReadRegDWORD $R0 HKLM "Software\NSIS" "VersionMinor"
    IntCmp $R0 ${VER_MINOR} revision_check new_version older_version
  revision_check:
    ReadRegDWORD $R0 HKLM "Software\NSIS" "VersionRevision"
    IntCmp $R0 ${VER_REVISION} build_check new_version older_version
  build_check:
    ReadRegDWORD $R0 HKLM "Software\NSIS" "VersionBuild"
    IntCmp $R0 ${VER_BUILD} same_version new_version older_version

  new_version:

   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 1" "Text" "An older version of NSIS is installed on your system. It's recommended that you uninstall the current version before installing. Select the operation you want to perform and click Next to continue."
   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 2" "Text" "Uninstall before installing"
   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 3" "Text" "Do not uninstall"
   !insertmacro MUI_HEADER_TEXT "Already Installed" "Choose how you want to install NSIS."
   StrCpy $R0 "1"
   Goto reinst_start

  older_version:

   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 1" "Text" "A newer version of NSIS is already installed! It is not recommended that you install an older version. If you really want to install this older version, it's better to uninstall the current version first. Select the operation you want to perform and click Next to continue."
   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 2" "Text" "Uninstall before installing"
   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 3" "Text" "Do not uninstall"
   !insertmacro MUI_HEADER_TEXT "Already Installed" "Choose how you want to install NSIS."
   StrCpy $R0 "1"
   Goto reinst_start

  same_version:

   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 1" "Text" "NSIS ${VER_DISPLAY} is already installed. Select the operation you want to perform and click Next to continue."
   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 2" "Text" "Add/Reinstall components"
   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 3" "Text" "Uninstall NSIS"
   !insertmacro MUI_HEADER_TEXT "Already Installed" "Choose the maintenance option to perform."
   StrCpy $R0 "2"

  reinst_start:

  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "makensis.ini"

FunctionEnd

Function PageLeaveReinstall

  !insertmacro MUI_INSTALLOPTIONS_READ $R1 "makensis.ini" "Field 2" "State"

  StrCmp $R0 "1" 0 +2
    StrCmp $R1 "1" reinst_uninstall reinst_done

  StrCmp $R0 "2" 0 +3
    StrCmp $R1 "1" reinst_done reinst_uninstall

  reinst_uninstall:
  ReadRegStr $R1 HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "UninstallString"

  ;Run uninstaller
  HideWindow

    ClearErrors
    ExecWait '$R1 _?=$INSTDIR'

    IfErrors no_remove_uninstaller
    IfFileExists "$INSTDIR\makensis.exe" no_remove_uninstaller

      Delete $R1
      RMDir $INSTDIR

    no_remove_uninstaller:

  StrCmp $R0 "2" 0 +2
    Quit

  BringToFront

  reinst_done:

FunctionEnd

!macro secSelected SEC
  SectionGetFlags ${SEC} $R7
  IntOp $R7 $R7 & ${SF_SELECTED}
  IntCmp $R7 ${SF_SELECTED} 0 +2 +2
    IntOp $R0 $R0 + 1
!macroend

Function .onSelChange
  ;Plugins are linked to ExDLL
  StrCpy $R0 0
  !insertmacro secSelected ${SecPluginsSplashTS}
  !insertmacro secSelected ${SecPluginsBannerS}
  !insertmacro secSelected ${SecPluginsBgImageS}
  !insertmacro secSelected ${SecPluginsIOS}
  !insertmacro secSelected ${SecPluginsLangDLLS}
  !insertmacro secSelected ${SecPluginsnsExecS}
  !insertmacro secSelected ${SecPluginsNSISdlS}
  !insertmacro secSelected ${SecPluginsSplashS}
  !insertmacro secSelected ${SecPluginsStartMenuS}
  !insertmacro secSelected ${SecPluginsUserInfoS}
  !insertmacro secSelected ${SecPluginsDialerS}
  SectionGetFlags ${SecSrcEx} $R7
  StrCmp $R0 0 notRequired
    IntOp $R7 $R7 | ${SF_SELECTED}
    SectionSetFlags ${SecSrcEx} $R7
    SectionSetText ${SecSrcEx} "ExDLL Source (required)"
    Goto done
  notRequired:
    SectionSetText ${SecSrcEx} "ExDLL Source"
  done:
FunctionEnd

!ifndef NO_STARTMENUSHORTCUTS
Function AddContribToStartMenu
  Pop $0 ; link
  Pop $1 ; file
  IfFileExists $INSTDIR\Contrib\$1 0 +2
    CreateShortCut $SMPROGRAMS\NSIS\Contrib\$0.lnk $INSTDIR\Contrib\$1
FunctionEnd

Function AddWorkspaceToStartMenu
  Pop $0
  IfFileExists $INSTDIR\Contrib\$0\$0.dsw 0 done
    Push $0\$0.dsw
    Push "Source\$0 project workspace"
    Call AddContribToStartMenu
  done:
FunctionEnd

Function AddReadmeToStartMenu
  Pop $0
  IfFileExists $INSTDIR\Contrib\$0\$0.txt 0 +3
    Push $0\$0.txt
    Goto create
  IfFileExists $INSTDIR\Contrib\$0\$0.html 0 +3
    Push $0\$0.html
    Goto create
  IfFileExists $INSTDIR\Contrib\$0\Readme.txt 0 +3
    Push $0\Readme.txt
    Goto create
  IfFileExists $INSTDIR\Contrib\$0\Readme.html 0 done
    Push $0\Readme.html
  create:
    Push "$0 Readme"
    Call AddContribToStartMenu
  done:
FunctionEnd
!endif

;--------------------------------
;Uninstaller Section

Section Uninstall

  SetDetailsPrint textonly
  DetailPrint "Uninstalling NSI Development Shell Extensions..."
  SetDetailsPrint listonly

  IfFileExists $INSTDIR\makensis.exe nsis_installed
    MessageBox MB_YESNO "It does not appear that NSIS is installed in the directory '$INSTDIR'.$\r$\nContinue anyway (not recommended)?" IDYES nsis_installed
    Abort "Uninstall aborted by user"
  nsis_installed:

  SetDetailsPrint textonly
  DetailPrint "Deleting Registry Keys..."
  SetDetailsPrint listonly

  ReadRegStr $R0 HKCR ".nsi" ""
  StrCmp $R0 "NSIS.Script" 0 +2
    DeleteRegKey HKCR ".nsi"

  ReadRegStr $R0 HKCR ".nsh" ""
  StrCmp $R0 "NSIS.Header" 0 +2
    DeleteRegKey HKCR ".nsh"

  DeleteRegKey HKCR "NSIS.Script"
  DeleteRegKey HKCR "NSIS.Header"

  System::Call 'Shell32::SHChangeNotify(i ${SHCNE_ASSOCCHANGED}, i ${SHCNF_IDLIST}, i 0, i 0)'

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS"
  DeleteRegKey HKLM "Software\NSIS"

  SetDetailsPrint textonly
  DetailPrint "Deleting Files..."
  SetDetailsPrint listonly

  RMDir /r $SMPROGRAMS\NSIS
  Delete "$DESKTOP\Nullsoft Install System.lnk"
  Delete $INSTDIR\makensis.exe
  Delete $INSTDIR\makensisw.exe
  Delete $INSTDIR\NSIS.exe
  Delete $INSTDIR\license.txt
  Delete $INSTDIR\uninst-nsis.exe
  Delete $INSTDIR\nsisconf.nsi
  Delete $INSTDIR\nsisconf.nsh
  Delete $INSTDIR\NSIS.chm
  RMDIR /r $INSTDIR\CVS
  RMDir /r $INSTDIR\Contrib
  RMDir /r $INSTDIR\Menu
  RMDir /r $INSTDIR\Source
  RMDir /r $INSTDIR\Bin
  RMDir /r $INSTDIR\Plugins
  RMDir /r $INSTDIR\Examples
  RMDir /r $INSTDIR\Include
  RMDir $INSTDIR

  SetDetailsPrint both

SectionEnd
