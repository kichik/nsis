;NSIS Setup Script

!define VER_MAJOR 2
!define VER_MINOR 0
!define VER_REVISION 0
!define VER_BUILD 12

!define VER_FILE "20b4"
!define VER_DISPLAY "2.0 beta 4"

;--------------------------------
;Compile CVS Data Setup

!system '"${NSISDIR}\Bin\InstallCVSData.exe" nooverwrite'
!echo "Compiling CVS Data Setup..."
!system '"${NSISDIR}\makensis.exe" /V1 cvsdata.nsi' = 0

;--------------------------------
;Configuration

OutFile ..\nsis${VER_FILE}.exe
SetCompressor bzip2

InstType "Full (w/ Source and Contrib)"
InstType "Normal (w/ Contrib, w/o Source)"
InstType "Lite (w/o Source or Contrib)"

InstallDir $PROGRAMFILES\NSIS
InstallDirRegKey HKLM SOFTWARE\NSIS ""

;--------------------------------

;Include Modern UI
!include "MUI.nsh"

;--------------------------------
;Configuration

;Names
Name "NSIS"
Caption "NSIS ${VER_DISPLAY} Setup"

;Interface Settings
!define MUI_ABORTWARNING

!define MUI_HEADERIMAGE
!define MUI_SPECIALBITMAP "${NSISDIR}\Contrib\Graphics\Wizard\nullsoft.bmp"

!define MUI_COMPONENTSPAGE_SMALLDESC

;Pages
!define MUI_WELCOMEPAGE_TEXT "This wizard will guide you through the installation of NSIS (Nullsoft Scriptable Install System), the Windows installer/uninstaller system that doesn't suck and isn't huge.\r\n\r\n\r\n$_CLICK"

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "..\license.txt"
Page custom PageReinstall PageLeaveReinstall
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!define MUI_FINISHPAGE_LINK "Visit the NSIS website for the latest news, a FAQ and support"
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
  ;Keep these lines before any File command
  ;Only for BZIP2 (solid) compression
  
  ReserveFile "makensis.ini"
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

;--------------------------------
;Installer Sections

!define SF_SELECTED 1

Section "NSIS Development System (required)" SecCore

  SetDetailsPrint textonly
  DetailPrint "Installing NSIS Development System..."
  SetDetailsPrint listonly

  SectionIn 1 2 3 RO
  SetOutPath $INSTDIR
  RMDir /r $SMPROGRAMS\NSIS

  SetOverwrite on
  Delete $INSTDIR\makensis-bz2.exe
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
  File ..\Include\UpgradeDLL.nsh

  SetOutPath $INSTDIR\Contrib\Makensisw
  File ..\contrib\makensisw\*.txt
  
  SetOutPath $INSTDIR\Menu
  File ..\Menu\*.html
  SetOutPath $INSTDIR\Menu\images
  File ..\Menu\images\*.gif
  
  Delete $INSTDIR\makensis.htm
  Delete $INSTDIR\Docs\*.html
  Delete $INSTDIR\Docs\style.css
  RMDir $INSTDIR\Docs
  
SectionEnd

Section "Script Examples" SecExample

  SetDetailsPrint textonly
  DetailPrint "Installing Script Examples..."
  SetDetailsPrint listonly

  SectionIn 1 2 3
  SetOutPath $INSTDIR\Examples
  Delete $INSTDIR\functions.htm
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
SectionEnd

Section "NSIS Update" SecUpdate

  SetDetailsPrint textonly
  DetailPrint "Installing NSIS Update..."
  SetDetailsPrint listonly

  SectionIn 1 2 3
  SetOutPath $INSTDIR\Bin
  File ..\Bin\NSISUpdate.exe
  File ..\Bin\InstallCVSData.exe
SectionEnd

Section "NSI Development Shell Extensions" SecExtention

  SetDetailsPrint textonly
  DetailPrint "Installing NSI Development Shell Extensions..."
  SetDetailsPrint listonly

  SectionIn 1 2 3
  ; back up old value of .nsi
  ReadRegStr $1 HKCR ".nsi" ""
  StrCmp $1 "" Label1
    StrCmp $1 "NSISFile" Label1
    WriteRegStr HKCR ".nsi" "backup_val" $1
  Label1:
  WriteRegStr HKCR ".nsh" "" "NSHFile"
  ReadRegStr $0 HKCR "NSHFile" ""
  StrCmp $0 "" 0 skipNSHAssoc
	WriteRegStr HKCR "NSHFile" "" "NSIS Header File"
	WriteRegStr HKCR "NSHFile\shell" "" "open"
	WriteRegStr HKCR "NSHFile\DefaultIcon" "" $INSTDIR\makensisw.exe,1
  skipNSHAssoc:
  WriteRegStr HKCR "NSHFile\shell\open\command" "" 'notepad.exe "%1"'
  WriteRegStr HKCR ".nsi" "" "NSISFile"
  ReadRegStr $0 HKCR "NSISFile" ""
  StrCmp $0 "" 0 skipNSIAssoc
	WriteRegStr HKCR "NSISFile" "" "NSIS Script File"
	WriteRegStr HKCR "NSISFile\shell" "" "open"
	WriteRegStr HKCR "NSISFile\DefaultIcon" "" $INSTDIR\makensisw.exe,1
  skipNSIAssoc:
  WriteRegStr HKCR "NSISFile\shell\open\command" "" 'notepad.exe "%1"'
  WriteRegStr HKCR "NSISFile\shell\compile" "" "Compile NSI"
  WriteRegStr HKCR "NSISFile\shell\compile\command" "" '"$INSTDIR\makensisw.exe" "%1"'
  WriteRegStr HKCR "NSISFile\shell\compile-bz2" "" "Compile NSI (with bz2)"
  WriteRegStr HKCR "NSISFile\shell\compile-bz2\command" "" '"$INSTDIR\makensisw.exe" /X"SetCompressor bzip2" "%1"'
SectionEnd

!ifndef NO_STARTMENUSHORTCUTS
Section "Start Menu + Desktop Shortcuts" SecIcons

  SetDetailsPrint textonly
  DetailPrint "Installing Start Menu + Desktop Shortcuts..."
  SetDetailsPrint listonly

!else
Section "Desktop Shortcut" SecIcons

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

SubSection "Contrib" SecContrib

SubSection "Extra User Interfaces" SecContribUIs
  Section "Modern User Interface" SecContribModernUI

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Extra User Interfaces | Modern User Interface..."
  SetDetailsPrint listonly

    SectionIn 1 2
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
    File "..\Contrib\Modern UI\Readme.gif"
    File "..\Contrib\Modern UI\Readme.html"
    File "..\Contrib\Modern UI\Changelog.txt"
    File "..\Contrib\Modern UI\Screenshot.png"
    File "..\Contrib\Modern UI\Screenshot2.png"
    File "..\Contrib\Modern UI\License.txt"
    File "..\Contrib\Modern UI\ioSpecial.ini"
    File "..\Contrib\Modern UI\ioSpecial3.ini"

    SetOutPath $INSTDIR\Contrib\UIs
    File "..\Contrib\UIs\modern.exe"
    File "..\Contrib\UIs\modern_headerbmp.exe"
    File "..\Contrib\UIs\modern_headerbmpr.exe"
    File "..\Contrib\UIs\modern_nodesc.exe"
    File "..\Contrib\UIs\modern_smalldesc.exe"

    Delete "$INSTDIR\Contrib\Modern UI\Readme.jpg"
    Delete "$INSTDIR\Contrib\UIs\modern2.exe"
    Delete "$INSTDIR\Contrib\UIs\modern3.exe"

    SetOutPath $INSTDIR\Include
    File "..\Include\MUI.nsh"
	
  SectionEnd

  Section "Default User Interface" SecContribDefaultUI

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Extra User Interfaces | Default User Interface..."
  SetDetailsPrint listonly

    SectionIn 1 2
    SetOutPath "$INSTDIR\Contrib\UIs"
    File "..\Contrib\UIs\default.exe"
  SectionEnd

  Section "Tiny User Interface" SecContribTinyUI

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Extra User Interfaces | Tiny User Interface..."
  SetDetailsPrint listonly

    SectionIn 1 2
    SetOutPath "$INSTDIR\Contrib\UIs"
    File "..\Contrib\UIs\sdbarker_tiny.exe"
  SectionEnd

SubSectionEnd

Section "Graphics" SecContribGraphics

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Graphics..."
  SetDetailsPrint listonly

  SectionIn 1 2
  Delete $INSTDIR\Contrib\Icons\*.ico
  Delete $INSTDIR\Contrib\Icons\*.bmp
  RMDir $INSTDIR\Contrib\Icons
  SetOutPath $INSTDIR\Contrib\Graphics
  File /r "..\Contrib\Graphics\*.ico"
  File /r "..\Contrib\Graphics\*.bmp"
SectionEnd

Section "Language files" SecContribLang

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Language Files..."
  SetDetailsPrint listonly

  SectionIn 1 2

  SetOutPath "$INSTDIR\Contrib\Language files"
  File "..\Contrib\Language files\*.nlf"

  SetOutPath $INSTDIR\Bin
  File ..\Bin\MakeLangID.exe

  SectionGetFlags ${SecContribModernUI} $R0
    IntOp $R0 $R0 & ${SF_SELECTED}
	IntCmp $R0 ${SF_SELECTED} 0 nomui nomui
	  SetOutPath "$INSTDIR\Contrib\Modern UI\Language files"
      File "..\Contrib\Modern UI\Language files\*.nsh"
  nomui:

SectionEnd

SubSection "Plugins" SecContribPlugins

Section "Banner" SecContribBanner

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | Banner..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\Banner.dll
  SetOutPath $INSTDIR\Contrib\Banner
  File ..\Contrib\Banner\Readme.txt
  File ..\Contrib\Banner\Example.nsi
SectionEnd

Section "Language DLL" SecContribLangDLL

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | Language DLL..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\LangDLL.dll
SectionEnd

Section "nsExec" SecContribnsExec

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | nsExec..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\nsExec.dll
  SetOutPath $INSTDIR\Contrib\nsExec
  File ..\Contrib\nsExec\*.txt
  File ..\Contrib\nsExec\*.nsi
SectionEnd

Section "Splash" SecContribSplash

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | Splash..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\splash.dll
  SetOutPath $INSTDIR\Contrib\Splash
  File ..\Contrib\splash\splash.txt
  File ..\Contrib\splash\Example.nsi
SectionEnd

Section "AdvSplash w/transparency" SecContribSplashT

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | AdvSplash w/transparency..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\advsplash.dll
  SetOutPath $INSTDIR\Contrib\AdvSplash
  File ..\Contrib\AdvSplash\advsplash.txt
  File ..\Contrib\AdvSplash\Example.nsi
SectionEnd

Section "BgImage" SecContribBgImage

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | BgImage..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\BgImage.dll
  SetOutPath $INSTDIR\Contrib\BgImage
  File ..\Contrib\BgImage\BgImage.txt
  File ..\Contrib\BgImage\Example.nsi
SectionEnd

Section "InstallOptions" SecContribIO

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | InstallOptions..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\InstallOptions.dll
  SetOutPath $INSTDIR\Contrib\InstallOptions
  File ..\Contrib\InstallOptions\Readme.html
  File ..\Contrib\InstallOptions\Changelog.txt
  File ..\contrib\installoptions\test.ini
  File ..\contrib\installoptions\test.nsi
  File ..\contrib\installoptions\testlink.ini
  File ..\contrib\installoptions\testlink.nsi
SectionEnd

Section "Math" SecContribMath

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | Math..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\Math.dll
  SetOutPath $INSTDIR\Contrib\Math
  File ..\Contrib\Math\Math.txt
  File ..\Contrib\Math\Math.nsi
  File ..\Contrib\Math\MathTest.txt
  File ..\Contrib\Math\MathTest.nsi
  File ..\Contrib\Math\MathTest.ini

SectionEnd

Section "NSISdl" SecContribNSISDL

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | NSISdl..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\nsisdl.dll
  SetOutPath $INSTDIR\Contrib\NSISdl
  File ..\contrib\NSISdl\ReadMe.txt
  File ..\contrib\NSISdl\License.txt
SectionEnd

Section "System" SecContribSystem

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | System..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\System.dll
  SetOutPath $INSTDIR\Contrib\System
  File ..\Contrib\System\*.dll
  File ..\Contrib\System\*.nsh
  File ..\Contrib\System\*.nsi
  File ..\Contrib\System\*.txt
SectionEnd

Section "StartMenu" SecContribStartMenu

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | StartMenu..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\StartMenu.dll
  SetOutPath $INSTDIR\Contrib\StartMenu
  File ..\Contrib\StartMenu\Example.nsi
  File ..\Contrib\StartMenu\Readme.txt
SectionEnd

Section "UserInfo" SecContribUserInfo

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | UserInfo..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\UserInfo.dll
  SetOutPath $INSTDIR\Contrib\UserInfo
  File ..\Contrib\UserInfo\UserInfo.nsi
SectionEnd

Section "Dialer" SecContribDialer

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | Dialer..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\Dialer.dll
  SetOutPath $INSTDIR\Contrib\Dialer
  File ..\Contrib\Dialer\Dialer.txt
SectionEnd

Section "VPatch" SecContribVPatch

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Plugins | VPatch..."
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

SubSectionEnd

Section "Zip2Exe" SecContribZ2E

  SetDetailsPrint textonly
  DetailPrint "Installing Contrib | Zip2Exe..."
  SetDetailsPrint listonly

  SectionIn 1 2
  SetOutPath $INSTDIR\Bin
  File ..\Bin\zip2exe.exe
  SetOutPath $INSTDIR\Contrib\zip2exe
  File ..\Contrib\zip2exe\Base.nsh
  File ..\Contrib\zip2exe\Modern.nsh
  File ..\Contrib\zip2exe\Classic.nsh
SectionEnd


SubSectionEnd


SubSection "Source code" SecSrc
Section "NSIS Source Code" SecSrcNSIS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | NSIS Source Code..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Source
  File ..\Source\*.cpp
  File ..\Source\*.c
  File ..\Source\*.h
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
SectionEnd

SubSection "Contrib" SecSrcContrib
# required for other plugins sources
# text changes in .onSelChange
Section "ExDLL Source (required)" SecSrcEx

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | ExDLL Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\ExDLL
  File ..\Contrib\exdll\exdll.c
  File ..\Contrib\exdll\exdll.h
  File ..\Contrib\exdll\exdll.dsp
  File ..\Contrib\exdll\exdll.dsw
  File ..\Contrib\exdll\exdll.dpr
  File ..\Contrib\exdll\exdll_with_unit.dpr
  File ..\Contrib\exdll\nsis.pas
  File ..\Contrib\exdll\extdll.inc
SectionEnd

Section "MakeNSISW Source" SecSrcMNW

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | MakeNSISW Source..."
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

Section "UI Holder Source" SecContribUIHolderS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | UI Holder..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath "$INSTDIR\Contrib\UIs\UI Holder"
  File "..\Contrib\UIs\UI Holder\*.h"
  File "..\Contrib\UIs\UI Holder\*.cpp"
  File "..\Contrib\UIs\UI Holder\*.rc"
  File "..\Contrib\UIs\UI Holder\*.dsw"
  File "..\Contrib\UIs\UI Holder\*.dsp"
SectionEnd

SubSection "Plugins" SecContribPluginsS

Section "Banner Source" SecContribBannerS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | Banner Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\Banner
  File ..\Contrib\Banner\Banner.dsw
  File ..\Contrib\Banner\Banner.dsp
  File ..\Contrib\Banner\Banner.c
SectionEnd

Section "Language DLL Source" SecContribLangDLLS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | Language DLL Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\LangDLL
  File ..\Contrib\LangDLL\LangDLL.c
  File ..\Contrib\LangDLL\resource.h
  File ..\Contrib\LangDLL\resource.rc
  File ..\Contrib\LangDLL\LangDLL.dsw
  File ..\Contrib\LangDLL\LangDLL.dsp
SectionEnd

Section "nsExec Source" SecContribnsExecS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | nsExec Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\nsExec
  File ..\Contrib\nsExec\*.c
  File ..\Contrib\nsExec\*.dsw
  File ..\Contrib\nsExec\*.dsp
SectionEnd

Section "Splash Source" SecContribSplashS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | Splash Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\Splash
  File ..\Contrib\Splash\splash.c
  File ..\Contrib\Splash\splash.dsp
  File ..\Contrib\Splash\splash.dsw
SectionEnd

Section "AdvSplash Source" SecContribSplashTS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | AdvSplash Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\AdvSplash
  File ..\Contrib\AdvSplash\*.c
  File ..\Contrib\AdvSplash\*.dsw
  File ..\Contrib\AdvSplash\*.dsp
SectionEnd

Section "BgImage Source" SecContribBgImageS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | BgImage Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\BgImage
  File ..\Contrib\BgImage\BgImage.cpp
  File ..\Contrib\BgImage\BgImage.dsw
  File ..\Contrib\BgImage\BgImage.dsp
SectionEnd

Section "InstallOptions Source" SecContribIOS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | InstallOptions Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\InstallOptions
  File ..\contrib\installoptions\io.dsp
  File ..\contrib\installoptions\io.dsw
  File ..\contrib\installoptions\InstallerOptions.cpp
  File ..\contrib\installoptions\*.rc
  File ..\contrib\installoptions\*.h
SectionEnd

Section "Math Source" SecContribMathS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | Math Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\Math\Source
  File ..\contrib\Math\Source\*.c
  File ..\contrib\Math\Source\*.h
  File ..\contrib\Math\Source\*.sln
  File ..\contrib\Math\Source\*.lib
  File ..\contrib\Math\Source\*.vcproj
SectionEnd

Section "NSISdl Source" SecContribNSISDLS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | NSISdl Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\NSISdl
  File ..\contrib\NSISdl\nsisdl.dsw
  File ..\contrib\NSISdl\nsisdl.dsp
  File ..\contrib\NSISdl\*.cpp
  File ..\contrib\NSISdl\*.h
  File ..\contrib\NSISdl\*.rc
SectionEnd

Section "System Source" SecContribSystemS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | System Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\System\Source
  File ..\contrib\System\Source\*.c
  File ..\contrib\System\Source\*.h
  File ..\contrib\System\Source\*.sln
  File ..\contrib\System\Source\*.obj
  File ..\contrib\System\Source\*.vcproj
SectionEnd

Section "StartMenu Source" SecContribStartMenuS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | StartMenu Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\StartMenu
  File ..\Contrib\StartMenu\StartMenu.c
  File ..\Contrib\StartMenu\StartMenu.dsp
  File ..\Contrib\StartMenu\StartMenu.dsw
  File ..\Contrib\StartMenu\StartMenu.rc
  File ..\Contrib\StartMenu\resource.h
SectionEnd

Section "UserInfo Source" SecContribUserInfoS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | UserInfo Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\UserInfo
  File ..\Contrib\UserInfo\UserInfo.c
  File ..\Contrib\UserInfo\UserInfo.dsp
  File ..\Contrib\UserInfo\UserInfo.dsw
SectionEnd

Section "Dialer Source" SecContribDialerS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | Dialer Source..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Contrib\Dialer
  File ..\Contrib\Dialer\dialer.c
  File ..\Contrib\Dialer\dialer.dsp
  File ..\Contrib\Dialer\dialer.dsw
SectionEnd

Section "VPatch Source" SecContribVPatchS

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Plugins | VPatch Source..."
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

SubSectionEnd ; plugins

Section "Zip2Exe Source" SecContribZ2ES

  SetDetailsPrint textonly
  DetailPrint "Installing Source Code | Contrib | Zip2exe Source..."
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

SubSectionEnd
SubSectionEnd

Section -post

  ; When Modern UI is installed:
  ; * Always install the English language file
  ; * Always install default icons / bitmaps

  SectionGetFlags ${SecContribModernUI} $R0
  IntOp $R0 $R0 & ${SF_SELECTED}
	IntCmp $R0 ${SF_SELECTED} "" nomui nomui

      SetDetailsPrint textonly
      DetailPrint "Configurating Modern UI..."
      SetDetailsPrint listonly


    SectionGetFlags ${SecContribLang} $R0
    IntOp $R0 $R0 & ${SF_SELECTED}
    IntCmp $R0 ${SF_SELECTED} langfiles
	
	  SetOutPath "$INSTDIR\Contrib\Modern UI\Language files"
      File "..\Contrib\Modern UI\Language files\English.nsh"

    langfiles:

    SectionGetFlags ${SecContribGraphics} $R0
    IntOp $R0 $R0 & ${SF_SELECTED}
    IntCmp $R0 ${SF_SELECTED} graphics

      SetOutPath $INSTDIR\Contrib\Graphics
      SetOutPath $INSTDIR\Contrib\Graphics\Icons
      File "..\Contrib\Graphics\Icons\modern-install.ico"
      File "..\Contrib\Graphics\Icons\modern-uninstall.ico"
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
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "URLInfoAbout" "http://nsis.sf.net/"
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

  Push "MakeNSISW"
  Call AddWorkspaceToStartMenu

  ; ExDLL
  Push "ExDLL"
  Call AddWorkspaceToStartMenu

  ; InstallOptions
  Push "InstallOptions\Readme.html"
  Push "InstallOptions Readme"
  Call AddContribToStartMenu

  Push "InstallOptions\io.dsw"
  Push "Source\InstallOptions project workspace"
  Call AddContribToStartMenu

  ; ZIP2EXE
  IfFileExists "$INSTDIR\Bin\zip2exe.exe" 0 +2
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\ZIP 2 EXE converter.lnk" "$INSTDIR\Bin\zip2exe.exe"

  Push ZIP2EXE
  Call AddWorkspaceToStartMenu

  ; Modern UI
  Push "Modern UI\Readme.html"
  Push "Modern UI Readme"
  Call AddContribToStartMenu

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
  Push "VPatch\Readme.html"
  Push "VPatch Readme"
  Call AddContribToStartMenu

  no_startshortcuts:
!endif

  ; will only be removed if empty
  SetDetailsPrint none
  RMDir $INSTDIR\Contrib\Source
  SetDetailsPrint lastused

  Delete $INSTDIR\uninst-nsis.exe
  WriteUninstaller $INSTDIR\uninst-nsis.exe

  SetDetailsPrint both

SectionEnd

;--------------------------------
;Descriptions

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCore} "The core files required to use NSIS (compiler etc.)"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecExample} "Example installation scripts that show you how to use NSIS"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecUpdate} "A tool that lets you check for new NSIS releases and download the latest development files"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecExtention} "Adds right mouse click integration to nsi files so you can compile scripts easily"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecIcons} "Adds icons to your start menu and your desktop for easy access"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContrib} "User interfaces, plugins, graphics, and utilities contributed by other NSIS developers"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribUIs} "User interface designs that can be used to change the installer look and feel"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribModernUI} "A modern user interface like the wizards of recent Windows versions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribDefaultUI} "The default NSIS user interface which you can customize to make your own UI"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribTinyUI} "A tiny version of the default user interface"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribGraphics} "Icons, checkbox images and other graphics"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribLang} "Language files used to support multiple languages in an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribPlugins} "Useful plugins that extend NSIS's functionality"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribPluginsS} "Source code for plugins"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribBanner} "Plugin that lets you show a banner before installation starts"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribBannerS} "Source code to plugin that lets you show a banner before installation starts"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribLangDLL} "Plugin that lets you add a language select dialog to your installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribLangDLLS} "Source code to plugin that lets you add a language select dialog to your installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribnsExec} "Plugin that executes console programs and prints its output in the NSIS log window or hides it"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribnsExecS} "Source code to plugin that executes console programs and prints its output in the NSIS log window or hides it"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribSplash} "Splash screen add-on that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribSplashS} "Source code to splash screen add-on that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribSplashT} "Splash screen add-on with transparency support that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribSplashTS} "Source code to splash screen add-on with transparency support that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribSystem} "Plugin that lets you call Win32 API from NSIS scripts"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribSystemS} "Source code to plugin that lets you call Win32 API from NSIS scripts"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribMath} "Plugin that lets you evaluate complicated mathematical expressions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribMathS} "Source code to plugin that lets you evaluate complicated mathematical expressions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribDialer} "Plugin that provides internet connection functions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribDialerS} "Source code to plugin that provides internet connection functions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribZ2E} "A utility that converts zip files into an NSIS installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribZ2ES} "Source code to a utility that converts zip files into an NSIS installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribIO} "Plugin that lets you add custom pages to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribIOS} "Source code to plugin that lets you add custom pages to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribStartMenu} "Plugin that lets the user select the start menu folder"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribStartMenuS} "Source code to plugin that lets the user select the start menu folder"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribBgImage} "Plugin that lets you show a persistent background image plugin and play sounds"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribBgImageS} "Source code to plugin that lets you show a persistent background image plugin and play sounds"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribUserInfo} "Plugin that that gives you the user name and the user account type"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribUserInfoS} "Source code to plugin that that gives you the user name and the user account type"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribNSISDL} "Plugin that lets you create a web based installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribNSISDLS} "Source code to plugin that lets you create a web based installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribUiHolderS} "Source code to the UI Holder where you can put your recources in to preview your user interface"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribVPatch} "Plugin that lets you create patches to upgrade older files"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecContribVPatchS} "Source code to plugin that lets you create patches to upgrade older files"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrc} "Source code to NSIS and all related files"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrcNSIS} "Source code to NSIS"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrcContrib} "Source code to user contributed utilities"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrcEx} "Example DLL plugin source in C and plugin function header"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrcMNW} "Source code to MakeNSIS Wrapper"
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

   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 1" "Text" "A older version of NSIS is installed on your system. It's recommanded that you uninstall the curent version before installing. Select the operation you want to perform and click Next to continue."
   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 2" "Text" "Uninstall before installing"
   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 3" "Text" "Do not uninstall"
   !insertmacro MUI_HEADER_TEXT "Already Installed" "Choose how you want to install NSIS."
   StrCpy $R0 "1"
   Goto reinst_start

  older_version:

   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 1" "Text" "A newer version of NSIS is already installed! It is not recommanded that you install an older version. If you really want to install this older version, it's better to uninstall the current version first. Select the operation you want to perform and click Next to continue."
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
  !insertmacro secSelected ${SecContribSplashTS}
  !insertmacro secSelected ${SecContribBannerS}
  !insertmacro secSelected ${SecContribBgImageS}
  !insertmacro secSelected ${SecContribIOS}
  !insertmacro secSelected ${SecContribLangDLLS}
  !insertmacro secSelected ${SecContribnsExecS}
  !insertmacro secSelected ${SecContribNSISdlS}
  !insertmacro secSelected ${SecContribSplashS}
  !insertmacro secSelected ${SecContribStartMenuS}
  !insertmacro secSelected ${SecContribUserInfoS}
  !insertmacro secSelected ${SecContribDialerS}
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
  IfFileExists $INSTDIR\Contrib\$0\Readme.txt 0 done
    Push $0\Readme.txt
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

  SetDetailsPrint textonly
  DetailPrint "Deleting Registry Keys..."
  SetDetailsPrint listonly

  DeleteRegKey HKCR "NSISFile"
  DeleteRegKey HKCR "NSHFile"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS"
  DeleteRegKey HKLM SOFTWARE\NSIS

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