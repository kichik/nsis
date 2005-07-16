;NSIS Setup Script
;--------------------------------

!ifndef VERSION
  !define VERSION 'anonymous-build'
!endif

;--------------------------------
;Configuration

!ifdef OUTFILE
  OutFile "${OUTFILE}"
!else
  OutFile ..\nsis-${VERSION}.exe
!endif

SetCompressor /SOLID lzma

InstType "Full"
InstType "Lite"
InstType "Minimal"

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
Caption "NSIS ${VERSION} Setup"

;Interface Settings
!define MUI_ABORTWARNING

!define MUI_HEADERIMAGE
!define MUI_WELCOMEFINISHPAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Wizard\nsis.bmp"

!define MUI_COMPONENTSPAGE_SMALLDESC

;Pages
!define MUI_WELCOMEPAGE_TITLE "Welcome to the NSIS ${VERSION} Setup Wizard"
!define MUI_WELCOMEPAGE_TEXT "This wizard will guide you through the installation of NSIS (Nullsoft Scriptable Install System) ${VERSION}, the next generation of the Windows installer and uninstaller system that doesn't suck and isn't huge.\r\n\r\nNSIS 2 includes a new Modern User Interface, LZMA compression, support for multiple languages and an easy plug-in system.\r\n\r\n$_CLICK"

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "..\license.txt"
!ifdef VER_MAJOR & VER_MINOR & VER_REVISION & VER_BUILD
Page custom PageReinstall PageLeaveReinstall
!endif
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

  SetOutPath $INSTDIR\Stubs
  File ..\Stubs\bzip2
  File ..\Stubs\bzip2_solid
  File ..\Stubs\lzma
  File ..\Stubs\lzma_solid
  File ..\Stubs\zlib
  File ..\Stubs\zlib_solid
  File ..\Stubs\uninst

  SetOutPath $INSTDIR\Include
  File ..\Include\WinMessages.nsh
  File ..\Include\Sections.nsh
  File ..\Include\Library.nsh
  File ..\Include\UpgradeDLL.nsh
  File ..\Include\LogicLib.nsh
  File ..\Include\StrFunc.nsh
  File ..\Include\Colors.nsh

  SetOutPath $INSTDIR\Docs\StrFunc
  File ..\Docs\StrFunc\StrFunc.txt

  SetOutPath $INSTDIR\Docs\makensisw
  File ..\Docs\makensisw\*.txt

  SetOutPath $INSTDIR\Menu
  File ..\Menu\*.html
  SetOutPath $INSTDIR\Menu\images
  File ..\Menu\images\*.gif

  Delete $INSTDIR\makensis.htm
  Delete $INSTDIR\Docs\*.html
  Delete $INSTDIR\Docs\style.css
  RMDir $INSTDIR\Docs

  SetOutPath $INSTDIR\Bin
  File ..\Bin\LibraryLocal.exe
  File ..\Bin\RegTool.bin

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

  SectionIn 1 2
  SetOutPath $INSTDIR\Examples
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
  SectionIn 1 2
  SetOutPath $INSTDIR
!ifndef NO_STARTMENUSHORTCUTS
  CreateDirectory $SMPROGRAMS\NSIS

  CreateShortCut "$SMPROGRAMS\NSIS\NSIS Menu.lnk" "$INSTDIR\NSIS.exe" ""

  CreateShortCut "$SMPROGRAMS\NSIS\MakeNSISW (Compiler GUI).lnk" "$INSTDIR\makensisw.exe"

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
  File "..\Contrib\Modern UI\ioSpecial.ini"

  SetOutPath "$INSTDIR\Docs\Modern UI"
  File "..\Docs\Modern UI\Readme.html"
  File "..\Docs\Modern UI\Changelog.txt"
  File "..\Docs\Modern UI\License.txt"

  SetOutPath "$INSTDIR\Docs\Modern UI\images"
  File "..\Docs\Modern UI\images\header.gif"
  File "..\Docs\Modern UI\images\screen1.png"
  File "..\Docs\Modern UI\images\screen2.png"
  File "..\Docs\Modern UI\images\open.gif"
  File "..\Docs\Modern UI\images\closed.gif"

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

  SectionIn 1

  SetOutPath "$INSTDIR\Contrib\UIs"
  File "..\Contrib\UIs\default.exe"

SectionEnd

Section "Tiny User Interface" SecInterfacesTinyUI

  SetDetailsPrint textonly
  DetailPrint "Installing User Interfaces | Tiny User Interface..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath "$INSTDIR\Contrib\UIs"
  File "..\Contrib\UIs\sdbarker_tiny.exe"

SectionEnd

SectionGroupEnd

Section "Graphics" SecGraphics

  SetDetailsPrint textonly
  DetailPrint "Installing Graphics..."
  SetDetailsPrint listonly

  SectionIn 1

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

  SectionIn 1

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

Section "Zip2Exe" SecToolsZ2E

  SetDetailsPrint textonly
  DetailPrint "Installing Tools | Zip2Exe..."
  SetDetailsPrint listonly

  SectionIn 1

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

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\Banner.dll
  SetOutPath $INSTDIR\Docs\Banner
  File ..\Docs\Banner\Readme.txt
  SetOutPath $INSTDIR\Examples\Banner
  File ..\Examples\Banner\Example.nsi
SectionEnd

Section "Language DLL" SecPluginsLangDLL

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | Language DLL..."
  SetDetailsPrint listonly

  SectionIn 1
  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\LangDLL.dll
SectionEnd

Section "nsExec" SecPluginsnsExec

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | nsExec..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\nsExec.dll
  SetOutPath $INSTDIR\Docs\nsExec
  File ..\Docs\nsExec\nsExec.txt
  SetOutPath $INSTDIR\Examples\nsExec
  File ..\Examples\nsExec\test.nsi
SectionEnd

Section "Splash" SecPluginsSplash

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | Splash..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\splash.dll
  SetOutPath $INSTDIR\Docs\Splash
  File ..\Docs\Splash\splash.txt
  SetOutPath $INSTDIR\Examples\Splash
  File ..\Examples\Splash\Example.nsi
SectionEnd

Section "AdvSplash" SecPluginsSplashT

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | AdvSplash..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\advsplash.dll
  SetOutPath $INSTDIR\Docs\AdvSplash
  File ..\Docs\AdvSplash\advsplash.txt
  SetOutPath $INSTDIR\Examples\AdvSplash
  File ..\Examples\AdvSplash\Example.nsi
SectionEnd

Section "BgImage" SecPluginsBgImage

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | BgImage..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\BgImage.dll
  SetOutPath $INSTDIR\Docs\BgImage
  File ..\Docs\BgImage\BgImage.txt
  SetOutPath $INSTDIR\Examples\BgImage
  File ..\Examples\BgImage\Example.nsi
SectionEnd

Section "InstallOptions" SecPluginsIO

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | InstallOptions..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\InstallOptions.dll
  SetOutPath $INSTDIR\Docs\InstallOptions
  File ..\Docs\InstallOptions\Readme.html
  File ..\Docs\InstallOptions\Changelog.txt
  SetOutPath $INSTDIR\Examples\InstallOptions
  File ..\Examples\InstallOptions\test.ini
  File ..\Examples\InstallOptions\test.nsi
  File ..\Examples\InstallOptions\testimgs.ini
  File ..\Examples\InstallOptions\testimgs.nsi
  File ..\Examples\InstallOptions\testlink.ini
  File ..\Examples\InstallOptions\testlink.nsi
  File ..\Examples\InstallOptions\testnotify.ini
  File ..\Examples\InstallOptions\testnotify.nsi
SectionEnd

Section "Math" SecPluginsMath

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | Math..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\Math.dll
  SetOutPath $INSTDIR\Docs\Math
  File ..\Docs\Math\Math.txt
  SetOutPath $INSTDIR\Examples\Math
  File ..\Examples\Math\math.nsi
  File ..\Examples\Math\mathtest.txt
  File ..\Examples\Math\mathtest.nsi
  File ..\Examples\Math\mathtest.ini

SectionEnd

Section "NSISdl" SecPluginsNSISDL

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | NSISdl..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\nsisdl.dll
  SetOutPath $INSTDIR\Docs\NSISdl
  File ..\Docs\NSISdl\ReadMe.txt
  File ..\Docs\NSISdl\License.txt
SectionEnd

Section "System" SecPluginsSystem

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | System..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\System.dll
  SetOutPath $INSTDIR\Docs\System
  File ..\Docs\System\System.html
  File ..\Docs\System\WhatsNew.txt
  SetOutPath $INSTDIR\Examples\System
  File ..\Examples\System\Resource.dll
  File ..\Examples\System\SysFunc.nsh
  File ..\Examples\System\System.nsh
  File ..\Examples\System\System.nsi
SectionEnd

Section "StartMenu" SecPluginsStartMenu

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | StartMenu..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\StartMenu.dll
  SetOutPath $INSTDIR\Docs\StartMenu
  File ..\Docs\StartMenu\Readme.txt
  SetOutPath $INSTDIR\Examples\StartMenu
  File ..\Examples\StartMenu\Example.nsi
SectionEnd

Section "UserInfo" SecPluginsUserInfo

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | UserInfo..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\UserInfo.dll
  SetOutPath $INSTDIR\Examples\UserInfo
  File ..\Examples\UserInfo\UserInfo.nsi
SectionEnd

Section "Dialer" SecPluginsDialer

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | Dialer..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\Dialer.dll
  SetOutPath $INSTDIR\Docs\Dialer
  File ..\Docs\Dialer\Dialer.txt
SectionEnd

Section "VPatch" SecPluginsVPatch

  SetDetailsPrint textonly
  DetailPrint "Installing Plug-ins | VPatch..."
  SetDetailsPrint listonly

  SectionIn 1

  SetOutPath $INSTDIR\Plugins
  File ..\Plugins\VPatch.dll
  SetOutPath $INSTDIR\Examples\VPatch
  File ..\Examples\VPatch\example.nsi
  File ..\Examples\VPatch\oldfile.txt
  File ..\Examples\VPatch\newfile.txt
  File ..\Examples\VPatch\patch.pat
  SetOutPath $INSTDIR\Docs\VPatch
  File ..\Docs\VPatch\Readme.html
  SetOutPath $INSTDIR\Bin
  File ..\Bin\GenPat.exe
SectionEnd

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
!ifdef VER_MAJOR & VER_MINOR & VER_REVISION & VER_BUILD
  WriteRegDword HKLM "Software\NSIS" "VersionMajor" "${VER_MAJOR}"
  WriteRegDword HKLM "Software\NSIS" "VersionMinor" "${VER_MINOR}"
  WriteRegDword HKLM "Software\NSIS" "VersionRevision" "${VER_REVISION}"
  WriteRegDword HKLM "Software\NSIS" "VersionBuild" "${VER_BUILD}"
!endif

  WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "UninstallString" '"$INSTDIR\uninst-nsis.exe"'
  WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "InstallLocation" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "DisplayName" "Nullsoft Install System"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "DisplayIcon" "$INSTDIR\NSIS.exe,0"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "DisplayVersion" "${VERSION}"
!ifdef VER_MAJOR & VER_MINOR & VER_REVISION & VER_BUILD
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "VersionMajor" "${VER_MAJOR}"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NSIS" "VersionMinor" "${VER_MINOR}.${VER_REVISION}"
!endif
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

  ; MakeNSISW
  CreateDirectory $SMPROGRAMS\NSIS\Contrib
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\MakeNSISw Readme.lnk" "$INSTDIR\Docs\makensisw\readme.txt"

  ; ZIP2EXE
  IfFileExists "$INSTDIR\Bin\zip2exe.exe" 0 +2
    CreateShortCut "$SMPROGRAMS\NSIS\Contrib\zip2exe (Create SFX).lnk" "$INSTDIR\Bin\zip2exe.exe"

  ; Modern UI
  Push "Modern UI"
  Call AddReadmeToStartMenu

  ; Splash
  Push Splash
  Call AddReadmeToStartMenu

  ; Advanced splash
  Push AdvSplash
  Call AddReadmeToStartMenu

  ; Math
  Push Math
  Call AddReadmeToStartMenu

  ; NSISdl
  Push NSISdl
  Call AddReadmeToStartMenu

  ; nsExec
  Push nsExec
  Call AddReadmeToStartMenu

  ; StartMenu
  Push StartMenu
  Call AddReadmeToStartMenu

  ; BgImage
  Push BgImage
  Call AddReadmeToStartMenu

  ; Banner
  Push Banner
  Call AddReadmeToStartMenu

  ; System
  Push System
  Call AddReadmeToStartMenu

  ; VPatch
  Push VPatch
  Call AddReadmeToStartMenu

  ; InstallOptions
  Push InstallOptions
  Call AddReadmeToStartMenu

  no_startshortcuts:
!endif

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
  !insertmacro MUI_DESCRIPTION_TEXT ${SecToolsZ2E} "A utility that converts a ZIP file to a NSIS installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecGraphics} "Icons, checkbox images and other graphics"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecLangFiles} "Language files used to support multiple languages in an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsPlugins} "Useful plugins that extend NSIS's functionality"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsBanner} "Plugin that lets you show a banner before installation starts"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsLangDLL} "Plugin that lets you add a language select dialog to your installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsnsExec} "Plugin that executes console programs and prints its output in the NSIS log window or hides it"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsSplash} "Splash screen add-on that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsSplashT} "Splash screen add-on with transparency support that lets you add a splash screen to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsSystem} "Plugin that lets you call Win32 API or external DLLs"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsMath} "Plugin that lets you evaluate complicated mathematical expressions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsDialer} "Plugin that provides internet connection functions"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsIO} "Plugin that lets you add custom pages to an installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsStartMenu} "Plugin that lets the user select the start menu folder"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsBgImage} "Plugin that lets you show a persistent background image plugin and play sounds"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsUserInfo} "Plugin that that gives you the user name and the user account type"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsNSISDL} "Plugin that lets you create a web based installer"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPluginsVPatch} "Plugin that lets you create patches to upgrade older files"
!insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Installer Functions

!ifdef VER_MAJOR & VER_MINOR & VER_REVISION & VER_BUILD

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

   !insertmacro MUI_INSTALLOPTIONS_WRITE "makensis.ini" "Field 1" "Text" "NSIS ${VERSION} is already installed. Select the operation you want to perform and click Next to continue."
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

!endif # VER_MAJOR & VER_MINOR & VER_REVISION & VER_BUILD

!ifndef NO_STARTMENUSHORTCUTS
Function AddReadmeToStartMenu
  Pop $0
  StrCpy $1 "$0 Readme"
  IfFileExists $INSTDIR\Docs\$0\$0.txt 0 +3
    StrCpy $0 Docs\$0\$0.txt
    Goto create
  IfFileExists $INSTDIR\Docs\$0\$0.html 0 +3
    StrCpy $0 Docs\$0\$0.html
    Goto create
  IfFileExists $INSTDIR\Docs\$0\Readme.txt 0 +3
    StrCpy $0 Docs\$0\Readme.txt
    Goto create
  IfFileExists $INSTDIR\Docs\$0\Readme.html 0 done
    StrCpy $0 Docs\$0\Readme.html
  create:
    CreateShortCut $SMPROGRAMS\NSIS\Contrib\$1.lnk $INSTDIR\$0
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
  RMDir /r $INSTDIR\Bin
  RMDir /r $INSTDIR\Contrib
  RMDir /r $INSTDIR\Docs
  RMDir /r $INSTDIR\Examples
  RMDir /r $INSTDIR\Include
  RMDir /r $INSTDIR\Menu
  RMDir /r $INSTDIR\Plugins
  RMDir /r $INSTDIR\Stubs
  RMDir $INSTDIR

  SetDetailsPrint both

SectionEnd
