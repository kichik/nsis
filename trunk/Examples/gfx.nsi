; gfx.nsi
;
; This script shows some examples of using all of the new
; graphic related additions made in NSIS 1.99
;
; Written by Amir Szkeley 22nd July 2002
;

!macro BIMAGE IMAGE PARMS
	Push $0
	GetTempFileName $0
	File /oname=$0 "${IMAGE}"
	SetBrandingImage ${PARMS} $0
	Delete $0
	Pop $0
!macroend

Name "Graphical effects"

OutFile "gfx.exe"

; Adds an XP manifest to the installer
XPStyle on

; Add branding image to the installer (an image on the side)
AddBrandingImage left 100

; Sets the font of the installer
SetFont "Comic Sans MS" 8

; Just to make it three pages...
SubCaption 0 ": Yet another page..."
SubCaption 2 ": Yet another page..."
LicenseText "Second page"
LicenseData "gfx.nsi"
DirText "Lets make a third page!"

; Pages
Page license licenseImage
Page custom customPage
Page directory dirImage
Page instfiles instImage

Function licenseImage
	!insertmacro BIMAGE "${NSISDIR}\Contrib\Icons\checks1.bmp" /RESIZETOFIT
	MessageBox MB_YESNO 'Would you like to skip the license page?' IDNO no
		Abort
	no:
FunctionEnd

Function customPage
	!insertmacro BIMAGE "${NSISDIR}\Contrib\Icons\modern.bmp" /RESIZETOFIT
	MessageBox MB_OK 'This is a nice custom "page" with yet another image :P'
	#insert install options/start menu/<insert plugin name here> here
FunctionEnd

Function dirImage
	!insertmacro BIMAGE "${NSISDIR}\Contrib\Icons\checks2.bmp" /RESIZETOFIT
FunctionEnd

Function instImage
	!insertmacro BIMAGE "${NSISDIR}\Contrib\Icons\checks-sdbarker.bmp" /RESIZETOFIT
FunctionEnd

; Install dir
InstallDir "${NSISDIR}\Examples"

Section
	; You can also use the BI_NEXT macro here...
	MessageBox MB_YESNO "We can change the branding image from within a section too!$\nDo you want me to change it?" IDNO done
		!insertmacro BIMAGE "${NSISDIR}\Contrib\Icons\checksX2.bmp" ""
	done:
	WriteUninstaller uninst.exe
SectionEnd

; Another page for uninstaller
UninstallText "Another page..."

; Uninstall pages
UninstPage uninstConfirm un.uninstImage
UninstPage instfiles un.instImage

Function un.uninstImage
	!insertmacro BIMAGE "${NSISDIR}\Contrib\Icons\checksX.bmp" /RESIZETOFIT
FunctionEnd

Function un.instImage
	!insertmacro BIMAGE "${NSISDIR}\Contrib\Icons\jarsonic-checks.bmp" /RESIZETOFIT
FunctionEnd

Section uninstall
	MessageBox MB_OK "Bla"
SectionEnd