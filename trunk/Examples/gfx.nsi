; gfx.nsi
;
; This script shows some examples of using all of the new
; graphic related additions made in NSIS 1.99
;
; Written by Amir Szkeley 22nd July 2002
;

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

; Install dir
InstallDir "${NSISDIR}\Examples"

; Branding helper functions
!include "branding.nsh"

Function .onInit
	!insertmacro BI_INIT $R0
FunctionEnd

Function .onNextPage
	!insertmacro BI_NEXT
FunctionEnd

Function .onPrevPage
	!insertmacro BI_PREV
FunctionEnd

!insertmacro BI_LIST
!insertmacro BI_LIST_ADD "${NSISDIR}\Contrib\Icons\checks1.bmp" /RESIZETOFIT
!insertmacro BI_LIST_ADD "${NSISDIR}\Contrib\Icons\checks2.bmp" /RESIZETOFIT
!insertmacro BI_LIST_ADD "${NSISDIR}\Contrib\Icons\checks4.bmp" /RESIZETOFIT
!insertmacro BI_LIST_END

Section
	; You can also use the BI_NEXT macro here...
	MessageBox MB_YESNO "We can change the branding image from within a section too!$\nDo you want me to change it?" IDNO done
		GetTempFileName $1
		File /oname=$1 "${NSISDIR}\Contrib\Icons\checksX2.bmp"
		SetBrandingImage $1
		Delete $1
	done:
	WriteUninstaller uninst.exe
SectionEnd

; Another page for uninstaller
UninstallText "Another page..."

; Uninstall branding helper functions
!define BI_UNINSTALL
!include "branding.nsh"

Function un.onInit
	!insertmacro BI_INIT $R0
FunctionEnd

Function un.onNextPage
	!insertmacro BI_NEXT
FunctionEnd

!insertmacro BI_LIST
!insertmacro BI_LIST_ADD "${NSISDIR}\Contrib\Icons\checksX.bmp" /RESIZETOFIT
!insertmacro BI_LIST_ADD "${NSISDIR}\Contrib\Icons\jarsonic-checks.bmp" /RESIZETOFIT
!insertmacro BI_LIST_END

Section uninstall
	MessageBox MB_OK "Bla"
SectionEnd