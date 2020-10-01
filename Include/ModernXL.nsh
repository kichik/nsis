
!ifdef MUI_INCLUDED
!error '"ModernXL.nsh" must be included before "MUI2.nsh"'
!endif

!define /ifndef MUI_WELCOMEFINISHPAGE_BITMAP		"${NSISDIR}\Contrib\Graphics\Wizard\modern-xl-install.bmp"
!define /ifndef MUI_UNWELCOMEFINISHPAGE_BITMAP		"${NSISDIR}\Contrib\Graphics\Wizard\modern-xl-uninstall.bmp"

;!define MUI_WELCOMEFINISHPAGE_BITMAP_STRETCH		NoStretchNoCrop
;!define MUI_UNWELCOMEFINISHPAGE_BITMAP_STRETCH		NoStretchNoCrop

!define /ifndef MUI_UI								"${NSISDIR}\Contrib\UIs\xl.exe"
!define /ifndef MUI_UI_HEADERIMAGE					"${NSISDIR}\Contrib\UIs\xl_headerbmp.exe"
!define /ifndef MUI_UI_HEADERIMAGE_RIGHT			"${NSISDIR}\Contrib\UIs\xl_headerbmpr.exe"

!define /ifndef MUI_UI_COMPONENTSPAGE_SMALLDESC 	"${NSISDIR}\Contrib\UIs\xl_smalldesc.exe"
!define /ifndef MUI_UI_COMPONENTSPAGE_NODESC		"${NSISDIR}\Contrib\UIs\xl_nodesc.exe"
