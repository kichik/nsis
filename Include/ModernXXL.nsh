
!ifdef MUI_INCLUDED
!error '"ModernXXL.nsh" must be included before "MUI2.nsh"'
!endif

!define /ifndef MUI_WELCOMEFINISHPAGE_BITMAP		"${NSISDIR}\Contrib\Graphics\Wizard\modern-xxl-install.bmp"
!define /ifndef MUI_UNWELCOMEFINISHPAGE_BITMAP		"${NSISDIR}\Contrib\Graphics\Wizard\modern-xxl-uninstall.bmp"

;!define MUI_WELCOMEFINISHPAGE_BITMAP_STRETCH		NoStretchNoCrop
;!define MUI_UNWELCOMEFINISHPAGE_BITMAP_STRETCH		NoStretchNoCrop

!define /ifndef MUI_UI								"${NSISDIR}\Contrib\UIs\xxl.exe"
!define /ifndef MUI_UI_HEADERIMAGE					"${NSISDIR}\Contrib\UIs\xxl_headerbmp.exe"
!define /ifndef MUI_UI_HEADERIMAGE_RIGHT			"${NSISDIR}\Contrib\UIs\xxl_headerbmpr.exe"

!define /ifndef MUI_UI_COMPONENTSPAGE_SMALLDESC 	"${NSISDIR}\Contrib\UIs\xxl_smalldesc.exe"
!define /ifndef MUI_UI_COMPONENTSPAGE_NODESC		"${NSISDIR}\Contrib\UIs\xxl_nodesc.exe"
