
/****************************************************************
	This script generates a ModernXL demo installer
*****************************************************************/

; External definitions:
; BUILD_MODERN       | Build a Modern UI (default NSIS UI)
; BUILD_MODERN_XL    | Build a "Modern XL" UI installer
; BUILD_MODERN_XXL   | Build a "Modern XXL" UI installer

Target x86-unicode

InstallDir "$DESKTOP"	; No files will be actually written

ManifestDPIAware true
RequestExecutionLevel user

!ifdef BUILD_MODERN_XXL

# Modern XXL UI (fits 1024*768 and higher resolutions)
Name     "ModernXXL Demo"
OutFile  "ModernXXL.exe"
!include "ModernXXL.nsh"

!else ifdef BUILD_MODERN

# Modern UI (NSIS default)
Name     "Modern UI Demo"
OutFile  "Modern.exe"

!else

# ModernXL UI (fits 800*600 and higher resolutions)
Name     "ModernXL Demo"
OutFile  "ModernXL.exe"
!include "ModernXL.nsh"

!endif

!include "MUI2.nsh"

!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\nsis-menu.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\nsis3-uninstall.ico"

!define MUI_WELCOMEPAGE_TITLE_3LINES

;!define MUI_LICENSEPAGE_CHECKBOX
;!define MUI_LICENSEPAGE_RADIOBUTTONS

!define MUI_COMPONENTSPAGE_SMALLDESC
;!define MUI_COMPONENTSPAGE_NODESC

!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_UNFINISHPAGE_NOAUTOCLOSE

!define MUI_FINISHPAGE_TITLE_3LINES
!define MUI_FINISHPAGE_RUN "https://nsis.sourceforge.io/Category:Code_Examples"
!define MUI_FINISHPAGE_RUN_NOTCHECKED
!define MUI_FINISHPAGE_SHOWREADME "https://nsis.sourceforge.io/License"
!define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
!define MUI_FINISHPAGE_LINK "See more NSIS tutorials"
!define MUI_FINISHPAGE_LINK_LOCATION "https://nsis.sourceforge.io/Category:Tutorials"

Var StartMenuFolder

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "${__FILE__}"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_STARTMENU "$(^Name)" $StartMenuFolder
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_LANGUAGE "Arabic"
!insertmacro MUI_LANGUAGE "Armenian"
!insertmacro MUI_LANGUAGE "Bulgarian"
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "Farsi"
!insertmacro MUI_LANGUAGE "French"
!insertmacro MUI_LANGUAGE "German"
!insertmacro MUI_LANGUAGE "Greek"
!insertmacro MUI_LANGUAGE "Hebrew"
!insertmacro MUI_LANGUAGE "Hindi"
!insertmacro MUI_LANGUAGE "Japanese"
!insertmacro MUI_LANGUAGE "Mongolian"
!insertmacro MUI_LANGUAGE "Pashto"
!insertmacro MUI_LANGUAGE "Romanian"
!insertmacro MUI_LANGUAGE "Russian"
!insertmacro MUI_LANGUAGE "SimpChinese"
!insertmacro MUI_LANGUAGE "Spanish"
!insertmacro MUI_LANGUAGE "Thai"
!insertmacro MUI_LANGUAGE "TradChinese"
!insertmacro MUI_LANGUAGE "Vietnamese"


Function .onInit
	!insertmacro MUI_LANGDLL_DISPLAY
FunctionEnd

InstType "Full"   IT_FULL
InstType "Group1" IT_GROUP1
InstType "Group2" IT_GROUP2
InstType "Group3" IT_GROUP3

SectionGroup /e "Group 1" Group1
Section "Feature 1.1" Section11
	SectionInstType ${IT_FULL} ${IT_GROUP1}
	DetailPrint "${__SECTION__}"
SectionEnd
Section "Feature 1.2" Section12
	SectionInstType ${IT_FULL} ${IT_GROUP1}
	DetailPrint "${__SECTION__}"
SectionEnd
SectionGroupEnd

SectionGroup /e "Group 2" Group2
Section "Feature 2.1" Section21
	SectionInstType ${IT_FULL} ${IT_GROUP2}
	DetailPrint "${__SECTION__}"
SectionEnd
Section "Feature 2.2" Section22
	SectionInstType ${IT_FULL} ${IT_GROUP2}
	DetailPrint "${__SECTION__}"
SectionEnd
SectionGroupEnd

SectionGroup /e "Group 3" Group3
Section "Feature 3.1" Section31
	SectionInstType ${IT_FULL} ${IT_GROUP3}
	DetailPrint "${__SECTION__}"
SectionEnd
Section "Feature 3.2" Section32
	SectionInstType ${IT_FULL} ${IT_GROUP3}
	DetailPrint "${__SECTION__}"
SectionEnd
SectionGroupEnd

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${Group1}    "Description of group 1"
  !insertmacro MUI_DESCRIPTION_TEXT ${Section11} "Description of feature 1.1"
  !insertmacro MUI_DESCRIPTION_TEXT ${Section12} "Description of feature 1.2"
  !insertmacro MUI_DESCRIPTION_TEXT ${Group2}    "Description of group 2"
  !insertmacro MUI_DESCRIPTION_TEXT ${Section21} "Description of feature 2.1"
  !insertmacro MUI_DESCRIPTION_TEXT ${Section22} "Description of feature 2.2"
  !insertmacro MUI_DESCRIPTION_TEXT ${Group3}    "Description of group 3"
  !insertmacro MUI_DESCRIPTION_TEXT ${Section31} "Description of feature 3.1"
  !insertmacro MUI_DESCRIPTION_TEXT ${Section32} "Description of feature 3.2"
!insertmacro MUI_FUNCTION_DESCRIPTION_END
