OutFile languages.exe

XPStyle on

; First is default
LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
Name English
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Dutch.nlf"
Name Dutch
LoadLanguageFile "${NSISDIR}\Contrib\Language files\French.nlf"
Name French
LoadLanguageFile "${NSISDIR}\Contrib\Language files\German.nlf"
Name German
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Korean.nlf"
Name Korean
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Russian.nlf"
Name Russian
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Spanish.nlf"
Name Spanish
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Swedish.nlf"
Name Swedish
LoadLanguageFile "${NSISDIR}\Contrib\Language files\TradChinese.nlf"
Name "Traditional Chinese"

; The language can be the last used language like above, but it can be defined using /LANG
ComponentText /LANG=1033 "English component page"
ComponentText /LANG=1043 "Dutch component page"
ComponentText /LANG=1036 "French component page"
ComponentText /LANG=1031 "German component page"
ComponentText /LANG=1042 "Korean component page"
ComponentText /LANG=1049 "Russian component page"
ComponentText /LANG=1034 "Spanish component page"
ComponentText /LANG=1053 "Swedish component page"
ComponentText /LANG=1028 "Traditional Chinese component page"

; scetion names will be given in .onInit to match the language choosen by the user
Section " " sec1
	; $0 is already set from .onInit
	MessageBox MB_OK "$0 section"
SectionEnd

Section " " sec2
	StrCmp $LANGUAGE 1033 0 +2
		MessageBox MB_OK "Installing English stuff"
	StrCmp $LANGUAGE 1043 0 +2
		MessageBox MB_OK "Installing Dutch stuff"
	StrCmp $LANGUAGE 1036 0 +2
		MessageBox MB_OK "Installing French stuff"
	StrCmp $LANGUAGE 1031 0 +2
		MessageBox MB_OK "Installing German stuff"
	StrCmp $LANGUAGE 1042 0 +2
		MessageBox MB_OK "Installing Korean stuff"
	StrCmp $LANGUAGE 1049 0 +2
		MessageBox MB_OK "Installing Russian stuff"
	StrCmp $LANGUAGE 1034 0 +2
		MessageBox MB_OK "Installing Spanish stuff"
	StrCmp $LANGUAGE 1053 0 +2
		MessageBox MB_OK "Installing Swedish stuff"
	StrCmp $LANGUAGE 1028 0 +2
		MessageBox MB_OK "Installing Traditional Chinese stuff"
SectionEnd

Function .onInit
	Push "Installer Language" ; caption
	Push "Please select the language of the installer" ; text
	Push English
	Push 1033
	Push Dutch
	Push 1043
	Push French
	Push 1036
	Push German
	Push 1031
	Push Korean
	Push 1042
	Push Russian
	Push 1049
	Push Spanish
	Push 1034
	Push Swedish
	Push 1053
	Push "Traditional Chinese"
	Push 1028

	LandDLL::LangDialog 9 ; 9 is the number of languages

	Pop $LANGUAGE
	StrCmp $LANGUAGE "cancel" 0 +2
		Abort

	StrCmp $LANGUAGE 1033 0 +2
		StrCpy $0 "English"
	StrCmp $LANGUAGE 1043 0 +2
		StrCpy $0 "Dutch"
	StrCmp $LANGUAGE 1036 0 +2
		StrCpy $0 "French"
	StrCmp $LANGUAGE 1031 0 +2
		StrCpy $0 "German"
	StrCmp $LANGUAGE 1042 0 +2
		StrCpy $0 "Korean"
	StrCmp $LANGUAGE 1049 0 +2
		StrCpy $0 "Russian"
	StrCmp $LANGUAGE 1034 0 +2
		StrCpy $0 "Spanish"
	StrCmp $LANGUAGE 1053 0 +2
		StrCpy $0 "Swedish"
	StrCmp $LANGUAGE 1028 0 +2
		StrCpy $0 "Traditional Chinese"

	; Set the section name to something localized
	SectionSetText ${sec1} "!$0 section #1"
	SectionSetText ${sec2} "$0 section #2"
FunctionEnd