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
LoadLanguageFile "${NSISDIR}\Contrib\Language files\SimpChinese.nlf"
Name "Simplified Chinese"

; The language can be the last used language like above, but it can be defined using /LANG
ComponentText /LANG=${LANG_ENGLISH} "English component page"
ComponentText /LANG=${LANG_DUTCH} "Dutch component page"
ComponentText /LANG=${LANG_FRENCH} "French component page"
ComponentText /LANG=${LANG_GERMAN} "German component page"
ComponentText /LANG=${LANG_KOREAN} "Korean component page"
ComponentText /LANG=${LANG_RUSSIAN} "Russian component page"
ComponentText /LANG=${LANG_SPANISH} "Spanish component page"
ComponentText /LANG=${LANG_SWEDISH} "Swedish component page"
ComponentText /LANG=${LANG_TRADCHINESE} "Traditional Chinese component page"
ComponentText /LANG=${LANG_SIMPCHINESE} "Simplified Chinese component page"

; scetion names will be given in .onInit to match the language choosen by the user
Section " " sec1
	; $0 is already set from .onInit
	MessageBox MB_OK "$0 section"
SectionEnd

Section " " sec2
	StrCmp $LANGUAGE ${LANG_ENGLISH} 0 +2
		MessageBox MB_OK "Installing English stuff"
	StrCmp $LANGUAGE ${LANG_DUTCH} 0 +2
		MessageBox MB_OK "Installing Dutch stuff"
	StrCmp $LANGUAGE ${LANG_FRENCH} 0 +2
		MessageBox MB_OK "Installing French stuff"
	StrCmp $LANGUAGE ${LANG_GERMAN} 0 +2
		MessageBox MB_OK "Installing German stuff"
	StrCmp $LANGUAGE ${LANG_KOREAN} 0 +2
		MessageBox MB_OK "Installing Korean stuff"
	StrCmp $LANGUAGE ${LANG_RUSSIAN} 0 +2
		MessageBox MB_OK "Installing Russian stuff"
	StrCmp $LANGUAGE ${LANG_SPANISH} 0 +2
		MessageBox MB_OK "Installing Spanish stuff"
	StrCmp $LANGUAGE ${LANG_SWEDISH} 0 +2
		MessageBox MB_OK "Installing Swedish stuff"
	StrCmp $LANGUAGE ${LANG_TRADCHINESE} 0 +2
		MessageBox MB_OK "Installing Traditional Chinese stuff"
	StrCmp $LANGUAGE ${LANG_SIMPCHINESE} 0 +2
		MessageBox MB_OK "Installing Simplified Chinese stuff"
SectionEnd

Function .onInit
	Push ${LANG_ENGLISH}
	Push English
	Push ${LANG_DUTCH}
	Push Dutch
	Push ${LANG_FRENCH}
	Push French
	Push ${LANG_GERMAN}
	Push German
	Push ${LANG_KOREAN}
	Push Korean
	Push ${LANG_RUSSIAN}
	Push Russian
	Push ${LANG_SPANISH}
	Push Spanish
	Push ${LANG_SWEDISH}
	Push Swedish
	Push ${LANG_TRADCHINESE}
	Push "Traditional Chinese"
	Push ${LANG_SIMPCHINESE}
	Push "Simplified Chinese"
	Push 10 ; 10 is the number of languages
	LangDLL::LangDialog "Installer Language" "Please select the language of the installer"

	Pop $LANGUAGE
	StrCmp $LANGUAGE "cancel" 0 +2
		Abort

	StrCmp $LANGUAGE ${LANG_ENGLISH} 0 +2
		StrCpy $0 "English"
	StrCmp $LANGUAGE ${LANG_DUTCH} 0 +2
		StrCpy $0 "Dutch"
	StrCmp $LANGUAGE ${LANG_FRENCH} 0 +2
		StrCpy $0 "French"
	StrCmp $LANGUAGE ${LANG_GERMAN} 0 +2
		StrCpy $0 "German"
	StrCmp $LANGUAGE ${LANG_KOREAN} 0 +2
		StrCpy $0 "Korean"
	StrCmp $LANGUAGE ${LANG_RUSSIAN} 0 +2
		StrCpy $0 "Russian"
	StrCmp $LANGUAGE ${LANG_SPANISH} 0 +2
		StrCpy $0 "Spanish"
	StrCmp $LANGUAGE ${LANG_SWEDISH} 0 +2
		StrCpy $0 "Swedish"
	StrCmp $LANGUAGE ${LANG_TRADCHINESE} 0 +2
		StrCpy $0 "Traditional Chinese"
	StrCmp $LANGUAGE ${LANG_SIMPCHINESE} 0 +2
		StrCpy $0 "Simplified Chinese"

	; Set the section name to something localized
	SectionSetText ${sec1} "!$0 section #1"
	SectionSetText ${sec2} "$0 section #2"
FunctionEnd