!include nsDialogs.nsh

!define PBM_SETPOS     0x0402
!define PBM_DELTAPOS   0x0403
!define PBM_GETPOS     1032

!addplugindir "."

Name "nsDialogs Example"
OutFile "nsDialogs Example.exe"
XpStyle on

Var DIALOG
Var TEXT
Var PROGBAR
Var PROGBAR2
Var PROGBAR3
Var BUTTON
Var BUTTON2
Var TIMERID
Var TIMERID2

Page custom nsDialogsPage

Function OnTimer
	Pop $0 ; Timer id

  	SendMessage $PROGBAR ${PBM_GETPOS} 0 0 $1
	StrCmp $1 100 0 +3
	SendMessage $PROGBAR ${PBM_SETPOS} 0 0
	Goto +2
	SendMessage $PROGBAR ${PBM_DELTAPOS} 10 0

FunctionEnd

Function OnTimer2
	Pop $0 ; Timer id

  	SendMessage $PROGBAR2 ${PBM_GETPOS} 0 0 $1
	StrCmp $1 100 0 +3
	SendMessage $PROGBAR2 ${PBM_SETPOS} 0 0
	Goto +2
	SendMessage $PROGBAR2 ${PBM_DELTAPOS} 5 0

FunctionEnd

Function OnTimer3
	Pop $0 ; Timer id

  	SendMessage $PROGBAR3 ${PBM_GETPOS} 0 0 $1
	IntCmp $1 85 0 +4 0
 	nsDialogs::DestroyTimer /NOUNLOAD $0
 	MessageBox MB_OK "Timer 3 killed"
	Goto +2
	SendMessage $PROGBAR3 ${PBM_DELTAPOS} 2 0

FunctionEnd

Function OnClick
	Pop $0

 	nsDialogs::DestroyTimer /NOUNLOAD $TIMERID

FunctionEnd

Function OnClick2
	Pop $0

 	nsDialogs::DestroyTimer /NOUNLOAD $TIMERID2

FunctionEnd

Function nsDialogsPage

	nsDialogs::Create /NOUNLOAD 1018
	Pop $DIALOG

	nsDialogs::CreateControl /NOUNLOAD "STATIC" ${DEFAULT_STYLES} ${WS_EX_TRANSPARENT} 0u 0u 100% 9u "nsDialogs timer example"
	Pop $TEXT

	nsDialogs::CreateControl /NOUNLOAD "msctls_progress32" ${DEFAULT_STYLES} "" 0u 10u 100% 12u ""
	Pop $PROGBAR

	nsDialogs::CreateControl /NOUNLOAD "BUTTON" ${DEFAULT_STYLES}|${WS_TABSTOP} "" 0u 25u 100u 14u "Kill Timer 1"
	Pop $BUTTON
	GetFunctionAddress $0 OnClick
	nsDialogs::OnClick /NOUNLOAD $BUTTON $0

	nsDialogs::CreateControl /NOUNLOAD "msctls_progress32" ${DEFAULT_STYLES} "" 0u 52u 100% 12u ""
	Pop $PROGBAR2

	nsDialogs::CreateControl /NOUNLOAD "BUTTON" ${DEFAULT_STYLES}|${WS_TABSTOP} "" 0u 67u 100u 14u "Kill Timer 2"
	Pop $BUTTON2
	GetFunctionAddress $0 OnClick2
	nsDialogs::OnClick /NOUNLOAD $BUTTON2 $0

	nsDialogs::CreateControl /NOUNLOAD "msctls_progress32" ${DEFAULT_STYLES} "" 0u 114u 100% 12u ""
	Pop $PROGBAR3

	GetFunctionAddress $0 OnTimer
	nsDialogs::CreateTimer /NOUNLOAD 1000 $0
	Pop $TIMERID

	GetFunctionAddress $0 OnTimer2
	nsDialogs::CreateTimer /NOUNLOAD 100 $0
	Pop $TIMERID2
	
	GetFunctionAddress $0 OnTimer3
	nsDialogs::CreateTimer /NOUNLOAD 200 $0
	Pop $0

	nsDialogs::Show

FunctionEnd

Section
SectionEnd
