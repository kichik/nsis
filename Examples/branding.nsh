; Written by Amir Szekely 24th July 2002
; Please see gfx.nsi for example of usage

!verbose 3

; If we haven't included this as install macros yet
!ifndef BI_MACROS_USED
; If this isn't supposed to be uninstall macros
!ifndef BI_UNINSTALL
!define BI_MACROS_USED
; Undefine BI_FUNC if already defined by uninstaller macros
!ifdef BI_FUNC
!undef BI_FUNC
!endif
; Define BI_FUNC
!define BI_FUNC "BIChange"
; If BI_VAR or BI_TEMPFILE was already defined undefine it so BI_INIT can redefine it
!ifdef BI_VAR
!undef BI_VAR
!endif
!ifdef BI_TEMPFILE
!undef BI_TEMPFILE
!endif
; If macros aren't defined yet, define them
!ifndef UBI_MACROS_USED
!define BI_OK
!endif
; Done
!endif
!endif

; If we haven't included this as uninstall macros yet
!ifndef UBI_MACROS_USED
; If this is supposed to be uninstall macros
!ifdef BI_UNINSTALL
!define UBI_MACROS_USED
; Undefine BI_FUNC if already defined by installer macros
!ifdef BI_FUNC
!undef BI_FUNC
!endif
; Define BI_FUNC
!define BI_FUNC "un.BIChange"
; If BI_VAR or BI_TEMPFILE was already defined undefine it so BI_INIT can redefine it
!ifdef BI_VAR
!undef BI_VAR
!endif
!ifdef BI_TEMPFILE
!undef BI_TEMPFILE
!endif
; If macros aren't defined yet, define them
!ifndef BI_MACROS_USED
!define BI_OK
!endif
; Done
!endif
!endif

!ifdef BI_OK

!macro BI_INIT VAR
!define BI_VAR ${VAR}
StrCpy ${BI_VAR} 0
!macroend

!macro BI_NEXT
IntOp ${BI_VAR} ${BI_VAR} + 1
Call ${BI_FUNC}
!macroend

!macro BI_PREV
IntOp ${BI_VAR} ${BI_VAR} - 1
Call ${BI_FUNC}
!macroend

!macro BI_LIST
Function ${BI_FUNC}
Push $0
Push $1
StrCpy $0 0
GetTempFileName $1
!macroend

!macro BI_LIST_ADD IMAGE PARMS
IntOp $0 $0 + 1
StrCmp ${BI_VAR} $0 0 +4
	File /oname=$1 "${IMAGE}"
	SetBrandingImage ${PARMS} $1
	Goto BI_done
!macroend

!macro BI_LIST_END
BI_done:
	Delete $1
	Pop $1
	Pop $0
FunctionEnd
!macroend

!undef BI_OK
!endif ; ifdef BI_OK

!verbose 4

!echo "Branding macros defined successfully!"