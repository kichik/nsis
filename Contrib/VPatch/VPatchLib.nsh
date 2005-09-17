; PatchLib v3.0
; =============
;
; Library with macro for use with VPatch (DLL version) in NSIS 2.0.5+
; Created by Koen van de Sande

!macro VPatchFile PATCHDATA SOURCEFILE TEMPFILE
  InitPluginsDir
  File /oname=$PLUGINSDIR\${PATCHDATA} ${PATCHDATA}
  vpatch::vpatchfile "$PLUGINSDIR\${PATCHDATA}" "${SOURCEFILE}" "${TEMPFILE}"
  Pop $1
  DetailPrint $1
  StrCpy $1 $1 2
  StrCmp $1 "OK" ok_${SOURCEFILE}
  SetErrors
ok_${SOURCEFILE}:
  IfFileExists "${TEMPFILE}" +1 end_${SOURCEFILE}
  Delete "${SOURCEFILE}"
  Rename /REBOOTOK "${TEMPFILE}" "${SOURCEFILE}"
end_${SOURCEFILE}:
  Delete "$PLUGINSDIR\${PATCHDATA}"
!macroend
