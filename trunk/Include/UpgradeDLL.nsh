!ifndef UPGRADEDLL_INCLUDED

!define UPGRADEDLL_INCLUDED

; Macro - Upgrade DLL File
; Written by Joost Verburg
; ------------------------
;
; Parameters:
; LOCALFILE   - Location of the new DLL file (on the compiler system)
; DESTFILE    - Location of the DLL file that should be upgraded (on the user's system)
; TEMPBASEDIR - Directory on the user's system to store a temporary file when the system has
;               to be rebooted.
;               For Win9x support, this should be on the same volume as the DESTFILE!
;               The Windows temp directory could be located on any volume, so you cannot use
;               this directory.
;
; Define UPGRADEDLL_NOREGISTER if you want to upgrade a DLL that does not have to be registered.
;
; Note: If you want to support Win9x, you can only use short filenames (8.3).
;
; Example of usage:
; !insertmacro UpgradeDLL "dllname.dll" "$SYSDIR\dllname.dll" "$SYSDIR"
;

!macro UpgradeDLL LOCALFILE DESTFILE TEMPBASEDIR

  Push $R0
  Push $R1
  Push $R2
  Push $R3
  Push $R4
  Push $R5

  ;------------------------
  ;Unique number for labels

  !define UPGRADEDLL_UNIQUE ${__LINE__}

  ;------------------------
  ;Copy the parameters used on run-time to a variable
  ;This allows the usage of variables as paramter

  StrCpy $R4 "${DESTFILE}"
  StrCpy $R5 "${TEMPBASEDIR}"

  ;------------------------
  ;Check file and version

  IfFileExists $R4 0 upgradedll.copy_${UPGRADEDLL_UNIQUE}

  ClearErrors
    GetDLLVersionLocal "${LOCALFILE}" $R0 $R1
    GetDLLVersion $R4 $R2 $R3
  IfErrors upgradedll.upgrade_${UPGRADEDLL_UNIQUE}

  IntCmpU $R0 $R2 0 upgradedll.done_${UPGRADEDLL_UNIQUE} upgradedll.upgrade_${UPGRADEDLL_UNIQUE}
  IntCmpU $R1 $R3 upgradedll.done_${UPGRADEDLL_UNIQUE} upgradedll.done_${UPGRADEDLL_UNIQUE} \
    upgradedll.upgrade_${UPGRADEDLL_UNIQUE}

  ;------------------------
  ;Let's upgrade the DLL!

  SetOverwrite try

  upgradedll.upgrade_${UPGRADEDLL_UNIQUE}:
    !ifndef UPGRADEDLL_NOREGISTER
      ;Unregister the DLL
      UnRegDLL $R4
    !endif

  ;------------------------
  ;Try to copy the DLL directly

  ClearErrors
    StrCpy $R0 $R4
    Call :upgradedll.file_${UPGRADEDLL_UNIQUE}
  IfErrors 0 upgradedll.noreboot_${UPGRADEDLL_UNIQUE}

  ;------------------------
  ;DLL is in use. Copy it to a temp file and Rename it on reboot.

  GetTempFileName $R0 $R5
    Call :upgradedll.file_${UPGRADEDLL_UNIQUE}
  Rename /REBOOTOK $R0 $R4

  ;------------------------
  ;Register the DLL on reboot

  !ifndef UPGRADEDLL_NOREGISTER
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\RunOnce" \
      "Register $R4" 'rundll32.exe "$R4",DllRegisterServer'
  !endif

  Goto upgradedll.done_${UPGRADEDLL_UNIQUE}

  ;------------------------
  ;DLL does not exist - just extract

  upgradedll.copy_${UPGRADEDLL_UNIQUE}:
    StrCpy $R0 $R4
    Call :upgradedll.file_${UPGRADEDLL_UNIQUE}

  ;------------------------
  ;Register the DLL

  upgradedll.noreboot_${UPGRADEDLL_UNIQUE}:
    !ifndef UPGRADEDLL_NOREGISTER
      RegDLL $R4
    !endif

  ;------------------------
  ;Done

  upgradedll.done_${UPGRADEDLL_UNIQUE}:

  Pop $R5
  Pop $R4
  Pop $R3
  Pop $R2
  Pop $R1
  Pop $R0

  ;------------------------
  ;End

  Goto upgradedll.end_${UPGRADEDLL_UNIQUE}

  ;------------------------
  ;Called to extract the DLL

  upgradedll.file_${UPGRADEDLL_UNIQUE}:
    File /oname=$R0 "${LOCALFILE}"
    Return

  upgradedll.end_${UPGRADEDLL_UNIQUE}:

 ;------------------------
 ;Restore settings

 SetOverwrite lastused
 
 !undef UPGRADEDLL_UNIQUE

!macroend

!endif