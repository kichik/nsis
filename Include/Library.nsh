#
# Library.nsh
#
# A system for the installation and uninstallation of dynamic
# link libraries (DLL) and type libraries (TLB). Using this
# system you can handle the complete setup with one single
# line of code:
#
#  * File copying
#  * File copying on reboot
#  * Version checks
#  * Registration and unregistration
#  * Registration and unregistration on reboot
#  * Shared DLL counting
#  * Windows File Protection checks
#
# For more information, read appendix B in the documentation.
#

!ifndef LIB_INCLUDED

!define LIB_INCLUDED

!ifndef SHCNE_ASSOCCHANGED
  !define SHCNE_ASSOCCHANGED 0x08000000
!endif
!ifndef SHCNF_IDLIST
  !define SHCNF_IDLIST 0x0000
!endif

### Initialize session id (GUID)
!macro __InstallLib_Helper_InitSession

  !ifndef __InstallLib_SessionGUID_Defined

    !define __InstallLib_SessionGUID_Defined

    Var /GLOBAL __INSTALLLLIB_SESSIONGUID

  !endif

  !define __InstallLib_Helper_InitSession_Label "${__FILE__}${__LINE__}"

  StrCmp $__INSTALLLLIB_SESSIONGUID '' 0 "${__InstallLib_Helper_InitSession_Label}"

    System::Call 'ole32::CoCreateGuid(g .s)'
    Pop $__INSTALLLLIB_SESSIONGUID

  "${__InstallLib_Helper_InitSession_Label}:"

  !undef __InstallLib_Helper_InitSession_Label

!macroend

### Add a RegTool entry to register after reboot
!macro __InstallLib_Helper_AddRegToolEntry mode filename tempdir

  Push $R0
  Push $R1
  Push $R2
  Push $R3

  ;------------------------
  ;Copy the parameters

  Push "${filename}"
  Push "${tempdir}"

  Pop $R2 ; temporary directory
  Pop $R1 ; file name to register

  ;------------------------
  ;Initialize session id

  !insertmacro __InstallLib_Helper_InitSession

  ;------------------------
  ;Advance counter

  StrCpy $R0 0
  ReadRegDWORD $R0 HKLM "Software\NSIS.Library.RegTool.v2\$__INSTALLLLIB_SESSIONGUID" "count"
  IntOp $R0 $R0 + 1
  WriteRegDWORD HKLM "Software\NSIS.Library.RegTool.v2\$__INSTALLLLIB_SESSIONGUID" "count" "$R0"

  ;------------------------
  ;Setup RegTool

  ReadRegStr $R3 HKLM "Software\Microsoft\Windows\CurrentVersion\RunOnce" "NSIS.Library.RegTool.v2"
  StrCpy $R3 $R3 -4 1
  IfFileExists $R3 +3

    File /oname=$R2\NSIS.Library.RegTool.v2.$__INSTALLLLIB_SESSIONGUID.exe "${NSISDIR}\Bin\RegTool.bin"
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\RunOnce" \
      "NSIS.Library.RegTool.v2" '"$R2\NSIS.Library.RegTool.v2.$__INSTALLLLIB_SESSIONGUID.exe" /S'

  ;------------------------
  ;Add RegTool entry

  WriteRegStr HKLM "Software\NSIS.Library.RegTool.v2\$__INSTALLLLIB_SESSIONGUID" "$R0.file" "$R1"
  WriteRegStr HKLM "Software\NSIS.Library.RegTool.v2\$__INSTALLLLIB_SESSIONGUID" "$R0.mode" "${mode}"

  Pop $R3
  Pop $R2
  Pop $R1
  Pop $R0

!macroend

### Get library version
!macro __InstallLib_Helper_GetVersion TYPE FILE

  !ifdef NSIS_WIN32_MAKENSIS

    !tempfile LIBRARY_TEMP_NSH
    !execute '"${NSISDIR}\Bin\LibraryLocal.exe" "${TYPE}" "${FILE}" "${LIBRARY_TEMP_NSH}"'
    !include "${LIBRARY_TEMP_NSH}"
    !delfile "${LIBRARY_TEMP_NSH}"
    !undef LIBRARY_TEMP_NSH

  !else

    !ifndef INSTALLLIB_GETVERSION_VARS_DEFINED

      !define INSTALLLIB_GETVERSION_VARS_DEFINED

      Var /GLOBAL INSTALLLIB_VER_LOW
      Var /GLOBAL INSTALLLIB_VER_HIGH

    !endif

    !define LIBRARY_VERSION_LOW $INSTALLLIB_VER_LOW
    !define LIBRARY_VERSION_HIGH $INSTALLLIB_VER_HIGH

    GetDLLVersionLocal "${FILE}" $INSTALLLIB_VER_HIGH $INSTALLLIB_VER_LOW

  !endif

!macroend

### Install library
!macro InstallLib libtype shared install localfile destfile tempbasedir

  !verbose push
  !verbose 3

  Push $R0
  Push $R1
  Push $R2
  Push $R3
  Push $R4
  Push $R5

  ;------------------------
  ;Define

  !define INSTALLLIB_UNIQUE "${__FILE__}${__LINE__}"

  !define INSTALLLIB_LIBTYPE_${libtype}
  !define INSTALLLIB_LIBTYPE_SET INSTALLLIB_LIBTYPE_${libtype}
  !define INSTALLLIB_SHARED_${shared}
  !define INSTALLLIB_SHARED_SET INSTALLLIB_SHARED_${shared}
  !define INSTALLLIB_INSTALL_${install}
  !define INSTALLLIB_INSTALL_SET INSTALLLIB_INSTALL_${install}

  ;------------------------
  ;Validate

  !ifndef INSTALLLIB_LIBTYPE_DLL & INSTALLLIB_LIBTYPE_REGDLL & INSTALLLIB_LIBTYPE_TLB & \
    INSTALLLIB_LIBTYPE_REGDLLTLB
    !error "InstallLib: Incorrect setting for parameter: libtype"
  !endif

  !ifndef INSTALLLIB_INSTALL_REBOOT_PROTECTED & INSTALLLIB_INSTALL_REBOOT_NOTPROTECTED & \
    INSTALLLIB_INSTALL_NOREBOOT_PROTECTED & INSTALLLIB_INSTALL_NOREBOOT_NOTPROTECTED
    !error "InstallLib: Incorrect setting for parameter: install"
  !endif

  ;------------------------
  ;Copy the parameters used on run-time to a variable
  ;This allows the usage of variables as parameter

  StrCpy $R4 "${destfile}"
  StrCpy $R5 "${tempbasedir}"

  ;------------------------
  ;Shared library count

  !ifndef INSTALLLIB_SHARED_NOTSHARED

    StrCmp ${shared} "" 0 "installlib.noshareddllincrease_${INSTALLLIB_UNIQUE}"

      ReadRegDword $R0 HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R4
      ClearErrors
      IntOp $R0 $R0 + 1
      WriteRegDWORD HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R4 $R0

    "installlib.noshareddllincrease_${INSTALLLIB_UNIQUE}:"

  !endif

  ;------------------------
  ;Check Windows File Protection

  !ifdef INSTALLLIB_INSTALL_REBOOT_PROTECTED | INSTALLLIB_INSTALL_NOREBOOT_PROTECTED

    !define LIBRARY_DEFINE_DONE_LABEL

    System::Call "sfc::SfcIsFileProtected(i 0, w R4) i.R0"

      StrCmp $R0 "error" "installlib.notprotected_${INSTALLLIB_UNIQUE}"
      StrCmp $R0 "0" "installlib.notprotected_${INSTALLLIB_UNIQUE}"

    Goto "installlib.done_${INSTALLLIB_UNIQUE}"

    "installlib.notprotected_${INSTALLLIB_UNIQUE}:"

  !endif

  ;------------------------
  ;Check file

  IfFileExists $R4 0 "installlib.copy_${INSTALLLIB_UNIQUE}"

  ;------------------------
  ;Get version information

  !insertmacro __InstallLib_Helper_GetVersion D "${LOCALFILE}"

  !ifdef LIBRARY_VERSION_FILENOTFOUND
    !error "InstallLib: The library ${LOCALFILE} could not be found."
  !endif

  !ifndef LIBRARY_VERSION_NONE

    !define LIBRARY_DEFINE_UPGRADE_LABEL
    !define LIBRARY_DEFINE_REGISTER_LABEL

    StrCpy $R0 ${LIBRARY_VERSION_HIGH}
    StrCpy $R1 ${LIBRARY_VERSION_LOW}

    GetDLLVersion $R4 $R2 $R3

    !undef LIBRARY_VERSION_HIGH
    !undef LIBRARY_VERSION_LOW

    !ifndef INSTALLLIB_LIBTYPE_TLB & INSTALLLIB_LIBTYPE_REGDLLTLB

      IntCmpU $R0 $R2 0 "installlib.register_${INSTALLLIB_UNIQUE}" "installlib.upgrade_${INSTALLLIB_UNIQUE}"
      IntCmpU $R1 $R3 "installlib.register_${INSTALLLIB_UNIQUE}" "installlib.register_${INSTALLLIB_UNIQUE}" \
        "installlib.upgrade_${INSTALLLIB_UNIQUE}"

    !else

      !insertmacro __InstallLib_Helper_GetVersion T "${LOCALFILE}"

      !ifdef LIBRARY_VERSION_FILENOTFOUND
        !error "InstallLib: The library ${LOCALFILE} could not be found."
      !endif

      !ifndef LIBRARY_VERSION_NONE

        IntCmpU $R0 $R2 0 "installlib.register_${INSTALLLIB_UNIQUE}" "installlib.upgrade_${INSTALLLIB_UNIQUE}"
        IntCmpU $R1 $R3 0 "installlib.register_${INSTALLLIB_UNIQUE}" \
          "installlib.upgrade_${INSTALLLIB_UNIQUE}"

      !else

        IntCmpU $R0 $R2 0 "installlib.register_${INSTALLLIB_UNIQUE}" "installlib.upgrade_${INSTALLLIB_UNIQUE}"
        IntCmpU $R1 $R3 "installlib.register_${INSTALLLIB_UNIQUE}" "installlib.register_${INSTALLLIB_UNIQUE}" \
          "installlib.upgrade_${INSTALLLIB_UNIQUE}"

      !endif

    !endif

  !else

    !undef LIBRARY_VERSION_NONE

    !ifdef INSTALLLIB_LIBTYPE_TLB | INSTALLLIB_LIBTYPE_REGDLLTLB

      !insertmacro __InstallLib_Helper_GetVersion T "${LOCALFILE}"

    !endif

  !endif

  !ifdef INSTALLLIB_LIBTYPE_TLB | INSTALLLIB_LIBTYPE_REGDLLTLB

    !ifndef LIBRARY_VERSION_NONE

      !ifndef LIBRARY_DEFINE_UPGRADE_LABEL

        !define LIBRARY_DEFINE_UPGRADE_LABEL

      !endif

      !ifndef LIBRARY_DEFINE_REGISTER_LABEL

        !define LIBRARY_DEFINE_REGISTER_LABEL

      !endif

      StrCpy $R0 ${LIBRARY_VERSION_HIGH}
      StrCpy $R1 ${LIBRARY_VERSION_LOW}

      TypeLib::GetLibVersion $R4
      Pop $R3
      Pop $R2

      IntCmpU $R0 $R2 0 "installlib.register_${INSTALLLIB_UNIQUE}" "installlib.upgrade_${INSTALLLIB_UNIQUE}"
      IntCmpU $R1 $R3 "installlib.register_${INSTALLLIB_UNIQUE}" "installlib.register_${INSTALLLIB_UNIQUE}" \
        "installlib.upgrade_${INSTALLLIB_UNIQUE}"

      !undef LIBRARY_VERSION_HIGH
      !undef LIBRARY_VERSION_LOW

    !else

      !undef LIBRARY_VERSION_NONE

    !endif

  !endif

  ;------------------------
  ;Upgrade

  !ifdef LIBRARY_DEFINE_UPGRADE_LABEL

    !undef LIBRARY_DEFINE_UPGRADE_LABEL

    "installlib.upgrade_${INSTALLLIB_UNIQUE}:"

  !endif

  ;------------------------
  ;Copy

  !ifdef INSTALLLIB_INSTALL_NOREBOOT_PROTECTED | INSTALLLIB_INSTALL_NOREBOOT_NOTPROTECTED

    "installlib.copy_${INSTALLLIB_UNIQUE}:"

    StrCpy $R0 $R4
    Call ":installlib.file_${INSTALLLIB_UNIQUE}"

  !else

    !ifndef LIBRARY_DEFINE_REGISTER_LABEL

      !define LIBRARY_DEFINE_REGISTER_LABEL

    !endif

    !ifndef LIBRARY_DEFINE_DONE_LABEL

      !define LIBRARY_DEFINE_DONE_LABEL

    !endif

    ClearErrors

    StrCpy $R0 $R4
    Call ":installlib.file_${INSTALLLIB_UNIQUE}"

    IfErrors 0 "installlib.register_${INSTALLLIB_UNIQUE}"

    SetOverwrite lastused

    ;------------------------
    ;Copy on reboot

    GetTempFileName $R0 $R5
    Call ":installlib.file_${INSTALLLIB_UNIQUE}"
    Rename /REBOOTOK $R0 $R4

    ;------------------------
    ;Register on reboot

    Call ":installlib.regonreboot_${INSTALLLIB_UNIQUE}"

    Goto "installlib.done_${INSTALLLIB_UNIQUE}"

    "installlib.copy_${INSTALLLIB_UNIQUE}:"
      StrCpy $R0 $R4
      Call ":installlib.file_${INSTALLLIB_UNIQUE}"

  !endif

  ;------------------------
  ;Register

  !ifdef LIBRARY_DEFINE_REGISTER_LABEL

    !undef LIBRARY_DEFINE_REGISTER_LABEL

    "installlib.register_${INSTALLLIB_UNIQUE}:"

  !endif

  !ifdef INSTALLLIB_LIBTYPE_REGDLL | INSTALLLIB_LIBTYPE_TLB | INSTALLLIB_LIBTYPE_REGDLLTLB

    !ifdef INSTALLLIB_INSTALL_REBOOT_PROTECTED | INSTALLLIB_INSTALL_REBOOT_NOTPROTECTED

      IfRebootFlag 0 "installlib.regnoreboot_${INSTALLLIB_UNIQUE}"

        Call ":installlib.regonreboot_${INSTALLLIB_UNIQUE}"

        Goto "installlib.registerfinish_${INSTALLLIB_UNIQUE}"

      "installlib.regnoreboot_${INSTALLLIB_UNIQUE}:"

    !endif

    !ifdef INSTALLLIB_LIBTYPE_TLB | INSTALLLIB_LIBTYPE_REGDLLTLB

      TypeLib::Register $R4

    !endif

    !ifdef INSTALLLIB_LIBTYPE_REGDLL | INSTALLLIB_LIBTYPE_REGDLLTLB

      RegDll $R4

    !endif

    !ifdef INSTALLLIB_INSTALL_REBOOT_PROTECTED | INSTALLLIB_INSTALL_REBOOT_NOTPROTECTED

      "installlib.registerfinish_${INSTALLLIB_UNIQUE}:"

    !endif

  !endif

  !ifdef LIBRARY_SHELL_EXTENSION

    System::Call 'Shell32::SHChangeNotify(i ${SHCNE_ASSOCCHANGED}, i ${SHCNF_IDLIST}, i 0, i 0)'

  !endif

  !ifdef LIBRARY_COM

    System::Call 'Ole32::CoFreeUnusedLibraries()'

  !endif

  ;------------------------
  ;Done

  !ifdef LIBRARY_DEFINE_DONE_LABEL

    !undef LIBRARY_DEFINE_DONE_LABEL

  "installlib.done_${INSTALLLIB_UNIQUE}:"

  !endif

  Pop $R5
  Pop $R4
  Pop $R3
  Pop $R2
  Pop $R1
  Pop $R0

  ;------------------------
  ;End

  Goto "installlib.end_${INSTALLLIB_UNIQUE}"

  ;------------------------
  ;Extract

  !ifdef INSTALLLIB_INSTALL_REBOOT_PROTECTED | INSTALLLIB_INSTALL_REBOOT_NOTPROTECTED

    SetOverwrite try

  !else

    SetOverwrite on

  !endif

  "installlib.file_${INSTALLLIB_UNIQUE}:"
    SetFileAttributes $R0 FILE_ATTRIBUTE_NORMAL
    ClearErrors
    File /oname=$R0 "${LOCALFILE}"
    Return

  SetOverwrite lastused

  ;------------------------
  ;Register on reboot

  !ifdef INSTALLLIB_INSTALL_REBOOT_PROTECTED | INSTALLLIB_INSTALL_REBOOT_NOTPROTECTED

    "installlib.regonreboot_${INSTALLLIB_UNIQUE}:"

      !ifdef INSTALLLIB_LIBTYPE_REGDLL
        !insertmacro __InstallLib_Helper_AddRegToolEntry 'D' "$R4" "$R5"
      !endif

      !ifdef INSTALLLIB_LIBTYPE_TLB
        !insertmacro __InstallLib_Helper_AddRegToolEntry 'T' "$R4" "$R5"
      !endif

      !ifdef INSTALLLIB_LIBTYPE_REGDLLTLB
        !insertmacro __InstallLib_Helper_AddRegToolEntry 'D' "$R4" "$R5"
        !insertmacro __InstallLib_Helper_AddRegToolEntry 'T' "$R4" "$R5"
      !endif

      Return

  !endif

  ;------------------------
  ;End label

  "installlib.end_${INSTALLLIB_UNIQUE}:"

  ;------------------------
  ;Undefine

  !undef INSTALLLIB_UNIQUE

  !undef ${INSTALLLIB_LIBTYPE_SET}
  !undef INSTALLLIB_LIBTYPE_SET
  !undef ${INSTALLLIB_SHARED_SET}
  !undef INSTALLLIB_SHARED_SET
  !undef ${INSTALLLIB_INSTALL_SET}
  !undef INSTALLLIB_INSTALL_SET

  !verbose pop

!macroend

### Uninstall library
!macro UnInstallLib libtype shared uninstall file

  !verbose push
  !verbose 3

  Push $R0
  Push $R1

  ;------------------------
  ;Define

  !define UNINSTALLLIB_UNIQUE "${__FILE__}${__LINE__}"

  !define UNINSTALLLIB_LIBTYPE_${libtype}
  !define UNINSTALLLIB_LIBTYPE_SET UNINSTALLLIB_LIBTYPE_${libtype}
  !define UNINSTALLLIB_SHARED_${shared}
  !define UNINSTALLLIB_SHARED_SET UNINSTALLLIB_SHARED_${shared}
  !define UNINSTALLLIB_UNINSTALL_${uninstall}
  !define UNINSTALLLIB_UNINSTALL_SET UNINSTALLLIB_UNINSTALL_${uninstall}

  ;------------------------
  ;Validate

  !ifndef UNINSTALLLIB_LIBTYPE_DLL & UNINSTALLLIB_LIBTYPE_REGDLL & UNINSTALLLIB_LIBTYPE_TLB & \
    UNINSTALLLIB_LIBTYPE_REGDLLTLB
    !error "UnInstallLib: Incorrect setting for parameter: libtype"
  !endif

  !ifndef UNINSTALLLIB_SHARED_NOTSHARED & UNINSTALLLIB_SHARED_SHARED
    !error "UnInstallLib: Incorrect setting for parameter: shared"
  !endif

  !ifndef UNINSTALLLIB_UNINSTALL_NOREMOVE & UNINSTALLLIB_UNINSTALL_REBOOT_PROTECTED & \
    UNINSTALLLIB_UNINSTALL_REBOOT_NOTPROTECTED & UNINSTALLLIB_UNINSTALL_NOREBOOT_PROTECTED & \
    UNINSTALLLIB_UNINSTALL_NOREBOOT_NOTPROTECTED
    !error "UnInstallLib: Incorrect setting for parameter: uninstall"
  !endif

  ;------------------------
  ;Copy the parameters used on run-time to a variable
  ;This allows the usage of variables as parameter

  StrCpy $R1 "${file}"

  ;------------------------
  ;Shared library count

  !ifdef UNINSTALLLIB_SHARED_SHARED

    !define UNINSTALLLIB_DONE_LABEL

    ReadRegDword $R0 HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1
    StrCmp $R0 "" "uninstalllib.shareddlldone_${UNINSTALLLIB_UNIQUE}"

    IntOp $R0 $R0 - 1
    IntCmp $R0 0 "uninstalllib.shareddllremove_${UNINSTALLLIB_UNIQUE}" \
      "uninstalllib.shareddllremove_${UNINSTALLLIB_UNIQUE}" "uninstalllib.shareddllinuse_${UNINSTALLLIB_UNIQUE}"

    "uninstalllib.shareddllremove_${UNINSTALLLIB_UNIQUE}:"
      DeleteRegValue HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1
      !ifndef UNINSTALLLIB_SHARED_SHAREDNOREMOVE
        Goto "uninstalllib.shareddlldone_${UNINSTALLLIB_UNIQUE}"
      !endif

    "uninstalllib.shareddllinuse_${UNINSTALLLIB_UNIQUE}:"
      WriteRegDWORD HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1 $R0
      Goto "uninstalllib.done_${UNINSTALLLIB_UNIQUE}"

  "uninstalllib.shareddlldone_${UNINSTALLLIB_UNIQUE}:"

  !endif

  ;------------------------
  ;Remove

  !ifndef UNINSTALLLIB_UNINSTALL_NOREMOVE

    ;------------------------
    ;Check Windows File Protection

    !ifdef UNINSTALLLIB_UNINSTALL_REBOOT_PROTECTED | UNINSTALLLIB_UNINSTALL_NOREBOOT_PROTECTED

      !ifndef UNINSTALLLIB_DONE_LABEL

        !define UNINSTALLLIB_DONE_LABEL

      !endif

      System::Call "sfc::SfcIsFileProtected(i 0, w $R1) i.R0"

        StrCmp $R0 "error" "uninstalllib.notprotected_${UNINSTALLLIB_UNIQUE}"
        StrCmp $R0 "0" "uninstalllib.notprotected_${UNINSTALLLIB_UNIQUE}"

      Goto "uninstalllib.done_${UNINSTALLLIB_UNIQUE}"

      "uninstalllib.notprotected_${UNINSTALLLIB_UNIQUE}:"

    !endif

    ;------------------------
    ;Unregister

    !ifdef UNINSTALLLIB_LIBTYPE_REGDLL | UNINSTALLLIB_LIBTYPE_REGDLLTLB

      UnRegDLL $R1

    !endif

    !ifdef UNINSTALLLIB_LIBTYPE_TLB | UNINSTALLLIB_LIBTYPE_REGDLLTLB

      TypeLib::UnRegister $R1

    !endif

    !ifdef LIBRARY_SHELL_EXTENSION

      System::Call 'Shell32::SHChangeNotify(i ${SHCNE_ASSOCCHANGED}, i ${SHCNF_IDLIST}, i 0, i 0)'

    !endif

    !ifdef LIBRARY_COM

      System::Call 'Ole32::CoFreeUnusedLibraries()'

    !endif

    ;------------------------
    ;Delete

    !ifdef UNINSTALLLIB_UNINSTALL_REBOOT_PROTECTED | UNINSTALLLIB_UNINSTALL_REBOOT_NOTPROTECTED

      Delete /REBOOTOK $R1

    !else

      Delete $R1

    !endif

  !endif

  ;------------------------
  ;Done

  !ifdef UNINSTALLLIB_DONE_LABEL

    !undef UNINSTALLLIB_DONE_LABEL

    "uninstalllib.done_${UNINSTALLLIB_UNIQUE}:"

  !endif

  Pop $R1
  Pop $R0

  ;------------------------
  ;Undefine

  !undef UNINSTALLLIB_UNIQUE

  !undef ${UNINSTALLLIB_LIBTYPE_SET}
  !undef UNINSTALLLIB_LIBTYPE_SET
  !undef ${UNINSTALLLIB_SHARED_SET}
  !undef UNINSTALLLIB_SHARED_SET
  !undef ${UNINSTALLLIB_UNINSTALL_SET}
  !undef UNINSTALLLIB_UNINSTALL_SET

  !verbose pop

!macroend

!endif
