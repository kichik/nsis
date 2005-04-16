/*

***********************
Macro - Install Library
***********************

This macro can be used to install DLL and TLB libraries. It checks for version numbers and Windows file protection,
registers the files and can update files on reboot.

To ask the user for a reboot if required, use the Modern UI with a Finish page or use IfRebootFlag and make your
own page or message box.

Usage:

!insertmacro InstallLib libtype shared install localfile destfile tempbasedir

Parameters:

libtype			The type of the library

				DLL				Dynamic link library (DLL)
				REGDLL			DLL that has to be registered
				TLB 			Type library or DLL that contains a type LIBRARY
				REGDLLTLB		DLL that has to be registered and contains a type library

shared			Specify whether the library is shared with other applications

				NOTSHARED		The library is not shared
				$VARNAME		Variable that is empty when the application is installed for the first time,
								which is when the shared library count will be increased.

install			Specify the installation method

				REBOOT_PROTECTED		* Upgrade the library on reboot when in use (required for system files).
										* Upgrade the library if the file is not protected by Windows File Protection.

				NOREBOOT_PROTECTED		* Warns the user when the library is in use. The user will have to close
										  applications using the library.
										* Upgrade the library if the file is not protected by Windows File Protection.

				REBOOT_NOTPROTECTED		* Upgrade the library on reboot when in use (required for system files).
										* Upgrade the library without checking for Windows File Protection.

				NOREBOOT_NOTPROTECTED	* Warns the user when the library is in use. The user will have to close
										  applications using the library.
										* Upgrade the library without checking for Windows File Protection.

localfile		Location of the library on the compiler system

destfile		Location to store the library on the user's system

tempbasedir		Directory on the user's system to store a temporary file when the system has
				to be rebooted.

				For Windows 9x/ME support, this directory should be on the same volume as the
				destination file (destfile).
				The Windows temp directory could be located on any volume, so you cannot use
				this directory.

Options:

LIBRARY_SHELL_EXTENSION

		Define this before inserting InstallLib macro to call SHChangeNotify with SHCNE_ASSOCCHANGED after registration. Use this to refresh the shell when installing a shell extension or when changing file associations.

LIBRARY_COM

		Define this before inserting InstallLib macro to call CoFreeUnusedLibraries after registration. Use this for unloading all unnecessary libraries from memory when installing COM libraries.

Notes:

* If you want to support Windows 9x/ME, you can only use short filenames (8.3).

* You can only compile scripts using this macro on Windows systems.

------------------------

Example:

Var ALREADY_INSTALLED
;Add code here that sets $ALREADY_INSTALLED to a non-zero value if the application is
;already installed.

!insertmacro InstallLib REGDLL $ALREADY_INSTALLED REBOOT_NOTPROTECTED dllname.dll $SYSDIR\dllname.dll $SYSDIR


*************************
Macro - Uninstall Library
*************************

This macro can be used to uninstall DLL and TLB libraries. It unregisters files and can remove them on reboot.

Usage:

!insertmacro UnInstallLib libtype shared uninstall file

Parameters:

libtype			The type of the library

				DLL				Dynamic link library (DLL)
				REGDLL			DLL that has to be registered
				TLB				Type library or DLL that contains a type LIBRARY
				REGTLB			DLL that has to be registered and contains a type library

shared			Specify whether the library is shared with other applications

				NOTSHARED		The library is not shared
				SHARE			The library is shared and should be removed if the shared library count
								indicates that the file is not in use anymore.

uninstall		Specify the uninstallation method

				NOREMOVE				The library should not be removed.
										You should use this option for common or important system files such as the
										Visual Basic/C++/MFC runtimes.

				REBOOT_PROTECTED		* Remove the library on reboot when in use (required for system files).
										* Remove the library if the file is not protected by Windows File Protection.

				NOREBOOT_PROTECTED		* Warns the user when the library is in use. The user will have to close
										  applications using the library.
										* Remove the library if the file is not protected by Windows File Protection.

				REBOOT_NOTPROTECTED		* Remove the library on reboot when in use (required for system files).
										* Remove the library without checking for Windows File Protection.

				NOREBOOT_NOTPROTECTED	* Warns the user when the library is in use. The user will have to close
										  applications using the library.
										* Remove the library without checking for Windows File Protection.

file			Location of the library

Options:

LIBRARY_SHELL_EXTENSION

		Define this before inserting UninstallLib macro to call SHChangeNotify with SHCNE_ASSOCCHANGED after unregistration. Use this to refresh the shell when uninstalling a shell extension or when changing file associations.

LIBRARY_COM

		Define this before inserting UninstallLib macro to call CoFreeUnusedLibraries after unregistration. Use this for unloading all unnecessary libraries from memory when uninstalling COM libraries.

------------------------

Example:

!insertmacro UnInstallLib REGDLL SHARED REBOOT_NOTPROTECTED $SYSDIR\dllname.dll

*/

!ifndef LIB_INCLUDED

!define LIB_INCLUDED

!ifndef SHCNE_ASSOCCHANGED
  !define SHCNE_ASSOCCHANGED 0x08000000
!endif
!ifndef SHCNF_IDLIST
  !define SHCNF_IDLIST 0x0000
!endif

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

  !define INSTALLLIB_UNIQUE ${__LINE__}

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

    StrCmp ${shared} "" 0 installlib.noshareddllincrease_${INSTALLLIB_UNIQUE}

      ReadRegDword $R0 HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R4
      ClearErrors
      IntOp $R0 $R0 + 1
      WriteRegDWORD HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R4 $R0

    installlib.noshareddllincrease_${INSTALLLIB_UNIQUE}:

  !endif

  ;------------------------
  ;Check Windows File Protection

  !ifdef INSTALLLIB_INSTALL_REBOOT_PROTECTED | INSTALLLIB_INSTALL_NOREBOOT_PROTECTED

    !define LIBRARY_DEFINE_DONE_LABEL

    System::Call "sfc::SfcIsFileProtected(i 0, w R4) i.R0"

      StrCmp $R0 "error" installlib.notprotected_${INSTALLLIB_UNIQUE}
      StrCmp $R0 "0" installlib.notprotected_${INSTALLLIB_UNIQUE}

    Goto installlib.done_${INSTALLLIB_UNIQUE}

    installlib.notprotected_${INSTALLLIB_UNIQUE}:

  !endif

  ;------------------------
  ;Check file

  IfFileExists $R4 0 installlib.copy_${INSTALLLIB_UNIQUE}

  ;------------------------
  ;Get version information

  !execute '"${NSISDIR}\Bin\LibraryLocal.exe" D ${LOCALFILE}'
  !include "${NSISDIR}\Bin\LibraryLocal.nsh"

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

      IntCmpU $R0 $R2 0 installlib.register_${INSTALLLIB_UNIQUE} installlib.upgrade_${INSTALLLIB_UNIQUE}
      IntCmpU $R1 $R3 installlib.register_${INSTALLLIB_UNIQUE} installlib.register_${INSTALLLIB_UNIQUE} \
        installlib.upgrade_${INSTALLLIB_UNIQUE}

    !else

      !execute '"${NSISDIR}\Bin\LibraryLocal.exe" T ${LOCALFILE}'
      !include "${NSISDIR}\Bin\LibraryLocal.nsh"

      !ifdef LIBRARY_VERSION_FILENOTFOUND
        !error "InstallLib: The library ${LOCALFILE} could not be found."
      !endif

      !ifndef LIBRARY_VERSION_NONE

        IntCmpU $R0 $R2 0 installlib.register_${INSTALLLIB_UNIQUE} installlib.upgrade_${INSTALLLIB_UNIQUE}
        IntCmpU $R1 $R3 0 installlib.register_${INSTALLLIB_UNIQUE} \
          installlib.upgrade_${INSTALLLIB_UNIQUE}

      !else

        IntCmpU $R0 $R2 0 installlib.register_${INSTALLLIB_UNIQUE} installlib.upgrade_${INSTALLLIB_UNIQUE}
        IntCmpU $R1 $R3 installlib.register_${INSTALLLIB_UNIQUE} installlib.register_${INSTALLLIB_UNIQUE} \
          installlib.upgrade_${INSTALLLIB_UNIQUE}

      !endif

    !endif

  !else

    !undef LIBRARY_VERSION_NONE

    !ifdef INSTALLLIB_LIBTYPE_TLB | INSTALLLIB_LIBTYPE_REGDLLTLB

      !execute '"${NSISDIR}\Bin\LibraryLocal.exe" T ${LOCALFILE}'
      !include "${NSISDIR}\Bin\LibraryLocal.nsh"

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
      Pop $R2
      Pop $R3

      IntCmpU $R0 $R2 0 installlib.register_${INSTALLLIB_UNIQUE} installlib.upgrade_${INSTALLLIB_UNIQUE}
      IntCmpU $R1 $R3 installlib.register_${INSTALLLIB_UNIQUE} installlib.register_${INSTALLLIB_UNIQUE} \
        installlib.upgrade_${INSTALLLIB_UNIQUE}

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

    installlib.upgrade_${INSTALLLIB_UNIQUE}:

  !endif

  ;------------------------
  ;Copy

  !ifdef INSTALLLIB_INSTALL_NOREBOOT_PROTECTED | INSTALLLIB_INSTALL_NOREBOOT_NOTPROTECTED

    installlib.copy_${INSTALLLIB_UNIQUE}:

    StrCpy $R0 $R4
    Call :installlib.file_${INSTALLLIB_UNIQUE}

  !else

    !ifndef LIBRARY_DEFINE_REGISTER_LABEL

      !define LIBRARY_DEFINE_REGISTER_LABEL

    !endif

    !ifndef LIBRARY_DEFINE_DONE_LABEL

      !define LIBRARY_DEFINE_DONE_LABEL

    !endif

    ClearErrors

    StrCpy $R0 $R4
    Call :installlib.file_${INSTALLLIB_UNIQUE}

    IfErrors 0 installlib.register_${INSTALLLIB_UNIQUE}

    SetOverwrite lastused

    ;------------------------
    ;Copy on reboot

    GetTempFileName $R0 $R5
    Call :installlib.file_${INSTALLLIB_UNIQUE}
    Rename /REBOOTOK $R0 $R4

    ;------------------------
    ;Register on reboot

    !ifdef INSTALLLIB_LIBTYPE_REGDLL | INSTALLLIB_LIBTYPE_TLB | INSTALLLIB_LIBTYPE_REGDLLTLB

      ReadRegStr $R0 HKLM "Software\Microsoft\Windows\CurrentVersion\RunOnce" "NSIS.Library.RegTool"
      IfFileExists $R0 installlib.rebootreg_${INSTALLLIB_UNIQUE}

        File /oname=$R5\NSIS.Library.RegTool.exe "${NSISDIR}\Contrib\Library\RegTool\RegTool.bin"
        WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\RunOnce" \
          "NSIS.Library.RegTool" '"$R5\NSIS.Library.RegTool.exe"'

      installlib.rebootreg_${INSTALLLIB_UNIQUE}:

    !endif

    !ifdef INSTALLLIB_LIBTYPE_REGDLL

      WriteRegStr HKLM "Software\NSIS.Library.RegTool" "$R4" 'D'

    !endif

    !ifdef INSTALLLIB_LIBTYPE_TLB

      WriteRegStr HKLM "Software\NSIS.Library.RegTool" "$R4" 'T'

    !endif

    !ifdef INSTALLLIB_LIBTYPE_REGDLLTLB

      WriteRegStr HKLM "Software\NSIS.Library.RegTool" "$R4" 'DT'

    !endif

    Goto installlib.done_${INSTALLLIB_UNIQUE}

    installlib.copy_${INSTALLLIB_UNIQUE}:
      StrCpy $R0 $R4
      Call :installlib.file_${INSTALLLIB_UNIQUE}

  !endif

  ;------------------------
  ;Register

  !ifdef LIBRARY_DEFINE_REGISTER_LABEL

    !undef LIBRARY_DEFINE_REGISTER_LABEL

    installlib.register_${INSTALLLIB_UNIQUE}:

  !endif

  !ifdef INSTALLLIB_LIBTYPE_REGDLL | INSTALLLIB_LIBTYPE_REGDLLTLB

    RegDLL $R4

  !endif

  !ifdef INSTALLLIB_LIBTYPE_TLB | INSTALLLIB_LIBTYPE_REGDLLTLB

    TypeLib::Register $R4

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

    installlib.done_${INSTALLLIB_UNIQUE}:

  !endif

  Pop $R5
  Pop $R4
  Pop $R3
  Pop $R2
  Pop $R1
  Pop $R0

  ;------------------------
  ;End

  Goto installlib.end_${INSTALLLIB_UNIQUE}

  ;------------------------
  ;Extract

  !ifdef INSTALLLIB_INSTALL_REBOOT_PROTECTED | INSTALLLIB_INSTALL_REBOOT_NOTPROTECTED

    SetOverwrite try

    installlib.file_${INSTALLLIB_UNIQUE}:
      File /oname=$R0 "${LOCALFILE}"
      Return

    installlib.end_${INSTALLLIB_UNIQUE}:

    SetOverwrite lastused

  !else

    SetOverwrite on

    installlib.file_${INSTALLLIB_UNIQUE}:
      File /oname=$R0 "${LOCALFILE}"
      Return

    installlib.end_${INSTALLLIB_UNIQUE}:

    SetOverwrite lastused

  !endif

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

!macro UnInstallLib libtype shared uninstall file

  !verbose push
  !verbose 3

  Push $R0
  Push $R1

  ;------------------------
  ;Define

  !define UNINSTALLLIB_UNIQUE ${__LINE__}

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

    ReadRegDword $R0 HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1
    StrCmp $R0 "" uninstalllib.shareddlldone_${UNINSTALLLIB_UNIQUE}

    IntOp $R0 $R0 - 1
    IntCmp $R0 0 uninstalllib.shareddllremove_${UNINSTALLLIB_UNIQUE} \
      uninstalllib.shareddllremove_${UNINSTALLLIB_UNIQUE} uninstalllib.shareddllinuse_${UNINSTALLLIB_UNIQUE}

    uninstalllib.shareddllremove_${UNINSTALLLIB_UNIQUE}:
      DeleteRegValue HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1
      !ifndef UNINSTALLLIB_SHARED_SHAREDNOREMOVE
        Goto uninstalllib.shareddlldone_${UNINSTALLLIB_UNIQUE}
      !endif

    uninstalllib.shareddllinuse_${UNINSTALLLIB_UNIQUE}:
      WriteRegDWORD HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1 $R0
      Goto uninstalllib.done_${UNINSTALLLIB_UNIQUE}

  uninstalllib.shareddlldone_${UNINSTALLLIB_UNIQUE}:

  !endif

  ;------------------------
  ;Remove

  !ifndef UNINSTALLLIB_UNINSTALL_NOREMOVE

    ;------------------------
    ;Check Windows File Protection

    !ifdef UNINSTALLLIB_UNINSTALL_REBOOT_PROTECTED | UNINSTALLLIB_UNINSTALL_NOREBOOT_PROTECTED

      System::Call "sfc::SfcIsFileProtected(i 0, w $R1) i.R0"

        StrCmp $R0 "error" uninstalllib.notprotected_${UNINSTALLLIB_UNIQUE}
        StrCmp $R0 "0" uninstalllib.notprotected_${UNINSTALLLIB_UNIQUE}

      Goto uninstalllib.done_${UNINSTALLLIB_UNIQUE}

      uninstalllib.notprotected_${UNINSTALLLIB_UNIQUE}:

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

  uninstalllib.done_${UNINSTALLLIB_UNIQUE}:

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
