# NSIS Update
#####################################################################
# Original version Copyright (C) 2002-2003 Nathan Purciful.
# Version for NSIS distribution Copyright (C) 2003 Joost Verburg.
#
# This software is provided 'as-is', without any express or implied
# warranty.  In no event will the authors be held liable for any 
# damages arising from the use of this software.
#
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute
# it freely, subject to the following restrictions:
#
# 1. The origin of this software must not be misrepresented; you must
#    not claim that you wrote the original software. If you use this
#    software in a product, an acknowledgment in the product
#    documentation would be appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must
#    not be misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source
#    distribution.
#
# This program uses CVSNT software, http://www.cvsnt.org/
#
#####################################################################
# Defines

!define MISSINGFILES $0
!define NSISBINPATH $1

!define TEMP1 $R0
!define TEMP2 $R1
!define TEMP3 $R2

#####################################################################
# Modern UI

!define MUI_PRODUCT "NSIS Update"
!define MUI_VERSION ""
!define MUI_BRANDINGTEXT " "
!define MUI_UI "Resources\GUI\NSISUpdate.exe"
!define MUI_ICON "${NSISDIR}\Contrib\Icons\yi-simple2_install.ico"

!define MUI_CUSTOMPAGECOMMANDS

!include "MUI.nsh"

!insertmacro MUI_SYSTEM

!insertmacro MUI_LANGUAGE "English"

Page custom UpdateMethod ": Update Method"
!insertmacro MUI_PAGECOMMAND_INSTFILES

#####################################################################
# Configuration

Caption /LANG=${LANG_ENGLISH} "${MUI_PRODUCT}"
OutFile "..\..\Bin\NSISUpdate.exe"
InstallButtonText "Update"
ShowInstDetails show

#####################################################################
# Macros

!macro checkFile PATH FILE

  IfFileExists "${PATH}\${FILE}" +2
    StrCpy ${MISSINGFILES} "${FILE} ${MISSINGFILES}"
    
!macroend

!macro checkFileDownload PATH FILE

  IfFileExists "${PATH}\${FILE}" "Done_${FILE}"
    
    IfFileExists "$PLUGINSDIR\bzip2.exe" +3"
      File "/oname=$PLUGINSDIR\bzip2.exe" "Resources\bin\bzip2.exe"
    
    NSISdl::download "http://nsis.sourceforge.net/nsisupdate/${FILE}.bz2" "${PATH}\${FILE}.bz2"
    Pop ${TEMP1}
    
    StrCmp ${TEMP1} "success" "Extract_${FILE}"
      MessageBox MB_OK|MB_ICONSTOP "Download failed: ${TEMP1}."
      Quit
      
    "Extract_${FILE}:"
      nsExec::ExecToLog '"$PLUGINSDIR\bzip2.exe" -vd "${PATH}\${FILE}.bz2"'
      
    IfFileExists "${PATH}\${FILE}" "Done_${FILE}"
      MessageBox MB_OK|MB_ICONSTOP "Extraction failed."
      Quit
    
    "Done_${FILE}:"
    
!macroend

#####################################################################
# Functions

Function .onInit
  
  Call GetInstallerFile
  Pop $R0
  
  StrCpy $R0 $R0 "" -14
    StrCmp $R0 "NSISUpdate.bin" temp
    
    # Create a temporary file, so NSIS Update can update itself
    
    CopyFiles /SILENT "$EXEDIR\NSISUpdate.exe" "$TEMP\NSISUpdate.bin"
    Exec '"$TEMP\NSISUpdate.bin" $EXEDIR'
    Quit
    
  temp:

  # Close the NSIS Menu (files in use cannot be updated)
  
  Call CloseMenu

  # Remove temporary file on next reboot
  
  Delete /REBOOTOK "$TEMP\NSISUpdate.bin"
  
  # Get NSIS directory
  
  Call GetParameters
  Pop ${NSISBINPATH}
  
  # InstallOptions INI File for the "Update Method" dialog
  
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS "Resources\GUI\io.ini" "io.ini"
  
  # InitPluginsDir called by Modern UI InstallOptions extract macro

FunctionEnd

Function CheckCVSAccess
  
  IfFileExists "${NSISBINPATH}\..\Cvs\Root" +2
    Return
  
  Push ${TEMP1}
  Push ${TEMP2}
    
    FileOpen ${TEMP1} "${NSISBINPATH}\..\CVS\Root" r
    FileRead ${TEMP1} ${TEMP2} 9
    FileClose ${TEMP1}
    
    StrCmp ${TEMP2} ":pserver:" AccessOK
      
      MessageBox MB_OK|MB_ICONSTOP "NSIS Update only supports anonymous CVS access.$\r$\nNSIS developers should use a client with support for the :ext: access mode."
      Quit
    
    AccessOK:
      
  Pop ${TEMP2}
  Pop ${TEMP1}
    
FunctionEnd

Function CheckCVSFiles

  !insertmacro checkFile "${NSISBINPATH}" "cvs95.exe"
  !insertmacro checkFile "$SYSDIR" "msvcr70.dll"
  !insertmacro checkFile "$SYSDIR" "msvcp70.dll"
  !insertmacro checkFile "${NSISBINPATH}" "pserver_protocol.dll"
  
  StrCmp ${MISSINGFILES} "" done
    MessageBox MB_YESNO|MB_ICONQUESTION "NSIS update has to download a few small CVS client files in order to be able to update your NSIS files.$\r$\nThese files only have to be download once. Do you want to download them now?$\r$\n$\r$\nRequired Files: ${MISSINGFILES}" IDYES Done
    Quit
    
  done:
  
FunctionEnd

Function CheckCVSDownload

  StrCmp ${MISSINGFILES} "" done
  
    SendMessage ${TEMP3} ${WM_SETTEXT} 0 "STR:Downloading CVS client files..."
  
    !insertmacro checkFileDownload "${NSISBINPATH}" "cvs95.exe"
    !insertmacro checkFileDownload "$SYSDIR" "msvcr70.dll"
    !insertmacro checkFileDownload "$SYSDIR" "msvcp70.dll"
    !insertmacro checkFileDownload "${NSISBINPATH}" "pserver_protocol.dll"
    
  done:
  
FunctionEnd

Function CheckCVSData

  IfFileExists "${NSISBINPATH}\..\CVS\Root" datainstalled
    
    IfFileExists "${NSISBINPATH}\InstallCVSData.exe" +3
      MessageBox MB_OK|MB_ICONSTOP "CVS Data Setup not found."
      Quit
    
    SetDetailsPrint listonly
    DetailPrint "Installing CVS data..."
    SetDetailsPrint none
    Exec "${NSISBINPATH}\InstallCVSData.exe"

  datainstalled:

FunctionEnd

Function UpdateMethod

  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "io.ini"
  
FunctionEnd

Function GetInstallerFile

  Push $R0
  Push $R1
  Push $R2
  
  StrCpy $R0 $CMDLINE 1
  StrCpy $R1 '"'
  StrCpy $R2 1
  StrCmp $R0 '"' loop
    StrCpy $R1 ' ' ; we're scanning for a space instead of a quote
 
  loop:
    StrCpy $R0 $CMDLINE 1 $R2
    StrCmp $R0 $R1 done
    StrCmp $R0 "" done
    IntOp $R2 $R2 + 1
    Goto loop
    
  done:
    StrCpy $R0 $CMDLINE $R2
  
  Pop $R2
  Pop $R1
  Exch $R0
  
FunctionEnd

Function GetParameters

  Push $R0
  Push $R1
  Push $R2
  
  StrCpy $R0 $CMDLINE 1
  StrCpy $R1 '"'
  StrCpy $R2 1
  StrCmp $R0 '"' loop
    StrCpy $R1 ' ' ; we're scanning for a space instead of a quote
  loop:
    StrCpy $R0 $CMDLINE 1 $R2
    StrCmp $R0 $R1 loop2
    StrCmp $R0 "" loop2
    IntOp $R2 $R2 + 1
    Goto loop
  loop2:
    IntOp $R2 $R2 + 1
    StrCpy $R0 $CMDLINE 1 $R2
    StrCmp $R0 " " loop2
  StrCpy $R0 $CMDLINE "" $R2
  
  Pop $R2
  Pop $R1
  Exch $R0
  
FunctionEnd

Function CloseMenu

  Push $R0

    FindWindow $R0 "NSIS Menu"
    IntCmp $R0 0 +2
      SendMessage $R0 ${WM_CLOSE} 0 0

  Pop $R0
  
FunctionEnd

Function ConnectInternet
    
  ClearErrors
  Dialer::AttemptConnect
  IfErrors noie3
    
  Pop $R0
  StrCmp $R0 "online" connected
    MessageBox MB_OK|MB_ICONSTOP "Cannot connect to the internet."
    Quit
  
  noie3:
  
  # IE3 not installed
  MessageBox MB_OK|MB_ICONINFORMATION "Please connect to the internet now."
  
  connected:
  
FunctionEnd

#####################################################################
# Update (Installer Section)

Section ""

  FindWindow ${TEMP3} "#32770" "" $HWNDPARENT
  GetDlgItem ${TEMP3} ${TEMP3} 1111
  
  SetDetailsPrint none
  
  Call ConnectInternet

  !insertmacro MUI_INSTALLOPTIONS_READ ${TEMP1} "io.ini" "Field 2" "State"
  StrCmp ${TEMP1} "1" "" CVS
  
    # Check for a new release
    
    SetDetailsPrint listonly
    
    SendMessage ${TEMP3} ${WM_SETTEXT} 0 "STR:Checking for a new NSIS release..."
    
    nsExec::ExecToStack '"${NSISBINPATH}\..\makensis.exe" "/version"'
    Pop ${TEMP1}
    
    StrCmp ${TEMP1} "error" "" +3
      MessageBox MB_OK|MB_ICONSTOP "Can't get NSIS version."
      Quit
    
    Pop ${TEMP1}
    DetailPrint "Your NSIS version: ${TEMP1}"
    DetailPrint ""
    
    StrCpy ${TEMP2} ${TEMP1} "" -5
    StrCmp ${TEMP2} "(CVS)" "" NoCVSVersion

      StrLen ${TEMP2} ${TEMP1}
      IntOp ${TEMP2} ${TEMP2} - 6
      StrCpy ${TEMP1} ${TEMP1} ${TEMP2}
      StrCpy ${TEMP2} 1
      
      DetailPrint "NOTE: You are using a development version of NSIS."
      DetailPrint "To get the latest files, use NSIS Update to download the development files."
      DetailPrint ""
      
      Goto CheckUpdate
    
    NoCVSVersion:
    
      StrCpy ${TEMP2} 0
    
    CheckUpdate:
    
    DetailPrint "Checking for a new release..."
    DetailPrint ""
    
    NSISdl::download_quiet "http://nsis.sourceforge.net/update.php?version=${TEMP1}&cvs=${TEMP2}" "$PLUGINSDIR\Update"    
    Pop ${TEMP1}
     
    StrCmp ${TEMP1} "success" ReadVersion
      MessageBox MB_OK|MB_ICONSTOP "Download failed: ${TEMP1}."
      Quit
    
    ReadVersion:
    
    FileOpen ${TEMP1} "$PLUGINSDIR\Update" r
    FileRead ${TEMP1} ${TEMP2}
    FileClose ${TEMP1}

    StrCmp ${TEMP2} "" "" +3
      MessageBox MB_OK|MB_ICONSTOP "Invalid version data."
      Quit
      
    StrCpy ${TEMP1} ${TEMP2} 1
    StrCpy ${TEMP2} ${TEMP2} "" 2
    
    SendMessage ${TEMP3} ${WM_SETTEXT} 0 "STR:Task completed."
    
    StrCmp ${TEMP1} "1" "" +3
      DetailPrint "A new stable release is available: ${TEMP2}"
      Goto UpdateMsg
    
    StrCmp ${TEMP1} "2" "" +3
      DetailPrint "A new preview release is available: ${TEMP2}"
      Goto UpdateMsg
      
    DetailPrint "No new release is available. Check again later."
    
    Goto done
    
    UpdateMsg:
    
    MessageBox MB_YESNO|MB_ICONQUESTION "A new release is available. Would you like to go to the download page?" IDNO done
    
      SetDetailsPrint none
      ExecShell "open" "http://sourceforge.net/project/showfiles.php?group_id=22049"
      Goto done
    
  CVS:
  
    # CVS Update
    
    SetOutPath ${NSISBINPATH}\..
    
    Call CheckCVSAccess
    Call CheckCVSFiles
    Call CheckCVSDownload
    Call CheckCVSData
    
    SetDetailsPrint listonly
    
    SendMessage ${TEMP3} ${WM_SETTEXT} 0 "STR:Updating your NSIS files..."
    
    DetailPrint "Initializing CVS Update..."
    
    !insertmacro MUI_INSTALLOPTIONS_READ ${TEMP1} "io.ini" "Field 3" "State"
    StrCmp ${TEMP1} "1" "" CleanCVSUpdate
      
      # Normal update
    
      nsExec::ExecToLog '"${NSISBINPATH}\cvs95.exe" -q -z3 update -d -P'
      Pop ${TEMP1}
      Goto CheckCVSReturn
      
    CleanCVSUpdate:
      
      # Clean copy
      
      nsExec::ExecToLog '"${NSISBINPATH}\cvs95.exe" -q -z3 update -C -d -P'
      Pop ${TEMP1}
    
    CheckCVSReturn:
    
      StrCmp ${TEMP1} "error" "" +3
        MessageBox MB_OK|MB_ICONSTOP "Can't execute CVS client."
        Quit
        
    SendMessage ${TEMP3} ${WM_SETTEXT} 0 "STR:Task completed."
      
  done:
  
  SetDetailsPrint none

SectionEnd