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
# Defines / Includes

!define MISSINGFILES $0
!define NSISBINPATH $1

!define TEMP1 $R0
!define TEMP2 $R1
!define TEMP3 $R2

!verbose 3
  !include "WinMessages.nsh"
!verbose 4

!addplugindir "${NSISDIR}\Contrib\NSIS Update\Resources\BIN"

#####################################################################
# Configuration

Name "NSIS Update"
Caption "NSIS Update"
OutFile "..\..\Bin\NSISUpdate.exe"
BrandingText " "

InstallButtonText "Update"
ShowInstDetails show
InstallColors /windows

ChangeUI IDD_INST "Resources\GUI\NSISUpdate.exe"
ChangeUI IDD_INSTFILES "Resources\GUI\NSISUpdate.exe"

Icon "${NSISDIR}\Contrib\Icons\yi-simple2_install.ico"

XPStyle on

Page custom UpdateMethod "" ": Update Method"
Page instfiles

#####################################################################
# Macros

!macro checkFile PATH FILE

  IfFileExists "${PATH}\${FILE}" +2
    StrCpy ${MISSINGFILES} "${FILE} ${MISSINGFILES}"
    
!macroend

!macro checkFileDownload PATH FILE

  IfFileExists "${PATH}\${FILE}" "Done_${FILE}"
        
    NSISdl::download "http://nsis.sourceforge.net/nsisupdate/${FILE}.dat" "${PATH}\${FILE}.dat"
    Pop ${TEMP1}
    
    StrCmp ${TEMP1} "success" "Extract_${FILE}"
      MessageBox MB_OK|MB_ICONSTOP "Download failed: ${TEMP1}."
      Quit
      
    Push ${TEMP1}
      
    "Extract_${FILE}:"
      ExtractDLL::extract "${PATH}\${FILE}" "${PATH}\${FILE}.dat"
      Delete "${PATH}\${FILE}.dat"
      
    Pop ${TEMP1}
    StrCmp ${TEMP1} "success" +3
      MessageBox MB_OK|MB_ICONSTOP "Extraction failed: ${TEMP1}."
      Quit
      
    Pop ${TEMP1}
    
    "Done_${FILE}:"
    
!macroend

#####################################################################
# Functions

Function .onInit
 
  Push ${TEMP1}
 
  Call GetInstallerFile
  Pop ${TEMP1}
  
  StrCpy ${TEMP1} ${TEMP1} "" -14
    StrCmp ${TEMP1} "NSISUpdate.bin" temp
    
    # Create a temporary file, so NSIS Update can update itself
    
    CopyFiles /SILENT "$EXEDIR\NSISUpdate.exe" "$TEMP\NSISUpdate.bin"
    Exec '"$TEMP\NSISUpdate.bin" $EXEDIR'
    Quit
    
  temp:
  
  Pop ${TEMP1}

  # Close the NSIS Menu (files in use cannot be updated)
  
  Call CloseMenu

  # Remove temporary file on next reboot
  
  Delete /REBOOTOK "$TEMP\NSISUpdate.bin"
  
  # Get NSIS directory
  
  Call GetParameters
  Pop ${NSISBINPATH}
  
  # InstallOptions INI File for the "Update Method" dialog
  
  InitPluginsdir
  File "/oname=$PLUGINSDIR\io.ini" "Resources\GUI\io.ini"

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

  Push ${TEMP1}

    InstallOptions::dialog "$PLUGINSDIR\io.ini"
    Push ${TEMP1}
  
  Pop ${TEMP1}
  
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

  Push $R0
    
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
  
  Pop $R0
  
FunctionEnd

Function TrimNewlines
  Exch $R0
  Push $R1
  Push $R2
  StrCpy $R1 0

loop:
  IntOp $R1 $R1 - 1
  StrCpy $R2 $R0 1 $R1
  StrCmp $R2 "$\r" loop
  StrCmp $R2 "$\n" loop
  IntOp $R1 $R1 + 1
  IntCmp $R1 0 no_trim_needed
  StrCpy $R0 $R0 $R1

no_trim_needed:
  Pop $R2
  Pop $R1
  Exch $R0
FunctionEnd

Function FindFiles
  Exch $R5 # callback function
  Exch 
  Exch $R4 # file name
  Exch 2
  Exch $R0 # directory
  Push $R1
  Push $R2
  Push $R3
  Push $R6

  Push $R0 # first dir to search

  StrCpy $R3 1

  nextDir:
    Pop $R0
    IntOp $R3 $R3 - 1
    ClearErrors
    FindFirst $R1 $R2 "$R0\*.*"
    nextFile:
      StrCmp $R2 "." gotoNextFile
      StrCmp $R2 ".." gotoNextFile

      StrCmp $R2 $R4 0 isDir
        Push "$R0\$R2"
        Call $R5
        Pop $R6
        StrCmp $R6 "stop" 0 isDir
          loop:
            StrCmp $R3 0 done
            Pop $R0
            IntOp $R3 $R3 - 1
            Goto loop

      isDir:
        IfFileExists "$R0\$R2\*.*" 0 gotoNextFile
          IntOp $R3 $R3 + 1
          Push "$R0\$R2"

  gotoNextFile:
    FindNext $R1 $R2
    IfErrors 0 nextFile

  done:
    FindClose $R1
    StrCmp $R3 0 0 nextDir

  Pop $R6
  Pop $R3
  Pop $R2
  Pop $R1
  Pop $R0
  Pop $R5
  Pop $R4
FunctionEnd

!macro CallFindFiles DIR FILE CBFUNC
Push ${DIR}
Push ${FILE}
Push $0
GetFunctionAddress $0 ${CBFUNC}
Exch $0
Call FindFiles
!macroend

Function PatchCVSRoot
  Pop $5
  FileOpen $6 $5 "r"
  FileRead $6 $7
  FileClose $6
  Push $7
  Call TrimNewLines
  Pop $7
  StrCmp $7 ":pserver:anonymous@cvs1:/cvsroot/nsis" go
    Push "stop"
    Return
go:
  FileOpen $6 $5 "w"
  FileWrite $6 ":pserver:anonymous@cvs.sourceforge.net:/cvsroot/nsis$\r$\n"
  FileClose $6
  Push "go"
FunctionEnd

#####################################################################
# Update (Installer Section)

Section ""

  FindWindow ${TEMP3} "#32770" "" $HWNDPARENT
  GetDlgItem ${TEMP3} ${TEMP3} 1111
  
  SetDetailsPrint none
  
  Call ConnectInternet

  ReadINIStr ${TEMP1} "$PLUGINSDIR\io.ini" "Field 2" "State"
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
      DetailPrint "A new pre-release is available: ${TEMP2}"
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
    # patch CVS Root files that come from the development snapshot
    GetFullPathName $9 $OUTDIR
    !insertmacro CallFindFiles $9 Root PatchCVSRoot

    SetDetailsPrint listonly
    
    SendMessage ${TEMP3} ${WM_SETTEXT} 0 "STR:Updating your NSIS files..."
    
    DetailPrint "Initializing CVS Update..."
    
    ReadINIStr ${TEMP1} "$PLUGINSDIR\io.ini" "Field 3" "State"
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