; NSIS Update
;--------------------------------

; Original version Copyright (C) 2002-2003 Nathan Purciful.
; Version for NSIS distribution Copyright (C) 2003-2004 Joost Verburg.
;
; This software is provided 'as-is', without any express or implied
; warranty.  In no event will the authors be held liable for any 
; damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute
; it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim that you wrote the original software. If you use this
;    software in a product, an acknowledgment in the product
;    documentation would be appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source
;    distribution.
;
; This program uses CVSNT software, http://www.cvsnt.org/

;--------------------------------
;Header files

  !include "MUI.nsh"

  !addplugindir "${NSISDIR}\Contrib\NSIS Update"
  
;--------------------------------
;Configuration

  Name "NSIS Update"
  Caption "NSIS Update"
  OutFile "..\..\Bin\NSISUpdate.exe"
  BrandingText " "

  InstallButtonText "Update"
  ShowInstDetails show
  InstallColors /windows

;--------------------------------
;Variables

  Var TEMP1
  Var TEMP2
  Var TEMP3

  Var MISSINGFILES
  Var NSISBINPATH
  
  Var OUTDIRFULL

;--------------------------------
;Interface Settings

  !define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\arrow2-install.ico"

;--------------------------------
;Pages

  Page custom UpdateMethod UpdateLeave
  
  !define MUI_PAGE_HEADER_TEXT ""
  !define MUI_PAGE_HEADER_SUBTEXT ""
  !define MUI_INSTFILESPAGE_FINISHHEADER_TEXT "Task Completed"
  !define MUI_INSTFILESPAGE_FINISHHEADER_SUBTEXT "See the log window below for details."
  !define MUI_INSTFILESPAGE_ABORTHEADER_TEXT "Error"
  !define MUI_INSTFILESPAGE_ABORTHEADER_SUBTEXT "NSIS Update was not completed succesfully."
  
  !insertmacro MUI_PAGE_INSTFILES

;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Custom Interface  
  
  ChangeUI IDD_INSTFILES "UI.exe"

;--------------------------------
;Macros

!macro checkFile PATH FILE

  IfFileExists "${PATH}\${FILE}" +2
    StrCpy $MISSINGFILES "${FILE} $MISSINGFILES"
    
!macroend

!macro checkFileDownload PATH FILE

  IfFileExists "${PATH}\${FILE}" "Done_${FILE}"
        
    NSISdl::download "http://nsis.sourceforge.net/nsisupdate/${FILE}.dat" "${PATH}\${FILE}.dat"
    Pop $TEMP1
    
    StrCmp $TEMP1 "success" "Extract_${FILE}"
      MessageBox MB_OK|MB_ICONSTOP "Download failed: $TEMP1."
      Quit
      
    "Extract_${FILE}:"
      ExtractDLL::extract "${PATH}\${FILE}" "${PATH}\${FILE}.dat"
      Delete "${PATH}\${FILE}.dat"
      
    Pop $TEMP1
    StrCmp $TEMP1 "success" +3
      MessageBox MB_OK|MB_ICONSTOP "Extraction failed: $TEMP1."
      Quit
      
    "Done_${FILE}:"
    
!macroend

;--------------------------------
; Functions

Function .onInit

  System::Call 'kernel32::GetModuleFileNameA(i 0, t .R0, i 1024) i r1'
  
  StrCpy $R0 $R0 "" -14
    StrCmp $R0 "NSISUpdate.bin" temp
    
    ;Create a temporary file, so NSIS Update can update itself
    
    CopyFiles /SILENT "$EXEDIR\NSISUpdate.exe" "$TEMP\NSISUpdate.bin"
    Exec '"$TEMP\NSISUpdate.bin" $EXEDIR'
    Quit
    
  temp:
  
  ;Close the NSIS Menu (files in use cannot be updated)
  Call CloseMenu

  ;Remove temporary file on next reboot
  Delete /REBOOTOK "$TEMP\NSISUpdate.bin"
  
  ;Get NSIS directory
  Call GetParameters
  Pop $NSISBINPATH
  
  ;InstallOptions INI File for the "Update Method" dialog
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "Method.ini"
  
  ;Check for pserver CVS access
  IfFileExists "$NSISBINPATH\..\Cvs\Root" 0 done
  
  FileOpen $TEMP1 "$NSISBINPATH\..\CVS\Root" r
  FileRead $TEMP1 $TEMP2 9
  FileClose $TEMP1
  
  StrCmp $TEMP2 ":pserver:" done
  
    !insertmacro MUI_INSTALLOPTIONS_WRITE "Method.ini" "Field 2" "Flags" "DISABLED"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "Method.ini" "Field 3" "Flags" "DISABLED"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "Method.ini" "Field 5" "Flags" "DISABLED"
    !insertmacro MUI_INSTALLOPTIONS_WRITE "Method.ini" "Field 6" "Flags" "DISABLED"
  
  done:

FunctionEnd

Function UpdateMethod

  !insertmacro MUI_HEADER_TEXT "Update Method" "Please select how you would like to update your NSIS files."
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "Method.ini"

FunctionEnd

Function UpdateLeave

  !insertmacro MUI_INSTALLOPTIONS_READ $TEMP1 "Method.ini" "Field 1" "State"
  StrCmp $TEMP1 "1" nocvs
  
    StrCpy $MISSINGFILES ""
  
    Call CheckCVSData
    Call CheckCVSFiles
    Call CheckCVSDownload
  
  nocvs:

FunctionEnd

Function CheckCVSFiles

  !insertmacro checkFile "$NSISBINPATH" "cvs95.exe"
  !insertmacro checkFile "$SYSDIR" "msvcr70.dll"
  !insertmacro checkFile "$SYSDIR" "msvcp70.dll"
  !insertmacro checkFile "$NSISBINPATH" "pserver_protocol.dll"
  
  StrCmp $MISSINGFILES "" done
    MessageBox MB_YESNO|MB_ICONQUESTION "NSIS update has to download a few small CVS client files in order to be able to update your NSIS files.$\r$\nThese files only have to be download once. Do you want to download them now?$\r$\n$\r$\nRequired Files: $MISSINGFILES" IDYES Done
    Abort
    
  done:
  
FunctionEnd

Function CheckCVSDownload

  StrCmp $MISSINGFILES "" done
  
    !insertmacro MUI_HEADER_TEXT "Downloading..." "Please wait while NSIS Update downloads CVS client files."
  
    !insertmacro checkFileDownload "$NSISBINPATH" "cvs95.exe"
    !insertmacro checkFileDownload "$SYSDIR" "msvcr70.dll"
    !insertmacro checkFileDownload "$SYSDIR" "msvcp70.dll"
    !insertmacro checkFileDownload "$NSISBINPATH" "pserver_protocol.dll"
    
  done:
  
FunctionEnd

Function CheckCVSData

  IfFileExists "$NSISBINPATH\..\CVS\Root" datainstalled
    
    IfFileExists "$NSISBINPATH\InstallCVSData.exe" +3
      MessageBox MB_OK|MB_ICONSTOP "CVS Data Setup not found."
      Abort
    
    SetDetailsPrint listonly
    DetailPrint "Installing CVS data..."
    SetDetailsPrint none
    Exec "$NSISBINPATH\InstallCVSData.exe"

  datainstalled:

FunctionEnd

Function GetParameters

 Push $R0
 Push $R1
 Push $R2
 Push $R3
 
 StrCpy $R2 1
 StrLen $R3 $CMDLINE
 
 ;Check for quote or space
 StrCpy $R0 $CMDLINE $R2
 StrCmp $R0 '"' 0 +3
   StrCpy $R1 '"'
   Goto loop
 StrCpy $R1 " "
 
 loop:
   IntOp $R2 $R2 + 1
   StrCpy $R0 $CMDLINE 1 $R2
   StrCmp $R0 $R1 get
   StrCmp $R2 $R3 get
   Goto loop
 
 get:
   IntOp $R2 $R2 + 1
   StrCpy $R0 $CMDLINE 1 $R2
   StrCmp $R0 " " get
   StrCpy $R0 $CMDLINE "" $R2
 
 Pop $R3
 Pop $R2
 Pop $R1
 Exch $R0

FunctionEnd

Function CloseMenu

  FindWindow $TEMP1 "NSIS Menu"
  IntCmp $TEMP1 0 +2
    SendMessage $TEMP1 ${WM_CLOSE} 0 0
  
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
  
    ; IE3 not installed
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
  Exch $R5 ; callback function
  Exch 
  Exch $R4 ; file name
  Exch 2
  Exch $R0 ; directory
  Push $R1
  Push $R2
  Push $R3
  Push $R6

  Push $R0 ; first dir to search

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
  
  GetFunctionAddress $TEMP1 ${CBFUNC}
  
  Push $TEMP1
  
  Call FindFiles
!macroend

Function PatchCVSRoot
  Pop $TEMP1
  FileOpen $TEMP2 $TEMP1 "r"
  FileRead $TEMP2 $TEMP3
  FileClose $TEMP3
  Push $TEMP3
  Call TrimNewLines
  Pop $TEMP3
  StrCmp $TEMP3 ":pserver:anonymous:@cvs1:/cvsroot/nsis" go
    Push "stop"
    Return
go:
  FileOpen $TEMP1 $TEMP1 "w"
  FileWrite $TEMP2 ":pserver:anonymous:@cvs.sourceforge.net:/cvsroot/nsis$\r$\n"
  FileClose $TEMP2
  Push "go"
FunctionEnd

;--------------------------------
; Update (Installer Section)

Section ""
  
  SetDetailsPrint none
  
  Call ConnectInternet

  !insertmacro MUI_INSTALLOPTIONS_READ $TEMP1 "Method.ini" "Field 1" "State"
  StrCmp $TEMP1 "1" 0 CVS
  
    ;Check for a new release
    
    SetDetailsPrint listonly
    
    !insertmacro MUI_HEADER_TEXT "Checking for a release..." "Please wait while NSIS Update checks whether a new NSIS release is available."
    
    nsExec::ExecToStack '"$NSISBINPATH\..\makensis.exe" "/version"'
    Pop $TEMP1
    
    StrCmp $TEMP1 "error" "" +3
      MessageBox MB_OK|MB_ICONSTOP "Can't get NSIS version."
      Quit
    
    Pop $TEMP1
    DetailPrint "Your NSIS version: $TEMP1"
    DetailPrint ""
    
    StrCpy $TEMP2 $TEMP1 "" -5
    StrCmp $TEMP2 "(CVS)" "" NoCVSVersion

      StrLen $TEMP2 $TEMP1
      IntOp $TEMP2 $TEMP2 - 6
      StrCpy $TEMP1 $TEMP1 $TEMP2
      StrCpy $TEMP2 1
      
      DetailPrint "NOTE: You are using a development version of NSIS."
      DetailPrint "You can also use NSIS Update to get the latest development files."
      DetailPrint ""
      
      Goto CheckUpdate
    
    NoCVSVersion:
    
      StrCpy $TEMP2 0
    
    CheckUpdate:
    
    DetailPrint "Checking for a new release..."
    DetailPrint ""
    
    NSISdl::download_quiet "http://nsis.sourceforge.net/update.php?version=$TEMP1&cvs=$TEMP2" "$PLUGINSDIR\Update"    
    Pop $TEMP1
     
    StrCmp $TEMP1 "success" ReadVersion
      MessageBox MB_OK|MB_ICONSTOP "Download failed: $TEMP1."
      Quit
    
    ReadVersion:
    
    FileOpen $TEMP1 "$PLUGINSDIR\Update" r
    FileRead $TEMP1 $TEMP2
    FileClose $TEMP1

    StrCmp $TEMP2 "" "" +3
      MessageBox MB_OK|MB_ICONSTOP "Invalid version data."
      Quit
      
    StrCpy $TEMP1 $TEMP2 1
    StrCpy $TEMP2 $TEMP2 "" 2
    
    StrCmp $TEMP1 "1" "" +3
      DetailPrint "A new stable release is available: $TEMP2"
      Goto UpdateMsg
    
    StrCmp $TEMP1 "2" "" +3
      DetailPrint "A new pre-release is available: $TEMP2"
      Goto UpdateMsg
      
    DetailPrint "No new release is available. Please check again later."
    
    Goto done
    
    UpdateMsg:
    
    MessageBox MB_YESNO|MB_ICONQUESTION "A new release is available. Would you like to go to the download page?" IDNO done
    
      SetDetailsPrint none
      ExecShell "open" "http://nsis.sourceforge.net/download/"
      Goto done
    
  CVS:
  
    ;CVS Update
    
    SetOutPath $NSISBINPATH\..
    
    ;patch CVS Root files that come from the development snapshot
    GetFullPathName $OUTDIRFULL $OUTDIR
    !insertmacro CallFindFiles $OUTDIRFULL Root PatchCVSRoot

    SetDetailsPrint listonly
    
    !insertmacro MUI_HEADER_TEXT "Updating..." "Please wait while NSIS Update is downloading development files. This may take several minutes to complete."
    
    DetailPrint "Initializing CVS Update..."
    
    !insertmacro MUI_INSTALLOPTIONS_READ $TEMP1 "Method.ini" "Field 2" "State"
    StrCmp $TEMP1 "1" 0 CleanCVSUpdate
      
      ;Normal update
    
      nsExec::ExecToLog '"$NSISBINPATH\cvs95.exe" -q -z3 update -d -P'
      Pop $TEMP1
      Goto CheckCVSReturn
      
    CleanCVSUpdate:
      
      ;Clean copy
      
      nsExec::ExecToLog '"$NSISBINPATH\cvs95.exe" -q -z3 update -C -d -P'
      Pop $TEMP1
    
    CheckCVSReturn:
    
      StrCmp $TEMP1 "error" "" +3
        MessageBox MB_OK|MB_ICONSTOP "Can't execute CVS client."
        Quit
        
      DetailPrint "CVS Update Completed"
  
  done:
  
  SetDetailsPrint none

SectionEnd