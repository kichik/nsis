# Microsoft Developer Studio Project File - Name="exehead_bzip2" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=exehead_bzip2 - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "exehead-bzip2.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "exehead-bzip2.mak" CFG="exehead_bzip2 - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "exehead_bzip2 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release-bzip2"
# PROP Intermediate_Dir "Release-bzip2"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /Gz /W3 /GX /O1 /Oy /D "_WINDOWS" /D "EXEHEAD" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "WIN32_LEAN_AND_MEAN" /D "NSIS_COMPRESS_USE_BZIP2" /FD /c
# SUBTRACT CPP /Fr /YX /Yc /Yu
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 comctl32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib version.lib shell32.lib /nologo /entry:"WinMain" /subsystem:windows /pdb:none /map /machine:I386 /nodefaultlib /out:"Release-bzip2/exehead_bzip2.exe" /opt:nowin98
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=generating include file for makenssi
PostBuild_Cmds=bin2h Release-bzip2\exehead_bzip2.exe Release-bzip2\exehead_bzip2.h bzip2_header_data
# End Special Build Tool
# Begin Target

# Name "exehead_bzip2 - Win32 Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "zlib"

# PROP Default_Filter ""
# Begin Group "headers"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\zlib\Infblock.h
# End Source File
# Begin Source File

SOURCE=..\zlib\Infcodes.h
# End Source File
# Begin Source File

SOURCE=..\zlib\Inftrees.h
# End Source File
# Begin Source File

SOURCE=..\zlib\Infutil.h
# End Source File
# Begin Source File

SOURCE=..\zlib\Zconf.h
# End Source File
# Begin Source File

SOURCE=..\zlib\Zlib.h
# End Source File
# End Group
# Begin Source File

SOURCE=..\zlib\INFBLOCK.C
# End Source File
# Begin Source File

SOURCE=..\zlib\INFCODES.C
# End Source File
# Begin Source File

SOURCE=..\zlib\INFLATE.C
# End Source File
# Begin Source File

SOURCE=..\zlib\INFTREES.C
# End Source File
# Begin Source File

SOURCE=..\zlib\INFUTIL.C
# End Source File
# End Group
# Begin Group "bzip2"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\bzip2\bzlib.c
# End Source File
# Begin Source File

SOURCE=..\bzip2\bzlib.h
# End Source File
# Begin Source File

SOURCE=..\bzip2\bzlib_private.h
# End Source File
# Begin Source File

SOURCE=..\bzip2\decompress.c
# End Source File
# Begin Source File

SOURCE=..\bzip2\huffman.c
# End Source File
# Begin Source File

SOURCE=..\bzip2\randtable.c
# End Source File
# End Group
# Begin Source File

SOURCE=.\bgbg.c
# End Source File
# Begin Source File

SOURCE=..\crc32.c
# End Source File
# Begin Source File

SOURCE=.\exec.c
# End Source File
# Begin Source File

SOURCE=.\fileform.c
# End Source File
# Begin Source File

SOURCE=.\Main.c
# End Source File
# Begin Source File

SOURCE=.\Ui.c
# End Source File
# Begin Source File

SOURCE=.\util.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\config.h
# End Source File
# Begin Source File

SOURCE=.\exec.h
# End Source File
# Begin Source File

SOURCE=.\fileform.h
# End Source File
# Begin Source File

SOURCE=.\lang.h
# End Source File
# Begin Source File

SOURCE=.\state.h
# End Source File
# Begin Source File

SOURCE=.\ui.h
# End Source File
# Begin Source File

SOURCE=.\util.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\bitmap1.bmp
# End Source File
# Begin Source File

SOURCE=.\bitmap2.bmp
# End Source File
# Begin Source File

SOURCE=.\bitmap3.bmp
# End Source File
# Begin Source File

SOURCE=.\icon3.ico
# End Source File
# Begin Source File

SOURCE=.\nsis.ico
# End Source File
# Begin Source File

SOURCE=.\nullsoft.ico
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# Begin Source File

SOURCE=.\resource.rc
# End Source File
# Begin Source File

SOURCE=.\uninst.ico
# End Source File
# End Group
# Begin Source File

SOURCE=.\exehead.xml
# End Source File
# End Target
# End Project
