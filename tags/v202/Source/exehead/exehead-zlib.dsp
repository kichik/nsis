# Microsoft Developer Studio Project File - Name="exehead_zlib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=exehead_zlib - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "exehead-zlib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "exehead-zlib.mak" CFG="exehead_zlib - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "exehead_zlib - Win32 Release" (based on "Win32 (x86) Application")
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
# PROP Output_Dir "Release-zlib"
# PROP Intermediate_Dir "Release-zlib"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O1 /Oy /D "_WINDOWS" /D "EXEHEAD" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "WIN32_LEAN_AND_MEAN" /D "NSIS_COMPRESS_USE_ZLIB" /D ZEXPORT=__stdcall /FD /c
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
# ADD LINK32 comctl32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib version.lib shell32.lib /nologo /entry:"WinMain" /subsystem:windows /pdb:none /map /machine:I386 /nodefaultlib /out:"Release-zlib/exehead_zlib.exe" /opt:nowin98
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=generating include file for makenssi
PostBuild_Cmds=bin2h Release-zlib\exehead_zlib.exe Release-zlib\exehead_zlib.h zlib_exehead	bin2h bitmap1.bmp Release-zlib\bitmap1.h bitmap1_data	bin2h nsis.ico Release-zlib\icon.h icon_data	bin2h uninst.ico Release-zlib\unicon.h unicon_data
# End Special Build Tool
# Begin Target

# Name "exehead_zlib - Win32 Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "zlib"

# PROP Default_Filter ""
# Begin Group "headers"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\zlib\Zconf.h
# End Source File
# Begin Source File

SOURCE=..\zlib\Zlib.h
# End Source File
# Begin Source File

SOURCE=..\zlib\Zutil.h
# End Source File
# End Group
# Begin Source File

SOURCE=..\zlib\INFBLOCK.C
# ADD CPP /Gz
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

SOURCE=.\resource.h
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

SOURCE=.\nsis.ico
# End Source File
# Begin Source File

SOURCE=.\resource.rc
# End Source File
# Begin Source File

SOURCE=.\uninst.ico
# End Source File
# End Group
# End Target
# End Project
