# Microsoft Developer Studio Project File - Name="makenssi" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=makenssi - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "makenssi.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "makenssi.mak" CFG="makenssi - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "makenssi - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /Ob2 /D "_CONSOLE" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "WIN32_LEAN_AND_MEAN" /FD /c
# SUBTRACT CPP /Fr /YX /Yc /Yu
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib version.lib /nologo /subsystem:console /pdb:none /machine:I386 /out:"../makensis.exe" /opt:nowin98
# Begin Target

# Name "makenssi - Win32 Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "zlib"

# PROP Default_Filter ""
# Begin Group "headers"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\zlib\Deflate.h
# End Source File
# Begin Source File

SOURCE=.\zlib\Infblock.h
# End Source File
# Begin Source File

SOURCE=.\zlib\Infcodes.h
# End Source File
# Begin Source File

SOURCE=.\zlib\Inftrees.h
# End Source File
# Begin Source File

SOURCE=.\zlib\Infutil.h
# End Source File
# Begin Source File

SOURCE=.\zlib\Zconf.h
# End Source File
# Begin Source File

SOURCE=.\zlib\Zlib.h
# End Source File
# Begin Source File

SOURCE=.\zlib\Zutil.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\zlib\deflate.c
# ADD CPP /G6
# End Source File
# Begin Source File

SOURCE=.\zlib\trees.c
# ADD CPP /G6
# End Source File
# End Group
# Begin Group "bzip2"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\bzip2\blocksort.c
# End Source File
# Begin Source File

SOURCE=.\bzip2\bzlib.c
# End Source File
# Begin Source File

SOURCE=.\bzip2\bzlib.h
# End Source File
# Begin Source File

SOURCE=.\bzip2\bzlib_private.h
# End Source File
# Begin Source File

SOURCE=.\bzip2\compress.c
# End Source File
# Begin Source File

SOURCE=.\bzip2\huffman.c
# End Source File
# End Group
# Begin Source File

SOURCE=.\build.cpp
# ADD CPP /G6
# End Source File
# Begin Source File

SOURCE=.\crc32.c
# End Source File
# Begin Source File

SOURCE=.\DialogTemplate.cpp
# End Source File
# Begin Source File

SOURCE=.\exedata.cpp
# ADD CPP /G6
# End Source File
# Begin Source File

SOURCE=.\lang.cpp
# End Source File
# Begin Source File

SOURCE=.\makenssi.cpp
# ADD CPP /G6
# End Source File
# Begin Source File

SOURCE=.\Plugins.cpp
# End Source File
# Begin Source File

SOURCE=.\ResourceEditor.cpp
# End Source File
# Begin Source File

SOURCE=.\script.cpp
# ADD CPP /Ot /Ow /Oy
# SUBTRACT CPP /Og
# End Source File
# Begin Source File

SOURCE=.\tokens.cpp
# End Source File
# Begin Source File

SOURCE=.\util.cpp
# ADD CPP /G6
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\build.h
# End Source File
# Begin Source File

SOURCE=.\cbzip2.h
# End Source File
# Begin Source File

SOURCE=.\compressor.h
# End Source File
# Begin Source File

SOURCE=.\czlib.h
# End Source File
# Begin Source File

SOURCE=.\DialogTemplate.h
# End Source File
# Begin Source File

SOURCE=.\exedata.h
# End Source File
# Begin Source File

SOURCE=.\lang.h
# End Source File
# Begin Source File

SOURCE=.\lineparse.h
# End Source File
# Begin Source File

SOURCE=.\Plugins.h
# End Source File
# Begin Source File

SOURCE=.\ResourceEditor.h
# End Source File
# Begin Source File

SOURCE=.\strlist.h
# End Source File
# Begin Source File

SOURCE=.\tokens.h
# End Source File
# Begin Source File

SOURCE=.\util.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\Script1.rc
# End Source File
# End Group
# Begin Source File

SOURCE=..\makensis.htm
# End Source File
# Begin Source File

SOURCE=..\Examples\makensis.nsi
# End Source File
# End Target
# End Project
