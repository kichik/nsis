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
!MESSAGE "makenssi - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "makenssi - Win32 Release"

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
# ADD CPP /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_MBCS" /YX"Platform.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib version.lib /nologo /subsystem:console /pdb:none /map /machine:I386 /out:"../makensis.exe" /opt:nowin98

!ELSEIF  "$(CFG)" == "makenssi - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /Ob2 /D "_CONSOLE" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "WIN32_LEAN_AND_MEAN" /FD /c
# SUBTRACT BASE CPP /Fr /YX /Yc /Yu
# ADD CPP /nologo /MLd /W3 /GX /ZI /Od /D "_CONSOLE" /D "WIN32" /D "_DEBUG" /D "_MBCS" /Fr /FD /Zm200 /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib version.lib /nologo /subsystem:console /pdb:none /machine:I386 /out:"../makensis.exe" /opt:nowin98
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib version.lib /nologo /subsystem:console /incremental:yes /debug /machine:I386 /out:"../makensis.exe" /opt:nowin98
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "makenssi - Win32 Release"
# Name "makenssi - Win32 Debug"
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

SOURCE=.\bzip2\compress.c
# End Source File
# Begin Source File

SOURCE=.\bzip2\huffman.c
# End Source File
# End Group
# Begin Group "7zip"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\7zip\7zGuids.cpp
# End Source File
# Begin Source File

SOURCE=.\7zip\Common\Alloc.cpp
# ADD CPP /D "COMPRESS_MF_BT"
# End Source File
# Begin Source File

SOURCE=.\7zip\Common\CRC.cpp
# ADD CPP /D "COMPRESS_MF_BT"
# End Source File
# Begin Source File

SOURCE=.\7zip\7zip\Compress\LZ\LZInWindow.cpp
# ADD CPP /D "COMPRESS_MF_BT"
# End Source File
# Begin Source File

SOURCE=.\7zip\7zip\Compress\LZMA\LZMAEncoder.cpp
# ADD CPP /D "COMPRESS_MF_BT"
# End Source File
# Begin Source File

SOURCE=.\7zip\7zip\Common\OutBuffer.cpp
# ADD CPP /D "COMPRESS_MF_BT"
# End Source File
# Begin Source File

SOURCE=.\7zip\7zip\Compress\RangeCoder\RangeCoderBit.cpp
# ADD CPP /D "COMPRESS_MF_BT"
# End Source File
# End Group
# Begin Source File

SOURCE=.\build.cpp
# ADD CPP /G6
# End Source File
# Begin Source File

SOURCE=.\clzma.cpp
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

SOURCE=.\growbuf.cpp
# End Source File
# Begin Source File

SOURCE=.\lang.cpp
# End Source File
# Begin Source File

SOURCE=.\lineparse.cpp
# End Source File
# Begin Source File

SOURCE=.\makenssi.cpp
# ADD CPP /G6
# End Source File
# Begin Source File

SOURCE=.\mmap.cpp
# End Source File
# Begin Source File

SOURCE=.\Plugins.cpp
# End Source File
# Begin Source File

SOURCE=.\ResourceEditor.cpp
# End Source File
# Begin Source File

SOURCE=.\ResourceVersionInfo.cpp
# End Source File
# Begin Source File

SOURCE=.\script.cpp
# ADD CPP /Ot /Ow /Oy
# SUBTRACT CPP /Og
# End Source File
# Begin Source File

SOURCE=.\ShConstants.cpp
# End Source File
# Begin Source File

SOURCE=.\strlist.cpp
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

SOURCE=.\clzma.h
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

SOURCE=.\growbuf.h
# End Source File
# Begin Source File

SOURCE=.\lang.h
# End Source File
# Begin Source File

SOURCE=.\lineparse.h
# End Source File
# Begin Source File

SOURCE=.\mmap.h
# End Source File
# Begin Source File

SOURCE=.\Platform.h
# End Source File
# Begin Source File

SOURCE=.\Plugins.h
# End Source File
# Begin Source File

SOURCE=.\ResourceEditor.h
# End Source File
# Begin Source File

SOURCE=.\ResourceVersionInfo.h
# End Source File
# Begin Source File

SOURCE=.\ShConstants.h
# End Source File
# Begin Source File

SOURCE=.\strlist.h
# End Source File
# Begin Source File

SOURCE=.\tokens.h
# End Source File
# Begin Source File

SOURCE=.\uservars.h
# End Source File
# Begin Source File

SOURCE=.\util.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=..\makensis.htm
# End Source File
# Begin Source File

SOURCE=..\Examples\makensis.nsi
# End Source File
# End Target
# End Project
