substart - Redirect to an executable located in the 'Bin' sub folder 
           relative to the starter executable.
------------------------------------------------------------------------

Copyright (C) 2010 Thomas Gaugler
Licensed under the zlib/libpng license

This tool runs the executable of the same name in the 'Bin' sub folder
and passes along the command line options.

The main purpose of the tool is that scripts expecting an executable in
the root of the program installation folder continue to run.

USAGE 
----- 

The original executable has to go into the 'Bin' sub folder and the
the substart executable renamed to the original executable name needs
to be placed at the original location.

EXAMPLE
-------

Directory hierarchy:
C:\Program Files\NSIS
C:\Program Files\NSIS\Bin\makensis.exe
C:\Program Files\NSIS\makensis.exe (-> substart.exe renamed to makensis.exe)

C:\Program Files\NSIS\makensis.exe /VERSION


Please be aware that the name of the substart executable has to match
with the one in the sub folder.

