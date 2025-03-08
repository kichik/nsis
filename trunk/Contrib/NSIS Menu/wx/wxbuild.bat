@echo off
if not exist setup.h goto setup_error
if not exist "%WXWIN%\build\msw\makefile.vc" goto error
set OLDCD=%CD%
cd /D "%WXWIN%\build\msw"
rem Save the old setup.h as old_setup.h in the starting dir.
copy /y "%WXWIN%\include\wx\msw\setup.h" "%OLDCD%\old_setup.h"

if "%1"=="unicode" goto unicode
if "%1"=="ansi" goto ansi

:ansi
rem Ask if we want to delete this dir.
echo Deleting...
rd /S ..\..\lib\vc_libnsis

echo ### building ANSI version ###
rem Copy the ANSI version of setup.h to WXWIN setup.
copy /y "%OLDCD%\setup.h" "%WXWIN%\include\wx\msw\setup.h"
echo Deleting...
rd /S vc_mswnsis
nmake -f makefile.vc CFG=nsis BUILD=release RUNTIME_LIBS=static SHARED=0 UNICODE=0 WXUNIV=0 USE_OPENGL=0 USE_ODBC=0 USE_HTML=1 USE_XRC=0
if "%1"=="" goto unicode
if "%1"=="all" goto unicode
goto finish

:unicode
rem Ask if we want to delete this dir.
echo Deleting...
rd /S ..\..\lib\vc_libunsis

echo ### building UNICODE version ###
rem Copy the UNICODE version of setup.h to WXWIN setup.
copy /y "%OLDCD%\setup.h" "%WXWIN%\include\wx\msw\setup.h"
echo Deleting...
rd /S vc_mswuunsis
nmake -f makefile.vc CFG=unsis BUILD=release RUNTIME_LIBS=static SHARED=0 UNICODE=1 WXUNIV=0 USE_OPENGL=0 USE_ODBC=0 USE_HTML=1 USE_XRC=0

:finish
rem Restore the setup.h
copy /y "%OLDCD%\old_setup.h" "%WXWIN%\include\wx\msw\setup.h"
cd /D "%OLDCD%"
goto done
:error
echo WXWIN is not properly set
goto done
:setup_error
echo setup.h cannot be found, wrong working directory?
:done
