
REM :: Marius Negrutiu (marius.negrutiu@protonmail.com)

@echo off
title %~nx0


:MAIN
set ORIGINAL_PATH=%PATH%

set CONFIG_ARCH=x86
call :BUILD
if %errorlevel% neq 0 pause & exit /B %errorlevel%
set PATH=%ORIGINAL_PATH%

set CONFIG_ARCH=amd64
call :BUILD
if %errorlevel% neq 0 pause & exit /B %errorlevel%
set PATH=%ORIGINAL_PATH%

:: Finish
REM pause
exit /B 0


:BUILD
call "%~dp0\_config.bat"
if %errorlevel% neq 0 echo Aborted. & pause & exit /B %errorlevel%
set PATH=%MINGW%\bin;%MSYS2%\usr\bin;%PATH%

title zlib-mingw-%CONFIG_ARCH%...

:: "zlib" -> "zlib-mingw-[arch]"
cd /d "%~dp0"
robocopy "%ZLIB_PATH%" "%ZLIB_PATH%-mingw-%CONFIG_ARCH%" *.* /E /XO /XD .git ... /XF .git ... /NJH /NJS /NDL
echo.

:: Build
cd /d "%ZLIB_PATH%-mingw-%CONFIG_ARCH%"
set LOC=%CONFIG_CFLAGS% %CONFIG_LFLAGS%
REM mingw32-make -f win32/Makefile.gcc clean
mingw32-make -f win32/Makefile.gcc zlib1.dll
