
REM :: Marius Negrutiu (marius.negrutiu@protonmail.com)

@echo off
title %~nx0


:MAIN
set ORIGINAL_PATH=%PATH%

set CONFIG_ARCH=x86
call :BUILD
if %ERRORLEVEL% neq 0 pause && exit /B %ERRORLEVEL%
set PATH=%ORIGINAL_PATH%

set CONFIG_ARCH=amd64
call :BUILD
if %ERRORLEVEL% neq 0 pause && exit /B %ERRORLEVEL%
set PATH=%ORIGINAL_PATH%

:: Finish
REM pause
exit /B 0


:BUILD
call "%~dp0\_config.bat"
if not exist "%ZLIB_PATH%"			pause && exit /B 2
if not exist "%MSYS2%"				pause && exit /B 2
if not exist "%MINGW%"				pause && exit /B 2
title zlib %CONFIG_ARCH%...

set PATH=%MINGW%\bin;%MSYS2%\usr\bin;%PATH%

:: "zlib" -> "zlib-mingw-[arch]"
cd /d "%~dp0"
robocopy "%ZLIB_PATH%" "%ZLIB_PATH%-mingw-%CONFIG_ARCH%" *.* /E /XO /XD .git ... /XF .git ... /NJH /NJS /NDL
echo.

:: Build
cd /d "%ZLIB_PATH%-mingw-%CONFIG_ARCH%"
set LOC=%CONFIG_CFLAGS% %CONFIG_LFLAGS%
REM mingw32-make -f win32/Makefile.gcc clean
mingw32-make -f win32/Makefile.gcc zlib1.dll
