REM :: Marius Negrutiu (marius.negrutiu@protonmail.com)

@echo off
echo.

cd /d "%~dp0"
call _config.bat

call :CLEANUP
call :CLEANUP
call :CLEANUP
exit /B 0


:CLEANUP
for /D %%a in (nsis-*)    do rd /S /Q "%%a"
for /D %%a in (zlib-*)    do rd /S /Q "%%a"

del /Q flag-*.*