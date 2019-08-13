
REM :: Marius Negrutiu (marius.negrutiu@protonmail.com)

@echo off

title NSIS Master

echo.
echo zlib ...
cd /d "%~dp0"
echo   Start:  %date% %time%
start "" /wait %COMSPEC% /C _build_zlib.bat
echo   Finish: %date% %time%
echo   Error:  %errorlevel%
if %errorlevel% neq 0 exit /B %errorlevel%


echo.
echo NSIS ...
cd /d "%~dp0"
echo   Start:  %date% %time%
start "" /wait %COMSPEC% /C _build_NSIS.bat
echo   Finish: %date% %time%
echo   Error:  %errorlevel%
if %errorlevel% neq 0 exit /B %errorlevel%

echo.
echo Done
pause
