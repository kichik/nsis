if not exist "%WXWIN%\build\msw\makefile.vc" goto error
set OLDCD=%CD%
cd "%WXWIN%\build\msw"
copy /y "%WXWIN%\include\wx\msw\setup.h" "%OLDCD%\old_setup.h"
copy /y "%OLDCD%\setup.h" "%WXWIN%\include\wx\msw\setup.h"
rd /S ..\..\lib\vc_libnsis
rd /S vc_mswnsis
nmake -f makefile.vc CFG=nsis BUILD=release RUNTIME_LIBS=dynamic SHARED=0 UNICODE=0 WXUNIV=0 USE_OPENGL=0 USE_ODBC=0 USE_HTML=1 USE_XRC=0 
copy /y "%OLDCD%\old_setup.h" "%WXWIN%\include\wx\msw\setup.h"
cd "%OLDCD%"
goto done
:error
echo WXWIN is not properly set
:done
