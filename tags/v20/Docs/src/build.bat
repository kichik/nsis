bin\halibut.exe chm_config.but intro.but tutorial.but usage.but script.but var.but labels.but jumps.but pages.but sections.but usection.but functions.but callback.but attributes.but compilerflags.but basic.but registry.but generalpurpose.but flowcontrol.but file.but uninstall.but misc.but string.but stack.but int.but reboot.but log.but sec.but ui.but langs.but plugin.but silent.but compiler.but defines.but modernui.but usefulfunc.but usefulinfos.but history.but credits.but license.but
@echo off
copy Contents.html IndexPage.html

copy ..\*.css .
hhc nsis.hhp
del /F *.html *.hhc *.hhk *.css
if not "x%OS%x" == "xWindows_NTx" goto CHMCopy
fc /B nsis.chm ..\..\nsis.chm > nul
if %errorlevel% == 0 goto SkipCHMCopy
:CHMCopy
  copy nsis.chm ..\..\
:SkipCHMCopy
del nsis.chm

@echo on
bin\halibut.exe config.but intro.but tutorial.but usage.but script.but var.but labels.but jumps.but pages.but sections.but usection.but functions.but callback.but attributes.but compilerflags.but basic.but registry.but generalpurpose.but flowcontrol.but file.but uninstall.but misc.but string.but stack.but int.but reboot.but log.but sec.but ui.but langs.but plugin.but silent.but compiler.but defines.but modernui.but usefulfunc.but usefulinfos.but history.but credits.but license.but
@echo off
copy Contents.html index.html > nul
copy index.html IndexPage.html > nul
if "x%OS%x" == "xWindows_NTx" goto nt
  copy *.html .. > nul
  goto done
:nt
  for %%A in (*.html) do (fc "%%A" "..\%%~nxA" || copy "%%A" ..) > nul
:done
del *.html > nul
