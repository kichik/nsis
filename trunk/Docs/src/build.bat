bin\halibut.exe config.but intro.but usage.but script.but attributes.but compilerflags.but sections.but functions.but labels.but basic.but registry.but generalpurpose.but flowcontrol.but file.but misc.but string.but stack.but int.but reboot.but uninstall.but log.but sec.but var.but usection.but callback.but compiler.but defines.but plugin.but history.but license.but
@del *.hlp
@del *.cnt
@copy Contents.html index.html
@copy index.html IndexPage.html
@del output.txt
@copy *.html ..\\
@del -f *.html
