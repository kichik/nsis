bin\halibut.exe config.but intro.but usage.but script.but attributes.but compilerflags.but sections.but functions.but labels.but basic.but registry.but generalpurpose.but flowcontrol.but history.but license.but
@del *.hlp
@del *.cnt
@copy Contents.html index.html
@copy index.html IndexPage.html
@del output.txt
@copy *.html ..\\
@del -f *.html
