OBJS = config.but intro.but usage.but script.but attributes.but compilerflags.but history.but license.but

CC = bin\halibut.exe

all: 
	$(CC) $(OBJS)
	@del *.hlp
	@del *.cnt
	@copy Contents.html index.html
	@copy index.html IndexPage.html
	@del output.txt
	@copy *.html ..\\
	@del -f *.html
