OBJS = biblio.o bk_text.o bk_whlp.o bk_xhtml.o contents.o error.o help.o index.o input.o keywords.o licence.o main.o malloc.o misc.o style.o tree234.o ustring.o version.o winhelp.o
LIBS = 

# -- Programs --
MAKE = make
CC = gcc
RM = del

# -- Compilers and linker flags --
DEFINES = 
CFLAGS = -Wall -W $(DEFINES)
LFLAGS = -s


all : halibut

halibut : $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o ..\halibut.exe $(OBJS) $(LIBS)

clean ::
	$(RM) *.o
	$(RM) ..\halibut.exe
