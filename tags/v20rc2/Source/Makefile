#
# This makefile for mingw32 by Nels. Thanks, Nels
#
#
# -- Subdirs --
SUBDIRS = exehead

# -- Objects and source files --
SRCS = crc32.c build.cpp exedata.cpp makenssi.cpp script.cpp tokens.cpp util.cpp ResourceEditor.cpp DialogTemplate.cpp ./zlib/deflate.c ./zlib/trees.c ./bzip2/blocksort.c ./bzip2/bzlib.c ./bzip2/compress.c ./bzip2/huffman.c
OBJS = build.o exedata.o makenssi.o script.o tokens.o util.o script1.res crc32.o ResourceEditor.o DialogTemplate.o deflate.o trees.o blocksort.o bzlib.o compress.o huffman.o
LIBS = -lgdi32 -lversion

# -- Programs --
MAKE = make
CC = gcc
RC = windres
RM = del

# -- Compilers and linker flags --
DEFINES = -DWIN32 -D_WINDOWS_
CFLAGS = -O2 $(DEFINES)
CPPFLAGS = -O2 -fvtable-thunks $(DEFINES)
LFLAGS = -s
RCFLAGS = --input-format rc --output-format coff

all : subdirs makensis

subdirs: $(SUBDIRS)

$(SUBDIRS)::
	$(MAKE) -C $@ all

makensis : $(OBJS)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LFLAGS) -o makensis.exe $(OBJS) $(LIBS)

# -- Dependencies --
build.o : build.cpp ./zlib/zlib.h ./exehead/config.h ./exehead/fileform.h ./exehead/resource.h exedata.h build.h util.h strlist.h lineparse.h ResourceEditor.h Makefile
exedata.o : exedata.cpp exedata.h ./exehead/Release/bitmap1.h ./exehead/Release/bitmap2.h ./exehead/Release/icon.h ./exehead/Release/unicon.h  ./exehead/Release/exehead.h Makefile
makenssi.o : makenssi.cpp build.h util.h exedata.h strlist.h lineparse.h ./exehead/fileform.h ./exehead/config.h Makefile
script.o : script.cpp tokens.h build.h util.h exedata.h strlist.h lineparse.h ResourceEditor.h DialogTemplate.h ./exehead/resource.h ./exehead/fileform.h ./exehead/config.h Makefile
tokens.o : tokens.cpp build.h tokens.h Makefile
util.o : util.cpp ./exehead/fileform.h util.h strlist.h ResourceEditor.h Makefile
crc32.o : crc32.c ./exehead/config.h Makefile
ResourceEditor.o : ResourceEditor.cpp
DialogTemplate.o : DialogTemplate.cpp

# -- Special command line for the resource file --
script1.res : script1.rc resource.h Makefile
	$(RC) $(RCFLAGS) -o script1.res -i script1.rc

# -- Special command lines for zlib --
deflate.o : ./zlib/deflate.c ./zlib/deflate.h ./zlib/zutil.h ./zlib/zlib.h ./zlib/zconf.h Makefile ./exehead/config.h
	$(CC) $(CFLAGS) -c ./zlib/deflate.c -o deflate.o

trees.o : ./zlib/trees.c ./zlib/deflate.h ./zlib/zutil.h ./zlib/zlib.h ./zlib/zconf.h Makefile ./exehead/config.h
	$(CC) $(CFLAGS) -c ./zlib/trees.c -o trees.o

# -- Special command lines for bzip2 --
blocksort.o : ./bzip2/blocksort.c ./bzip2/bzlib.h ./bzip2/bzlib_private.h ./exehead/config.h
	$(CC) $(CFLAGS) -c ./bzip2/blocksort.c -o blocksort.o
bzlib.o : ./bzip2/bzlib.c ./bzip2/bzlib.h ./bzip2/bzlib_private.h ./exehead/config.h
	$(CC) $(CFLAGS) -c ./bzip2/bzlib.c -o bzlib.o
compress.o : ./bzip2/compress.c ./bzip2/bzlib.h ./bzip2/bzlib_private.h ./exehead/config.h
	$(CC) $(CFLAGS) -c ./bzip2/compress.c -o compress.o
huffman.o : ./bzip2/huffman.c ./bzip2/bzlib.h ./bzip2/bzlib_private.h ./exehead/config.h
	$(CC) $(CFLAGS) -c ./bzip2/huffman.c -o huffman.o


# -- Clean script --
clean ::
	$(MAKE) -C exehead clean
	$(RM) *.o
	$(RM) script1.res
	$(RM) makensis.exe
