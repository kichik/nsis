#
# This makefile for mingw32 by Nels. Thanks, Nels
#

# -- Objects and source files --
SRCS = zlib/deflate.c zlib/trees.c bzip2/blocksort.c bzip2/bzlib.c bzip2/compress.c bzip2/huffman.c 7zip/7zGuids.cpp 7zip/Common/CRC.cpp 7zip/7zip/Compress/LZ/LZInWindow.cpp 7zip/7zip/Compress/LZMA/LZMAEncoder.cpp 7zip/7zip/Compress/LZMA/LZMALen.cpp 7zip/7zip/Compress/LZMA/LZMALiteral.cpp 7zip/7zip/Common/OutBuffer.cpp 7zip/7zip/Compress/RangeCoder/RangeCoderBit.cpp build.cpp crc32.c DialogTemplate.cpp exedata.cpp lang.cpp makenssi.cpp Plugins.cpp ResourceEditor.cpp ResourceVersionInfo.cpp script.cpp tokens.cpp util.cpp
OBJS = 7zGuids.o blocksort.o build.o bzlib.o compress.o CRC.o crc32.o deflate.o DialogTemplate.o exedata.o huffman.o lang.o LZInWindow.o LZMAEncoder.o LZMALen.o LZMALiteral.o makenssi.o OutBuffer.o Plugins.o RangeCoderBit.o ResourceEditor.o ResourceVersionInfo.o script.o tokens.o trees.o util.o
LIBS = -lgdi32 -lversion -lstdc++

# -- Required .h files --
BASEINC = Platform.h exehead/config.h

# -- Programs --
MAKE = make
CC = gcc
RC = windres
RM = rm

# -- Compilers and linker flags --
DEFINES = -DWIN32 -D_WINDOWS_
CFLAGS = -Wall -O2 $(DEFINES)
CPPFLAGS = -Wall -O2 $(DEFINES)
LFLAGS = -s
RCFLAGS = --input-format rc --output-format coff

all : exehead_zlib exehead_bzip2 exehead_lzma exehead_resources makensis

exehead_zlib exehead_bzip2 exehead_lzma exehead_resources :
	$(MAKE) -C exehead $@

makensis : $(OBJS)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LFLAGS) -o ../makensis.exe $(OBJS) $(LIBS)

# -- Dependencies --
build.o : build.cpp ./zlib/zlib.h $(BASEINC) ./exehead/fileform.h ./exehead/resource.h exedata.h build.h util.h strlist.h lineparse.h ResourceEditor.h Makefile
exedata.o : exedata.cpp exedata.h ./exehead/Release-zlib/bitmap1.h ./exehead/Release-zlib/icon.h ./exehead/Release-zlib/unicon.h ./exehead/Release-zlib/exehead_zlib.h Makefile
makenssi.o : makenssi.cpp build.h util.h exedata.h strlist.h lineparse.h ./exehead/fileform.h $(BASEINC) Makefile
script.o : script.cpp tokens.h build.h util.h exedata.h strlist.h lineparse.h ResourceEditor.h DialogTemplate.h ./exehead/resource.h ./exehead/fileform.h $(BASEINC) Makefile
tokens.o : tokens.cpp build.h tokens.h Makefile
util.o : util.cpp ./exehead/fileform.h util.h strlist.h ResourceEditor.h Makefile
crc32.o : crc32.c $(BASEINC) Makefile
ResourceEditor.o : ResourceEditor.cpp
DialogTemplate.o : DialogTemplate.cpp

# -- Special command lines for zlib --
deflate.o : zlib/deflate.c
	$(CC) $(CFLAGS) -c $< -o $@
trees.o : zlib/trees.c
	$(CC) $(CFLAGS) -c $< -o $@

# -- Special command lines for bzip2 --
blocksort.o : bzip2/blocksort.c
	$(CC) $(CFLAGS) -c $< -o $@
bzlib.o : bzip2/bzlib.c
	$(CC) $(CFLAGS) -c $< -o $@
compress.o : bzip2/compress.c
	$(CC) $(CFLAGS) -c $< -o $@
huffman.o : bzip2/huffman.c
	$(CC) $(CFLAGS) -c $< -o $@

# -- Special command lines for lzma --
7zGuids.o : 7zip/7zGuids.cpp
	$(CC) $(CFLAGS) -c $< -o $@
CRC.o : 7zip/Common/CRC.cpp
	$(CC) $(CFLAGS) -c $< -o $@
LZInWindow.o : 7zip/7zip/Compress/LZ/LZInWindow.cpp
	$(CC) $(CFLAGS) -c $< -o $@
LZMAEncoder.o : 7zip/7zip/Compress/LZMA/LZMAEncoder.cpp
	$(CC) $(CFLAGS) -c $< -o $@
LZMALen.o : 7zip/7zip/Compress/LZMA/LZMALen.cpp
	$(CC) $(CFLAGS) -c $< -o $@
LZMALiteral.o : 7zip/7zip/Compress/LZMA/LZMALiteral.cpp
	$(CC) $(CFLAGS) -c $< -o $@
OutBuffer.o : 7zip/7zip/Common/OutBuffer.cpp
	$(CC) $(CFLAGS) -c $< -o $@
RangeCoderBit.o : 7zip/7zip/Compress/RangeCoder/RangeCoderBit.cpp
	$(CC) $(CFLAGS) -c $< -o $@

# -- Clean script --
clean ::
	$(MAKE) -C exehead clean
	$(RM) *.o
	$(RM) ../makensis.exe
