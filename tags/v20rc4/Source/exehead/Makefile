# -- Objects and source files --
SRCS = bgbg.c exec.c fileform.c main.c ui.c util.c ../crc32.c ../zlib/infblock.c ../zlib/infcodes.c ../zlib/inflate.c ../zlib/inftrees.c ../zlib/infutil.c ../bzip2/bzlib.c ../bzip2/decompress.c ../bzip2/huffman.c ../bzip2/randtable.c
OBJS = bgbg.o exec.o fileform.o main.o ui.o util.o resource.res crc32.o infblock.o infcodes.o inflate.o inftrees.o infutil.o bzlib.o decompress.o huffman.o randtable.o
LIBS = -lole32 -lgdi32 -lversion -luuid -lcomctl32 -lkernel32 -luser32 -lshell32 -ladvapi32

# -- Programs --
CC = gcc
RC = windres
RM = del

# -- Compilers and linker flags --
DEFINES = -DWIN32 -D_WINDOWS_ -DEXEHEAD -DWinMain=WinMainCRTStartup
CFLAGS = -Os -fomit-frame-pointer -fno-inline $(DEFINES)
LFLAGS = -s -mwindows -nostdlib -nostartfiles -Wl,--enable-stdcall-fixup
RCFLAGS = --input-format rc --output-format coff

all : exehead

exehead : $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o exehead.exe $(OBJS) $(LIBS)
	bin2h exehead.exe Release\exehead.h header_data
	bin2h bitmap1.bmp Release\bitmap1.h bitmap1_data
	bin2h bitmap2.bmp Release\bitmap2.h bitmap2_data
	bin2h nsis.ico Release\icon.h icon_data
	bin2h uninst.ico Release\unicon.h unicon_data

# -- Dependencies --
bgbg.o : bgbg.c resource.h config.h Makefile
exec.o : exec.c fileform.h util.h state.h ui.h exec.h config.h lang.h Makefile
fileform.o : fileform.c fileform.h util.h state.h ../zlib/zlib.h ../zlib/zconf.h ui.h config.h Makefile
main.o : main.c resource.h util.h fileform.h state.h ui.h ../zlib/zlib.h ../zlib/zconf.h config.h lang.h Makefile
ui.o : ui.c resource.h fileform.h state.h ui.h config.h Makefile
util.o : util.c util.h state.h fileform.h ui.h config.h Makefile

# -- Special command line for the resource file --
resource.res : resource.rc resource.h config.h Makefile
	$(RC) $(RCFLAGS) -o resource.res -i resource.rc

crc32.o : ../crc32.c config.h Makefile
	$(CC) $(CFLAGS) -c ../crc32.c -o crc32.o


# -- Special command lines for zlib --
infblock.o : ../zlib/infblock.c ../zlib/zutil.h ../zlib/infblock.h ../zlib/inftrees.h ../zlib/infcodes.h ../zlib/infutil.h ../zlib/zlib.h ../zlib/zconf.h Makefile
	$(CC) $(CFLAGS) -c ../zlib/infblock.c -o infblock.o

infcodes.o : ../zlib/infcodes.c ../zlib/zutil.h ../zlib/inftrees.h ../zlib/infblock.h ../zlib/infcodes.h ../zlib/infutil.h ../zlib/zlib.h ../zlib/zconf.h Makefile
	$(CC) $(CFLAGS) -c ../zlib/infcodes.c -o infcodes.o

inflate.o : ../zlib/inflate.c ../zlib/zutil.h ../zlib/infblock.h ../zlib/zlib.h ../zlib/zconf.h  Makefile
	$(CC) $(CFLAGS) -c ../zlib/inflate.c -o inflate.o

inftrees.o : ../zlib/inftrees.c ../zlib/zutil.h ../zlib/inftrees.h ../zlib/zlib.h ../zlib/zconf.h Makefile
	$(CC) $(CFLAGS) -c ../zlib/inftrees.c -o inftrees.o

infutil.o : ../zlib/infutil.c ../zlib/zutil.h ../zlib/infblock.h ../zlib/inftrees.h ../zlib/infcodes.h ../zlib/infutil.h ../zlib/zlib.h ../zlib/zconf.h Makefile
	$(CC) $(CFLAGS) -c ../zlib/infutil.c -o infutil.o

# -- Special command lines for bzip2 --
bzlib.o : ../bzip2/bzlib.c ../bzip2/bzlib.h ../bzip2/bzlib_private.h config.h
	$(CC) $(CFLAGS) -c ../bzip2/bzlib.c -o bzlib.o

decompress.o : ../bzip2/decompress.c ../bzip2/bzlib.h ../bzip2/bzlib_private.h config.h
	$(CC) $(CFLAGS) -c ../bzip2/decompress.c -o decompress.o

huffman.o : ../bzip2/huffman.c ../bzip2/bzlib.h ../bzip2/bzlib_private.h config.h
	$(CC) $(CFLAGS) -c ../bzip2/huffman.c -o huffman.o

randtable.o : ../bzip2/randtable.c ../bzip2/bzlib.h ../bzip2/bzlib_private.h config.h
	$(CC) $(CFLAGS) -c ../bzip2/randtable.c -o randtable.o

# -- Clean script --
clean ::
	$(RM) *.o
	$(RM) resource.res
	$(RM) exehead.exe
	$(RM) Release\exehead.h
	$(RM) Release\bitmap1.h
	$(RM) Release\bitmap2.h
	$(RM) Release\icon.h
	$(RM) Release\unicon.h
