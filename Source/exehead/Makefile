# -- Objects and source files --
DEPENDS = ../Platform.h config.h

ZLIB_SRCS = bgbg.c exec.c fileform.c main.c ui.c util.c ../crc32.c resource.rc ../zlib/infblock.c
ZLIB_OBJS = Release-zlib/bgbg.o Release-zlib/exec.o Release-zlib/fileform.o Release-zlib/main.o Release-zlib/ui.o Release-zlib/util.o Release-zlib/crc32.o Release-zlib/resource.res Release-zlib/infblock.o

BZIP2_SRCS = bgbg.c exec.c fileform.c main.c ui.c util.c ../crc32.c resource.rc ../bzip2/bzlib.c ../bzip2/decompress.c ../bzip2/huffman.c
BZIP2_OBJS = Release-bzip2/bgbg.o Release-bzip2/exec.o Release-bzip2/fileform.o Release-bzip2/main.o Release-bzip2/ui.o Release-bzip2/util.o Release-bzip2/crc32.o Release-bzip2/resource.res Release-bzip2/bzlib.o Release-bzip2/decompress.o Release-bzip2/huffman.o

LZMA_SRCS = bgbg.c exec.c fileform.c main.c ui.c util.c ../crc32.c resource.rc ../7zip/LZMADecode.c
LZMA_OBJS = Release-lzma/bgbg.o Release-lzma/exec.o Release-lzma/fileform.o Release-lzma/main.o Release-lzma/ui.o Release-lzma/util.o Release-lzma/crc32.o Release-lzma/resource.res Release-lzma/LZMADecode.o

LIBS = -lole32 -lgdi32 -lversion -luuid -lcomctl32 -lkernel32 -luser32 -lshell32 -ladvapi32

# -- Programs --
CC = gcc
RC = windres
RM = rm
MKDIR = mkdir

# -- Compilers and linker flags --
DEFINES = -DWIN32 -D_WINDOWS_ -DEXEHEAD -DWIN32_LEAN_AND_MEAN -DZEXPORT=__stdcall -DLZMACALL=__stdcall
CFLAGS = -Wall -Os -fno-common -fomit-frame-pointer -fno-inline $(DEFINES)
LFLAGS = -s -mwindows -nostdlib -nostartfiles --enable-stdcall-fixup -Wl,-Bdynamic -Wl,--file-alignment,512 -Wl,--exclude-libs,msvcrt.a -Wl,-e,_WinMain@16 -Wl,sections_script.ld
RCFLAGS = --input-format rc --output-format coff

all : exehead_zlib exehead_bzip2 exehead_lzma exehead_resources

missing_dirs = $(filter-out $(wildcard Release-*),Release-zlib Release-bzip2 Release-lzma)

ifneq ($(strip $(missing_dirs)),)
  mkdirline = $(MKDIR) $(missing_dirs)
else
  mkdirline = 
endif

dirs:
	$(mkdirline)

exehead_zlib : dirs $(ZLIB_SRCS) $(ZLIB_OBJS) sections_script
	$(CC) $(CFLAGS) $(LFLAGS) -o Release-zlib/exehead_zlib.exe $(ZLIB_OBJS) $(LIBS)
	bin2h Release-zlib/exehead_zlib.exe Release-zlib/exehead_zlib.h zlib_header_data

exehead_bzip2 : dirs $(BZIP2_SRCS) $(BZIP2_OBJS) sections_script
	$(CC) $(CFLAGS) $(LFLAGS) -o Release-bzip2/exehead_bzip2.exe $(BZIP2_OBJS) $(LIBS)
	bin2h Release-bzip2/exehead_bzip2.exe Release-bzip2/exehead_bzip2.h bzip2_header_data

exehead_lzma : dirs $(LZMA_SRCS) $(LZMA_OBJS) sections_script
	$(CC) $(CFLAGS) $(LFLAGS) -o Release-lzma/exehead_lzma.exe $(LZMA_OBJS) $(LIBS)
	bin2h Release-lzma/exehead_lzma.exe Release-lzma/exehead_lzma.h lzma_header_data

exehead_resources : exehead_zlib
	bin2h bitmap1.bmp Release-zlib/bitmap1.h bitmap1_data
	bin2h nsis.ico Release-zlib/icon.h icon_data
	bin2h uninst.ico Release-zlib/unicon.h unicon_data

sections_script:
	echo SECTIONS > sections_script.ld
	echo { >> sections_script.ld
	echo   .text : { *(.text) } >> sections_script.ld
	echo   .data : { *(.data) } >> sections_script.ld
	echo   .rdata : { *(.rdata) } >> sections_script.ld
	echo   .bss : { *(.bss) } >> sections_script.ld
	echo   .idata : { *(.idata) } >> sections_script.ld
	echo   .ndata BLOCK(__section_alignment__) : { [ .ndata ] } >> sections_script.ld
	echo   .rsrc : { *(.rsrc) } >> sections_script.ld
	echo } >> sections_script.ld

Release-zlib/ = -DNSIS_COMPRESS_USE_ZLIB
Release-bzip2/ = -DNSIS_COMPRESS_USE_BZIP2
Release-lzma/ = -DNSIS_COMPRESS_USE_LZMA
getdefine = $($(dir $@))

Release-zlib/%.o Release-bzip2/%.o Release-lzma/%.o : %.c $(DEPENDS)
	$(CC) $(CFLAGS) $(getdefine) -c $< -o $@

Release-zlib/%.o Release-bzip2/%.o Release-lzma/%.o : ../%.c $(DEPENDS)
	$(CC) $(CFLAGS) $(getdefine) -c $< -o $@

Release-zlib/%.o Release-bzip2/%.o Release-lzma/%.o : ../zlib/%.c $(DEPENDS)
	$(CC) $(CFLAGS) $(getdefine) -c $< -o $@

Release-zlib/%.o Release-bzip2/%.o Release-lzma/%.o : ../bzip2/%.c $(DEPENDS)
	$(CC) $(CFLAGS) $(getdefine) -c $< -o $@

Release-zlib/%.o Release-bzip2/%.o Release-lzma/%.o : ../7zip/%.c $(DEPENDS)
	$(CC) $(CFLAGS) $(getdefine) -c $< -o $@

%/resource.res : resource.rc resource.h config.h Makefile
	$(RC) $(RCFLAGS) -o $*/resource.res -i resource.rc

clean ::
	$(RM) sections_script.ld
	$(RM) Release-zlib/*.o
	$(RM) Release-zlib/resource.res
	$(RM) Release-zlib/exehead_zlib.exe
	$(RM) Release-zlib/exehead_zlib.h
	$(RM) Release-zlib/bitmap1.h
	$(RM) Release-zlib/icon.h
	$(RM) Release-zlib/unicon.h
	$(RM) Release-bzip2/*.o
	$(RM) Release-bzip2/resource.res
	$(RM) Release-bzip2/exehead_bzip2.exe
	$(RM) Release-bzip2/exehead_bzip2.h
	$(RM) Release-lzma/*.o
	$(RM) Release-lzma/resource.res
	$(RM) Release-lzma/exehead_lzma.exe
	$(RM) Release-lzma/exehead_lzma.h
