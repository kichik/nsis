#
# Adapted for cross-compiling by Bas Mevissen
#
# -- Objects and source files --
DEPENDS = ../Platform.h config.h

ZLIB_SRCS = bgbg.c exec.c fileform.c Main.c Ui.c components.c util.c ../crc32.c resource.rc ../zlib/INFBLOCK.C
ZLIB_OBJS = Release-zlib/bgbg.o Release-zlib/exec.o Release-zlib/fileform.o Release-zlib/Main.o Release-zlib/Ui.o Release-zlib/components.o Release-zlib/util.o Release-zlib/crc32.o Release-zlib/resource.res Release-zlib/INFBLOCK.o

BZIP2_SRCS = bgbg.c exec.c fileform.c Main.c Ui.c components.c util.c ../crc32.c resource.rc ../bzip2/bzlib.c ../bzip2/decompress.c ../bzip2/huffman.c
BZIP2_OBJS = Release-bzip2/bgbg.o Release-bzip2/exec.o Release-bzip2/fileform.o Release-bzip2/Main.o Release-bzip2/Ui.o Release-bzip2/components.o Release-bzip2/util.o Release-bzip2/crc32.o Release-bzip2/resource.res Release-bzip2/bzlib.o Release-bzip2/decompress.o Release-bzip2/huffman.o

LZMA_SRCS = bgbg.c exec.c fileform.c Main.c Ui.c components.c util.c ../crc32.c resource.rc ../7zip/LZMADecode.c
LZMA_OBJS = Release-lzma/bgbg.o Release-lzma/exec.o Release-lzma/fileform.o Release-lzma/Main.o Release-lzma/Ui.o Release-lzma/components.o Release-lzma/util.o Release-lzma/crc32.o Release-lzma/resource.res Release-lzma/LZMADecode.o

SRCS = $(ZLIB_SRCS) $(BZIP2_SRCS) $(LZMA_SRCS)

LIBS = -lole32 -lgdi32 -lversion -luuid -lcomctl32 -lkernel32 -luser32 -lshell32 -ladvapi32

# Detect if cross compiling. If so, set cross prefix and NATIVE EXE suffix
ifeq "$(strip $(findstring i386pe,$(shell ld -V)))" ""

   # Helper functions. Why so many? So I can keep track.
   ccsuffix=gcc
   findexec = $(strip $(shell which $(1)))
   checkprefix = $(call findexec, $(1)$(ccsuffix))
   map = $(foreach x,$(2),$(call $(1),$(x)))
   chooseexec = $(firstword $(call map,checkprefix,$(1)))
   chooseprefix = $(subst $(ccsuffix),,$(notdir $(call chooseexec,$(1))))

   EXESUFF =
   POSSIBLE_CROSS_PREFIXES = mingw32- i386-mingw32msvc- i486-mingw32msvc- i586-mingw32msvc- i686-mingw32msvc-
   CROSS_PREFIX := $(call chooseprefix,$(POSSIBLE_CROSS_PREFIXES))
else
   CROSS_PREFIX =
   EXESUFF = .exe
endif

# -- Programs --
NATIVE_CC = gcc
CC        = $(CROSS_PREFIX)gcc
CXX       = $(CROSS_PREFIX)g++
RC        = $(CROSS_PREFIX)windres
RM        = rm
MKDIR     = mkdir
BIN2H     = ./bin2h$(EXESUFF)

# -- Compilers and linker flags --
CPPFLAGS = -DEXEHEAD -DWIN32_LEAN_AND_MEAN -DZEXPORT=__stdcall -DLZMACALL=__fastcall
CFLAGS = -Wall -Os
LDFLAGS = -s -mwindows -nostdlib -nostartfiles --enable-stdcall-fixup -Wl,-Bdynamic -Wl,--file-alignment,512 -Wl,--exclude-libs,msvcrt.a -Wl,-e,_WinMain@16 -Wl,sections_script -Wl,-Map,$(subst .exe,.map,$@)
RCFLAGS = --input-format rc --output-format coff

vpath %.c .:..:../zlib:../bzip2:../7zip
vpath %.C ../zlib

%.c : config.h fileform.h

ifeq ($(USE_PRECOMPILED_EXEHEADS),1)
all : Release-zlib/exehead_zlib.h Release-bzip2/exehead_bzip2.h Release-lzma/exehead_lzma.h
else
all : dirs exehead_zlib exehead_bzip2 exehead_lzma exehead_resources
endif

missing_dirs = $(filter-out $(wildcard Release-*),Release-zlib Release-bzip2 Release-lzma)

ifneq ($(strip $(missing_dirs)),)
  mkdirline = $(MKDIR) $(missing_dirs)
else
  mkdirline = 
endif

dirs :
	$(mkdirline)

$(BIN2H) : bin2h.c
	$(NATIVE_CC) -Wall -ansi -Wl,-s -o $@ $^

exehead_zlib : dirs Release-zlib/exehead_zlib.exe Release-zlib/exehead_zlib.h

Release-zlib/exehead_zlib.exe : $(ZLIB_OBJS) sections_script
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(ZLIB_OBJS) $(LIBS)

ifneq ($(USE_PRECOMPILED_EXEHEADS),1)
Release-zlib/exehead_zlib.h : Release-zlib/exehead_zlib.exe $(BIN2H)
	$(BIN2H) $< $@ zlib_exehead
endif

Release-zlib/%.o : %.c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $< -o $@

Release-zlib/%.o : %.C
	$(CC) -x c -c $(CPPFLAGS) $(CFLAGS) $< -o $@

Release-zlib/resource.res : resource.rc resource.h config.h
	$(RC) $(RCFLAGS) -o $@ -i $<

exehead_bzip2 : dirs Release-bzip2/exehead_bzip2.exe Release-bzip2/exehead_bzip2.h

Release-bzip2/exehead_bzip2.exe : $(BZIP2_OBJS) sections_script
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(BZIP2_OBJS) $(LIBS)

ifneq ($(USE_PRECOMPILED_EXEHEADS),1)
Release-bzip2/exehead_bzip2.h : Release-bzip2/exehead_bzip2.exe $(BIN2H)
	$(BIN2H) $< $@ bzip2_exehead
endif

Release-bzip2/%.o : %.c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $< -o $@

Release-bzip2/resource.res : resource.rc resource.h config.h
	$(RC) $(RCFLAGS) -o $@ -i $<

exehead_lzma : dirs Release-lzma/exehead_lzma.exe Release-lzma/exehead_lzma.h

Release-lzma/exehead_lzma.exe : $(LZMA_OBJS) sections_script
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(LZMA_OBJS) $(LIBS)

ifneq ($(USE_PRECOMPILED_EXEHEADS),1)
Release-lzma/exehead_lzma.h :  Release-lzma/exehead_lzma.exe $(BIN2H)
	$(BIN2H) $< $@ lzma_exehead
endif

Release-lzma/%.o : %.c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $< -o $@

Release-lzma/resource.res : resource.rc resource.h config.h
	$(RC) $(RCFLAGS) -o $@ -i $<

exehead_resources : dirs Release-zlib/bitmap1.h Release-zlib/icon.h Release-zlib/unicon.h

Release-zlib/bitmap1.h : bitmap1.bmp $(BIN2H)
	$(BIN2H) bitmap1.bmp Release-zlib/bitmap1.h bitmap1_data

Release-zlib/icon.h : nsis.ico $(BIN2H)
	$(BIN2H) nsis.ico Release-zlib/icon.h icon_data

Release-zlib/unicon.h : uninst.ico $(BIN2H)
	$(BIN2H) uninst.ico Release-zlib/unicon.h unicon_data

sections_script: 
	echo "SECTIONS" > sections_script
	echo "{" >> sections_script
	echo "  .text : { *(.text) }" >> sections_script
	echo "  .data : { *(.data) }" >> sections_script
	echo "  .rdata : { *(.rdata) }" >> sections_script
	echo "  .bss : { *(.bss) }" >> sections_script
	echo "  .idata : { *(.idata) }" >> sections_script
	echo "  .ndata BLOCK(__section_alignment__) : { [ .ndata ] }" >> sections_script
	echo "  .rsrc : { *(.rsrc) }" >> sections_script
	echo "}" >> sections_script

Release-zlib/ = -DNSIS_COMPRESS_USE_ZLIB
Release-bzip2/ = -DNSIS_COMPRESS_USE_BZIP2
Release-lzma/ = -DNSIS_COMPRESS_USE_LZMA
getdefine = $($(dir $@))

CFLAGS += $(getdefine)

clean ::
	$(RM) -f sections_script
	$(RM) -rf Release-{zlib,bzip2,lzma}
	$(RM) bin2h 2>/dev/null || true

.PHONY : exehead_zlib exehead_bzip2 exehead_lzma exehead_resources dirs clean
