#
# This makefile for mingw32 by Nels. Thanks, Nels
#
# Generalized for building on POSIX systems by Bas Mevissen
#

BUILDDIR=../build
# -- Objects and source files --
SRCS = zlib/deflate.c zlib/trees.c bzip2/blocksort.c bzip2/bzlib.c bzip2/compress.c bzip2/huffman.c 7zip/7zGuids.cpp 7zip/Common/CRC.cpp 7zip/7zip/Compress/LZ/LZInWindow.cpp 7zip/7zip/Compress/LZMA/LZMAEncoder.cpp 7zip/7zip/Common/OutBuffer.cpp 7zip/7zip/Compress/RangeCoder/RangeCoderBit.cpp 7zip/Common/Alloc.cpp build.cpp crc32.c DialogTemplate.cpp exedata.cpp lang.cpp makenssi.cpp Plugins.cpp ResourceEditor.cpp ResourceVersionInfo.cpp script.cpp tokens.cpp util.cpp strlist.cpp growbuf.cpp mmap.cpp clzma.cpp lineparse.cpp ShConstants.cpp
OBJS = $(addprefix $(BUILDDIR)/,$(addsuffix .o,$(basename $(SRCS))))
ifeq "$(strip $(findstring i386pe,$(shell ld -V)))" ""
LIBS = -lstdc++ -lpthread
EXESUFF =
else
LIBS = -lgdi32 -lversion -lstdc++
EXESUFF = .exe
endif

# -- Programs --
MAKE = make
CC = gcc
CXX = g++

# -- Compilers and linker flags --
DEFINES = -DCOMPRESS_MF_BT
CFLAGS += -Wall
CXXFLAGS += -Wall
CPPFLAGS = $(DEFINES)
RELEASE = 1
ifneq ($(EXESUFF),)
  LDFLAGS += -Wl,-Map,$(subst $(EXESUFF),.map,$@)
else
  LDFLAGS += -Wl,-Map,$@.map
endif

ifeq ($(PROFILING),1)
  CFLAGS += -g -pg -O3
  CXXFLAGS += -g -pg -O3
  LDFLAGS += -g -pg
  RELEASE = 0
endif

ifeq ($(DEBUG),1)
  CFLAGS += -g -O0
  CXXFLAGS += -g -O0
  LDFLAGS += -g
  RELEASE = 0
endif

ifeq ($(RELEASE),1)
  CFLAGS += -O3
  CXXFLAGS += -O3
  LDFLAGS += -s
endif

all : exehead makensis

exehead :
	$(MAKE) -C exehead

$(BUILDDIR)/%.o : %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@
$(BUILDDIR)/%.o : %.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $< -o $@

cs = zlib bzip2 lzma
rs = bitmap1.h icon.h unicon.h
deps = $(foreach c,$(cs),exehead/Release-$(c)/exehead_$(c).h) $(foreach r,$(rs),exehead/Release-zlib/$(r))
$(deps) : exehead

makensis : ../makensis$(EXESUFF)

exedata.o : $(deps)

../makensis$(EXESUFF) : $(OBJS)
	$(CC) -Wall $(LDFLAGS) -o ../makensis$(EXESUFF) $(OBJS) $(LIBS)

VPATH=zlib:bzip2:7zip:7zip/Common:7zip/7zip:7zip/7zip/Compress/LZ:7zip/7zip/Compress/LZMA:7zip/7zip/Common:7zip/7zip/Compress/RangeCoder

# -- Some dependencies --
$(OBJS) : Platform.h exehead/config.h
build.o : czlib.h cbzip2.h clzma.h exehead/fileform.h
script.o : exehead/fileform.h tokens.h

# -- Clean script --
clean : clean_makensis
	$(MAKE) -C exehead clean

clean_makensis :
	$(RM) $(OBJS)
	$(RM) -r $(BUILDDIR)
	$(RM) ../makensis$(EXESUFF) ../makensis.map

# -- Phony targets --
.PHONY : exehead makensis clean clean_makensis depend

depend :
	makedepend -Y. -p$(BUILDDIR)/ $(SRCS) >& /dev/null

# DO NOT DELETE

../build/zlib/deflate.o: zlib/DEFLATE.H zlib/ZUTIL.H Platform.h zlib/ZLIB.H
../build/zlib/deflate.o: zlib/ZCONF.H
../build/zlib/trees.o: zlib/DEFLATE.H zlib/ZUTIL.H Platform.h zlib/ZLIB.H
../build/zlib/trees.o: zlib/ZCONF.H
../build/bzip2/blocksort.o: bzip2/bzlib.h exehead/config.h Platform.h
../build/bzip2/bzlib.o: bzip2/bzlib.h exehead/config.h Platform.h
../build/bzip2/compress.o: bzip2/bzlib.h exehead/config.h Platform.h
../build/bzip2/huffman.o: bzip2/bzlib.h exehead/config.h Platform.h
../build/7zip/7zGuids.o: Platform.h 7zip/7zip/ICoder.h 7zip/7zip/IStream.h
../build/7zip/7zGuids.o: 7zip/Common/MyUnknown.h 7zip/Common/MyWindows.h
../build/7zip/7zGuids.o: Platform.h 7zip/Common/Types.h 7zip/Common/Types.h
../build/7zip/7zGuids.o: 7zip/7zip/Compress/LZ/IMatchFinder.h
../build/7zip/Common/CRC.o: 7zip/Common/StdAfx.h Platform.h 7zip/Common/CRC.h
../build/7zip/Common/CRC.o: 7zip/Common/Types.h
../build/7zip/7zip/Compress/LZ/LZInWindow.o: 7zip/Common/StdAfx.h Platform.h
../build/7zip/7zip/Compress/LZ/LZInWindow.o: 7zip/7zip/Compress/LZ/LZInWindow.h
../build/7zip/7zip/Compress/LZ/LZInWindow.o: 7zip/7zip/IStream.h
../build/7zip/7zip/Compress/LZ/LZInWindow.o: 7zip/Common/MyUnknown.h
../build/7zip/7zip/Compress/LZ/LZInWindow.o: 7zip/Common/MyWindows.h
../build/7zip/7zip/Compress/LZ/LZInWindow.o: 7zip/Common/Types.h
../build/7zip/7zip/Compress/LZ/LZInWindow.o: 7zip/Common/Types.h
../build/7zip/7zip/Compress/LZ/LZInWindow.o: 7zip/Common/MyCom.h
../build/7zip/7zip/Compress/LZ/LZInWindow.o: 7zip/Common/Alloc.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/StdAfx.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: Platform.h 7zip/Common/Defs.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZMA/LZMAEncoder.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/MyCom.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/MyWindows.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/Types.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/Alloc.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/ICoder.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/IStream.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/MyUnknown.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/Types.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/IMatchFinder.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBitTree.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBit.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/RangeCoder/RangeCoder.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Common/InBuffer.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/IStream.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Common/OutBuffer.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/RangeCoder/RangeCoderOpt.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZMA/LZMA.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/BinTree/BinTree2.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/BinTree/BinTreeMF.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/ICoder.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/BinTree/BinTree.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/LZInWindow.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/IStream.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/BinTree/BinTreeMFMain.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/BinTree/BinTreeMain.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/Defs.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/CRC.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/Common/Alloc.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/BinTree/BinTree3.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/BinTree/BinTree4.h
../build/7zip/7zip/Compress/LZMA/LZMAEncoder.o: 7zip/7zip/Compress/LZ/BinTree/BinTree4b.h
../build/7zip/7zip/Common/OutBuffer.o: 7zip/Common/StdAfx.h Platform.h
../build/7zip/7zip/Common/OutBuffer.o: 7zip/7zip/Common/OutBuffer.h
../build/7zip/7zip/Common/OutBuffer.o: 7zip/7zip/IStream.h
../build/7zip/7zip/Common/OutBuffer.o: 7zip/Common/Alloc.h
../build/7zip/7zip/Compress/RangeCoder/RangeCoderBit.o: 7zip/Common/StdAfx.h
../build/7zip/7zip/Compress/RangeCoder/RangeCoderBit.o: Platform.h
../build/7zip/7zip/Compress/RangeCoder/RangeCoderBit.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBit.h
../build/7zip/7zip/Compress/RangeCoder/RangeCoderBit.o: 7zip/7zip/Compress/RangeCoder/RangeCoder.h
../build/7zip/7zip/Compress/RangeCoder/RangeCoderBit.o: 7zip/7zip/Common/InBuffer.h
../build/7zip/7zip/Compress/RangeCoder/RangeCoderBit.o: 7zip/7zip/IStream.h
../build/7zip/7zip/Compress/RangeCoder/RangeCoderBit.o: 7zip/7zip/Common/OutBuffer.h
../build/7zip/Common/Alloc.o: 7zip/Common/StdAfx.h Platform.h
../build/7zip/Common/Alloc.o: 7zip/Common/Alloc.h
../build/build.o: Platform.h exehead/config.h exehead/fileform.h
../build/build.o: exehead/config.h exedata.h build.h strlist.h growbuf.h
../build/build.o: lineparse.h lang.h ResourceEditor.h ResourceVersionInfo.h
../build/build.o: uservars.h ShConstants.h mmap.h compressor.h czlib.h
../build/build.o: zlib/ZLIB.H zlib/ZCONF.H zlib/ZUTIL.H Platform.h
../build/build.o: zlib/ZLIB.H cbzip2.h bzip2/bzlib.h exehead/config.h clzma.h
../build/build.o: 7zip/7zip/IStream.h 7zip/Common/MyUnknown.h
../build/build.o: 7zip/Common/MyWindows.h Platform.h 7zip/Common/Types.h
../build/build.o: 7zip/Common/Types.h 7zip/7zip/Compress/LZMA/LZMAEncoder.h
../build/build.o: 7zip/Common/MyCom.h 7zip/Common/Alloc.h 7zip/7zip/ICoder.h
../build/build.o: 7zip/7zip/IStream.h 7zip/7zip/Compress/LZ/IMatchFinder.h
../build/build.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBitTree.h
../build/build.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBit.h
../build/build.o: 7zip/7zip/Compress/RangeCoder/RangeCoder.h
../build/build.o: 7zip/7zip/Common/InBuffer.h 7zip/7zip/IStream.h
../build/build.o: 7zip/7zip/Common/OutBuffer.h
../build/build.o: 7zip/7zip/Compress/RangeCoder/RangeCoderOpt.h
../build/build.o: 7zip/7zip/Compress/LZMA/LZMA.h 7zip/Common/MyCom.h
../build/build.o: 7zip/Common/Defs.h Plugins.h util.h exehead/resource.h
../build/build.o: DialogTemplate.h
../build/crc32.o: Platform.h exehead/config.h
../build/DialogTemplate.o: DialogTemplate.h Platform.h util.h
../build/DialogTemplate.o: ResourceEditor.h
../build/exedata.o: exedata.h exehead/Release-zlib/icon.h
../build/exedata.o: exehead/Release-zlib/unicon.h
../build/exedata.o: exehead/Release-zlib/exehead_zlib.h
../build/exedata.o: exehead/Release-bzip2/exehead_bzip2.h
../build/exedata.o: exehead/Release-lzma/exehead_lzma.h
../build/lang.o: Platform.h build.h strlist.h growbuf.h lineparse.h lang.h
../build/lang.o: exehead/fileform.h exehead/config.h ResourceEditor.h
../build/lang.o: ResourceVersionInfo.h exehead/config.h uservars.h
../build/lang.o: ShConstants.h mmap.h compressor.h czlib.h zlib/ZLIB.H
../build/lang.o: zlib/ZCONF.H zlib/ZUTIL.H Platform.h zlib/ZLIB.H cbzip2.h
../build/lang.o: bzip2/bzlib.h exehead/config.h clzma.h 7zip/7zip/IStream.h
../build/lang.o: 7zip/Common/MyUnknown.h 7zip/Common/MyWindows.h Platform.h
../build/lang.o: 7zip/Common/Types.h 7zip/Common/Types.h
../build/lang.o: 7zip/7zip/Compress/LZMA/LZMAEncoder.h 7zip/Common/MyCom.h
../build/lang.o: 7zip/Common/Alloc.h 7zip/7zip/ICoder.h 7zip/7zip/IStream.h
../build/lang.o: 7zip/7zip/Compress/LZ/IMatchFinder.h
../build/lang.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBitTree.h
../build/lang.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBit.h
../build/lang.o: 7zip/7zip/Compress/RangeCoder/RangeCoder.h
../build/lang.o: 7zip/7zip/Common/InBuffer.h 7zip/7zip/IStream.h
../build/lang.o: 7zip/7zip/Common/OutBuffer.h
../build/lang.o: 7zip/7zip/Compress/RangeCoder/RangeCoderOpt.h
../build/lang.o: 7zip/7zip/Compress/LZMA/LZMA.h 7zip/Common/MyCom.h
../build/lang.o: 7zip/Common/Defs.h Plugins.h util.h DialogTemplate.h
../build/lang.o: exehead/resource.h
../build/makenssi.o: Platform.h build.h strlist.h growbuf.h lineparse.h
../build/makenssi.o: lang.h exehead/fileform.h exehead/config.h
../build/makenssi.o: ResourceEditor.h ResourceVersionInfo.h exehead/config.h
../build/makenssi.o: uservars.h ShConstants.h mmap.h compressor.h czlib.h
../build/makenssi.o: zlib/ZLIB.H zlib/ZCONF.H zlib/ZUTIL.H Platform.h
../build/makenssi.o: zlib/ZLIB.H cbzip2.h bzip2/bzlib.h exehead/config.h
../build/makenssi.o: clzma.h 7zip/7zip/IStream.h 7zip/Common/MyUnknown.h
../build/makenssi.o: 7zip/Common/MyWindows.h Platform.h 7zip/Common/Types.h
../build/makenssi.o: 7zip/Common/Types.h
../build/makenssi.o: 7zip/7zip/Compress/LZMA/LZMAEncoder.h
../build/makenssi.o: 7zip/Common/MyCom.h 7zip/Common/Alloc.h
../build/makenssi.o: 7zip/7zip/ICoder.h 7zip/7zip/IStream.h
../build/makenssi.o: 7zip/7zip/Compress/LZ/IMatchFinder.h
../build/makenssi.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBitTree.h
../build/makenssi.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBit.h
../build/makenssi.o: 7zip/7zip/Compress/RangeCoder/RangeCoder.h
../build/makenssi.o: 7zip/7zip/Common/InBuffer.h 7zip/7zip/IStream.h
../build/makenssi.o: 7zip/7zip/Common/OutBuffer.h
../build/makenssi.o: 7zip/7zip/Compress/RangeCoder/RangeCoderOpt.h
../build/makenssi.o: 7zip/7zip/Compress/LZMA/LZMA.h 7zip/Common/MyCom.h
../build/makenssi.o: 7zip/Common/Defs.h Plugins.h util.h exedata.h
../build/Plugins.o: exehead/config.h Plugins.h Platform.h strlist.h growbuf.h
../build/Plugins.o: util.h ResourceEditor.h
../build/ResourceEditor.o: ResourceEditor.h Platform.h util.h
../build/ResourceVersionInfo.o: ResourceVersionInfo.h exehead/config.h
../build/ResourceVersionInfo.o: Platform.h strlist.h growbuf.h util.h
../build/ResourceVersionInfo.o: ResourceEditor.h
../build/script.o: Platform.h tokens.h build.h strlist.h growbuf.h
../build/script.o: lineparse.h lang.h exehead/fileform.h exehead/config.h
../build/script.o: ResourceEditor.h ResourceVersionInfo.h exehead/config.h
../build/script.o: uservars.h ShConstants.h mmap.h compressor.h czlib.h
../build/script.o: zlib/ZLIB.H zlib/ZCONF.H zlib/ZUTIL.H Platform.h
../build/script.o: zlib/ZLIB.H cbzip2.h bzip2/bzlib.h exehead/config.h
../build/script.o: clzma.h 7zip/7zip/IStream.h 7zip/Common/MyUnknown.h
../build/script.o: 7zip/Common/MyWindows.h Platform.h 7zip/Common/Types.h
../build/script.o: 7zip/Common/Types.h 7zip/7zip/Compress/LZMA/LZMAEncoder.h
../build/script.o: 7zip/Common/MyCom.h 7zip/Common/Alloc.h 7zip/7zip/ICoder.h
../build/script.o: 7zip/7zip/IStream.h 7zip/7zip/Compress/LZ/IMatchFinder.h
../build/script.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBitTree.h
../build/script.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBit.h
../build/script.o: 7zip/7zip/Compress/RangeCoder/RangeCoder.h
../build/script.o: 7zip/7zip/Common/InBuffer.h 7zip/7zip/IStream.h
../build/script.o: 7zip/7zip/Common/OutBuffer.h
../build/script.o: 7zip/7zip/Compress/RangeCoder/RangeCoderOpt.h
../build/script.o: 7zip/7zip/Compress/LZMA/LZMA.h 7zip/Common/MyCom.h
../build/script.o: 7zip/Common/Defs.h Plugins.h util.h exedata.h
../build/script.o: DialogTemplate.h exehead/resource.h
../build/tokens.o: Platform.h build.h strlist.h growbuf.h lineparse.h lang.h
../build/tokens.o: exehead/fileform.h exehead/config.h ResourceEditor.h
../build/tokens.o: ResourceVersionInfo.h exehead/config.h uservars.h
../build/tokens.o: ShConstants.h mmap.h compressor.h czlib.h zlib/ZLIB.H
../build/tokens.o: zlib/ZCONF.H zlib/ZUTIL.H Platform.h zlib/ZLIB.H cbzip2.h
../build/tokens.o: bzip2/bzlib.h exehead/config.h clzma.h 7zip/7zip/IStream.h
../build/tokens.o: 7zip/Common/MyUnknown.h 7zip/Common/MyWindows.h Platform.h
../build/tokens.o: 7zip/Common/Types.h 7zip/Common/Types.h
../build/tokens.o: 7zip/7zip/Compress/LZMA/LZMAEncoder.h 7zip/Common/MyCom.h
../build/tokens.o: 7zip/Common/Alloc.h 7zip/7zip/ICoder.h 7zip/7zip/IStream.h
../build/tokens.o: 7zip/7zip/Compress/LZ/IMatchFinder.h
../build/tokens.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBitTree.h
../build/tokens.o: 7zip/7zip/Compress/RangeCoder/RangeCoderBit.h
../build/tokens.o: 7zip/7zip/Compress/RangeCoder/RangeCoder.h
../build/tokens.o: 7zip/7zip/Common/InBuffer.h 7zip/7zip/IStream.h
../build/tokens.o: 7zip/7zip/Common/OutBuffer.h
../build/tokens.o: 7zip/7zip/Compress/RangeCoder/RangeCoderOpt.h
../build/tokens.o: 7zip/7zip/Compress/LZMA/LZMA.h 7zip/Common/MyCom.h
../build/tokens.o: 7zip/Common/Defs.h Plugins.h tokens.h
../build/util.o: Platform.h exedata.h exehead/fileform.h exehead/config.h
../build/util.o: util.h ResourceEditor.h strlist.h growbuf.h
../build/strlist.o: strlist.h Platform.h growbuf.h
../build/growbuf.o: growbuf.h Platform.h
../build/mmap.o: mmap.h Platform.h growbuf.h
