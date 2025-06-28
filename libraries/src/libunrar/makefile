#
# Makefile for Windows - unrar.dll

# MinGW
CXX=g++
CXXFLAGS=-O2 -std=c++11 -Wno-logical-op-parentheses -Wno-switch -Wno-dangling-else
LIBFLAGS=-fPIC
DEFINES=-DRARDLL -DUNRAR -DSILENT -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -D_WIN32_WINNT=0x0501
LDFLAGS=-Wl,--enable-stdcall-fixup -Wl,--strip-all

##########################

COMPILE=$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(DEFINES)
LINK=$(CXX)

TARGET=unrar.dll

LIB_OBJ=filestr.o scantree.o dll.o qopen.o version.o

OBJECTS=rar.o strlist.o strfn.o pathfn.o smallfn.o global.o file.o filefn.o filcreat.o \
	archive.o arcread.o unicode.o system.o crypt.o crc.o rawread.o encname.o \
	match.o timefn.o rdwrfn.o consio.o options.o errhnd.o rarvm.o secpassword.o \
	rijndael.o getbits.o sha1.o sha256.o blake2s.o hash.o extinfo.o extract.o volume.o \
	find.o unpack.o headers.o threadpool.o rs16.o cmddata.o ui.o largepage.o isnt.o motw.o

version.o:
	windres dll.rc -o version.o

.cpp.o:
	$(COMPILE) -c $<

all:	lib

clean:
	@rm -f $(OBJECTS) $(LIB_OBJ)
	@rm -f $(TARGET)

lib:	CXXFLAGS+=$(LIBFLAGS)
lib:	$(OBJECTS) $(LIB_OBJ)
	$(LINK) -shared -static -o $(TARGET) $(LDFLAGS) $(OBJECTS) $(LIB_OBJ) dll.def
