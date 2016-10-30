# This Makefile builds a shared version of the library libbz2.dll

CC=gcc
LDFLAGS=-Wl,--enable-stdcall-fixup -Wl,--strip-all
CFLAGS=-Wall -Winline -O2 -D_FILE_OFFSET_BITS=64 -DBZ_NO_STDIO

# Determine output directory
ARCH=$(shell $(LD) --print-output-format)
ifeq ($(ARCH), pei-i386)
  OUTPUT=..\..\bin\i386-win32
else ifeq ($(ARCH), pei-x86-64)
  OUTPUT=..\..\bin\x86_64-win64
endif

OBJS= blocksort.o  \
      huffman.o    \
      crctable.o   \
      randtable.o  \
      compress.o   \
      decompress.o \
      bzlib.o      \
      error.o

all: $(OBJS)
	$(CC) -shared -static $(LDFLAGS) -o $(OUTPUT)\bz2.dll $(OBJS) libbz2.def

clean: 
	$(RM) -f $(OBJS) $(OUTPUT)\bz2.dll
