# compiler options
CC = gcc
CFLAGS =-I. -I/usr/include -I../../../../sdk \
	-Wall -fPIC -O2 -g \
	-DG_DISABLE_DEPRECATED -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -D_GNU_SOURCE

LIB_SUFFIX=`if test \`uname -m\` = x86_64; then echo 64; fi`

VFS_OBJECTS=gvfs.o

.SUFFIXES: .c
.c.o:
	$(CC) $(CFLAGS) `pkg-config glib-2.0 gio-2.0 --cflags` -c $<

all shared static: libgvfs_plugin.wfx

libgvfs_plugin.wfx: $(VFS_OBJECTS)
	$(CC) -shared -o ../lib/libgvfs_plugin.wfx $(VFS_OBJECTS) $(CFLAGS) `pkg-config glib-2.0 gio-2.0 --libs`

clean: 
	rm -f *.o *.d *.gch
	rm -f ../lib/libgvfs_plugin.wfx
