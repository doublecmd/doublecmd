# path definitions
DESTDIR = /usr
INSTALL=install -c
INSTALL_DATA = ${INSTALL} -m 644

# compiler options
CC = gcc
CFLAGS =-I. -I/usr/include -I../../../../sdk \
	-Wall -fPIC -O2 -g \
	-DG_DISABLE_DEPRECATED -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -D_GNU_SOURCE


LIB_SUFFIX=`if test \`uname -m\` = x86_64; then echo 64; fi`

# VFS_COMMON_OBJECTS=strutils.o treepathutils.o treepath_vfs.o vfsutils.o
VFS_COMMON_OBJECTS=

VFS_OBJECTS=gvfs.o


.SUFFIXES: .c
.c.o:
	$(CC) $(CFLAGS) `pkg-config glib-2.0 gio-2.0 --cflags` -c $<


all shared static: libgvfs_plugin.wfx

libgvfs_plugin.wfx: $(VFS_COMMON_OBJECTS) $(VFS_OBJECTS)
	$(CC) -shared -o ../lib/libgvfs_plugin.wfx $(VFS_COMMON_OBJECTS) $(VFS_OBJECTS) $(CFLAGS) `pkg-config glib-2.0 gio-2.0 --libs`

strutils.o: strutils.c strutils.h
treepathutils.o: treepathutils.c treepathutils.h
vfsutils.o: vfsutils.c vfsutils.h
treepath_vfs.o: treepath_vfs.c treepath_vfs.h

install::
	$(INSTALL) ./libgvfs_plugin.wfx $(DESTDIR)/lib$(LIB_SUFFIX)/tuxcmd/

clean: 
	rm -f *.o *.d *.gch libgvfs_plugin.wfx
