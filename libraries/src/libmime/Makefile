# Needs to be linked with GLib2.

CPPFLAGS = -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include # -g3

ifndef CPU_TARGET
CPU_TARGET=i386
endif

ifeq ($(CPU_TARGET),x86_64)
CPPFLAGS += -m64
else
ifeq ($(CPU_TARGET),i386)
CPPFLAGS += -m32
else
$(error Invalid target CPU)
endif
endif

all: clean libmime

clean:
	rm -f *.o *.a *.so.*

libmime: mime-action.o mime-cache.o mime-type.o mime-util.o
	$(AR) rc libmime.a *.o
	cp libmime.a ../../$(CPU_TARGET)
# shared library:
#	$(CC) -shared -Wl,-soname,libmime.so.1 -o libmime.so.1.0.1 libmime.a
#	ln -s libmime.so.1 libmime.so
