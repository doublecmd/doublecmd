rem Build libssh2 library

mkdir release
copy NUL release\version.inc
make dll WITH_WINCNG=1 LDFLAGS+=" -static -shared"
