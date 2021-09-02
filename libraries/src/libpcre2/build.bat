rem Build libpcre2 library

cmake -G "MinGW Makefiles" -DBUILD_SHARED_LIBS=ON -DPCRE2_NEWLINE=CRLF
mingw32-make
