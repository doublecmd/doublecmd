set PATH=W:\Prog\cmake-3.16.2-win32-x86\bin;W:\Prog\MinGW\mingw64\bin;%PATH%

rem Build libpcre2 library

cmake -G "MinGW Makefiles" -DBUILD_SHARED_LIBS=ON -DPCRE2_NEWLINE=CRLF
mingw32-make

strip --strip-debug --strip-unneeded libpcre2-8.dll
