set PATH=C:\Qt\Tools\mingw810_32\bin;C:\Qt\Tools\CMake_64\bin;%PATH%

set SRC=%CD%

rem Build zlib

pushd zlib

rm -rf build

cmake.exe -B build -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_FLAGS="-static -D_WIN32_WINNT=0x0501"

cmake --build build

popd

rem Build mbedtls

pushd mbedtls

rm -rf build

cmake.exe -B build -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release -DENABLE_TESTING=Off -DCMAKE_C_FLAGS="-static -D_WIN32_WINNT=0x0501"

cmake --build build

popd

rem Build libssh2

rm -rf build

cmake.exe -B build -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_FLAGS="-static -D_WIN32_WINNT=0x0501" -DCRYPTO_BACKEND="mbedTLS" -DMBEDTLS_INCLUDE_DIR="%SRC%\mbedtls\include" -DMBEDCRYPTO_LIBRARY="%SRC%\mbedtls\build\library\libmbedtls.a" -DMBEDCRYPTO_LIBRARY="%SRC%\mbedtls\build\library\libmbedcrypto.a" -DENABLE_ZLIB_COMPRESSION=On -DZLIB_INCLUDE_DIR="%SRC%\zlib" -DZLIB_LIBRARY="%SRC%\zlib\build\libzlibstatic.a"

cmake --build build

strip --strip-debug --strip-unneeded build\src\libssh2.dll

pause
