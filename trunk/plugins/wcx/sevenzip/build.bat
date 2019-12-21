@echo off

set VERSION=19.12.21

rem The next two line must be changed before run on your computer
set lazpath=D:\Alexx\Prog\FreePascal\Lazarus

set PATH=%lazpath%;%PATH%

del /Q /S *.wcx*
del /Q /S lib\*.*

lazbuild.exe --cpu=x86_64 --os=win64 --bm=Release src\SevenZipWcx.lpi

ren sevenzip.wcx sevenzip.wcx64

del /Q /S lib\*.*

lazbuild.exe --cpu=i386 --os=win32 --bm=Release src\SevenZipWcx.lpi

del /Q /S lib\*.*

rem Prepare archive

del /Q /S release\*

copy "C:\Program Files (x86)\7-Zip\7z.dll"  release\i386\
copy "C:\Program Files\7-Zip\7z.dll"        release\x86_64\

copy LICENSE.txt     release\
copy pluginst.inf    release\
copy README.txt      release\
copy sevenzip.wcx    release\
copy sevenzip.wcx64  release\

del /Q sevenzip-*.zip
pushd release
"C:\Program Files\7-Zip\7z.exe" a ..\sevenzip-%VERSION%.zip .\*
popd

del /Q /S release\*
