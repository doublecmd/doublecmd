rem Build all plugins

rem This script run from _make.bat
rem If you run it direct, set up %lazpath% first

rem CD to plugins directory
pushd plugins

rem WCX plugins
%lazpath%\lazbuild.exe wcx\cpio\cpio.lpi
%lazpath%\lazbuild.exe wcx\deb\deb.lpi
%lazpath%\lazbuild.exe wcx\rpm\rpm.lpi
%lazpath%\lazbuild.exe wcx\unbz2\unbz2.lpi
%lazpath%\lazbuild.exe wcx\unrar\unrar.lpi
%lazpath%\lazbuild.exe wcx\zip\zip.lpi

rem WDX plugins
%lazpath%\lazbuild.exe wdx\rpm_wdx\rpm_wdx.lpi
%lazpath%\lazbuild.exe wdx\deb_wdx\src\deb_wdx.lpi

rem Strip and rename WCX
pushd wcx\cpio\lib\
strip --strip-all cpio.dll
rename cpio.dll cpio.wcx
popd

pushd wcx\deb\lib\
strip --strip-all deb.dll
rename deb.dll deb.wcx
popd

pushd wcx\rpm\lib\
strip --strip-all rpm.dll
rename rpm.dll rpm.wcx
popd

pushd wcx\unbz2\lib\
strip --strip-all unbz2.dll
rename unbz2.dll unbz2.wcx
popd

pushd wcx\unrar\lib\
strip --strip-all unrar.dll
rename unrar.dll unrar.wcx
popd

pushd wcx\zip\lib\
strip --strip-all zip.dll
rename zip.dll zip.wcx
popd

rem Strip and rename WDX
pushd wdx\rpm_wdx\lib\
strip --strip-all rpm_wdx.dll
rename rpm_wdx.dll rpm_wdx.wdx
popd

pushd wdx\deb_wdx\lib\
strip --strip-all deb_wdx.dll
rename deb_wdx.dll deb_wdx.wdx
popd

rem Return from plugins directory
popd