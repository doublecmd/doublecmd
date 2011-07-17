@echo off

rem Build all plugins

rem This script is run from build.bat.
rem If you run it directly set %lazpath% first
rem or have lazbuild in your PATH.

rem CD to plugins directory
pushd plugins

rem WCX plugins
lazbuild wcx\cpio\src\cpio.lpi %DC_ARCH%
lazbuild wcx\deb\src\deb.lpi %DC_ARCH%
lazbuild wcx\lzma\src\lzma.lpi %DC_ARCH%
lazbuild wcx\rpm\src\rpm.lpi %DC_ARCH%
lazbuild wcx\unrar\src\unrar.lpi %DC_ARCH%
lazbuild wcx\zip\src\zip.lpi %DC_ARCH%

rem WDX plugins
lazbuild wdx\rpm_wdx\src\rpm_wdx.lpi %DC_ARCH%
lazbuild wdx\deb_wdx\src\deb_wdx.lpi %DC_ARCH%
lazbuild wdx\svn_wdx\src\svn_wdx.lpi %DC_ARCH%
lazbuild wdx\xpi_wdx\src\xpi_wdx.lpi %DC_ARCH%

rem WFX plugins
lazbuild wfx\ftp\src\ftp.lpi %DC_ARCH%

rem Strip and rename WCX
pushd wcx\cpio\lib\
strip --strip-all cpio.wcx
popd

pushd wcx\deb\lib\
strip --strip-all deb.wcx
popd

pushd wcx\lzma\lib\
strip --strip-all lzma.wcx
popd

pushd wcx\rpm\lib\
strip --strip-all rpm.wcx
popd

pushd wcx\unrar\lib\
strip --strip-all unrar.wcx
popd

pushd wcx\zip\lib\
strip --strip-all zip.wcx
popd

rem Strip and rename WDX
pushd wdx\rpm_wdx\lib\
strip --strip-all rpm_wdx.wdx
popd

pushd wdx\deb_wdx\lib\
strip --strip-all deb_wdx.wdx
popd

pushd wdx\svn_wdx\lib\
strip --strip-all svn_wdx.wdx
popd

pushd wdx\xpi_wdx\lib\
strip --strip-all xpi_wdx.wdx
popd

rem Strip and rename WFX
pushd wfx\ftp\lib\
strip --strip-all ftp.wfx
popd

rem Return from plugins directory
popd
