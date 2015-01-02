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

rem Return from plugins directory
popd
