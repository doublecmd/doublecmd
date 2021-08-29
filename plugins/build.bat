@echo off

rem Build all plugins

rem Do not execute this script directly.
rem This script is called from ..\build.bat.

rem CD to plugins directory
pushd plugins

rem WCX plugins
lazbuild wcx\deb\src\deb.lpi              %DC_ARCH%
lazbuild wcx\rpm\src\rpm.lpi              %DC_ARCH%
lazbuild wcx\sevenzip\src\sevenzipwcx.lpi %DC_ARCH%
lazbuild wcx\unrar\src\unrar.lpi          %DC_ARCH%
lazbuild wcx\zip\src\zip.lpi              %DC_ARCH%

rem WDX plugins
lazbuild wdx\rpm_wdx\src\rpm_wdx.lpi     %DC_ARCH%
lazbuild wdx\deb_wdx\src\deb_wdx.lpi     %DC_ARCH%
lazbuild wdx\xpi_wdx\src\xpi_wdx.lpi     %DC_ARCH%
lazbuild wdx\audioinfo\src\AudioInfo.lpi %DC_ARCH%

rem WFX plugins
lazbuild wfx\ftp\src\ftp.lpi             %DC_ARCH%

rem WLX plugins
lazbuild wlx\preview\src\preview.lpi     %DC_ARCH%

rem Return from plugins directory
popd
