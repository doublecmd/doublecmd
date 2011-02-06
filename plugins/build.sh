#!/bin/sh
# Build all plugins

# This script run from main build.sh script
# If you run it direct, set up $lazbuild first

# CD to plugins directory
basedir=$(pwd)
cd plugins
pluginsdir=$(pwd)

# WCX plugins
$lazbuild wcx/cpio/src/cpio.lpi $DC_ARCH
$lazbuild wcx/deb/src/deb.lpi $DC_ARCH
$lazbuild wcx/lzma/src/lzma.lpi $DC_ARCH
$lazbuild wcx/rpm/src/rpm.lpi $DC_ARCH
$lazbuild wcx/unbz2/src/unbz2.lpi $DC_ARCH
$lazbuild wcx/unrar/src/unrar.lpi $DC_ARCH
$lazbuild wcx/zip/src/Zip.lpi $DC_ARCH

# WDX plugins
$lazbuild wdx/rpm_wdx/src/rpm_wdx.lpi $DC_ARCH
$lazbuild wdx/deb_wdx/src/deb_wdx.lpi $DC_ARCH

# WFX plugins
$lazbuild wfx/ftp/src/ftp.lpi $DC_ARCH

# WLX plugins
$lazbuild wlx/WlxMplayer/src/wlxMplayer.lpi $DC_ARCH

# DSX plugins
$lazbuild dsx/DSXLocate/src/DSXLocate.lpi $DC_ARCH

# Strip and rename WCX
cd wcx/cpio/lib/
strip --strip-all libcpio.so
mv libcpio.so cpio.wcx
cd $pluginsdir

cd wcx/deb/lib/
strip --strip-all libdeb.so
mv libdeb.so deb.wcx
cd $pluginsdir

cd wcx/lzma/lib/
strip --strip-all liblzma.so
mv liblzma.so lzma.wcx
cd $pluginsdir

cd wcx/rpm/lib/
strip --strip-all librpm.so
mv librpm.so rpm.wcx
cd $pluginsdir

cd wcx/unbz2/lib/
strip --strip-all libunbz2.so
mv libunbz2.so unbz2.wcx
cd $pluginsdir

cd wcx/unrar/lib/
strip --strip-all libunrar.so
mv libunrar.so unrar.wcx
cd $pluginsdir

cd wcx/zip/lib/
strip --strip-all libzip.so
mv libzip.so zip.wcx
cd $pluginsdir

# Strip and rename WDX
cd wdx/rpm_wdx/lib/
strip --strip-all librpm_wdx.so
mv librpm_wdx.so rpm_wdx.wdx
cd $pluginsdir

cd wdx/deb_wdx/lib/
strip --strip-all libdeb_wdx.so
mv libdeb_wdx.so deb_wdx.wdx
cd $pluginsdir

# Strip and rename WFX
cd wfx/ftp/lib/
strip --strip-all libftp.so
mv libftp.so ftp.wfx
cd $pluginsdir

# Strip and rename WLX
cd wlx/WlxMplayer/lib/
strip --strip-all libwlxmplayer.so
mv libwlxmplayer.so wlxmplayer.wlx
cd $pluginsdir

# Strip and rename DSX
cd dsx/DSXLocate/lib/
strip --strip-all libdsxlocate.so
mv libdsxlocate.so dsxlocate.dsx
cd $pluginsdir

# Return from plugins directory
cd $basedir
