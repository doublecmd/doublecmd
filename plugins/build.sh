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
$lazbuild wcx/unrar/src/unrar.lpi $DC_ARCH
$lazbuild wcx/zip/src/Zip.lpi $DC_ARCH

# WDX plugins
$lazbuild wdx/rpm_wdx/src/rpm_wdx.lpi $DC_ARCH
$lazbuild wdx/deb_wdx/src/deb_wdx.lpi $DC_ARCH
$lazbuild wdx/svn_wdx/src/svn_wdx.lpi $DC_ARCH
$lazbuild wdx/xpi_wdx/src/xpi_wdx.lpi $DC_ARCH

# WFX plugins
$lazbuild wfx/ftp/src/ftp.lpi $DC_ARCH
$lazbuild wfx/samba/src/samba.lpi $DC_ARCH

# WLX plugins
$lazbuild wlx/WlxMplayer/src/wlxMplayer.lpi $DC_ARCH

# DSX plugins
$lazbuild dsx/DSXLocate/src/DSXLocate.lpi $DC_ARCH

# Strip and rename WCX
cd wcx/cpio/lib/
strip --strip-all cpio.wcx
cd $pluginsdir

cd wcx/deb/lib/
strip --strip-all deb.wcx
cd $pluginsdir

cd wcx/lzma/lib/
strip --strip-all lzma.wcx
cd $pluginsdir

cd wcx/rpm/lib/
strip --strip-all rpm.wcx
cd $pluginsdir

cd wcx/unrar/lib/
strip --strip-all unrar.wcx
cd $pluginsdir

cd wcx/zip/lib/
strip --strip-all zip.wcx
cd $pluginsdir

# Strip and rename WDX
cd wdx/rpm_wdx/lib/
strip --strip-all rpm_wdx.wdx
cd $pluginsdir

cd wdx/deb_wdx/lib/
strip --strip-all deb_wdx.wdx
cd $pluginsdir

cd wdx/svn_wdx/lib/
strip --strip-all svn_wdx.wdx
cd $pluginsdir

cd wdx/xpi_wdx/lib/
strip --strip-all xpi_wdx.wdx
cd $pluginsdir

# Strip and rename WFX
cd wfx/ftp/lib/
strip --strip-all ftp.wfx
cd $pluginsdir

cd wfx/samba/lib/
strip --strip-all samba.wfx
cd $pluginsdir

# Strip and rename WLX
cd wlx/WlxMplayer/lib/
strip --strip-all wlxmplayer.wlx
cd $pluginsdir

# Strip and rename DSX
cd dsx/DSXLocate/lib/
strip --strip-all dsxlocate.dsx
cd $pluginsdir

# Return from plugins directory
cd $basedir
