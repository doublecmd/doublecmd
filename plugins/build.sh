#!/bin/sh

set -e

# Build all plugins

# This script run from main build.sh script
# If you run it direct, set up $lazbuild first

# CD to plugins directory
basedir=$(pwd)
cd plugins

# WCX plugins
$lazbuild wcx/cpio/src/cpio.lpi $DC_ARCH
strip wcx/cpio/lib/cpio.wcx

$lazbuild wcx/deb/src/deb.lpi $DC_ARCH
strip wcx/deb/lib/deb.wcx

$lazbuild wcx/rpm/src/rpm.lpi $DC_ARCH
strip wcx/rpm/lib/rpm.wcx

$lazbuild wcx/unrar/src/unrar.lpi $DC_ARCH
strip wcx/unrar/lib/unrar.wcx

$lazbuild wcx/zip/src/Zip.lpi $DC_ARCH
strip wcx/zip/lib/zip.wcx

# WDX plugins
$lazbuild wdx/rpm_wdx/src/rpm_wdx.lpi $DC_ARCH
strip wdx/rpm_wdx/lib/rpm_wdx.wdx

$lazbuild wdx/deb_wdx/src/deb_wdx.lpi $DC_ARCH
strip wdx/deb_wdx/lib/deb_wdx.wdx

$lazbuild wdx/svn_wdx/src/svn_wdx.lpi $DC_ARCH
strip wdx/svn_wdx/lib/svn_wdx.wdx

$lazbuild wdx/xpi_wdx/src/xpi_wdx.lpi $DC_ARCH
strip wdx/xpi_wdx/lib/xpi_wdx.wdx

# WFX plugins
$lazbuild wfx/ftp/src/ftp.lpi $DC_ARCH
strip wfx/ftp/lib/ftp.wfx

# Don't build under OS X
if [ ! -d /System/Library ]; then

  $lazbuild wfx/samba/src/samba.lpi $DC_ARCH
  strip wfx/samba/lib/samba.wfx

  # WLX plugins
  $lazbuild wlx/WlxMplayer/src/wlxMplayer.lpi $DC_ARCH
  strip wlx/WlxMplayer/lib/wlxmplayer.wlx

fi

# DSX plugins
$lazbuild dsx/DSXLocate/src/DSXLocate.lpi $DC_ARCH
strip dsx/DSXLocate/lib/dsxlocate.dsx

# Return from plugins directory
cd $basedir
