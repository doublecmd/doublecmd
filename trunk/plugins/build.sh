#!/bin/sh
# Build all plugins

# This script run from _make.sh
# If you run it direct, set up $lazpath first

# CD to plugins directory
pushd plugins

# WCX plugins
$lazpath/lazbuild wcx/cpio/cpio.lpi
$lazpath/lazbuild wcx/deb/deb.lpi
$lazpath/lazbuild wcx/rpm/rpm.lpi
$lazpath/lazbuild wcx/unbz2/unbz2.lpi
$lazpath/lazbuild wcx/unrar/unrar.lpi
$lazpath/lazbuild wcx/zip/Zip.lpi

# WDX plugins
$lazpath/lazbuild wdx/rpm_wdx/rpm_wdx.lpi

# WLX plugins
$lazpath/lazbuild wlx/WlxMplayer/wlxMplayer.lpi

# Strip and rename WCX
pushd wcx/cpio/lib/
strip --strip-all cpio.so
mv cpio.so cpio.wcx
popd

pushd wcx/deb/lib/
strip --strip-all deb.so
mv deb.so deb.wcx
popd

pushd wcx/rpm/lib/
strip --strip-all rpm.so
mv rpm.so rpm.wcx
popd

pushd wcx/unbz2/lib/
strip --strip-all unbz2.so
mv unbz2.so unbz2.wcx
popd

pushd wcx/unrar/lib/
strip --strip-all unrar.so
mv unrar.so unrar.wcx
popd

pushd wcx/zip/lib/
strip --strip-all Zip.so
mv Zip.so zip.wcx
popd

# Strip and rename WDX
pushd wdx/rpm_wdx/lib/
strip --strip-all rpm_wdx.so
mv rpm_wdx.so rpm_wdx.wdx
popd

# Strip and rename WLX
pushd wlx/WlxMplayer/lib/
strip --strip-all wlxMplayer.so
mv wlxMplayer.so wlxMplayer.wlx
popd

# Return from plugins directory
popd