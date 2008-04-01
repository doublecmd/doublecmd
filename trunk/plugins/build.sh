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

# Strip and rename WCX
pushd wcx/cpio/lib/
strip --strip-all cpio
mv cpio cpio.wcx
popd

pushd wcx/deb/lib/
strip --strip-all deb
mv deb deb.wcx
popd

pushd wcx/rpm/lib/
strip --strip-all rpm
mv rpm rpm.wcx
popd

pushd wcx/unbz2/lib/
strip --strip-all unbz2
mv unbz2 unbz2.wcx
popd

pushd wcx/unrar/lib/
strip --strip-all unrar
mv unrar unrar.wcx
popd

pushd wcx/zip/lib/
strip --strip-all Zip
mv Zip zip.wcx
popd

# Strip and rename WDX
pushd wdx/rpm_wdx/lib/
strip --strip-all rpm_wdx
mv rpm_wdx rpm_wdx.wcx
popd

# Return from plugins directory
popd