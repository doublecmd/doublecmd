#!/bin/sh
# Compiling components

# This script run from _make.bat
# If you run it direct, set up $lazpath first

pushd components
$lazpath/lazbuild KASToolBar/kascomp.lpk
$lazpath/lazbuild viewer/viewerpackage.lpk
popd