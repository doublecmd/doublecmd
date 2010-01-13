#!/bin/bash
# Compiling components

# This script run from _make.bat
# If you run it direct, set up $lazpath first

pushd components
$lazpath/lazbuild CmdLine/cmdbox.lpk $DC_ARCH
$lazpath/lazbuild KASToolBar/kascomp.lpk $DC_ARCH
$lazpath/lazbuild viewer/viewerpackage.lpk $DC_ARCH
popd
