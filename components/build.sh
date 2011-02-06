#!/bin/sh
# Compiling components

# This script run from main build.sh script
# If you run it direct, set up $lazbuild first

basedir=$(pwd)
cd components
$lazbuild CmdLine/cmdbox.lpk $DC_ARCH
$lazbuild KASToolBar/kascomp.lpk $DC_ARCH
$lazbuild viewer/viewerpackage.lpk $DC_ARCH
$lazbuild gifanim/pkg_gifanim.lpk $DC_ARCH
cd $basedir
