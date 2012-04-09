#!/bin/sh
# Compiling components

# This script run from main build.sh script
# If you run it direct, set up $lazbuild first

basedir=$(pwd)
cd components
$lazbuild chsdet/chsdet.lpk $DC_ARCH
$lazbuild CmdLine/cmdbox.lpk $DC_ARCH
$lazbuild dcpcrypt/dcpcrypt.lpk $DC_ARCH
$lazbuild doublecmd/doublecmd_common.lpk $%DC_ARCH
$lazbuild doublecmd/doublecmd_common_lcl.lpk $DC_ARCH
$lazbuild KASToolBar/kascomp.lpk $DC_ARCH
$lazbuild viewer/viewerpackage.lpk $DC_ARCH
$lazbuild gifanim/pkg_gifanim.lpk $DC_ARCH
$lazbuild ZVDateTimeCtrls/zvdatetimectrls.lpk $DC_ARCH
cd $basedir
