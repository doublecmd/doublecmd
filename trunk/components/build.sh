#!/bin/sh

set -e

# Compiling components

# Do not execute this script directly.
# This script is called from ../build.sh.

# Get processor architecture
if [ -z $CPU_TARGET ] ; then
  export CPU_TARGET=$(fpc -iTP)
fi

# Generate PIC code
if [ "$CPU_TARGET" != "arm" ] ; then
  if [ -f /etc/fpc.cfg ] ; then
    cp /etc/fpc.cfg ./
    echo "-fPIC" >> fpc.cfg
    export PPC_CONFIG_PATH=$(pwd)
  fi
fi

# Build components
basedir=$(pwd)
cd components
$lazbuild chsdet/chsdet.lpk $DC_ARCH
$lazbuild CmdLine/cmdbox.lpk $DC_ARCH
$lazbuild multithreadprocs/multithreadprocslaz.lpk $DC_ARCH
$lazbuild dcpcrypt/dcpcrypt.lpk $DC_ARCH
$lazbuild doublecmd/doublecmd_common.lpk $DC_ARCH
$lazbuild KASToolBar/kascomp.lpk $DC_ARCH
$lazbuild viewer/viewerpackage.lpk $DC_ARCH
$lazbuild gifanim/pkg_gifanim.lpk $DC_ARCH
$lazbuild synunihighlighter/synuni.lpk $DC_ARCH
cd $basedir

# Remove temporary file
if [ -f fpc.cfg ] ; then
  rm -f fpc.cfg
  export PPC_CONFIG_PATH=
fi
