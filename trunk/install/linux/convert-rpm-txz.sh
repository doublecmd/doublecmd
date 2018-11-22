#!/bin/bash

# This script converts *.rpm package to portable package

# Script directory
SCRIPT_DIR=$(pwd)

# Source directory
DC_SOURCE_DIR=$SCRIPT_DIR/../..

# The new package will be saved here
PACK_DIR=$SCRIPT_DIR/release

# Temp directory
DC_TEMP_DIR=/var/tmp/doublecmd-$(date +%y.%m.%d)

# Root directory
DC_ROOT_DIR=$DC_TEMP_DIR/doublecmd

# Base file name
BASE_NAME=$(basename $1 .rpm)

# Set widgetset
LCL_PLATFORM=$(echo $BASE_NAME | grep -Po '(?<=doublecmd-)[^-]+')

# Set version
DC_VER=$(echo $BASE_NAME | grep -Po "(?<=doublecmd-$LCL_PLATFORM-)[^-]+")

# Set processor architecture
CPU_TARGET=${BASE_NAME##*.}
if [ "$CPU_TARGET" = "i686" ]; then
  export CPU_TARGET=i386
fi

# Update widgetset
if [ "$LCL_PLATFORM" = "gtk" ]; then
  export LCL_PLATFORM=gtk2
fi

# Recreate temp directory
rm -rf $DC_TEMP_DIR
mkdir -p $DC_TEMP_DIR

pushd $DC_TEMP_DIR

$SCRIPT_DIR/rpm2cpio.sh $1 | cpio -idmv

if [ "$CPU_TARGET" = "x86_64" ]
  then
    mv usr/lib64/doublecmd ./
  else
    mv usr/lib/doublecmd   ./
fi

# Remove symlinks
rm -f doublecmd/doc
rm -f doublecmd/language
rm -f doublecmd/pixmaps
rm -f doublecmd/highlighters

# Move directories and files
mv usr/share/doublecmd/doc                       $DC_ROOT_DIR/
mv usr/share/doublecmd/language                  $DC_ROOT_DIR/
mv usr/share/doublecmd/pixmaps                   $DC_ROOT_DIR/
mv usr/share/doublecmd/highlighters              $DC_ROOT_DIR/
mv usr/share/pixmaps/doublecmd.png               $DC_ROOT_DIR/

# Copy libraries
pushd $DC_SOURCE_DIR/install/linux
cp -a lib/$CPU_TARGET/*.so*                      $DC_ROOT_DIR/
cp -a lib/$CPU_TARGET/$LCL_PLATFORM/*.so*        $DC_ROOT_DIR/
popd

# Copy script for execute portable version
install -m 755 $DC_SOURCE_DIR/doublecmd.sh       $DC_ROOT_DIR/

# Make portable config file
touch $DC_ROOT_DIR/doublecmd.inf

# Create archive
tar -cJvf $PACK_DIR/doublecmd-$DC_VER.$LCL_PLATFORM.$CPU_TARGET.tar.xz doublecmd

popd

# Clean
rm -rf $DC_TEMP_DIR
