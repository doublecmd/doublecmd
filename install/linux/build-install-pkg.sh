#!/bin/bash

# This script build and install Double Commander Arch Linux package

# Temp directory
DC_TEMP_DIR=/var/tmp/doublecmd-$(date +%y.%m.%d)
# Directory for DC source code
DC_SOURCE_DIR=$DC_TEMP_DIR/src
# Widgetset library (gtk2 or qt)
LCL_PLATFORM=$1

# Set widgetset
if [ -z $LCL_PLATFORM ]; then
  export LCL_PLATFORM=gtk2
fi

# Recreate temp directory
rm -rf $DC_TEMP_DIR
mkdir -p $DC_TEMP_DIR

# Export from GIT
pushd ../../
mkdir -p $DC_SOURCE_DIR
git archive HEAD | tar -x -C $DC_SOURCE_DIR
popd

# Save revision number
DC_REVISION=`$(pwd)/update-revision.sh ../../ $DC_SOURCE_DIR`

# Prepare PKGBUILD file
cp -a pkg/doublecmd-svn.install $DC_TEMP_DIR
echo "$DC_REVISION" > $DC_SOURCE_DIR/revision.txt
cp -a pkg/doublecmd-$LCL_PLATFORM.pkgbuild $DC_TEMP_DIR/PKGBUILD

# Set temporary HOME for lazarus primary config directory
export HOME=$DC_TEMP_DIR
mkdir -p $DC_TEMP_DIR/.lazarus
cp -a pkg/environmentoptions.xml $DC_TEMP_DIR/.lazarus

pushd $DC_TEMP_DIR

# Build and install
makepkg --install

popd

# Clean
rm -rf $DC_TEMP_DIR
