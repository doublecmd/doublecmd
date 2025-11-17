#!/bin/bash

# The new package will be saved here
PACK_DIR=$PWD/doublecmd-release

# Temp dir for creating *.dmg package
BUILD_PACK_DIR=/var/tmp/doublecmd-$(date +%y.%m.%d)

# Save revision number
DC_REVISION=$(install/linux/update-revision.sh ./ ./)

# Read version number
DC_MAJOR=$(grep 'MajorVersionNr' src/doublecmd.lpi | grep -o '[0-9.]\+')
DC_MINOR=$(grep 'MinorVersionNr' src/doublecmd.lpi | grep -o '[0-9.]\+' || echo 0)
DC_MICRO=$(grep 'RevisionNr' src/doublecmd.lpi | grep -o '[0-9.]\+' || echo 0)
DC_VER=$DC_MAJOR.$DC_MINOR.$DC_MICRO

# Get libraries
pushd install
wget https://github.com/doublecmd/snapshots/raw/main/darwin.tar.gz
tar xzf darwin.tar.gz
rm -f darwin.tar.gz
popd

# Set widgetset
export lcl=cocoa

# Update application bundle version
defaults write $(pwd)/doublecmd.app/Contents/Info CFBundleVersion $DC_REVISION
defaults write $(pwd)/doublecmd.app/Contents/Info CFBundleShortVersionString $DC_VER
plutil -convert xml1 $(pwd)/doublecmd.app/Contents/Info.plist
chmod 644 $(pwd)/doublecmd.app/Contents/Info.plist

build_doublecmd()
{
  # Build all components of Double Commander
  ./build.sh release

  # Copy libraries
  cp -a install/darwin/lib/$CPU_TARGET/*.dylib ./

  # Prepare *.dmg package
  mkdir -p $BUILD_PACK_DIR
  install/darwin/install.sh $BUILD_PACK_DIR
  pushd $BUILD_PACK_DIR
  mv doublecmd.app 'Double Commander.app'
  codesign --deep --force --verify --verbose --sign '-' 'Double Commander.app'
  popd

  # Create *.dmg package
  HDI_TRY=0
  HDI_MAX=5
  
  while true; do

  HDI_TRY=$((HDI_TRY+1))

  echo "Try to create a package $HDI_TRY ..."

  # Bug: https://github.com/actions/runner-images/issues/7522
  echo Killing XProtect...; sudo pkill -9 XProtect >/dev/null || true;
  echo Waiting for XProtect process...; while pgrep XProtect; do sleep 3; done;

  sudo install/darwin/create-dmg/create-dmg \
    --volname "Double Commander" \
    --volicon "$BUILD_PACK_DIR/.VolumeIcon.icns" \
    --background "$BUILD_PACK_DIR/.background/bg.jpg" \
    --window-pos 200 200 \
    --window-size 680 366 \
    --text-size 16 \
    --icon-size 128 \
    --icon "Double Commander.app" 110 120 \
    --app-drop-link 360 120 \
    --icon "install.txt" 566 123 \
    --icon ".background" 100 500 \
    "$PACK_DIR/doublecmd-$DC_VER-$DC_REVISION.$lcl.$CPU_TARGET.dmg" \
    "$BUILD_PACK_DIR/"

  if [ $? -eq 0 ]; then
    break
  fi

  if [ $HDI_TRY -eq $HDI_MAX ]; then
    break
  fi

  sleep 10

  done

  # Clean DC build dir
  ./clean.sh
  rm -rf $BUILD_PACK_DIR
}

mkdir -p $PACK_DIR

echo $DC_REVISION > $PACK_DIR/revision.php

# Set processor architecture
export CPU_TARGET=aarch64
# Set minimal Mac OS X target version
export MACOSX_DEPLOYMENT_TARGET=11.0

build_doublecmd

# Set processor architecture
export CPU_TARGET=x86_64
# Set minimal Mac OS X target version
export MACOSX_DEPLOYMENT_TARGET=10.11

build_doublecmd
