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

# Set widgetset
export lcl=cocoa

mkdir -p $PACK_DIR

# Update application bundle version
defaults write $(pwd)/doublecmd.app/Contents/Info CFBundleVersion $DC_REVISION
defaults write $(pwd)/doublecmd.app/Contents/Info CFBundleShortVersionString $DC_VER
plutil -convert xml1 $(pwd)/doublecmd.app/Contents/Info.plist

build_unrar()
{
  DEST_DIR=$(pwd)/install/darwin/lib/$CPU_TARGET
  pushd /tmp/unrar  
  make clean lib CXXFLAGS+="-std=c++14 -DSILENT --target=$TARGET" LDFLAGS+="-dylib --target=$TARGET"
  mkdir -p $DEST_DIR && mv libunrar.so $DEST_DIR/libunrar.dylib
  popd
}

build_doublecmd()
{
  # Build all components of Double Commander
  ./build.sh release

  # Copy libraries
  cp -a install/darwin/lib/$CPU_TARGET/*.dylib ./

  # Create *.dmg package
  mkdir -p $BUILD_PACK_DIR
  install/darwin/install.sh $BUILD_PACK_DIR
  pushd $BUILD_PACK_DIR
  mv doublecmd.app 'Double Commander.app'
  codesign --deep --force --verify --verbose --sign '-' 'Double Commander.app'
  hdiutil create -anyowners -volname "Double Commander" -imagekey zlib-level=9 -format UDZO -fs HFS+ -srcfolder 'Double Commander.app' $PACK_DIR/doublecmd-$DC_VER-$DC_REVISION.$lcl.$CPU_TARGET.dmg
  popd

  # Clean DC build dir
  ./clean.sh
  rm -f *.dylib
  rm -rf $BUILD_PACK_DIR
}

# Set processor architecture
export CPU_TARGET=aarch64
export TARGET=arm64-apple-darwin
# Set minimal Mac OS X target version
export MACOSX_DEPLOYMENT_TARGET=11.0

build_unrar
build_doublecmd

# Set processor architecture
export CPU_TARGET=x86_64
export TARGET=x86_64-apple-darwin
# Set minimal Mac OS X target version
export MACOSX_DEPLOYMENT_TARGET=10.11

build_unrar
build_doublecmd
