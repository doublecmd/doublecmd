#!/bin/sh

# Show help
if [ -z $1 ]; then
  echo
  echo "Script for build unrar library"
  echo
  echo "Syntax:"
  echo
  echo "make-unrar.sh <path to unrar source code>"
  echo
  exit 0
fi

# Save destination directory
DEST_DIR=$(pwd)/lib

# Set minimal Mac OS X target version
export MACOSX_DEPLOYMENT_TARGET=10.5

# Go to unrar source directory
cd $1

# Build 32 bit library
rm -f $DEST_DIR/i386/libunrar.dylib
make clean lib CXXFLAGS+="-fPIC -DSILENT -m32" LDFLAGS+="-dylib -arch i386" STRIP=true
mv libunrar.so $DEST_DIR/i386/libunrar.dylib

# Build 64 bit library
rm -f $DEST_DIR/x86_64/libunrar.dylib
make clean lib CXXFLAGS+="-fPIC -DSILENT -m64" LDFLAGS+="-dylib -arch x86_64" STRIP=true
mv libunrar.so $DEST_DIR/x86_64/libunrar.dylib
