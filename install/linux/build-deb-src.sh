#!/bin/bash

# This script creates a Debian source package

# Set distribution series
DISTRO=( testing )

# The new package will be saved here
PACK_DIR=$(pwd)/release

# Read version number
DC_MAJOR=$(grep 'MajorVersionNr' ../../src/doublecmd.lpi | grep -o '[0-9.]\+')
DC_MINOR=$(grep 'MinorVersionNr' ../../src/doublecmd.lpi | grep -o '[0-9.]\+' || echo 0)
DC_MICRO=$(grep 'RevisionNr' ../../src/doublecmd.lpi | grep -o '[0-9.]\+' || echo 0)
DC_VER=$DC_MAJOR.$DC_MINOR.$DC_MICRO

# Temp directory
DC_TEMP_DIR=/tmp/doublecmd-$(date +%y.%m.%d)
# Directory for DC source code
DC_SOURCE_DIR=$DC_TEMP_DIR/doublecmd-$DC_VER

# Recreate temp directory
rm -rf $DC_TEMP_DIR
mkdir -p $DC_TEMP_DIR

build_doublecmd()
{
  # Export from Git
  rm -rf $DC_SOURCE_DIR
  mkdir $DC_SOURCE_DIR
  git -C ../../ checkout-index -a -f --prefix=$DC_SOURCE_DIR/

  # Clean up
  rm -rf $DC_SOURCE_DIR/.github

  # Save revision number
  DC_REVISION=`$(pwd)/update-revision.sh ../../ $DC_SOURCE_DIR`

  # Create doublecmd-x.x.x.orig.tar.gz
  pushd $DC_SOURCE_DIR/..
  tar -cvzf $DC_TEMP_DIR/doublecmd_$DC_VER.orig.tar.gz doublecmd-$DC_VER
  popd

  # Prepare debian directory
  mkdir -p $DC_SOURCE_DIR/debian
  cp -r $DC_SOURCE_DIR/install/linux/deb/doublecmd/* $DC_SOURCE_DIR/debian
  rm -rf $DC_SOURCE_DIR/debian/patches

  # Create source package for each distro
  for DIST in "${DISTRO[@]}"
  do
    # Update changelog file
    pushd $DC_SOURCE_DIR/debian
    dch -b -D $DIST -v $DC_VER-0+svn$DC_REVISION~$DIST "Non-maintainer upload (revision $DC_REVISION)"
    popd

    # Create archive with source code
    pushd $DC_SOURCE_DIR
    if [ $DIST = ${DISTRO[0]} ]
      then
          debuild -S -sa -d
      else
          debuild -S -sd
    fi
    popd
  done
}

case $1 in
       doublecmd)  build_doublecmd;;
               *)  build_doublecmd;;
esac

# Move archives to release
cd $DC_TEMP_DIR
mkdir -p $PACK_DIR/deb
mv -f *.dsc *.tar.gz *.tar.xz $PACK_DIR/deb/

# Clean
rm -rf $DC_TEMP_DIR
