#!/bin/sh

# Set Double Commander version
DC_VER=0.3.5
# The new package will be saved here
PACK_DIR="`dirs`"/linux/release

# Create temp dir for building
BUILD_DC_TMP_DIR=/var/tmp/doublecmd-$DC_VER
rm -rf $BUILD_DC_TMP_DIR
svn export ../ $BUILD_DC_TMP_DIR

mkdir -p $BUILD_DC_TMP_DIR/install/linux
cp linux/install.sh $BUILD_DC_TMP_DIR/install/linux 
cp linux/doublecmd.desktop $BUILD_DC_TMP_DIR/install/linux

cd $BUILD_DC_TMP_DIR
# Build all components of Double Commander
./_make.sh all

# Create *.rpm package

/usr/local/sbin/checkinstall -R --default --pkgname=doublecmd --pkgversion=0.3.5 --pkgrelease=alpha --pkglicense=GPL --pkggroup=Applications/File --maintainer=Alexx2000@mail.ru --nodoc --pakdir=$PACK_DIR $BUILD_DC_TMP_DIR/install/linux/install.sh

# Create *.deb package

/usr/local/sbin/checkinstall -D --default --pkgname=doublecmd --pkgversion=$DC_VER --pkglicense=GPL --pkggroup=Applications/File --maintainer=Alexx2000@mail.ru --nodoc --pakdir=$PACK_DIR $BUILD_DC_TMP_DIR/install/linux/install.sh

# Clean DC build dir
rm -rf $BUILD_DC_TMP_DIR