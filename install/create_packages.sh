#!/bin/sh

# Set Double Commander version
DC_VER=0.4.0
# The new package will be saved here
PACK_DIR="`dirs`"/linux/release

# Temp dir for creating *.tar.bz2 package
BUILD_PACK_DIR=/var/tmp/doublecmd-$(date +%y.%m.%d)

# Create temp dir for building
BUILD_DC_TMP_DIR=/var/tmp/doublecmd-$DC_VER
rm -rf $BUILD_DC_TMP_DIR
svn export ../ $BUILD_DC_TMP_DIR

# Copy package description file
cp linux/description-pak $BUILD_DC_TMP_DIR/

# Copy libraries
cp -a linux/lib/*.so $BUILD_DC_TMP_DIR/

cd $BUILD_DC_TMP_DIR
if [ -z $1 ]
  then export lcl=gtk2
  else export lcl=$1
fi
# Build all components of Double Commander
./_make.sh all

# Create *.rpm package

/usr/local/sbin/checkinstall -R --default --pkgname=doublecmd --pkgversion=$DC_VER --pkgrelease=1.$lcl --pkglicense=GPL --pkggroup=Applications/File --maintainer=Alexx2000@mail.ru --nodoc --pakdir=$PACK_DIR $BUILD_DC_TMP_DIR/install/linux/install.sh

# Create *.deb package

/usr/local/sbin/checkinstall -D --default --pkgname=doublecmd --pkgversion=$DC_VER --pkgrelease=1.$lcl --pkglicense=GPL --pkggroup=Applications/File --maintainer=Alexx2000@mail.ru --nodoc --pakdir=$PACK_DIR $BUILD_DC_TMP_DIR/install/linux/install.sh

# Create *.tgz package

/usr/local/sbin/checkinstall -S --default --pkgname=doublecmd --pkgversion=$DC_VER --pkgrelease=1.$lcl --pkglicense=GPL --pkggroup=Applications/File --maintainer=Alexx2000@mail.ru --nodoc --pakdir=$PACK_DIR $BUILD_DC_TMP_DIR/install/linux/install.sh

# Create *.tar.bz2 package

mkdir -p $BUILD_PACK_DIR
install/linux/install.sh $BUILD_PACK_DIR
cd $BUILD_PACK_DIR
tar -cvjf $PACK_DIR/doublecmd-$DC_VER-1.$lcl.i386.tar.bz2 doublecmd

# Clean DC build dir
rm -rf $BUILD_DC_TMP_DIR
rm -rf $BUILD_PACK_DIR