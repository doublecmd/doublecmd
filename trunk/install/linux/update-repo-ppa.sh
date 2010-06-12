#!/bin/bash

# Set Double Commander version
DC_VER=0.4.6

# Temp directory
DC_TEMP_DIR=/var/tmp/doublecmd-$(date +%y.%m.%d)
# Directory for DC source code
DC_SOURCE_DIR=$DC_TEMP_DIR/doublecmd-$DC_VER

# Export from SVN
rm -rf $DC_TEMP_DIR
mkdir -p $DC_TEMP_DIR
svn export ../../ $DC_SOURCE_DIR

# Save revision number
mkdir $DC_SOURCE_DIR/.svn
cp -a ../../.svn/entries $DC_SOURCE_DIR/.svn/

# Remove help files
rm -rf $DC_SOURCE_DIR/doc/en
rm -rf $DC_SOURCE_DIR/doc/ru

# Prepare debian directory
mkdir -p $DC_SOURCE_DIR/debian
cp -r $DC_SOURCE_DIR/install/linux/deb/* $DC_SOURCE_DIR/debian

# Create archive with source code and upload it to PPA
cd $DC_SOURCE_DIR
debuild -S -sa
cd $DC_TEMP_DIR
dput ppa:alexx2000/doublecmd $(find -name '*.changes')

# Clean
rm -rf $DC_TEMP_DIR
