#!/bin/bash

# This script updates Double Commander Personal Package Archive (PPA) repository

# Set Double Commander version
DC_VER=0.5.5

# Temp directory
DC_TEMP_DIR=/var/tmp/doublecmd-$(date +%y.%m.%d)
# Directory for DC source code
DC_SOURCE_DIR=$DC_TEMP_DIR/doublecmd-$DC_VER
# Directory for DC help
DC_HELP_DIR=$DC_TEMP_DIR/doublecmd-help-$DC_VER
# DC revision number
DC_REVISION=$(svnversion -n ../../)

# Set distribution
if [ -z $2 ]
   then
       export DIST=lucid
   else
       export DIST=$2
fi

# Recreate temp directory
rm -rf $DC_TEMP_DIR
mkdir -p $DC_TEMP_DIR

update_doublecmd()
{
  # Export from SVN
  svn export ../../ $DC_SOURCE_DIR

  # Save revision number
  mkdir $DC_SOURCE_DIR/.svn
  cp -a ../../.svn/entries $DC_SOURCE_DIR/.svn/

  # Remove help files
  rm -rf $DC_SOURCE_DIR/doc/en
  rm -rf $DC_SOURCE_DIR/doc/ru
  rm -rf $DC_SOURCE_DIR/doc/uk

  # Prepare debian directory
  mkdir -p $DC_SOURCE_DIR/debian
  cp -r $DC_SOURCE_DIR/install/linux/deb/doublecmd/* $DC_SOURCE_DIR/debian

  # Update changelog file
  pushd $DC_SOURCE_DIR/debian
  dch -m -D $DIST -v $DC_VER-$DC_REVISION~$DIST "Update to revision $DC_REVISION"
  popd

  # Create archive with source code and upload it to PPA
  pushd $DC_SOURCE_DIR
  debuild -S -sa
  popd
}

update_doublecmd_help()
{
  # Export from SVN
  svn export ../../doc $DC_HELP_DIR

  # Remove text files
  rm -f $DC_HELP_DIR/*.txt

  # Prepare debian directory
  svn export deb/doublecmd-help $DC_HELP_DIR/debian

  # Update changelog file
  pushd $DC_HELP_DIR/debian
  dch -m -v $DC_VER-$DC_REVISION~ppa "Update to revision $DC_REVISION"
  popd

  # Create archive with source code
  pushd $DC_HELP_DIR
  debuild -S -sa
  popd
}

update_all()
{
  update_doublecmd
  update_doublecmd_help
}

case $1 in
  doublecmd-help)  update_doublecmd_help;;
       doublecmd)  update_doublecmd;;
               *)  update_all;;
esac

# Upload archives to PPA
cd $DC_TEMP_DIR
dput -U ppa:alexx2000/doublecmd $(find -name '*.changes')

# Clean
rm -rf $DC_TEMP_DIR
