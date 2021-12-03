#!/bin/bash

# This script updates Double Commander Personal Package Archive (PPA) repository

# Set Double Commander version
DC_VER=1.0.2
# Set Ubuntu series
DISTRO=( xenial zesty artful )

# Temp directory
DC_TEMP_DIR=/var/tmp/doublecmd-$(date +%y.%m.%d)
# Directory for DC source code
DC_SOURCE_DIR=$DC_TEMP_DIR/doublecmd-$DC_VER
# Directory for DC help
DC_HELP_DIR=$DC_TEMP_DIR/doublecmd-help-$DC_VER

# Recreate temp directory
rm -rf $DC_TEMP_DIR
mkdir -p $DC_TEMP_DIR

update_doublecmd()
{
  # Export from SVN
  svn export ../../ $DC_SOURCE_DIR

  # Save revision number
  DC_REVISION=`$(pwd)/update-revision.sh ../../ $DC_SOURCE_DIR`

  # Create doublecmd-x.x.x.orig.tar.gz
  pushd $DC_SOURCE_DIR/..
  tar -cvzf $DC_TEMP_DIR/doublecmd_$DC_VER.orig.tar.gz doublecmd-$DC_VER
  popd

  # Prepare debian directory
  mkdir -p $DC_SOURCE_DIR/debian
  cp -r $DC_SOURCE_DIR/install/linux/deb/doublecmd/* $DC_SOURCE_DIR/debian

  # Create source package for each distro
  for DIST in "${DISTRO[@]}"
  do
    # Update changelog file
    pushd $DC_SOURCE_DIR/debian
    dch -D $DIST -v $DC_VER-0+svn$DC_REVISION~$DIST "Non-maintainer upload (revision $DC_REVISION)"
    popd

    # Create archive with source code
    pushd $DC_SOURCE_DIR
    if [ $DIST = ${DISTRO[0]} ]
      then
          debuild -S -sa
      else
          debuild -S -sd
    fi
    popd
  done
}

update_doublecmd_svn()
{
  # Export from SVN
  svn export ../../ $DC_SOURCE_DIR

  # Save revision number
  DC_REVISION=`$(pwd)/update-revision.sh ../../ $DC_SOURCE_DIR`

  # Prepare debian directory
  mkdir -p $DC_SOURCE_DIR/debian
  cp -r $DC_SOURCE_DIR/install/linux/deb/doublecmd/* $DC_SOURCE_DIR/debian
  echo '1.0' > $DC_SOURCE_DIR/debian/source/format

  # Create source package for each distro
  for DIST in "${DISTRO[@]}"
  do
    # Update changelog file
    pushd $DC_SOURCE_DIR/debian
    dch -D $DIST -v $DC_VER-0+svn$DC_REVISION~$DIST "Non-maintainer upload (revision $DC_REVISION)"
    popd

    # Create archive with source code
    pushd $DC_SOURCE_DIR
    debuild -S -sa
    popd
  done

  # Upload archives to PPA
  cd $DC_TEMP_DIR
  dput -U ppa:alexx2000/doublecmd-svn $(ls -xrt --file-type *.changes)

  # Clean
  rm -rf $DC_TEMP_DIR

  # Exit
  exit 0
}

update_doublecmd_help()
{
  # Create output folder
  mkdir -p $DC_HELP_DIR

  # Save revision number
  DC_REVISION=`$(pwd)/update-revision.sh ../../ $DC_SOURCE_DIR`

  # Copy help files
  cp -r ../../doc/en $DC_HELP_DIR/
  cp -r ../../doc/ru $DC_HELP_DIR/
  cp -r ../../doc/uk $DC_HELP_DIR/

  # Create doublecmd-help-x.x.x.orig.tar.gz
  pushd $DC_HELP_DIR/..
  tar -cvzf $DC_TEMP_DIR/doublecmd-help_$DC_VER.orig.tar.gz doublecmd-help-$DC_VER
  popd

  # Prepare debian directory
  svn export deb/doublecmd-help $DC_HELP_DIR/debian

  # Create source package for each distro
  for DIST in "${DISTRO[@]}"
  do
    # Update changelog file
    pushd $DC_HELP_DIR/debian
    dch -m -D $DIST -v $DC_VER-$DC_REVISION~$DIST "Update to revision $DC_REVISION"
    popd

    # Create archive with source code
    pushd $DC_HELP_DIR
    if [ $DIST = ${DISTRO[0]} ]
      then
          debuild -S -sa
      else
          debuild -S -sd
    fi
    popd
  done
}

update_all()
{
  update_doublecmd
  update_doublecmd_help
}

case $1 in
  doublecmd-help)  update_doublecmd_help;;
   doublecmd-svn)  update_doublecmd_svn;;
       doublecmd)  update_doublecmd;;
               *)  update_all;;
esac

# Upload archives to PPA
cd $DC_TEMP_DIR
dput -U ppa:alexx2000/doublecmd $(ls -xrt --file-type *.changes)

# Clean
rm -rf $DC_TEMP_DIR
