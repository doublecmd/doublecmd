#!/bin/bash

# This script updates Double Commander Open Build Service (OBS) repository

# Set Double Commander version
DC_VER=0.5.5

# Temp directory
DC_TEMP_DIR=/var/tmp/doublecmd-$(date +%y.%m.%d)
# Directory for DC source code
DC_SOURCE_DIR=$DC_TEMP_DIR/doublecmd-$DC_VER
# Directory for DC help
DC_HELP_DIR=$DC_TEMP_DIR/doublecmd-help-$DC_VER
# Directory for the openSUSE Build Service (OBS)
DC_OBS_DIR=$HOME/.obs
# OBS project home directory
DC_OBS_WEB_DIR=home:Alexx2000
# OBS project directory
DC_OBS_PRJ_DIR=$DC_OBS_DIR/$DC_OBS_WEB_DIR
# DC revision number
DC_REVISION=$(svnversion -n ../../)

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

  # Prepare doublecmd-*.spec file
  cp -a rpm/doublecmd-*.spec $DC_TEMP_DIR

  # Create archive with source code
  pushd $DC_TEMP_DIR
  tar -cvzf doublecmd-$DC_VER.tar.gz doublecmd-$DC_VER

  if [ ! -d "$DC_OBS_DIR" ]
    then
      mkdir -p $DC_OBS_DIR
      cd $DC_OBS_DIR
      osc checkout $DC_OBS_WEB_DIR
    else
      pushd $DC_OBS_PRJ_DIR/doublecmd-gtk
      osc up
      popd
      pushd $DC_OBS_PRJ_DIR/doublecmd-qt
      osc up
      popd      
  fi

  # Upload GTK2 archive to OBS
  rm -f $DC_OBS_PRJ_DIR/doublecmd-gtk/doublecmd-gtk.spec
  rm -f $DC_OBS_PRJ_DIR/doublecmd-gtk/doublecmd-$DC_VER.tar.gz
  mv doublecmd-gtk.spec $DC_OBS_PRJ_DIR/doublecmd-gtk/
  cp -a doublecmd-$DC_VER.tar.gz $DC_OBS_PRJ_DIR/doublecmd-gtk/
  pushd $DC_OBS_PRJ_DIR/doublecmd-gtk
  osc commit doublecmd-gtk.spec doublecmd-$DC_VER.tar.gz -m "Update to revision $DC_REVISION"
  popd
  
  # Upload Qt4 archive to OBS
  rm -f $DC_OBS_PRJ_DIR/doublecmd-qt/doublecmd-qt.spec
  rm -f $DC_OBS_PRJ_DIR/doublecmd-qt/doublecmd-$DC_VER.tar.gz
  mv doublecmd-qt.spec $DC_OBS_PRJ_DIR/doublecmd-qt/
  cp -a doublecmd-$DC_VER.tar.gz $DC_OBS_PRJ_DIR/doublecmd-qt/
  pushd $DC_OBS_PRJ_DIR/doublecmd-qt
  osc commit doublecmd-qt.spec doublecmd-$DC_VER.tar.gz -m "Update to revision $DC_REVISION"
  popd

  popd
}

update_doublecmd_help()
{
  # Export from SVN
  svn export ../../doc $DC_HELP_DIR

  # Remove text files
  rm -f $DC_HELP_DIR/*.txt

  # Prepare doublecmd-help.spec file
  cp -a rpm/doublecmd-help.spec $DC_TEMP_DIR

  # Create archive with source code
  pushd $DC_TEMP_DIR
  tar -cvzf doublecmd-help-$DC_VER.tar.gz doublecmd-help-$DC_VER

  if [ ! -d "$DC_OBS_DIR" ]
    then
      mkdir -p $DC_OBS_DIR
      cd $DC_OBS_DIR
      osc checkout $DC_OBS_WEB_DIR
    else
      pushd $DC_OBS_PRJ_DIR/doublecmd-help
      osc up
      popd
  fi

  # Upload archive to OBS
  rm -f $DC_OBS_PRJ_DIR/doublecmd-help/doublecmd-help.spec
  rm -f $DC_OBS_PRJ_DIR/doublecmd-help/doublecmd-help-$DC_VER.tar.gz
  mv doublecmd-help.spec $DC_OBS_PRJ_DIR/doublecmd-help/
  mv doublecmd-help-$DC_VER.tar.gz $DC_OBS_PRJ_DIR/doublecmd-help/
  cd $DC_OBS_PRJ_DIR/doublecmd-help
  osc commit doublecmd-help.spec doublecmd-help-$DC_VER.tar.gz -m "Update to revision $DC_REVISION"
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

# Clean
rm -rf $DC_TEMP_DIR
